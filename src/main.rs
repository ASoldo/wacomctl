use anyhow::{Context, Result};

use crossterm::{
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event as TermEvent, KeyCode as TermKeyCode,
        KeyEvent, KeyModifiers, MouseEvent, MouseEventKind,
    },
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use evdev::{Device, EventType, KeyCode};
use ratatui::{
    prelude::*,
    widgets::canvas::{Canvas, Points},
    widgets::{Block, Borders, Paragraph},
};
use std::{
    collections::{BTreeMap, HashMap, VecDeque},
    fs,
    io::{self, Stdout},
    path::{Path, PathBuf},
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};
use tokio::{sync::watch, task, time::sleep};

// --------------------------------- constants ---------------------------------

const TICK_MS: u64 = 60;
const LOG_LINES: usize = 2000;

// -------------------------------- app state ----------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Side {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Inspect,
    Calibrate,
    Learn,
}
impl Default for Mode {
    fn default() -> Self {
        Mode::Inspect
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum CellId {
    // left
    LTop,
    LTall,
    LMidUR,
    LMidLR,
    LBottom,
    // right
    RTop,
    RTall,
    RMidUR,
    RMidLR,
    RBottom,
}
const CELLS: [CellId; 10] = [
    CellId::LTop,
    CellId::LTall,
    CellId::LMidUR,
    CellId::LMidLR,
    CellId::LBottom,
    CellId::RTop,
    CellId::RTall,
    CellId::RMidUR,
    CellId::RMidLR,
    CellId::RBottom,
];
fn cell_side(c: CellId) -> Side {
    match c {
        CellId::LTop | CellId::LTall | CellId::LMidUR | CellId::LMidLR | CellId::LBottom => {
            Side::Left
        }
        _ => Side::Right,
    }
}
fn cell_label(c: CellId) -> &'static str {
    match c {
        CellId::LTop | CellId::RTop => "Top",
        CellId::LTall | CellId::RTall => "Tall",
        CellId::LMidUR | CellId::RMidUR => "Mid-UR",
        CellId::LMidLR | CellId::RMidLR => "Mid-LR",
        CellId::LBottom | CellId::RBottom => "Bottom",
    }
}

#[derive(Debug, Clone, Default)]
struct PadUi {
    pressed: BTreeMap<u16, bool>, // raw code -> down
    // current raw values
    rx: i32,
    ry: i32,
    // observed ranges (updated live)
    rx_min: i32,
    rx_max: i32,
    ry_min: i32,
    ry_max: i32,
}

#[derive(Debug, Clone, Default)]
struct PenUi {
    x: i32,
    y: i32,
    touching: bool,
}

#[derive(Debug, Clone)]
struct CalibPoint {
    target_x: f32,
    target_y: f32,
    raw_x: Option<i32>,
    raw_y: Option<i32>,
}
impl CalibPoint {
    fn done(&self) -> bool {
        self.raw_x.is_some() && self.raw_y.is_some()
    }
}

#[derive(Debug, Clone, Default)]
struct Calibration {
    active: bool,
    stage: usize,
    points: Vec<CalibPoint>,
    // simple affine
    scale_x: f32,
    scale_y: f32,
    off_x: f32,
    off_y: f32,
    solved: bool,
}

#[derive(Debug, Clone, Default)]
struct UiState {
    pad_name: String,
    pen_name: String,

    pad: PadUi,
    pen: PenUi,

    mode: Mode,
    calib: Calibration,

    // button mapping: cell -> evdev key code (u16)
    bindings: HashMap<CellId, u16>,
    // learn-mode stage index into CELLS
    learn_stage: usize,

    log: VecDeque<String>,
    log_scroll: i32, // 0 = bottom; positive = lines scrolled up
}

// ------------------------------- small helpers -------------------------------

fn norm_preview(v: i32, samples: impl Iterator<Item = i32>) -> f32 {
    let mut min = i32::MAX;
    let mut max = i32::MIN;
    let mut count = 0;
    for s in samples {
        min = min.min(s);
        max = max.max(s);
        count += 1;
    }
    if count >= 2 && max > min {
        ((v - min) as f32 / (max - min) as f32).clamp(0.0, 1.0)
    } else {
        // before we have 2 samples, keep a very wide fallback so it still moves
        (v as f32 / 65535.0).clamp(0.0, 1.0)
    }
}

fn push_event_line(s: &mut UiState, line: impl Into<String>) {
    let at_bottom = s.log_scroll == 0;
    s.log.push_back(line.into());
    while s.log.len() > LOG_LINES {
        s.log.pop_front();
    }
    if at_bottom {
        s.log_scroll = 0;
    }
}

fn fmt_ts(ts: SystemTime) -> (u64, u32) {
    match ts.duration_since(UNIX_EPOCH) {
        Ok(d) => ((d.as_secs() % 1000) as u64, d.subsec_micros()),
        Err(_) => (0, 0),
    }
}

fn cfg_path() -> PathBuf {
    let home = std::env::var("HOME").unwrap_or_else(|_| ".".into());
    Path::new(&home).join(".config/wacomctl/mapping.json")
}

fn load_bindings() -> HashMap<CellId, u16> {
    let p = cfg_path();
    if let Ok(bytes) = fs::read(&p) {
        if let Ok(map) = serde_json::from_slice::<HashMap<String, u16>>(&bytes) {
            let mut out = HashMap::new();
            for (k, v) in map {
                let id_opt = match k.as_str() {
                    "LTop" => Some(CellId::LTop),
                    "LTall" => Some(CellId::LTall),
                    "LMidUR" => Some(CellId::LMidUR),
                    "LMidLR" => Some(CellId::LMidLR),
                    "LBottom" => Some(CellId::LBottom),
                    "RTop" => Some(CellId::RTop),
                    "RTall" => Some(CellId::RTall),
                    "RMidUR" => Some(CellId::RMidUR),
                    "RMidLR" => Some(CellId::RMidLR),
                    "RBottom" => Some(CellId::RBottom),
                    _ => None,
                };
                if let Some(id) = id_opt {
                    out.insert(id, v);
                }
            }
            return out;
        }
    }
    // defaults (learn-mode will fix quickly)
    let defaults = [
        (CellId::LTop, KeyCode::BTN_0.code()),
        (CellId::LTall, KeyCode::BTN_1.code()),
        (CellId::LMidUR, KeyCode::BTN_3.code()),
        (CellId::LMidLR, KeyCode::BTN_2.code()),
        (CellId::LBottom, KeyCode::BTN_5.code()),
        (CellId::RTop, KeyCode::BTN_4.code()),
        (CellId::RTall, KeyCode::BTN_6.code()),
        (CellId::RMidUR, KeyCode::BTN_8.code()),
        (CellId::RMidLR, KeyCode::BTN_7.code()),
        (CellId::RBottom, KeyCode::BTN_9.code()),
    ];
    defaults.into_iter().collect()
}

fn save_bindings(b: &HashMap<CellId, u16>) -> Result<()> {
    let p = cfg_path();
    if let Some(dir) = p.parent() {
        fs::create_dir_all(dir).ok();
    }
    let mut ser: HashMap<String, u16> = HashMap::new();
    for (k, v) in b {
        let name = match k {
            CellId::LTop => "LTop",
            CellId::LTall => "LTall",
            CellId::LMidUR => "LMidUR",
            CellId::LMidLR => "LMidLR",
            CellId::LBottom => "LBottom",
            CellId::RTop => "RTop",
            CellId::RTall => "RTall",
            CellId::RMidUR => "RMidUR",
            CellId::RMidLR => "RMidLR",
            CellId::RBottom => "RBottom",
        };
        ser.insert(name.to_string(), *v);
    }
    let bytes = serde_json::to_vec_pretty(&ser)?;
    fs::write(&p, &bytes).with_context(|| format!("writing {}", p.display()))?;
    Ok(())
}

// ------------------------------- discovery -----------------------------------

fn open_from_env(var: &str) -> Option<(String, Device)> {
    if let Ok(p) = std::env::var(var) {
        match Device::open(&p) {
            Ok(d) => {
                eprintln!("Using {var} = {p}");
                return Some((p, d));
            }
            Err(e) => eprintln!("{var}={p} open failed: {e}"),
        }
    }
    None
}

fn find_device<F>(predicate: F) -> Option<(String, Device)>
where
    F: Fn(&Device) -> bool,
{
    if let Ok(entries) = fs::read_dir("/dev/input/by-id") {
        for ent in entries.flatten() {
            let p: PathBuf = ent.path();
            if !p
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .contains("event")
            {
                continue;
            }
            if let Ok(tgt) = fs::canonicalize(&p) {
                if let Ok(d) = Device::open(&tgt) {
                    if predicate(&d) {
                        return Some((tgt.display().to_string(), d));
                    }
                }
            }
        }
    }
    if let Ok(entries) = fs::read_dir("/dev/input") {
        for ent in entries.flatten() {
            let p = ent.path();
            if !p
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .starts_with("event")
            {
                continue;
            }
            if let Ok(d) = Device::open(&p) {
                if predicate(&d) {
                    return Some((p.display().to_string(), d));
                }
            }
        }
    }
    None
}

fn is_wacom_pad(d: &Device) -> bool {
    let n = d.name().unwrap_or("");
    n.contains("Wacom") && n.contains("Pad")
}
fn is_wacom_pen(d: &Device) -> bool {
    let n = d.name().unwrap_or("");
    n.contains("Wacom") && n.contains("Pen")
}

// ------------------------------- readers -------------------------------------

async fn pad_reader(tx: watch::Sender<UiState>, rx: watch::Receiver<UiState>) {
    let (path, mut dev) = loop {
        if let Some(t) = open_from_env("WACOM_PAD") {
            break t;
        }
        if let Some(t) = find_device(is_wacom_pad) {
            break t;
        }
        eprintln!("No readable Wacom *Pad* yet; retrying…");
        sleep(Duration::from_millis(1200)).await;
    };

    {
        let mut s = rx.borrow().clone();
        s.pad_name = format!("{} ({})", dev.name().unwrap_or("Wacom Pad"), path);
        s.bindings = load_bindings();
        let _ = tx.send(s);
    }

    let clamp = |v: i32, lo: i32, hi: i32| v.clamp(lo, hi);

    loop {
        match dev.fetch_events() {
            Ok(iter) => {
                let mut any = false;
                for ev in iter {
                    any = true;
                    let mut s = rx.borrow().clone();

                    match ev.event_type() {
                        EventType::KEY => {
                            let down = ev.value() == 1;
                            let code = ev.code();
                            s.pad.pressed.insert(code, down);

                            if s.mode == Mode::Learn && down {
                                if let Some(target) = CELLS.get(s.learn_stage).copied() {
                                    s.bindings.insert(target, code);
                                    push_event_line(
                                        &mut s,
                                        format!("Learn: {:?} bound to code {}", target, code),
                                    );
                                    s.learn_stage += 1;
                                    if s.learn_stage >= CELLS.len() {
                                        s.mode = Mode::Inspect;
                                        if let Err(e) = save_bindings(&s.bindings) {
                                            push_event_line(&mut s, format!("Save failed: {e}"));
                                        } else {
                                            push_event_line(&mut s, "Mapping saved");
                                        }
                                    }
                                }
                            }

                            let k = KeyCode::new(code);
                            let (ss, us) = fmt_ts(ev.timestamp());
                            push_event_line(
                                &mut s,
                                format!(
                                    "[{:>3}.{:06}] PAD KEY {:?}({}) -> {}",
                                    ss,
                                    us,
                                    k,
                                    code,
                                    if down { "DOWN" } else { "UP" }
                                ),
                            );
                            let _ = tx.send(s);
                        }
                        EventType::ABSOLUTE => {
                            let code = ev.code();
                            let val = ev.value();
                            if code == 3 {
                                // RX
                                // update live range tracking
                                s.pad.rx_min = if s.pad.rx_min == 0 {
                                    val
                                } else {
                                    s.pad.rx_min.min(val)
                                };
                                s.pad.rx_max = s.pad.rx_max.max(val);
                                s.pad.rx = clamp(val, s.pad.rx_min, s.pad.rx_max);
                                let _ = tx.send(s);
                            } else if code == 4 {
                                // RY
                                s.pad.ry_min = if s.pad.ry_min == 0 {
                                    val
                                } else {
                                    s.pad.ry_min.min(val)
                                };
                                s.pad.ry_max = s.pad.ry_max.max(val);
                                s.pad.ry = clamp(val, s.pad.ry_min, s.pad.ry_max);
                                let _ = tx.send(s);
                            }
                        }
                        _ => {}
                    }
                }
                if !any {
                    sleep(Duration::from_millis(8)).await;
                }
            }
            Err(e) => {
                eprintln!("pad read error: {e}");
                sleep(Duration::from_millis(300)).await;
            }
        }
    }
}

async fn pen_reader(tx: watch::Sender<UiState>, rx: watch::Receiver<UiState>) {
    let (path, mut dev) = loop {
        if let Some(t) = open_from_env("WACOM_PEN") {
            break t;
        }
        if let Some(t) = find_device(is_wacom_pen) {
            break t;
        }
        eprintln!("No readable Wacom *Pen* yet; retrying…");
        sleep(Duration::from_millis(1200)).await;
    };

    {
        let mut s = rx.borrow().clone();
        s.pen_name = format!("{} ({})", dev.name().unwrap_or("Wacom Pen"), path);
        let _ = tx.send(s);
    }

    loop {
        match dev.fetch_events() {
            Ok(iter) => {
                let mut any = false;
                for ev in iter {
                    any = true;
                    match ev.event_type() {
                        EventType::ABSOLUTE => {
                            let code = ev.code();
                            let mut s = rx.borrow().clone();
                            if code == 0 {
                                s.pen.x = ev.value();
                                let _ = tx.send(s);
                            } else if code == 1 {
                                s.pen.y = ev.value();
                                let _ = tx.send(s);
                            }
                        }
                        EventType::KEY => {
                            let mut s = rx.borrow().clone();
                            s.pen.touching = ev.value() != 0;
                            let _ = tx.send(s);
                        }
                        _ => {}
                    }
                }
                if !any {
                    sleep(Duration::from_millis(8)).await;
                }
            }
            Err(e) => {
                eprintln!("pen read error: {e}");
                sleep(Duration::from_millis(300)).await;
            }
        }
    }
}

// ----------------------------- calibration math ------------------------------

fn start_calibration(s: &mut UiState) {
    s.mode = Mode::Calibrate;
    s.calib.active = true;
    s.calib.solved = false;
    s.calib.stage = 0;
    s.calib.points = vec![
        CalibPoint {
            target_x: 0.1,
            target_y: 0.1,
            raw_x: None,
            raw_y: None,
        }, // TL
        CalibPoint {
            target_x: 0.9,
            target_y: 0.1,
            raw_x: None,
            raw_y: None,
        }, // TR
        CalibPoint {
            target_x: 0.9,
            target_y: 0.9,
            raw_x: None,
            raw_y: None,
        }, // BR
        CalibPoint {
            target_x: 0.1,
            target_y: 0.9,
            raw_x: None,
            raw_y: None,
        }, // BL
    ];
}
fn finish_calibration(s: &mut UiState) {
    if s.calib.points.iter().all(|p| p.done()) {
        let p0 = &s.calib.points[0]; // TL
        let p1 = &s.calib.points[1]; // TR
        let p2 = &s.calib.points[2]; // BR

        let x0 = p0.raw_x.unwrap() as f32;
        let x1 = p1.raw_x.unwrap() as f32;
        let y0 = p0.raw_y.unwrap() as f32;
        let y2 = p2.raw_y.unwrap() as f32;

        // PRESERVE SIGN so Y won't get inverted by accident.
        let raw_dx = (x1 - x0).max(1.0e-6);
        let raw_dy = (y2 - y0).max(1.0e-6);

        s.calib.scale_x = 0.8f32 / raw_dx;
        s.calib.off_x = 0.1f32 - s.calib.scale_x * x0;

        s.calib.scale_y = 0.8f32 / raw_dy; // may be negative -> correct orientation
        s.calib.off_y = 0.1f32 - s.calib.scale_y * y0;

        s.calib.solved = true;
    }
    s.calib.active = false;
    s.mode = Mode::Inspect;
}

// ---------------------------------- TUI --------------------------------------

#[tokio::main]
async fn main() -> Result<()> {
    let (tx, rx) = watch::channel(UiState::default());

    task::spawn(pad_reader(tx.clone(), rx.clone()));
    task::spawn(pen_reader(tx.clone(), rx.clone()));

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    let mut last_redraw = Instant::now();

    'ui: loop {
        while event::poll(Duration::from_millis(0))? {
            match event::read()? {
                TermEvent::Key(KeyEvent {
                    code, modifiers, ..
                }) => match (code, modifiers) {
                    (TermKeyCode::Char('l'), _) => {
                        let mut s = rx.borrow().clone();
                        s.mode = Mode::Learn;
                        s.learn_stage = 0;
                        push_event_line(&mut s, "LEARN MODE: press buttons as prompted");
                        let _ = tx.send(s);
                    }
                    (TermKeyCode::Char('c'), _) => {
                        let mut s = rx.borrow().clone();
                        if s.mode == Mode::Calibrate {
                            s.calib.active = false;
                            s.mode = Mode::Inspect;
                            push_event_line(&mut s, "Calibration canceled");
                        } else {
                            push_event_line(&mut s, "Calibration started");
                            start_calibration(&mut s);
                        }
                        let _ = tx.send(s);
                    }

                    // --- Events pane scrolling ---
                    (TermKeyCode::PageUp, _) => {
                        let mut s = rx.borrow().clone();
                        s.log_scroll = s.log_scroll.saturating_add(10);
                        let _ = tx.send(s);
                    }
                    (TermKeyCode::PageDown, _) => {
                        let mut s = rx.borrow().clone();
                        s.log_scroll = (s.log_scroll - 10).max(0);
                        let _ = tx.send(s);
                    }
                    (TermKeyCode::Home, _) => {
                        let mut s = rx.borrow().clone();
                        s.log_scroll = s.log.len() as i32;
                        let _ = tx.send(s);
                    }
                    (TermKeyCode::End, _) => {
                        let mut s = rx.borrow().clone();
                        s.log_scroll = 0;
                        let _ = tx.send(s);
                    }
                    (TermKeyCode::Char('k'), _) => {
                        let mut s = rx.borrow().clone();
                        s.log_scroll = s.log_scroll.saturating_add(1);
                        let _ = tx.send(s);
                    }
                    (TermKeyCode::Char('j'), _) => {
                        let mut s = rx.borrow().clone();
                        s.log_scroll = (s.log_scroll - 1).max(0);
                        let _ = tx.send(s);
                    }
                    (TermKeyCode::Char('x'), _) => {
                        let mut s = rx.borrow().clone();
                        s.log.clear();
                        s.log_scroll = 0;
                        let _ = tx.send(s);
                    }

                    (TermKeyCode::Char('q'), KeyModifiers::NONE) | (TermKeyCode::Esc, _) => {
                        break 'ui;
                    }
                    _ => {}
                },
                TermEvent::Mouse(MouseEvent { kind, .. }) => match kind {
                    MouseEventKind::ScrollUp => {
                        let mut s = rx.borrow().clone();
                        s.log_scroll = s.log_scroll.saturating_add(3);
                        let _ = tx.send(s);
                    }
                    MouseEventKind::ScrollDown => {
                        let mut s = rx.borrow().clone();
                        s.log_scroll = (s.log_scroll - 3).max(0);
                        let _ = tx.send(s);
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        // calibration capture
        if rx.borrow().calib.active && rx.borrow().pen.touching {
            let mut s = rx.borrow().clone();
            let stg = s.calib.stage.min(s.calib.points.len() - 1);
            if !s.calib.points[stg].done() {
                s.calib.points[stg].raw_x = Some(s.pen.x);
                s.calib.points[stg].raw_y = Some(s.pen.y);
                let msg = format!("Captured P{} -> raw({}, {})", stg + 1, s.pen.x, s.pen.y);
                push_event_line(&mut s, msg);
                let _ = tx.send(s);
            }
        } else if rx.borrow().calib.active {
            let mut s = rx.borrow().clone();
            if s.calib.points[s.calib.stage].done() {
                s.calib.stage += 1;
                if s.calib.stage >= s.calib.points.len() {
                    finish_calibration(&mut s);
                    let msg = if s.calib.solved {
                        format!(
                            "Calibration OK: scale=({:.5},{:.5}) off=({:.5},{:.5})",
                            s.calib.scale_x, s.calib.scale_y, s.calib.off_x, s.calib.off_y
                        )
                    } else {
                        "Calibration finished".to_string()
                    };
                    push_event_line(&mut s, msg);
                }
                let _ = tx.send(s);
            }
        }

        if last_redraw.elapsed() >= Duration::from_millis(TICK_MS) {
            let state = rx.borrow().clone();
            terminal.draw(|f| draw(f, &state))?;
            last_redraw = Instant::now();
        }

        sleep(Duration::from_millis(8)).await;
    }

    disable_raw_mode()?;
    let mut out: Stdout = io::stdout();
    execute!(out, DisableMouseCapture, LeaveAlternateScreen)?;
    Ok(())
}

fn draw(f: &mut Frame, s: &UiState) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),
            Constraint::Min(12),
            Constraint::Length(8),
        ])
        .split(f.area());

    // header
    let title = format!(
        "wacomctl — Pad: {} | Pen: {}  —  [q] quit  [c] calibrate  [l] learn buttons",
        if s.pad_name.is_empty() {
            "…"
        } else {
            &s.pad_name
        },
        if s.pen_name.is_empty() {
            "…"
        } else {
            &s.pen_name
        }
    );
    let hdr = Paragraph::new(title).block(Block::default().borders(Borders::ALL).title("Status"));
    f.render_widget(hdr, chunks[0]);

    // main row
    let main = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Length(30),
            Constraint::Min(30),
            Constraint::Length(30),
        ])
        .split(chunks[1]);

    draw_button_cluster(f, s, Side::Left, main[0]);
    draw_screen(f, s, main[1]);
    draw_button_cluster(f, s, Side::Right, main[2]);

    // Events
    draw_events(f, s, chunks[2]);
}

fn draw_events(f: &mut Frame, s: &UiState, area: Rect) {
    let block = Block::default().borders(Borders::ALL).title("Events");
    f.render_widget(block, area);

    let inner = Rect {
        x: area.x + 1,
        y: area.y + 1,
        width: area.width.saturating_sub(2),
        height: area.height.saturating_sub(2),
    };

    let inner_height = inner.height as usize;
    let total = s.log.len();
    let mut start = total.saturating_sub(inner_height);
    if s.log_scroll > 0 {
        let off = s.log_scroll as usize;
        start = start.saturating_sub(off).min(total);
    }

    let mut text = String::new();
    for l in s.log.iter().skip(start).take(inner_height) {
        text.push_str(l);
        text.push('\n');
    }
    if s.mode == Mode::Learn {
        if let Some(target) = CELLS.get(s.learn_stage) {
            text.push_str(&format!(
                "\nLEARN: Press {} {} …",
                if matches!(cell_side(*target), Side::Left) {
                    "LEFT"
                } else {
                    "RIGHT"
                },
                cell_label(*target)
            ));
        } else {
            text.push_str("\nLEARN: complete");
        }
    }

    let p = Paragraph::new(text);
    f.render_widget(p, inner);
}

// side panel + numeric strip readout
fn draw_button_cluster(f: &mut Frame, s: &UiState, side: Side, area: Rect) {
    let block = Block::default()
        .borders(Borders::ALL)
        .title(if matches!(side, Side::Left) {
            "Left Pad"
        } else {
            "Right Pad"
        });
    f.render_widget(block.clone(), area);
    let inner = Rect {
        x: area.x + 1,
        y: area.y + 1,
        width: area.width.saturating_sub(2),
        height: area.height.saturating_sub(2),
    };

    // rows: top, middle, bottom, strip-value
    let rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),
            Constraint::Length(6),
            Constraint::Length(3),
            Constraint::Min(3),
        ])
        .split(inner);

    // Top
    draw_cell(
        f,
        s,
        rows[0],
        side,
        if matches!(side, Side::Left) {
            CellId::LTop
        } else {
            CellId::RTop
        },
    );

    // Middle split — mirror layout: Left pad Tall on the LEFT (narrow), Right pad Tall on the RIGHT (narrow)
    let mid = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(rows[1]);

    if matches!(side, Side::Left) {
        // LEFT PAD: Tall in mid[0] (narrow), mids stacked in mid[1] (wide)
        draw_cell(f, s, mid[0], side, CellId::LTall);
        let pair = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(mid[1]);
        draw_cell(f, s, pair[0], side, CellId::LMidUR);
        draw_cell(f, s, pair[1], side, CellId::LMidLR);
    } else {
        // RIGHT PAD: mids stacked in mid[0] (wide), Tall in mid[1] (narrow)
        let pair = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(mid[0]);
        draw_cell(f, s, pair[0], side, CellId::RMidUR);
        draw_cell(f, s, pair[1], side, CellId::RMidLR);
        draw_cell(f, s, mid[1], side, CellId::RTall);
    }

    // Bottom
    draw_cell(
        f,
        s,
        rows[2],
        side,
        if matches!(side, Side::Left) {
            CellId::LBottom
        } else {
            CellId::RBottom
        },
    );

    // Strip value panel
    let (val, mn, mx, title) = if matches!(side, Side::Left) {
        (s.pad.rx, s.pad.rx_min, s.pad.rx_max, "Strip RX")
    } else {
        (s.pad.ry, s.pad.ry_min, s.pad.ry_max, "Strip RY")
    };
    let block = Block::default().borders(Borders::ALL).title(title);
    f.render_widget(block, rows[3]);

    let inner = Rect {
        x: rows[3].x + 1,
        y: rows[3].y + 1,
        width: rows[3].width.saturating_sub(2),
        height: rows[3].height.saturating_sub(2),
    };
    let span = (mx - mn).max(1) as f32;
    let pct = ((val - mn) as f32 / span * 100.0).clamp(0.0, 100.0);
    let txt = format!("val: {}\nmin: {}  max: {}\n({:.1}%)", val, mn, mx, pct);
    f.render_widget(Paragraph::new(txt), inner);
}

fn draw_cell(f: &mut Frame, s: &UiState, area: Rect, _side: Side, id: CellId) {
    // resolve mapping for this cell
    let code = s.bindings.get(&id).copied();
    let is_down = code
        .and_then(|c| s.pad.pressed.get(&c).copied())
        .unwrap_or(false);

    let ttl = if let Some(c) = code {
        format!("{}:{}", cell_label(id), c)
    } else {
        format!("{}:(unmapped)", cell_label(id))
    };

    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(if is_down {
            Style::default().fg(Color::Green)
        } else {
            Style::default()
        })
        .title(ttl);
    f.render_widget(block, area);
}

fn draw_screen(f: &mut Frame, s: &UiState, area: Rect) {
    let title = match s.mode {
        Mode::Inspect => "Screen",
        Mode::Calibrate => "Screen — CALIBRATE (tap red dots) — [c] cancel",
        Mode::Learn => "Screen — LEARN (see Events for prompt) — [l] to restart",
    };
    let block = Block::default().borders(Borders::ALL).title(title);
    f.render_widget(block, area);

    let inner = Rect {
        x: area.x + 1,
        y: area.y + 1,
        width: area.width.saturating_sub(2),
        height: area.height.saturating_sub(2),
    };

    // normalize pen to [0..1], optional affine if solved (preserves Y sign)
    let (mut nx, mut ny) = (s.pen.x as f32, s.pen.y as f32);

    if s.calib.solved {
        nx = (s.calib.scale_x * nx + s.calib.off_x).clamp(0.0, 1.0);
        ny = (s.calib.scale_y * ny + s.calib.off_y).clamp(0.0, 1.0);
    } else if s.calib.active {
        // use captured raw points so far for live preview scaling
        let xs = s.calib.points.iter().filter_map(|p| p.raw_x);
        let ys = s.calib.points.iter().filter_map(|p| p.raw_y);
        nx = norm_preview(s.pen.x, xs);
        ny = norm_preview(s.pen.y, ys);
    } else {
        // non-calibration fallback (very wide range so it won’t clamp immediately)
        nx = (nx / 65535.0).clamp(0.0, 1.0);
        ny = (ny / 65535.0).clamp(0.0, 1.0);
    }

    ny = 1.0 - ny;

    let canvas = Canvas::default()
        .x_bounds([0.0, 1.0])
        .y_bounds([0.0, 1.0])
        .paint(|ctx| {
            if s.calib.active {
                for (i, p) in s.calib.points.iter().enumerate() {
                    let pts = Points {
                        coords: &[(p.target_x as f64, p.target_y as f64)],
                        color: if i == s.calib.stage {
                            Color::Red
                        } else {
                            Color::DarkGray
                        },
                    };
                    ctx.draw(&pts);
                }
            }
            let pen_pts = Points {
                coords: &[(nx as f64, ny as f64)],
                color: if s.pen.touching {
                    Color::Cyan
                } else {
                    Color::Gray
                },
            };
            ctx.draw(&pen_pts);
        });

    f.render_widget(canvas, inner);
}

// ------------------------------- serde helpers -------------------------------

impl serde::Serialize for CellId {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(match self {
            CellId::LTop => "LTop",
            CellId::LTall => "LTall",
            CellId::LMidUR => "LMidUR",
            CellId::LMidLR => "LMidLR",
            CellId::LBottom => "LBottom",
            CellId::RTop => "RTop",
            CellId::RTall => "RTall",
            CellId::RMidUR => "RMidUR",
            CellId::RMidLR => "RMidLR",
            CellId::RBottom => "RBottom",
        })
    }
}
