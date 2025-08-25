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
    widgets::{Block, Borders, Paragraph, Sparkline, Wrap},
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
const LOG_LINES: usize = 2000; // keep a larger scrollback
const STRIP_HIST_CAP: usize = 60; // ~3.6s at 60ms
const RAW_MIN: i32 = 0;
const RAW_MAX: i32 = 4096;

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
    Learn, // button mapping learn mode
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
    rx: i32,
    ry: i32,
    rx_hist: VecDeque<i32>,
    ry_hist: VecDeque<i32>,
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

fn push_event_line(s: &mut UiState, line: impl Into<String>) {
    let at_bottom = s.log_scroll == 0;
    s.log.push_back(line.into());
    while s.log.len() > LOG_LINES {
        s.log.pop_front();
    }
    if at_bottom {
        s.log_scroll = 0; // keep following new lines
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
            // convert "LTop" -> CellId
            let mut out = HashMap::new();
            for (k, v) in map {
                if let Some(id) = match k.as_str() {
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
                } {
                    out.insert(id, v);
                }
            }
            return out;
        }
    }
    // default naive mapping (learn-mode will fix quickly)
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
        // load bindings
        s.bindings = load_bindings();
        let _ = tx.send(s);
    }

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

                            // learn-mode: on the first DOWN for the current target, store mapping
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
                            let val = ev.value().clamp(RAW_MIN, RAW_MAX);
                            if code == 3 {
                                s.pad.rx = val;
                                s.pad.rx_hist.push_back(val);
                                while s.pad.rx_hist.len() > STRIP_HIST_CAP {
                                    s.pad.rx_hist.pop_front();
                                }
                                let _ = tx.send(s);
                            } else if code == 4 {
                                s.pad.ry = val;
                                s.pad.ry_hist.push_back(val);
                                while s.pad.ry_hist.len() > STRIP_HIST_CAP {
                                    s.pad.ry_hist.pop_front();
                                }
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
        let p0 = &s.calib.points[0];
        let p1 = &s.calib.points[1];
        let p2 = &s.calib.points[2];

        let x0 = p0.raw_x.unwrap() as f32;
        let x1 = p1.raw_x.unwrap() as f32;
        let y0 = p0.raw_y.unwrap() as f32;
        let y2 = p2.raw_y.unwrap() as f32;

        let raw_dx = (x1 - x0).abs().max(1.0);
        let raw_dy = (y2 - y0).abs().max(1.0);

        s.calib.scale_x = 0.8f32 / raw_dx;
        s.calib.off_x = 0.1f32 - s.calib.scale_x * x0;

        s.calib.scale_y = 0.8f32 / raw_dy;
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

    // --- Events (scrollable) ---
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
    let end = total;

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

    // simple scrollbar
    if total > inner_height {
        let ratio_top = start as f64 / (total.max(1) as f64);
        let ratio_h = (inner_height as f64) / (total as f64);
        let sb_x = inner.x + inner.width.saturating_sub(1);
        let sb_y = inner.y;
        let sb_h = inner.height;
        let thumb_h = ((sb_h as f64 * ratio_h).max(1.0)).round() as u16;
        let thumb_y = sb_y + ((sb_h as f64 * ratio_top).round() as u16);

        for i in 0..sb_h {
            let ch = if i >= (thumb_y - sb_y) && i < (thumb_y - sb_y + thumb_h) {
                '█'
            } else {
                '▕'
            };
            let cell = f.buffer_mut().get_mut(sb_x, sb_y + i);
            cell.set_symbol(&ch.to_string());
            cell.set_style(Style::default().fg(Color::DarkGray));
        }
    }
}

// side panel + strip sparklines
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

    // rows: top, middle, bottom, strip-history
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

    // middle split
    let mid = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Length(12), Constraint::Min(6)])
        .split(rows[1]);

    draw_cell(
        f,
        s,
        mid[0],
        side,
        if matches!(side, Side::Left) {
            CellId::LTall
        } else {
            CellId::RTall
        },
    );

    let right_mid = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(mid[1]);

    draw_cell(
        f,
        s,
        right_mid[0],
        side,
        if matches!(side, Side::Left) {
            CellId::LMidUR
        } else {
            CellId::RMidUR
        },
    );
    draw_cell(
        f,
        s,
        right_mid[1],
        side,
        if matches!(side, Side::Left) {
            CellId::LMidLR
        } else {
            CellId::RMidLR
        },
    );

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

    // strip history sparkline (inverted so "bottom=highest")
    let (hist, title) = if matches!(side, Side::Left) {
        (&s.pad.rx_hist, "Strip RX")
    } else {
        (&s.pad.ry_hist, "Strip RY")
    };
    // normalize & invert
    let data: Vec<u64> = hist
        .iter()
        .map(|&v| (RAW_MAX - v.clamp(RAW_MIN, RAW_MAX)) as u64)
        .collect();
    let spark = Sparkline::default()
        .block(Block::default().borders(Borders::ALL).title(title))
        .data(&data);
    f.render_widget(spark, rows[3]);
}

fn draw_cell(f: &mut Frame, s: &UiState, area: Rect, _side: Side, id: CellId) {
    // resolve mapping for this cell
    let code = s.bindings.get(&id).copied();
    let is_down = code
        .and_then(|c| s.pad.pressed.get(&c).copied())
        .unwrap_or(false);

    let ttl = if let Some(c) = code {
        format!("{}  (code {})", cell_label(id), c)
    } else {
        format!("{}  (unmapped)", cell_label(id))
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

    // normalize pen to [0..1], optional affine if solved
    let (mut nx, mut ny) = (
        (s.pen.x - RAW_MIN) as f32 / (RAW_MAX - RAW_MIN) as f32,
        (s.pen.y - RAW_MIN) as f32 / (RAW_MAX - RAW_MIN) as f32,
    );
    if s.calib.solved {
        nx = (s.calib.scale_x * s.pen.x as f32 + s.calib.off_x).clamp(0.0, 1.0);
        ny = (s.calib.scale_y * s.pen.y as f32 + s.calib.off_y).clamp(0.0, 1.0);
    } else {
        nx = nx.clamp(0.0, 1.0);
        ny = ny.clamp(0.0, 1.0);
    }

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
