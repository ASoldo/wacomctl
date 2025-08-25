<img width="1249" height="694" alt="image" src="https://github.com/user-attachments/assets/2e1cfcc0-a2ee-4191-b346-6b68c623de08" />

# wacomctl

A simple TUI utility for inspecting and calibrating Wacom tablet pad and pen input events.

## Usage

1. Ensure you have Rust installed and your Wacom devices are accessible at the correct `/dev/input/event*` paths.

2. Run the application specifying the pad and pen device paths:

```bash
WACOM_PAD=/dev/input/event29 \
WACOM_PEN=/dev/input/event28 \
cargo run --release
```

Replace `event29` and `event28` with the actual event numbers for your system.

## Features

* Displays button states for pad buttons.
* Shows pen position and touch state.
* Calibration and learn modes to map pad buttons to logical names.
* Event log with scrolling and clearing.

## Controls

* `q` or `Esc`: Quit
* `c`: Start/stop calibration mode
* `l`: Enter learn mode for mapping buttons
* `PageUp/PageDown` or `k/j`: Scroll event log
* `x`: Clear event log

Mappings are saved to `~/.config/wacomctl/mapping.json`.
