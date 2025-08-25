<img width="2" height="1" alt="image" src="https://github.com/user-attachments/assets/8fcf0a46-bd74-4951-957a-2725f1b95c17" />


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

## Set Permissions
```sh
sudo setfacl -m u:rootster:rw- /dev/input/event28
sudo setfacl -m u:rootster:rw- /dev/input/event29
```

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
