# AGENTS.md - emacs-server-menu

## Project Overview
A lightweight Emacs plugin to manage SSH connections using an external terminal emulator (gnome-terminal, konsole, or xterm) via a configuration file.

## Technical Details
- **Main entry point**: `server-menu.el`
- **Config file**: Defined by `ssh-servers-file` (default: `~/.config/server-menu.txt`)
- **Dependencies**: Requires `vertico` for the completion interface.

## Configuration Format
The configuration file uses a space-separated format:
`username@hostname title:alias option:ENV=VALUE rc:"command"`

- `title:` replaces the hostname in the selection menu.
- `option:` sets environment variables via `env` before the SSH command.
- `rc:` executes a remote command via `ssh -t`.

## Implementation Quirks
- **Terminal Dispatch**: The plugin checks for `gnome-terminal`, `konsole`, and `xterm` in that order.
- **Gnome-Terminal**: Specifically uses `-p -v -- bash -c <command>`.
- **Debugging**: Selected servers and final commands are logged to the `*server-menu*` buffer.

## Verification
- No automated test suite exists.
- Verification requires:
  1. Loading `server-menu.el` in Emacs.
  2. Creating a valid `~/.config/server-menu.txt`.
  3. Running `M-x server-menu`.
