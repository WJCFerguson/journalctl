# Emacs Journald Log Viewer

This package defines a mode and functions for rich monitoring of journald logs, including from the perspective of service authors/maintainers.

**Current status 2022-12**: early 'alpha' version - it works and is useful, but is immature and in active development for polish and feature development.

## What is it

It replaces and enhances executing `journalctl` through `async-shell-command` in a `comint` buffer, bringing the following:

 * Processes the complete (JSON) log record, facilitating richer features
 * Currently the display format is static, similar to `short-precise`, but format control is planned.
 * jump to message source file/line (if recorded)
 * Display tooltip with more message detail
 * Additional `journalctl` queries may be interleaved, e.g. run another focused query at a lower priority level - perhaps to see debug messages leading up to an error

Some keys/functions are currently defined:

 * `C-c C-c` - kill running command (as with comint shell commands)
 * `C-c C-o` - open a buffer with the full record details of the message at point
 * `C-c C-j` - execute a new query to insert in the current buffer
 * `C-c C-f` - re-run the most recent query with `--follow`, from the last message timestamp.

## Installation / usage

Install the `journalctl-mode.el` and launch with `M-x journalctl` to get a prompt for your `journalctl` command line.

Installing `bash-completion.el` will give good quality completion for composing commands.  An interface to help command composition is planned (probably with `transient.el`, á la `magit`)

## Current state and Future Plans

### pre-1.0 

2022-12 approaching ready for bare-bones first release:

 * **Critical Bug** new line processing sometimes seems to pause.
 * Allow multiple concurrent log processes, e.g. for `--follow` queries running overview queries concurrent with more focused queries at a lower priority level.
 * Generate new command based on selected region (another killer, and easy, first feature)

### Future plans

 * UI.  Requires thought and design!
   * Needs to be easy to construct a command-line, can't rely on bash-completion (often not present on remote servers)
   * `dwim` richness based on record(s) at point or region.
   * Manage active processes
   * Save restore query sets
   * probably use `transient.el`
 * User-configurable text display formats.  The `journalctl` executable is weirdly constrained here.
