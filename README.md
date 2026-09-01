# Emacs Journald Log Viewer

This package supplies an Emacs major mode and commands for monitoring, debugging, and forensics with journald logs.  It uses the `journalctl` command on Linux systems.

**Project Status**: This project is functional and active.  See [Status and Future](#status-and-future).

## Description

The basic method to view logs in Emacs is `async-shell-command` (`M-&`).  Initial usage of this package is the same: enter a `journalctl` query in the minibuffer.  A package like [`bash-completion.el`](https://github.com/szermatt/emacs-bash-completion) helps with command composition.

This package then adds these features:

 * Rich and clear text highlighting.  Long messages wrap with correct indentation.
 * Multiple `journalctl` processes can run in parallel in one buffer.  The package interleaves their output in timestamp order and removes duplicates.
   * For example, view one broad `--priority warning` query together with a narrow `--priority debug --grep "\<pattern\>"` query.
   * **NOTE this is particularly useful:** You can examine the detail around records of interest.  If the region is active when you press `C-c C-j`, the package puts a `--since=... --until=...` string for the region on the kill ring.  For example, query for errors or a service stop, and select the relevant lines.  Press `C-c C-j` to start a new query.  Then yank the string into the new command, and adjust the period if necessary.
 * The package parses the complete JSON record of each log line.
 * `C-c C-o` shows the full JSON record for the log line at point.
 * `C-c C-c` stops a running process that you select from a list (like `async-shell-command`).
 * If the record includes the file name and the line number, `M-.` goes to the source of the message.
 * Timestamps show in the timezone of the queried system.  Thus they stay valid in `--since`/`--until` arguments for remote hosts over TRAMP.  See the option `journalctl-timezone`.
 * The package trims the buffer to a maximum number of lines, and erases the oldest lines first.  Thus a long `--follow` does not make the buffer grow without limit.  See the option `journalctl-buffer-maximum-lines`.
 * Buffers are writable, so you can annotate or remove messages.  If you prefer read-only buffers, use `(add-hook 'journalctl-mode-hook #'read-only-mode)`.  Log output insertion is not affected.  `M-x read-only-mode` or `C-x C-q` toggles read-only mode.

## Installation / usage

**NOTE:** This package is named "journalctl", and its buffers use `journalctl-mode`.  Do not confuse this package with the older package "journalctl-mode".  The two packages define the same commands (`journalctl`, `journalctl-mode`), so install only one of them.  See [Other Packages](#other-packages) for a comparison.

Install `journalctl.el`.  Then start it with `M-x journalctl`, and enter a `journalctl` command at the prompt.

For example:
``` elisp
(use-package journalctl
 :vc "git@github.com:WJCFerguson/journalctl.git")
```

The package `bash-completion.el` improves command composition in Emacs, and helps with `journalctl` commands.

The Commentary section of [./journalctl.el](./journalctl.el) and the docstrings of the entry functions contain more details and the key bindings.

## Status and Future

Development activity is low and polish is limited, but the package is solid and valuable to the author.  It remains a maintained project.

The author has limited time.  Thus some candidate enhancements with a modest benefit-to-effort ratio are not implemented.  A possible future direction is to make the core generic for logs other than journald (https://github.com/WJCFerguson/journalctl/issues/16).

The Issues list contains more candidate improvements.  Suggestions, bug reports, and PRs are welcome.

## Other Packages

An older package named [`journalctl-mode`](https://github.com/SebastianMeisel/journalctl-mode/tree/transient) exists, with a different focus and approach.  This package fetches JSON data with multiple simultaneous asynchronous `journalctl` queries, and interleaves the output.  The other package supplies a UI for query composition, and loads data synchronously in chunks.

The two packages define the same top-level symbols (`journalctl`, `journalctl-mode`, and more).  Thus they cannot coexist in one Emacs session — the package that loads last wins.  Install only one.
