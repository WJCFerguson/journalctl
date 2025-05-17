# Emacs Journald Log Viewer

This package defines a mode and functions to aid with monitoring, debugging and
forensics of journald logs via `journalctl`.

Suggestions, Github Issues, and PRs gratefully received to advance this project that I consider at minimum-useful-package state (2023-10).

## Description

This builds upon the already decent baseline experience of viewing logs via `async-shell-command` (`M-&`).  

The `journalctl` log view command is spawned in a conventional Emacs way, for
which Emacs completion and history, along with packages such as
`bash-completion.el`, already provide a solid helpful interface.

It brings the following additional features:

 * rich and clear text highlighting
 * Multiple simultaneous journalctl processes may run, with output
   interleaved.  E.g.:
   *  simultaneously view a broad query, say at '--priority warning' along with
      a more narrowly focused '--priority debug' query.
   *  Dig into the output within or near a region by requesting additional log
      records.  To facilitate this, if a log region is active when spawning an
      additional command, the kill ring is augmented with a
      `--since=... --until=...` string generated from the region's timestamps.
 * The complete JSON log record is processed facilitating richer features
 * The full JSON record for a given log line under point may be summoned and
   examined
 * Where the record includes file name and line number data, you can jump from a
   message record line to the generating source file/line

## Installation / usage

Install the `journalctl-mode.el` and launch with `M-x journalctl` to get a prompt for your `journalctl` command line.

e.g.:
``` elisp
(use-package journalctl-mode
 :bind ("C-c C-j" . journalctl))
```

Installing `bash-completion.el` will give good quality completion for composing commands.  

Further details and key bindings are included in the Commentary section of [./journalctl-mode.el](./journalctl-mode.el) and docstrings of the entry functions.

## Status and Future

See Github Issues for some possible future enhancements and bugfixes.

## Other Packages

A prior package called [`journalctl-mode`](https://github.com/SebastianMeisel/journalctl-mode/tree/transient) (rather than just `journalctl`) exists with a rather different focus and approach.  This one fetches and interleaves JSON data using multiple simultaneous asynchronous `journalctl` queries, while the other offers a UI for query-building and chunked synchronous data loading.
