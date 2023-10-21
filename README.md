# Emacs Journald Log Viewer

For monitoring Linux Journald logs from Emacs, this package defines a mode and
functions for rich monitoring of journald logs.  

Suggestions, Github Issues, and PRs gratefully received to advance this project that I consider at minimum-useful-package state (2023-10).

## Description

It was originally written from the perspective of systemd service developers
performing monitoring and forensics.  It replaces and enhances conventionally executing `journalctl` through `async-shell-command`.

It brings the following additional features:

 * Multiple journalctl processes may run simultaneously with the output
   interleaved.  E.g.:
   *  simultaneously view a broad query, say at '--priority warning' along with
      a more narrowly focused '--priority debug' query.
   *  Dig into the output within or near a region by requesting additional log
      records.  To facilitate this, if the region is active when you add an
      additional command, the kill ring is augmented with a
      `--since=... --until=...` string generated from the region's timestamps.
 * The complete JSON log record is processed facilitating richer features
 * The full JSON record for a given log line under point may be summoned and
   examined
 * Where the record includes file name and line number, you can jump from a
   message record line to the generating source file/line

Further details and key bindings are included in the Commentary section of [./journalctl-mode.el](./journalctl-mode.el) and docstrings of the entry functions.

## Installation / usage

Install the `journalctl-mode.el` and launch with `M-x journalctl` to get a prompt for your `journalctl` command line.

e.g.:
``` elisp
(use-package journalctl-mode
 :bind ("C-c C-j" . journalctl))
````

Installing `bash-completion.el` will give good quality completion for composing commands.  

## Status and Future

See Github Issues for some possible future enhancements and bugfixes.
