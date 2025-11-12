# Emacs Journald Log Viewer

This package defines a mode and functions to aid with monitoring, debugging and
forensics using journald logs via `journalctl`.

**Project Status**:  This project is functional and active!  See [Status and Future](#status-and-future) below.

## Description

This provides an experience that builds upon that of viewing logs via `async-shell-command` (`M-&`).  

One or more `journalctl` log view commands are spawned via the minibuffer, with standard Emacs completion and history.  Packages such as [`bash-completion.el`](https://github.com/szermatt/emacs-bash-completion) are recommended to enhance command building.

This `journalctl` package brings the following additional features:

 * rich and clear text highlighting
 * Multiple simultaneous journalctl processes may run, interleaving output, E.g.:
   *  simultaneously view a broad query, say at '--priority warning' along with
      a more narrowly focused '--priority debug' query.
   *  Dig into the output within or near a region by requesting additional log
      records.  To facilitate this, if the region is active when spawning an
      additional command, the kill ring is augmented with a
      `--since=... --until=...` string generated from the region's timestamps.
 * The complete JSON log record is processed facilitating rich features
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

Installing `bash-completion.el` is highly recommended in general for command execution throughout Emacs, and assists greatly with generating journalctl commands.

Further details and key bindings are included in the Commentary section of [./journalctl-mode.el](./journalctl-mode.el) and docstrings of the entry functions.

## Status and Future

Despite minimal activity and limited bling and polish, this package has been solid and invaluable as-is to the author and is likely to remain a maintained project.  I hope you find it as useful as I do.

So far the author's time-poverty has meant the modest benefit:effort ratio of the currently conceived enhancements has not won out.  Though I am increasingly interested in genericizing the codebase for use beyond journald (https://github.com/WJCFerguson/journalctl/issues/16).

Some nice-to-haves are in the Issues list, and more suggestions or bug reports, and especially helpful PRs, are gratefully received to improve this project.

## Other Packages

A prior package called [`journalctl-mode`](https://github.com/SebastianMeisel/journalctl-mode/tree/transient) (rather than just `journalctl`) exists with a rather different focus and approach.  This one fetches and interleaves JSON data using multiple simultaneous asynchronous `journalctl` queries, while the other offers a UI for query-building and chunked synchronous data loading.
