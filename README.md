# Emacs Journald Log Viewer

This package defines a mode and functions to aid with monitoring, debugging and
forensics using journald logs via `journalctl`.

**Project Status**:  This project is functional and active!  See [Status and Future](#status-and-future) below.

## Description

This enhances the basic experience of viewing logs via `async-shell-command`
(`M-&`).  Initial usage is the same, entering a `journalctl` log query commands in
the minibuffer (a package like [`bash-completion.el`](https://github.com/szermatt/emacs-bash-completion)
is helpful to enhance command building).

Then this `journalctl` brings these additional features:

 * rich and clear text highlighting, message appropriately wrapped.
 * Multiple simultaneous journalctl processes may run in parallel, interleaving
   output, E.g.:
   *  simultaneously view one broad query at `--priority warning` along with
      a more narrowly focused `--priority debug --grep "\<pattern\>"` query.
   *  Dig into the detail within or near a region by requesting additional log
      records.  **NOTE: this is particularly useful:** When spawning an additional
      command (`C-c C-j`) with an active region selected, the kill ring is
      augmented with a `--since=... --until=...` string from that region's
      timestamps.  So e.g. if you query for interesting lines like errors or
      service shutdown, you could then select relevant lines, `C-c C-j` to run
      another query, then paste and maybe tweak the `since`/`until` to see detail in
      the interesting period.
 * The complete JSON log record is processed facilitating rich features
 * The full JSON record for a given log line under point may be summoned and
   examined (`C-c C-o`)
 * kill running processes by selecting from a list (`C-c C-c`, like `async-shell-command`)
 * Where the record includes file name and line number data, you can jump from a
   message record line to the generating source file/line
 * timestamps are displayed in the timezone of the system being queried (so
   they remain valid in `--since`/`--until` arguments when querying remote
   hosts over TRAMP) — see customization option `journalctl-timezone`

## Installation / usage

Install the `journalctl.el` and launch with `M-x journalctl` or `M-x journalctl-mode` to get a prompt for your `journalctl` command line.

e.g.:
``` elisp
(use-package journalctl)
```
Or, from the cloned git repository:
```elisp
(use-package journalctl
  :load-path "path/to/journalctl/")
```
Installing `bash-completion.el` is highly recommended in general for command execution throughout Emacs, and assists greatly with generating journalctl commands.

Further details and key bindings are included in the Commentary section of [./journalctl.el](./journalctl.el) and docstrings of the entry functions.

## Status and Future

Despite minimal activity and limited bling and polish, this package has been solid and invaluable as-is to the author and is likely to remain a maintained project.  I hope you find it as useful as I do.

So far the author's time-poverty has meant the modest benefit:effort ratio of some candidate enhancements has not won out.  Though I am increasingly interested in genericizing the codebase for use beyond journald (https://github.com/WJCFerguson/journalctl/issues/16).

Some nice-to-haves are in the Issues list, and more suggestions or bug reports, and especially helpful PRs, are gratefully received to improve this project.

## Other Packages

A prior package called [`journalctl-mode`](https://github.com/SebastianMeisel/journalctl-mode/tree/transient) (rather than just `journalctl`) exists with a rather different focus and approach.  This one fetches and interleaves JSON data using multiple simultaneous asynchronous `journalctl` queries, while the other offers a UI for query-building and chunked synchronous data loading.
