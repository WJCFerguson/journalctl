# Emacs Journald Log Viewer

For monitoring Linux Journald logs from Emacs, this package defines a mode and
functions for rich monitoring of journald logs.  It was originally written from
the perspective of service developers performing monitoring and forensics.

**Current status 2023-04**: early 'alpha' version - it works and is useful, but at a basic level with little polish, particularly in helpful UI to generate commands.

Suggestions, Github Issues, and PRs gratefully received to advance this project that I consider at minimum-useful-package state (04/2023).

## What is it

It replaces and enhances conventionally executing `journalctl` through `async-shell-command` but enhanced with the following:

 * Processing the complete (JSON) log record, facilitating more information and
   richer features
 * Multiple simultaneous journalctl processes may be launched at any time and
   the output interleaved.  E.g.:
   *  simultaneously view a broad query at '--priority warning' along with a
      more narrowly focused '--priority debug' query.
   *  augment the output with additional low-level logging for a period before
      error or warning messages
 * jump from a message record line to the generating source file/line (if
   recorded)
 * Display a tooltip or extra buffer with additional record detail

Further details and key bindings are included in the Commentary section of [./journalctl-mode.el](./journalctl-mode.el) and docstrings of the entry functions.

## Installation / usage

Install the `journalctl-mode.el` and launch with `M-x journalctl` to get a prompt for your `journalctl` command line.

Installing `bash-completion.el` will give good quality completion for composing commands.  

## Future

Short term list:
 * save and recall lists of commands
 * An interface to help command composition, probably with `transient.el`
   * at least for `--since`, `--until`, `--priority` and `--follow`.
   * extend timestamps easily e.g. to generate "prior 5 seconds to debug level"

