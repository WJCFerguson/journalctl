# Emacs Journald Log Viewer

This package defines a mode and functions for rich monitoring of journald logs, including from the perspective of service authors/maintainers.

**Current status 2023-04**: early 'beta' version - it works and is useful, but lacks polish, especially in helpful UI to generate commands.

## What is it

It replaces and enhances executing `journalctl` through `async-shell-command` in a `comint` buffer, bringing the following:

 * Processing the complete (JSON) log record, facilitating more information and richer features
 * Multiple `journalctl` queries may be interleaved, simultaneously executing, e.g. run a broad high priority-level query with other tightly focused queries at lower priority levels.  Or when browsing warnings and errors, fetch lower priority messages for the period leading up to a message.
 * jump from a message record line to the generating source file/line (if recorded)
 * Display tooltip with more message detail

Full details are included in the Commentary section of [./journalctl-mode.el](./journalctl-mode.el) and docstrings of the entry functions.

## Installation / usage

Install the `journalctl-mode.el` and launch with `M-x journalctl` to get a prompt for your `journalctl` command line.

Installing `bash-completion.el` will give good quality completion for composing commands.  An interface to help command composition, probably with `transient.el` would be a good enhancement for the future.

