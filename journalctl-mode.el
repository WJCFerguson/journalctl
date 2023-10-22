;;; journalctl-mode.el --- Journalctl browsing mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 James Ferguson

;; Author: James Ferguson <james@faff.org>
;; Keywords: lisp
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is an Emacs major-mode for viewing and '--follow'ing journald logs.
;;
;; It builds upon the experience of running journalctl via `shell-command'
;; in a `comint' buffer.  As such it leaves the buffer writeable, so you
;; add/remove/annotate or corrupt the messages in any way you wish.
;;
;; journalctl-mode allows interleaving multiple concurrent or completed
;; journalctl query processes.  Output is interleaved and de-duplicated in
;; timestamp order.  So for instance you can simultaneously view a broad query
;; at '--priority warning' along with a more narrowly focused '--priority debug'
;; or '--grep' query.
;;
;; Additional processes may be added at any time, so for instance around a
;; warning or error message, info or debug lines may be inserted.  To aid this,
;; if the region is active when adding a process ("C-c C-j"), a
;; '--since=... --until...' string will be generated and added to the kill ring
;; to help with query composition.
;;
;; At present it does not offer a specific rich UI for journalctl command
;; composition.  But enabling `bash-completion.el' or similar will often help
;; significantly.
;;
;; Launch with command `journalctl'.
;;
;;;; Example Installation
;;
;; (use-package journalctl-mode
;;  :bind ("C-c C-j" . journalctl))

;;;; Features and bindings:
;;
;; * prettified output like -o short-precise but with priority level displayed
;;   and ISO-esque timestamps in the same format used by --until etc.
;;
;; * "C-c C-j" - add an additional process.  If region is active, a --since /
;;               --until string will be added to the kill ring corresponding to
;;               the selected record lines
;;
;; * "C-c C-c" - kill a current journalctl process, or with prefix arg, kill all
;;               of them.  Analogous to the same key in a `comint' buffer
;;
;; * "C-c C-f"  - start/restart the original query with --follow
;;
;; * "M-."      - Jump to the source of the message if possible, `xref' style.
;;
;; * "C-c C-o"  - open a buffer showing the entire journal record at point
;;

;;;; Tips/Tricks
;;
;; * bash-completion.el helps you build a journalctl command in a generic way
;;
;; * Don't forget good old `highlight-regexp' - 'M-s h (r|l)' (including from
;;   isearch)
;;
;; * `symbol-overlay-mode' does a great job of jumping to next/prev of
;;   symbol under cursor (e.g. the journalctl identifier)
;;

;;; Code:

(require 'xref)
(require 'ansi-color)

;; =============================== Customization ===============================
(defgroup journalctl-mode nil
  "Journald log browsing mode."
  :group 'tools
  :group 'convenience
  :prefix "journalctl-mode")

(defcustom jcm-priority-faces
  '((0 . jcm-critical-face)
    (1 . jcm-critical-face)
    (2 . jcm-critical-face)
    (3 . jcm-error-face)
    (4 . jcm-warning-face)
    (5 . jcm-warning-face)
    (7 . jcm-debug-face))
  "Faces for messages by journald priority level."
  :type '(alist :key-type number :value-type string))

(defcustom jcm-field-format-functions
  '(("PRIORITY" . jcm--format-priority)
    ("__REALTIME_TIMESTAMP" . jcm--format-timestamp)
    ("MESSAGE" . jcm--format-message))
  "Alist mapping journalctl json keys to functions returning display string.

Functions receive arguments (FIELD-NAME RECORD), where RECORD is
the parsed-json record."
  :type '(alist :key-type string :value-type function))

(defcustom jcm-priority-strings
  '((0 . "EMERG")
    (1 . "ALERT")
    (2 . "CRIT ")
    (3 . "ERROR")
    (4 . "WARN ")
    (5 . "NOTE ")
    (6 . "INFO ")
    (7 . "DEBUG"))
  "Display strings for various priorities.

Should be configured to have equal length"
  :type '(alist :key-type number :value-type string))

(defgroup jcm-faces nil
  "Display informations of the current line."
  :group 'tools
  :group 'convenience
  :group 'journalctl-mode)

(defface jcm-critical-face
  '((((min-colors 88) (background dark))
     (:background "yellow1" :foreground "black"))
    (((background dark)) (:background "yellow" :foreground "black"))
    (((min-colors 88)) (:background "yellow1"))
    (t (:background "yellow")))
  "Face for critical or higher."
  :group 'jcm-faces)

(defface jcm-error-face
  '((t :inherit error))
  "Face for error messages."
  :group 'jcm-faces)

(defface jcm-warning-face
  '((t :inherit warning))
  "Face for warning messages."
  :group 'jcm-faces)

(defface jcm-debug-face
  '((t :inherit shadow))
  "Face for debug messages."
  :group 'jcm-faces)

(defface jcm-timestamp-face
  '((t :inherit font-lock-constant-face))
  "Face for timestamps."
  :group 'jcm-faces)

(defface jcm-source-face
  '((t :inherit font-lock-builtin-face))
  "Face for hosts in journalctl's output."
  :group 'jcm-faces)

(defface jcm-systemd-face
  '((default :weight bold))
  "Face for messages from systemd."
  :group 'jcm-faces)

(defface jcm-systemd-starting-face
  '((default :weight bold)
    (((class color) (min-colors 16) (background light)) :foreground "green4")
    (((class color) (min-colors 16) (background dark))  :foreground "green1"))
  "Face for messages from systemd."
  :group 'jcm-faces)

(defface jcm-systemd-finishing-face
  '((default :weight bold)
    (((class color) (min-colors 16) (background light)) :foreground "red4")
    (((class color) (min-colors 16) (background dark))  :foreground "red1"))
  "Face for messages from systemd."
  :group 'jcm-faces)

;; ============================= End Customization =============================

(defvar jcm-history (make-list 1 "journalctl --priority=info --follow ")
  "History list for journalctl commands.")

(defvar jcm--required-arguments
  '("--output=json"
    "--all"
    "--output-fields=\
MESSAGE,\
PRIORITY,\
SYSLOG_IDENTIFIER\
")
  "Arguments non-negotiable for journalctl.")

(defvar jcm--fields-to-purge
  '("PRIORITY"
    "SYSLOG_IDENTIFIER"
    "_BOOT_ID"
    "__MONOTONIC_TIMESTAMP")
  "Fields in journalctl JSON we no longer have use for after line generation.")

(defvar-local jcm--processes nil
  "Set in a jcm-mode buffer, holds the running journalctl processes.")

(defvar-local jcm--primary-commandline nil
  "The command line the process was launched with.")

(defvar-local jcm--point-marker nil
  "The position of point in the buffer.

When the buffer is re-displayed, point is often restored to where
it was when we want it to be at (point-max).")

(defconst jcm--max-json-buffer-size 100000
  "Size of the buffer for incoming JSON data before triggering a parse.")

(defconst jcm--max-json-buffer-time 0.1
  "Maximum age of buffer for incoming JSON data before triggering a parse.")

(defun jcm--select-process ()
  "Get user to select one of the current processes."
  (interactive)
  (let ((proc-name (completing-read
                    "Kill Process: "
                    (mapcar (lambda (p)
                              (process-get p 'name))
                            jcm--processes)
                    nil
                    t)))
    (seq-find (lambda (p) (string-equal (process-get p 'name) proc-name))
              jcm--processes)))

(defun jcm--kill-process ()
  "Have the user select a running journalctl process to kill."
  (interactive)
  (kill-process (jcm--select-process)))

(defun jcm--kill-processes ()
  "Kill all the journalctl processes."
  (dolist (proc jcm--processes)
    (when (and proc (process-live-p proc))
      (kill-process proc))))

(defun jcm--get-value (field-name record)
  "Return the value for FIELD-NAME from RECORD."
  (let ((msg (gethash field-name record)))
    (cond
     ((vectorp msg)
      ;; multibyte strings come as a vector so we have to convert.  NOTE: this seems
      ;; flawed, e.g. when starting Node there are some failed characters vs text
      ;; output.
      (decode-coding-string (mapconcat #'byte-to-string (gethash field-name record) "") 'utf-8))
     ((eq msg ':null) "")
     (t msg))))

(defun jcm--priority-face (record &optional priority-num)
  "Return the priority-based face (if any) for RECORD at PRIORITY-NUM.

If PRIORITY-NUM is nil its value will be fetched from RECORD."
  (let ((priority-num (or priority-num
                          (if-let (priority (jcm--get-value "PRIORITY" record))
                              (string-to-number priority)
                            6))))
    (alist-get priority-num jcm-priority-faces)))

(defun jcm--add-face (str face &optional start end)
  "Set FACE on STR (optionally on sub-string from START to END)."
  (set-text-properties (or start 0) (or end (length str))
                       (list 'font-lock-face face)
                       str)
  str)

(defun jcm--format-message (field-name record)
  "Return formatted string for FIELD-NAME from RECORD."
  (let ((result (jcm--get-value field-name record)))
    (if-let (priority-face (jcm--priority-face record))
        (jcm--add-face result priority-face))
    (when (string-equal "systemd" (jcm--get-value "SYSLOG_IDENTIFIER" record))
      (jcm--add-face result 'jcm-systemd-face)
      (when (string-match "Start\\(ed\\|ing\\)" result)
        (jcm--add-face result 'jcm-systemd-starting-face
                              (match-beginning 0) (match-end 0)))
      (when (string-match "Stopp\\(ed\\|ing\\)" result)
        (jcm--add-face result 'jcm-systemd-finishing-face
                              (match-beginning 0) (match-end 0))))
    result))

(defun jcm--format-priority (field-name record)
  "Return value for FIELD-NAME from RECORD colored by priority level."
  (let* ((value (jcm--get-value field-name record))
         (priority-num (if value (string-to-number value) 6)))
    (jcm--add-face (alist-get priority-num jcm-priority-strings)
                          (jcm--priority-face record priority-num))))

(defun jcm--timestamp-num (&optional record)
  "Return timestamp as a number from RECORD."
  (setq record (or record (jcm--get-line-record)))
  (if record
      (string-to-number (jcm--get-value "__REALTIME_TIMESTAMP" record))
    0))

(defun jcm--timestamp (record &optional field-name)
  "Return a timestamp cons of (seconds . microseconds) for a journald RECORD.

FIELD-NAME defaults to __REALTIME_TIMESTAMP."
  (let* ((timestr (jcm--get-value (or field-name "__REALTIME_TIMESTAMP")
                                         record))
         (len (length timestr))
         (seconds (string-to-number (substring timestr 0 (- len 6))))
         (microseconds (string-to-number (substring timestr (- len 6) len))))
    (cons seconds microseconds)))

(defun jcm--extract-timestamp (field-name &optional record)
  "Return timestamp string for display from FIELD-NAME in RECORD."
  (let* ((timestamp (jcm--timestamp record field-name))
         (display-time (format-time-string "%Y-%m-%d %H:%M:%S" (car timestamp))))
    (concat display-time "." (format "%06d" (cdr timestamp)))))

(defun jcm--format-timestamp (field-name &optional record)
  "Return face-annotated timestamp string for display from FIELD-NAME in RECORD."
  (jcm--add-face (jcm--extract-timestamp field-name record)
                        'jcm-timestamp-face))

(defun jcm--format-field (field-name record)
  "Format FIELD-NAME from RECORD for display.

Finds format function from alist `jcm-field-dformat-functions
falling back to simple string value display."
  (let ((format-function (alist-get field-name jcm-field-format-functions
                                    'jcm--get-value nil 'string-equal)))
    (funcall format-function field-name record)))

(defun jcm--set-mode-line-process ()
  "Set the `mode-line-process' for process info in the mode-line."
  (setq mode-line-process (if jcm--processes
                              (concat " running"
                                      (if (> (length jcm--processes) 1)
                                          (format " (%d)" (length jcm--processes))))
                            " done"))
  (force-mode-line-update))

(defun jcm--make-process (command)
  "Start journalctl COMMAND to be rendered to current jcm-mode buffer.

COMMAND may be a string or a list of string arguments."
  (let* ((target-buffer (current-buffer))
         (split-command (if (stringp command)
                            (split-string-shell-command (string-trim command))
                          command))
         (file-handler (find-file-name-handler default-directory 'make-process))
         (command-name (if (stringp command) command (combine-and-quote-strings command)))
         (make-process-args
          (list ':name command-name
                ':command (append split-command jcm--required-arguments)
                ':noquery t
                ':filter 'jcm--filter-incoming
                ':sentinel 'jcm--process-sentinel))
         (new-process (if file-handler
                          (apply file-handler 'make-process make-process-args)
                        (apply 'make-process make-process-args))))
    (set-process-plist new-process
                       (list 'name command-name
                             'partial-input ""
                             'target-buffer target-buffer
                             'start-time (float-time)
                             'insertion-marker (set-marker (make-marker) (point-max))))
    (setq jcm--processes (cons new-process jcm--processes))
    (jcm--set-mode-line-process)))

(defun jcm--filter-incoming (process incoming)
  "PROCESS filter receiving INCOMING json from journalctl; triggers parsing."
  (let ((unparsed (concat (process-get process 'partial-input) incoming))
        (flush-timer (process-get process 'flush-timer)))
    (process-put process 'partial-input unparsed)
    ;; if we have a lot pending, have it parse, otherwise set a timer to make
    ;; sure it will be parsed promptly.  End of process will also trigger a flush.
    (if (> (length unparsed) jcm--max-json-buffer-size)
        (jcm--flush-json process)
      ;; unless the timer is set
      (unless (and flush-timer (memq flush-timer timer-list))
        (process-put process
                     'flush-timer
                     (run-with-timer jcm--max-json-buffer-time nil
                                     'jcm--flush-json process))))))

(defun jcm--clear-timer (process)
  "Clear the flush-json timer for PROCESS."
  (when-let ((flush-timer (process-get process 'flush-timer)))
    (cancel-timer flush-timer)
    (process-put process 'flush-timer nil)))

(defun jcm--goto-insertion-point (process record)
  "Go to the insertion point for RECORD generated by PROCESS.

If RECORD is nil it implies a parse failure, so go to last insertion point.

Iterates line-by-line forward and back from last insertion to
find insertion point, which *should* be acceptably efficient, but
bear this in mind."
  (goto-char (process-get process 'insertion-marker))
  (when record
    (let ((ts (jcm--timestamp-num record)))
      ;; skip backwards over any later than us
      (while (and (not (eq (point) (point-min)))
                  (let ((line-record (jcm--get-line-record)))
                    (or (not line-record) (< ts (jcm--timestamp-num line-record)))))
        (forward-line -1))
      ;; skip forward over any earlier than us
      (while (and (not (eq (point) (point-max)))
                  (let ((line-record (jcm--get-line-record)))
                    (or (not line-record) (> ts (jcm--timestamp-num line-record)))))
        (forward-line 1)))))

(defun jcm--records-equal (record1 record2)
  "Return if RECORD1 and RECORD2 are the same message."
  (and record1 record2
       (equal (gethash "__REALTIME_TIMESTAMP" record1) (gethash "__REALTIME_TIMESTAMP" record2))
       (equal (gethash "MESSAGE" record1) (gethash "MESSAGE" record2))))

(defun jcm--insert-line (process record text)
  "Insert TEXT for RECORD to PROCESS's target buffer."
  (let ((target-buffer (process-get process 'target-buffer)))
    (if (not (buffer-live-p target-buffer))
        (when (process-live-p process) (kill-process process))
      (with-current-buffer target-buffer
        (let ((return-end (eq (point) (point-max))))
          (save-excursion
            (widen)
            (jcm--goto-insertion-point process record)
            ;; insert if it's not already there
            (when (not (jcm--records-equal (jcm--get-line-record) record))
              (undo-boundary)
              (let ((pre-insert-point (point)))
                (insert text)
                (ansi-color-filter-region pre-insert-point (point))
                (font-lock-fontify-region pre-insert-point (point)))
              (set-marker (process-get process 'insertion-marker) (point))
              (undo-boundary)))
          (when return-end
            (goto-char (point-max))
            ;; the window has its own idea of point so we must also update that
            (when (get-buffer-window)
              (set-window-point (get-buffer-window) (point-max))))
          (set-marker jcm--point-marker (point)))))))

(defun jcm--window-buffer-change (window)
  "Called when buffer is newly displayed; fixes `window-point' for WINDOW."
  ;; when we switch back to a window, e.g. with tab-bar-mode, sync the
  ;; window-point to (point) as otherwise tab-bar may restore it to wherever it
  ;; was the last time the tab was visible.
  (when (eq (type-of window) 'window)
    (with-current-buffer (window-buffer window)
      (when (eq 'journalctl-mode major-mode)
        (set-window-point window jcm--point-marker)))))

(defun jcm--flush-json (process)
  "Parse any complete json lines received from PROCESS and format into buffer."
  (jcm--clear-timer process)
  (let ((json-lines (process-get process 'partial-input)))
    ;; construct output lines
    (process-put process 'partial-input "")
    (dolist (line (string-lines json-lines t t))
      (if (not (string-suffix-p "\n" line))
          (process-put process 'partial-input line) ;; incomplete line
        (condition-case result
            (json-parse-string line) ;; value returned *is* used - silly checker
          (:success
           (jcm--insert-line process
                                    result
                                    (jcm--format-line result)))
          ((json-parse-error json-readtable-error)
           (jcm--insert-line process
                                    nil
                                    (format "JSON parse Failure: %S; when parsing %S"
                                            (cadr result)
                                            line))))))))

(defun jcm--process-sentinel (process _event-description)
  "Sentinel function for a journalctl PROCESS serving to a jcm-mode buffer."
  (jcm--flush-json process)
  (if (not (process-live-p process))
      (message "Journalctl process took %.2fs"
               (- (float-time) (process-get process 'start-time))))
  (let ((target-buffer (process-get process 'target-buffer)))
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (setq jcm--processes (remove process jcm--processes))
        (jcm--set-mode-line-process)))))

(defun jcm--format-line (record)
  "Return journald RECORD formatted as a propertized text line.

This stores RECORD as `jcm--record record' property on the line itself."
  (let* ((result (concat
                  (jcm--format-field "__REALTIME_TIMESTAMP" record) " "
                  (jcm--format-field "PRIORITY" record) " "
                  (jcm--add-face
                   (format "%-20s"(jcm--format-field "SYSLOG_IDENTIFIER" record))
                   'jcm-source-face)
                  " "))
         (message-prefix (make-string (length result) ?\ )))
    (setq result (concat result
                         (propertize
                          (jcm--format-field "MESSAGE" record)
                          'wrap-prefix message-prefix
                          'line-prefix message-prefix)
                         "\n"))
    ;; purge unwanted firlds from record and attach to line
    (mapc (lambda (f) (remhash f record)) jcm--fields-to-purge)
    (put-text-property 0 1 'jcm--record record result)
    result))

(defun jcm--get-line-record (&optional at-point)
  "Fetch the parsed json record for the line a (or AT-POINT (point))."
  ;; if user has annotated lines, we want to skip back to find the line
  (save-excursion
    (when at-point (goto-char at-point))
    (goto-char (pos-bol))
    (let ((result (get-text-property (pos-bol) 'jcm--record)))
      (while (not (or result (eq (point) (point-min))))
        (forward-line -1)
        (setq result (get-text-property (pos-bol) 'jcm--record)))
      result)))

(defun jcm-jump-to-line-source ()
  "Jump to the source of the message at point, if possible."
  (interactive)
  (let* ((line-record (jcm--get-line-record))
         (record (json-parse-string
                  (shell-command-to-string
                   (format
                    "journalctl --quiet --cursor='%s' --lines=1 --output json"
                    (gethash "__CURSOR" line-record)))))
         (local-file (gethash "CODE_FILE" record))
         (pathname (concat (file-remote-p default-directory) local-file)))
    (cond
     ((not local-file)
      (error "Log record does not include a 'CODE_FILE' entry"))
     ((not (file-readable-p pathname))
      (error "Log record source file '%s' not readable" pathname))
     (t
      ;; with M-. we're emulating xref so push marker for M-,
      (xref-push-marker-stack)
      (find-file pathname)
      (when-let ((line (jcm--get-value "CODE_LINE" record)))
        (goto-char (point-min))
        (forward-line (1- (string-to-number line))))))))

(defun jcm-full-message ()
  "Fetch the full journalctl message at point into a buffer."
  (interactive)
  (let* ((record (jcm--get-line-record))
         (command-root (format
                        "journalctl --quiet --cursor='%s' --lines=1 --output "
                        (gethash "__CURSOR" record))))
    (shell-command
     (concat "echo -e 'journal message with --output json-pretty:\n';"
             command-root "json-pretty;"
             "echo -e '\n\njournal message with --output short-precise:\n';"
             command-root "short-precise;"
             " &"))))

(defun jcm-follow ()
  "(Re) run the original command of the buffer with --follow.

Starts from the last line of the current buffer

WARNING: no line limit."
  (interactive)
  (jcm--make-process
   (append
    ;; command with since, until & lines removed
    (let ((delete-next nil)
          (case-fold-search nil))
      (seq-filter (lambda (x)
                    (cond
                     (delete-next
                      (setq delete-next nil))
                     ((string-match "^-\\([SUn]\\|-since\\|-until\\|-lines\\)$" x)
                      (setq delete-next t)
                      nil)
                     ((string-match "^--\\(since\\|until\\|lines\\)=" x)
                      nil)
                     (t t)))
                  (split-string-shell-command jcm--primary-commandline)))
    ;; add follow args
    (list "--follow"
          "--since"
          (jcm--extract-timestamp
           "__REALTIME_TIMESTAMP"
           (jcm--get-line-record (point-max)))))))

(defun jcm--extract-region-timestamps ()
  "If region is active, add --since/--until to the kill ring."
  (when (and (derived-mode-p 'journalctl-mode) (region-active-p))
    (kill-new
     (format "--since \"%s\" --until \"%s\""
             (jcm--extract-timestamp
              "__REALTIME_TIMESTAMP"
              (jcm--get-line-record (region-beginning)))
             (jcm--extract-timestamp
              "__REALTIME_TIMESTAMP"
              (jcm--get-line-record (region-end)))))))

(defun jcm-add (command)
  "Add an additional journalctl COMMAND to the current journalctl buffer.

If region is active, a --since/--until string will be in the kill
ring for the time range of the selected region."
  (interactive
   (progn
     (jcm--extract-region-timestamps)
     (list
      (read-shell-command "Journalctl command: " (car jcm-history) 'jcm-history))))
  (jcm--make-process command))

(defvar journalctl-mode-map
  (let ((map (make-sparse-keymap)))
    ;; example definition
    (define-key map (kbd "M-.") 'jcm-jump-to-line-source)
    (define-key map (kbd "C-c C-o") 'jcm-full-message)
    (define-key map (kbd "C-c C-c") 'jcm--kill-process)
    (define-key map (kbd "C-c C-j") 'jcm-add)
    (define-key map (kbd "C-c C-f") 'jcm-follow)
    map)
  "Basic mode map for `jcm-mode'.")

(define-derived-mode journalctl-mode fundamental-mode "journalctl"
  "Major mode for browsing journald records with `journalctl'.

\\{journalctl-mode-map}"
  (when (featurep 'symbol-overlay)
    (symbol-overlay-mode))
  ;; visual-line makes sense for messages to flow nicely
  (visual-line-mode))

;;;###autoload
(defun journalctl (command)
  "Browse journald logs inside Emacs.

With COMMAND and with prefix ARG, prompt for editing the command."
  (interactive
   (list (read-shell-command "Journalctl command: " (car jcm-history) 'jcm-history)))
  (when current-prefix-arg
    (setq command (read-shell-command "Journalctl command: " command 'jcm-history)))
  (let ((remote-host (file-remote-p default-directory)))
    (pop-to-buffer (generate-new-buffer
                    (concat "*" remote-host (and remote-host " ") command "*"))))
  (journalctl-mode)
  (setq-local jcm--primary-commandline (string-trim command)
              jcm--point-marker (set-marker (make-marker) (point-max)))
  (add-hook 'window-buffer-change-functions #'jcm--window-buffer-change nil t)
  (jcm--make-process command)
  (add-hook 'kill-buffer-hook #'jcm--kill-processes)
  (goto-char (point-max)))

(provide 'journalctl-mode)
;; Local Variables:
;; read-symbol-shorthands: (("jcm-" . "journalctl-mode-"))
;; End:
;;; journalctl-mode.el ends here
