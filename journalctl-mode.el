;;; journalctl-mode.el --- Journalctl browsing mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 James Ferguson

;; Author: James Ferguson <james@faff.org>
;; Keywords: lisp
;; Version: 0.0.1

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
;; This is a major-mode for Emacs to view journald logs in Emacs.
;;
;; It should be an analogous but enhanced experience to running journalctl via
;; `shell-command'.  As such it leaves the buffer writeable, so you
;; add/remove/annotate as you wish.
;;
;; Launch with command `journalctl'.
;;
;;;; Example Installation
;;
;; (use-package journalctl-mode
;;  :bind ("C-c j" . journalctl))

;;;; Features:
;;
;; * prettified output
;; * TODO
;;
;; Mode map bindings:
;; * "M-."      - Jump to the source of the message if possible, `xref' style.
;; * "C-c C-c"  - kill the current journalctl process

;;;; Tips/Tricks
;;
;; * Don't forget good old `highlight-regexp' - 'M-s h (r|l)' (incl. from
;;   isearch)
;;
;; * `highlight-symbol-nav-mode' does a great job of jumping to next/prev of
;;   symbol under cursor (e.g. the journalctl identifier)
;;

;;; Code:

(require 'xref)

;; =============================== Customization ===============================
(defgroup journalctl nil
  "Journald log browsing mode."
  :group 'tools
  :group 'convenience
  :prefix "journalctl-")

(defcustom journalctl-priority-faces
  '((0 . journalctl-critical-face)
    (1 . journalctl-critical-face)
    (2 . journalctl-critical-face)
    (3 . journalctl-error-face)
    (4 . journalctl-warning-face)
    (5 . journalctl-warning-face)
    (7 . journalctl-debug-face))
  "Faces for messages by journald priority level"
  :type '(alist :key-type number :value-type string))

(defcustom journalctl-field-format-functions
  '(("PRIORITY" . journalctl--format-priority)
    ("__REALTIME_TIMESTAMP" . journalctl--format-timestamp)
    ("_PID" . journalctl--format-pid)
    ("MESSAGE" . journalctl--format-message))
  "Alist mapping journalctl json keys to functions returning display string.

Functions receive arguments (FIELD-NAME RECORD), where RECORD is
the parsed-json record."
  :type '(alist :key-type string :value-type function))

(defcustom journalctl-priority-strings
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

(defgroup journalctl-faces nil
  "Display informations of the current line."
  :group 'tools
  :group 'convenience
  :group 'journalctl)

(defface journalctl-critical-face
  '((((min-colors 88) (background dark))
     (:background "yellow1" :foreground "black"))
    (((background dark)) (:background "yellow" :foreground "black"))
    (((min-colors 88)) (:background "yellow1"))
    (t (:background "yellow")))
  "Face for critical or higher."
  :group 'journalctl-faces)

(defface journalctl-error-face
  '((t :inherit error))
  "Face for error messages."
  :group 'journalctl-faces)

(defface journalctl-warning-face
  '((t :inherit warning))
  "Face for warning messages."
  :group 'journalctl-faces)

(defface journalctl-debug-face
  '((t :inherit shadow))
  "Face for debug messages."
  :group 'journalctl-faces)

(defface journalctl-timestamp-face
  '((t :inherit font-lock-constant-face))
  "Face for timestamps."
  :group 'journalctl-faces)

(defface journalctl-source-face
  '((t :inherit font-lock-builtin-face))
  "Face for hosts in journalctl's output."
  :group 'journalctl-faces)

(defface journalctl-systemd-face
  '((default :weight bold))
  "Face for messages from systemd"
  :group 'journalctl-faces)

(defface journalctl-systemd-starting-face
  '((default :weight bold)
    (((class color) (min-colors 16) (background light)) :foreground "green4")
    (((class color) (min-colors 16) (background dark))  :foreground "green1"))
  "Face for messages from systemd"
  :group 'journalctl-faces)

(defface journalctl-systemd-finishing-face
  '((default :weight bold)
    (((class color) (min-colors 16) (background light)) :foreground "red4")
    (((class color) (min-colors 16) (background dark))  :foreground "red1"))
  "Face for messages from systemd"
  :group 'journalctl-faces)

;; ============================= End Customization =============================

(defvar journalctl-history (make-list 1 "journalctl --priority=info --follow ")
  "History list for journalctl commands.")

(defvar journalctl--required-arguments
  '("--output=json"
    "--output-fields=\
CODE_FILE,\
CODE_LINE,\
MESSAGE,\
PRIORITY,\
SYSLOG_IDENTIFIER,\
_HOSTNAME,\
_PID,\
_SYSTEMD_UNIT,\
_SYSTEMD_USER_UNIT\
")
  "Arguments non-negotiable for journalctl ")

(defvar-local journalctl--process nil
  "Set in a journalctl-mode buffer, holds the running journalctl process")

(defvar-local journalctl--target-buffer nil
  "Set in a process buffer indicates the journalctl-mode buffer for insertion.")

(defun journalctl--kill-process ()
  "Kill a running journalctl process and its buffer."
  (interactive)
  (when (and journalctl--process (process-live-p journalctl--process))
    (kill-process journalctl--process))
  (setq journalctl--process nil))

(defun journalctl--get-value (field-name record)
  "Return the value for FIELD-NAME from RECORD."
  (let ((msg (gethash field-name record)))
    (cond
     ((vectorp msg)
      ;; multibyte strings come as a vector so we have to convert.  NOTE: this seems
      ;; flawed, e.g. when starting Node there are some failed characters vs text
      ;; output.
      (string-as-multibyte (mapconcat #'byte-to-string (gethash field-name record) "")))
     ((eq msg ':null) "")
     (t msg))))

(defun journalctl--priority-face (record &optional priority-num)
  "Return the priority-based face (if any) for RECORD at PRIORITY-NUM.

If PRIORITY-NUM is nil its value will be fetched from RECORD."
  (let ((priority-num (or priority-num
                          (string-to-number (journalctl--get-value "PRIORITY"
                                                                   record)))))
    (alist-get priority-num journalctl-priority-faces)))

(defun journalctl--add-face (str face &optional start end)
  "Set FACE on STR (optionally on sub-string from START to END)."
  (set-text-properties (or start 0) (or end (length str))
                       (list 'font-lock-face face)
                       str)
  str)

(defun journalctl--format-message (field-name record)
  "Return formatted string for FIELD-NAME from RECORD."
  (let ((result (journalctl--get-value field-name record)))
    (if-let (priority-face (journalctl--priority-face record))
        (journalctl--add-face result priority-face))
    (when (string-equal "systemd" (journalctl--get-value "SYSLOG_IDENTIFIER" record))
      (journalctl--add-face result 'journalctl-systemd-face)
      (when (string-match "Start\\(ed\\|ing\\)" result)
        (journalctl--add-face result 'journalctl-systemd-starting-face
                              (match-beginning 0) (match-end 0)))
      (when (string-match "Stopp\\(ed\\|ing\\)" result)
        (journalctl--add-face result 'journalctl-systemd-finishing-face
                              (match-beginning 0) (match-end 0))))
    result))

(defun journalctl--format-priority (field-name record)
  "Returns value for FIELD_NAME from RECORD colored by priority level."
  (let* ((value (journalctl--get-value field-name record))
         (priority-num (string-to-number value)))
    (journalctl--add-face (alist-get priority-num journalctl-priority-strings)
                          (journalctl--priority-face record priority-num))))

(defun journalctl--timestamp (record &optional field-name)
  "Return a timestamp cons of (seconds . microseconds) for a journald RECORD.

FIELD-NAME defaults to __REALTIME_TIMESTAMP."
  (let* ((timestr (journalctl--get-value (or field-name "__REALTIME_TIMESTAMP")
                                         record))
         (len (length timestr))
         (seconds (string-to-number (substring timestr 0 (- len 6))))
         (microseconds (string-to-number (substring timestr (- len 6) len))))
    (cons seconds microseconds)))

(defun journalctl--format-timestamp (field-name &optional record)
  "Returns timestamp string for display from FIELD-NAME in RECORD."
  (let* ((timestamp (journalctl--timestamp record field-name))
         (display-time (format-time-string "%Y-%m-%d %H:%M:%S" (car timestamp))))
    (journalctl--add-face (concat display-time "." (format "%06d" (cdr timestamp)))
                          'journalctl-timestamp-face)))

(defun journalctl--format-pid (field-name record)
  "Returns _PID field value for display"
  (format "[%s]" (journalctl--get-value field-name record)))

(defun journalctl--format-field (field-name record)
  "Format FIELD_NAME from RECORD for display.

Finds format function from alist `journalctl-field-dformat-functions
falling back to simple string value display."
  (let ((format-function (alist-get field-name journalctl-field-format-functions
                                    'journalctl--get-value nil 'string-equal)))
    (funcall format-function field-name record)))

(defun journalctl--set-mode-line-process ()
  "Set the process info in the mode-line"
  (setq mode-line-process (if journalctl--process " running" " done")))

(defun journalctl--make-process (command)
  "Start journalctl COMMAND to be rendered to current journalctl-mode buffer."
  (let* ((target-buffer (current-buffer))
         (split-command (split-string-shell-command (string-trim command)))
         (file-handler (find-file-name-handler default-directory 'make-process))
         (make-process-args
          (list ':name command
                ':buffer (generate-new-buffer-name (concat " *Process: " command "*"))
                ':command (append split-command journalctl--required-arguments)
                ':filter 'journalctl--filter-incoming
                ':sentinel 'journalctl--process-sentinel)))
    (setq journalctl--process
          (if file-handler
              (apply file-handler 'make-process make-process-args)
            (apply 'make-process make-process-args)))
    (with-current-buffer (process-buffer journalctl--process)
      (setq-local journalctl--target-buffer target-buffer))
    (add-hook 'kill-buffer-hook 'journalctl--kill-process)
    (journalctl--set-mode-line-process)))

(defun journalctl--filter-incoming (process incoming)
  "Process filter function receiving JSON from journalctl; triggers parsing."
  (when (buffer-name (process-buffer process))
    (with-current-buffer (process-buffer process)
      (insert incoming)
      (journalctl--flush-json (current-buffer)))))

(defun journalctl--flush-json (process-buffer)
  "Parse JSON found in PROCESS-BUFFER and insert to its journalctl--target-buffer."
  (with-current-buffer process-buffer
    (save-excursion
      (goto-char (point-min))
      (let (output newline-pos)
        ;; while we find newlines, extract and parse the line and insert it into
        ;; the target buffer.
        (while (setq newline-pos (search-forward "\n" nil t))
          (let ((line (delete-and-extract-region (point-min) newline-pos)))
            (setq output
                  (concat
                   output
                   (condition-case err
                       (journalctl--format-line (json-parse-string line))
                     ((json-parse-error json-readtable-error)
                      (format  "ERROR: json parse error: %S\n\n%S\n\n" err line))
                     (error (format "ERROR: Failed to parse data: %S\n\n%S\n\n" err line)))))))
        ;; put output into the target buffer
        (when output
          (with-current-buffer journalctl--target-buffer
            (let ((return-end (eq (point) (point-max))))
              (save-excursion
                (widen)
                (goto-char (point-max)) ;; TODO: should be a marker, right
                (undo-boundary)         ;; info for timers says to do this
                (let ((pre-insert-point (point)))
                  (insert output)
                  (ansi-color-filter-region pre-insert-point (point)))
                (undo-boundary))
              (when return-end
                (goto-char (point-max))))))))))

(defun journalctl--process-sentinel (process event-description)
  "Sentinel function for a journalctl PROCESS serving to a journalctl-mode buffer."
  (when (process-buffer process)
    (journalctl--flush-json (process-buffer process)))
  (when (not (and (process-live-p process) (buffer-name (process-buffer process))))
    (kill-buffer (process-buffer process)))
  (journalctl--set-mode-line-process))

(defun journalctl--make-help-message (record)
  "Return a help message for help-echo on the printed line for RECORD."
  (let* ((timestamp (journalctl--timestamp record))
         (timestr (format (format-time-string "%Y-%m-%d %H:%M:%S.%%06d %p %Z" (car timestamp))
                          (cdr timestamp)))
         (file (journalctl--get-value "CODE_FILE" record))
         (unit (or (journalctl--get-value "_SYSTEMD_USER_UNIT" record)
                   (journalctl--get-value "_SYSTEMD_UNIT" record))))
    (concat timestr
            (if file (format "\nSource: %s:%s"
                             file
                             (journalctl--get-value "CODE_LINE" record)))
            "\nHost  : " (journalctl--get-value "_HOSTNAME" record)
            "\nUnit  : " unit
            "\nPID   : " (journalctl--get-value "_PID" record)
            )))

(defun journalctl--format-line (record)
  "Return journald RECORD formatted as a propertized text line.

This stores RECORD as `journalctl--record record' property on the line itself."
  (let* ((result (concat
                  (journalctl--format-field "__REALTIME_TIMESTAMP" record) " "
                  (journalctl--format-field "PRIORITY" record) " "
                  (journalctl--add-face
                   (format "%-20s"(journalctl--format-field "SYSLOG_IDENTIFIER" record))
                   'journalctl-source-face)
                  " "))
         (help-message (journalctl--make-help-message record))
         (message-prefix (make-string (length result) ?\ )))
    (setq result (concat result
                         (propertize
                          (journalctl--format-field "MESSAGE" record)
                          'wrap-prefix message-prefix
                          'line-prefix message-prefix
                          'help-echo help-message)))
    (put-text-property 0 1 'journalctl--record record result)
    (concat result "\n")))

(defun journalctl--get-line-record (&optional at-point)
  "Fetch the parsed json record for the line a (or AT-POINT (point))."
  (save-excursion
    (when at-point (goto-char at-point))
    (get-text-property (pos-bol) 'journalctl--record)))

(defun journalctl-jump-to-line-source ()
  "Jump to the source of the message at point, if possible."
  (interactive)
  (let* ((record (journalctl--get-line-record))
         (local-file (journalctl--get-value "CODE_FILE" record))
         (pathname (concat (file-remote-p default-directory) local-file)))
    (when (file-readable-p pathname)
      ;; with M-. we're emulating xref so push marker for M-,
      (xref-push-marker-stack)
      (find-file pathname)
      (when-let ((line (journalctl--get-value "CODE_LINE" record)))
        (goto-line (string-to-number line))))))

(defvar journalctl-mode-map
  (let ((map (make-sparse-keymap)))
    ;; example definition
    (define-key map (kbd "M-.") 'journalctl-jump-to-line-source)
    (define-key map (kbd "C-c C-c") 'journalctl--kill-process)
    map)
  "Basic mode map for `journalctl-mode'")

(define-derived-mode journalctl-mode fundamental-mode "journalctl"
  "Major mode for browsing journald records with `journalctl'.

\\{journalctl-mode-map}"
  ;; highlight-symbol, and its nav mode, are highly useful.  Is this too
  ;; opinionated to include?:
  (when (featurep 'highlight-symbol)
    (highlight-symbol-mode)
    (highlight-symbol-nav-mode))
  ;; visual-line makes sense for messages to flow nicely
  (visual-line-mode))

;;;###autoload
(defun journalctl (command)
  "Browse journald logs inside Emacs.

With COMMAND and with prefix ARG, prompt for editing the command."
  (interactive
   (list
    (read-shell-command "Journalctl command: " (car journalctl-history) 'journalctl-history)
    current-prefix-arg))
  (when current-prefix-arg
    (setq command (read-shell-command "Journalctl command: " command 'journalctl-history)))
  (let ((remote-host (file-remote-p default-directory)))
    (pop-to-buffer (generate-new-buffer
                    (concat "*" remote-host (and remote-host " ") command "*"))))
  (journalctl-mode)
  (journalctl--make-process command)
  (goto-char (point-max)))

(provide 'journalctl-mode)
;;; journalctl-mode.el ends here
