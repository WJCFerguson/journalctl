;;; journalctl-mode.el --- Journalctl browsing mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  James Ferguson

;; Author: James Ferguson <james@faff.org>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major-mode for Emacs to view journald logs in Emacs.
;;
;; It derives from comint-mode to give the typical facilities of viewing command
;; output, but augmented.
;;
;;;; Example Installation
;;
;; (use-package journalctl-mode
;;  :bind ("C-c j" . journalctl))
;;
;;;; Features:
;;
;; * prettified output
;; * toggle-able follow (-f) mode
;; * TODO
;;
;; Key bindings
;; * TODO
;;
;;;; Tips/Tricks
;;
;; * Don't forget good old regex highlighting ... M-s h (r|l)' (also from isearch)
;;
;; xref integration (?)
;;
;;; Code:

(require 'xref)

;; =============================== Customization ===============================
(defgroup journalctl nil
  "Journalctl browsing mode."
  :group 'tools
  :group 'convenience
  :prefix "journalctl-")

(defcustom journalctl-priority-faces
  '((0 . journalctl-error-face)
    (1 . journalctl-error-face)
    (2 . journalctl-error-face)
    (3 . journalctl-error-face)
    (4 . journalctl-warning-face)
    (5 . journalctl-warning-face)
    (7 . journalctl-debug-face))
  "Display faces by priority"
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
  '((0 . "!")
    (1 . "A")
    (2 . "C")
    (3 . "E")
    (4 . "W")
    (5 . "N")
    (6 . "I")
    (7 . "D"))
  "Display strings for various priorities.

Should be configured to have equal length"
  :type '(alist :key-type number :value-type string))

(defgroup journalctl-faces nil
  "Display informations of the current line."
  :group 'tools
  :group 'convenience
  :group 'journalctl)

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
  "History list for journalctl.")

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

(defvar-local journalctl--read-buffer ""
  "A read buffer for incoming message data so it can be parsed line-wise.")

(defun journalctl--get-value (field-name record)
  "Return FIELD-NAME from RECORD"
  ;; multibyte strings come as a vector so we have to convert.  NOTE: this seems
  ;; flawed, e.g. when starting Node there are some failed characters vs text
  ;; output.
  (let ((msg (gethash field-name record)))
    (if (vectorp msg)
        (string-as-multibyte (mapconcat #'byte-to-string (gethash field-name record) ""))
      msg)))

(defun journalctl--priority-face (record &optional priority-num)
  "Return the priority-based face (if any) for RECORD.

If PRIORITY-NUM is supplied, it will not be fetched again from RECORD."
  (let ((priority-num (or priority-num
                          (string-to-number (journalctl--get-value "PRIORITY"
                                                                   record)))))
    (alist-get priority-num journalctl-priority-faces)))

(defun journalctl--add-face (str face &optional start end)
  (if (get-text-property 0 'font-lock-face str)
      (or (add-face-text-property (or start 0) (or end (length str))
                                  face
                                  t ;; append
                                  str)
          str) ;; return str
    (propertize str 'font-lock-face face)))

(defun journalctl--format-message (field-name record)
  "Returns FIELD_NAME from RECORD for display as a priority level."
  (let ((result (journalctl--get-value field-name record)))
    (if-let (priority-face (journalctl--priority-face record))
        (setq result (journalctl--add-face result priority-face)))
    (when (string-equal "systemd" (journalctl--get-value "SYSLOG_IDENTIFIER" record))
      (setq result (journalctl--add-face result 'journalctl-systemd-face))
      (when (string-match "Start\\(ed\\|ing\\)" result)
        (setq result (journalctl--add-face result 'journalctl-systemd-starting-face
                                           (match-beginning 0) (match-end 0))))
      (when (string-match "Stopp\\(ed\\|ing\\)" result)
        (setq result (journalctl--add-face result 'journalctl-systemd-finishing-face
                                           (match-beginning 0) (match-end 0)))))
    result))

(defun journalctl--format-priority (field-name record)
  "Returns FIELD_NAME from RECORD for display as a priority level."
  (let* ((value (journalctl--get-value field-name record))
         (priority-num (string-to-number value)))
    (journalctl--add-face (alist-get priority-num journalctl-priority-strings)
                   (journalctl--priority-face record priority-num))))

(defun journalctl--timestamp (record)
  "Return a cons of (seconds . microseconds) for a journald RECORD."
  (let* ((timestr (journalctl--get-value "__REALTIME_TIMESTAMP" record))
         (len (length timestr))
         (seconds (string-to-number (substring timestr 0 (- len 6))))
         (microseconds (string-to-number (substring timestr (- len 6) len))))
    (cons seconds microseconds)))

(defun journalctl--format-timestamp (field-name record)
  "Returns PRIORITY field value for display"
  (let* ((timestamp (journalctl--timestamp record))
         (display-time (format-time-string "%b %d %H:%M:%S" (car timestamp))))
    (journalctl--add-face (concat display-time "." (format "%06d" (cdr timestamp)))
                   'journalctl-timestamp-face)))

(defun journalctl--format-pid (field-name record)
  "Returns _PID field value for display"
  (format "[%s]" (journalctl--get-value field-name record)))

(defun journalctl--format-field (field-name record)
    "Format FIELD_NAME from RECORD for display.

Finds format function from alist `journalctl-field-dformat-functions
falling back to simple string value display.
"
  (funcall (alist-get field-name journalctl-field-format-functions
                      'journalctl--get-value nil 'string-equal)
           field-name
           record))

(defun journalctl--filter-incoming (incoming)
  "Capture incoming JSON stream and buffer to read line-wise."
  (setq journalctl--read-buffer (concat journalctl--read-buffer incoming))
  (let (output newline-pos)
    (while (setq newline-pos (string-search "\n" journalctl--read-buffer))
      (let ((line (substring journalctl--read-buffer 0 newline-pos)))
        (setq journalctl--read-buffer (substring journalctl--read-buffer (+ 1 newline-pos)))
        (setq output
              (concat
               output
               (condition-case err
                   (journalctl--format-line (json-parse-string line))
                 ((json-parse-error json-readtable-error)
                  (format  "ERROR: json parse error: %S\n\n%S\n\n" err line))
                 (error (format "ERROR: Failed to parse data: %S\n\n%S\n\n" err line)))))))
    output))

(defun journalctl--make-help-message (record)
  "Return a help message for help-echo on the printed line for RECORD."
  (let* ((timestamp (journalctl--timestamp record))
         (timestr (format (format-time-string "%a %F %H:%M:%S.%%06d %p %Z" (car timestamp))
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
                  (journalctl--add-face (journalctl--format-field "SYSLOG_IDENTIFIER" record)
                                 'journalctl-source-face)
                  " "
                  (journalctl--format-field "PRIORITY" record)
                  ":"))
         (help-message (journalctl--make-help-message record))
         (message-prefix (make-string (length result) ?\ )))
    (setq result (concat result
                         (propertize
                          (journalctl--format-field "MESSAGE" record)
                          'wrap-prefix message-prefix
                          'line-prefix message-prefix)))
    (put-text-property 0 (length result) 'help-echo help-message result)
    (put-text-property 0 (length result) 'journalctl--record record result)
    (concat result "\n")))

(defun journalctl--get-line-record (&optional at-point)
  "Get the parsed record from the current line, or AT-POINT if set."
  (let ((at-point (or at-point (point))))
    (get-text-property at-point 'journalctl--record)))

(defun journalctl-jump-to-line-source ()
  "Jump to the source of the message if possible."
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
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map (kbd "M-.") 'journalctl-jump-to-line-source)
    map)
  "Basic mode map for `journalctl'")

(define-derived-mode journalctl-mode comint-mode "Journalctl"
  "Major mode for `run-journalctl'.

\\{journalctl-mode-map}"
  ;; body here.  Does the previous line make any sense?

  ;; we handle all the highlighting.  Or does this break
  (visual-line-mode)
  (setq-local
   ;; parse incoming JSON into text and a record
   comint-preoutput-filter-functions '(journalctl--filter-incoming)
   ;; remove a comint function that may or may not be relevant
   comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom)
   ;; there is probably more we could disable in comint...
   comint-highlight-input nil))

;;;###autoload
(defun journalctl (command)
  "Browse journald logs inside Emacs.  With prefix ARG, allow editing command."
  ;; TODO: `transient' interface, but for now here's a foot-gun
  (interactive
   (list
    (read-shell-command "Journalctl command: " (car journalctl-history) 'journalctl-history)
    current-prefix-arg))
  (when current-prefix-arg
    (setq command (read-shell-command "Journalctl command: " command 'journalctl-history)))
  (let* ((remote-host (file-remote-p default-directory))
         (buffer-name (generate-new-buffer-name
                       (format "*%s%s*"
                               (if remote-host (concat remote-host " ") "")
                               command)))
        (split-command (split-string-shell-command (string-trim command))))
    (pop-to-buffer-same-window
     (apply 'make-comint-in-buffer "Journalctl"
            buffer-name
            (car split-command) nil
            (append (cdr split-command)
                    journalctl--required-arguments))))
  (journalctl-mode))

(provide 'journalctl-mode)
;;; journalctl-mode.el ends here
