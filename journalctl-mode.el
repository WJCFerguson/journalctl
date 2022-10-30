;;; journalctl-mode.el --- Journalctl browsing mode  -*- lexical-biding: t; -*-

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
  :group 'convenience
  :prefix "journalctl-")

(defcustom journalctl-field-format-functions
  '(("PRIORITY" . journalctl--format-priority)
    ("__REALTIME_TIMESTAMP" . journalctl--format-timestamp)
    ("_PID" . journalctl--format-pid)
    ("MESSAGE" . journalctl--priority-colored-field))
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

(defcustom journalctl-priority-faces
  '((0 . 'compilation-error)
    (1 . 'compilation-error)
    (2 . 'compilation-error)
    (3 . 'compilation-error)
    (4 . 'compilation-warning)
    (5 . 'compilation-warning)
    (7 . 'shadow))
  "Display faces by priority"
  :type '(alist :key-type number :value-type string))

;; ============================= End Customization =============================

(defvar journalctl-program (executable-find "journalctl")
  "Path to the program used `journalctl'")

(defvar journalctl-arguments '()
  "Command-line arguments to pass to `journalctl-program'")

(defvar journalctl--required-arguments '("--output=json")
  "Arguments non-negotiable for journalctl ")

;; =================================== debug ===================================
(setq journalctl-arguments '("-f" "-t" "flange"))
;; ================================= end debug =================================

(defvar-local journalctl--read-buffer ""
  "A read buffer for incoming message data so it can be parsed line-wise.")

(defun journalctl--get-value (field-name record)
  "Return FIELD-NAME from RECORD"
  (gethash field-name record))

(defun journalctl--priority-face (record &optional priority-num)
  "Return the priority-based face (if any) for RECORD.

If PRIORITY-NUM is supplied, it will not be fetched again from RECORD."
  (let ((priority-num (or priority-num
                          (string-to-number (journalctl--get-value "PRIORITY"
                                                                   record)))))
    (alist-get priority-num journalctl-priority-faces)))

(defun journalctl--priority-colored-field (field-name record)
  "Returns FIELD_NAME from RECORD for display as a priority level."
  (propertize (journalctl--get-value field-name record)
                'face (journalctl--priority-face record)))

(defun journalctl--format-priority (field-name record)
  "Returns FIELD_NAME from RECORD for display as a priority level."
  (let* ((value (journalctl--get-value field-name record))
         (priority-num (string-to-number value)))
    (propertize (alist-get priority-num journalctl-priority-strings)
                'face (journalctl--priority-face nil priority-num))))

(defun journalctl--timestamp (field-name record)
  "Return a cons of (seconds . microseconds) for a journald RECORD."
  (let* ((timestr (journalctl--get-value field-name record))
         (len (length timestr))
         (seconds (string-to-number (substring timestr 0 (- len 6))))
         (microseconds (string-to-number (substring timestr (- len 6) len))))
    (cons seconds microseconds)))

(defun journalctl--format-timestamp (field-name record)
  "Returns PRIORITY field value for display"
  (let* ((timestamp (journalctl--timestamp field-name record))
         (display-time (format-time-string "%b %d %H:%M:%S" (car timestamp))))
    (propertize (concat display-time "."
                        (format "%06d" (cdr timestamp)))
                'face 'font-lock-comment-face)))

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
  (let (output)
    (while-let ((newline-pos (string-search "\n" journalctl--read-buffer))
                (line (substring journalctl--read-buffer 0 newline-pos)))
      (setq journalctl--read-buffer (substring journalctl--read-buffer (+ 1 newline-pos)))
      (setq output
            (concat
             output
             (condition-case err
                 (journalctl--format-line (json-parse-string line))
               ((json-parse-error json-readtable-error)
                (format  "ERROR: parse fail: %S\n\n%S\n\n" err line))))))
    output))

(defun journalctl--format-line (record)
  "Return journald RECORD formatted as a propertized text line.

This stores RECORD as `journalctl--record record' property on the line itself."
  (let* ((result (concat
                  (journalctl--format-field "__REALTIME_TIMESTAMP" record) " "
                  (journalctl--format-field "_HOSTNAME" record) " "
                  (journalctl--format-field "SYSLOG_IDENTIFIER" record)
                  (journalctl--format-field "_PID" record) " "
                  (journalctl--format-field "PRIORITY" record) ": "))
         (pre-message-length (length result)))
    (setq result (concat result
                         (propertize
                          (journalctl--format-field "MESSAGE" record)
                          'wrap-prefix (make-string pre-message-length ?\ ))))
    ;; put the record as a text property on the line
    (put-text-property 0 (length result)
                       'journalctl--record record
                       result)
    (concat result "\n")))

(defun journalctl--get-line-record (&optional at-point)
  "Get the parsed record from the current line, or AT-POINT if set."
  (let ((at-point (or at-point (point))))
    (get-text-property at-point 'journalctl--record)))

(defun journalctl-jump-to-line-source ()
  "Jump to the source of the message if possible."
  (interactive)
  (let* ((record (journalctl--get-line-record))
         (file (journalctl--get-value "CODE_FILE" record)))
    (when (file-exists-p file)
      ;; with M-. we're emulating xref, so allow us to jump back with M-,
      (xref-push-marker-stack)
      (find-file file)
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
  (font-lock-mode -1)
  (visual-line-mode)
  (setq-local
   ;; parse incoming JSON into text and a record
   comint-preoutput-filter-functions '(journalctl--filter-incoming)
   ;; there is probably more we could disable in comint...
   comint-highlight-input nil))

;;;###autoload
(defun journalctl ()
  "Browse journald logs inside Emacs."
  (interactive)
  ;; use apply to expand journalctl-arg
  (let ((buffer-name (generate-new-buffer-name "*Journalctl*")))
    (pop-to-buffer-same-window
     (apply 'make-comint-in-buffer "Journalctl"
            buffer-name
            journalctl-program nil
            (append journalctl-arguments journalctl--required-arguments))))
  (journalctl-mode))

(provide 'journalctl-mode)
;;; journalctl-mode.el ends here
