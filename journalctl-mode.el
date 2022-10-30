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

(defvar journalctl-program (executable-find "journalctl")
  "Path to the program used `journalctl'")

(defvar journalctl-arguments '()
  "Command-line arguments to pass to `journalctl-program'")

(defvar journalctl--required-arguments '("--output=json")
  "Arguments non-negotiable for journalctl ")

;; =================================== debug ===================================
(setq journalctl-arguments '("-f"))
;; ================================= end debug =================================

(defvar journalctl-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `journalctl'")

(defvar-local journalctl--read-buffer ""
  "A read buffer for incoming message data so it can be parsed line-wise.")

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

(defun journalctl--timestamp (record)
  "Return a cons of (seconds . microseconds) for a journald RECORD."
  (let* ((timestr (gethash "__REALTIME_TIMESTAMP" record))
         (len (length timestr))
         (seconds (string-to-number (substring timestr 0 (- len 6))))
         (microseconds (string-to-number (substring timestr (- len 6) len))))
    (cons seconds microseconds)))

(defun journalctl--format-line (record)
  "Return journald RECORD formatted as a propertized text line.

This stores RECORD as `journalctl--record record' property on the line itself."
  (let* ((timestamp (journalctl--timestamp record))
         (display-time (format-time-string "%b %d %H:%M:%S" (car timestamp))))
    (let* ((result (propertize (concat display-time "." (number-to-string (cdr timestamp)) " ")
                               'face 'font-lock-comment-face))
           (pre-message-length (length result)))
      (setq result (concat result
                           (propertize
                            (gethash "MESSAGE" record)
                            'wrap-prefix (make-string pre-message-length ?\ ))))
      ;; put the record as a text property on the line
      (put-text-property 0 (length result)
                         'journalctl--record record
                         result)
      (concat result "\n"))))

(define-derived-mode journalctl-mode comint-mode "Journalctl"
  "Major mode for `run-journalctl'.

\\<journalctl-mode-map>"
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
