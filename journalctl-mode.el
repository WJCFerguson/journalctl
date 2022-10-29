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
;; journalctl-mode adds an xref source so...
;;
;;; Code:

(defvar journalctl-program (executable-find "journalctl")
  "Path to the program used `journalctl'")

(defvar journalctl-arguments '("--output=json")
  "Command-line arguments to pass to `journalctl-program'")

(defvar journalctl-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `journalctl'")

(defvar-local journalctl--read-buffer ""
  "A read buffer for incoming message data so it can be parsed line-wise.")

(defun journalctl--filter-input (incoming)
  "Parse an incoming json line into an annotated text line."
  ;; put it in the buffer
  (setq journalctl--read-buffer (concat journalctl--read-buffer incoming))
  ;; check buffer for complete messages
  (let (output)
    (while-let ((newline-pos (string-search "\n" journalctl--read-buffer))
                (line (substring journalctl--read-buffer 0 newline-pos)))
      (setq journalctl--read-buffer (substring journalctl--read-buffer (+ 1 newline-pos)))
      (setq output (concat output (journalctl--parse-line line))))
    output))

(defun journalctl--parse-line (line)
  "Take an incoming JSON line and return the text to insert into the buffer."
  (condition-case err
      (if-let ((data (json-parse-string line)))
          (concat (gethash "MESSAGE" data) "\n")
        (message "ERROR: failed to parse line: %s" line))
    ((json-parse-error json-readtable-error)
     (format  "ERROR: parse fail: %S\n\n%S\n\n" err line))))

(defun journalctl--initialize ()
  "Helper function to initialize Journalctl"
  (setq-local comint-preoutput-filter-functions '(journalctl--filter-input)))

(define-derived-mode journalctl-mode comint-mode "Journalctl"
  "Major mode for `run-journalctl'.

\\<journalctl-mode-map>"
  ;; body here.  Does the previous line make any sense?
)
(add-hook 'journalctl-mode-hook 'journalctl--initialize)

;;;###autoload
(defun journalctl ()
  "Browse journald logs inside Emacs."
  (interactive)
  ;; use apply to expand journalctl-arg
  (let ((buffer-name (generate-new-buffer-name "*Journalctl*")))
    (pop-to-buffer-same-window
     (apply 'make-comint-in-buffer "Journalctl"
            buffer-name
            journalctl-program nil journalctl-arguments)))
  (journalctl-mode))

(provide 'journalctl-mode)
;;; journalctl-mode.el ends here
