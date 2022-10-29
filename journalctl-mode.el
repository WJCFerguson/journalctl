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
;; journalctl-mode adds an xref source so...
;;
;;; Code:

(defvar journalctl-program (executable-find "journalctl")
  "Path to the program used `journalctl'")

(defvar journalctl-arguments '("--follow" "--lines=10")
  "Command-line arguments to pass to `journalctl-program'")

(defvar journalctl-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `journalctl'")

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

(defun journalctl--initialize ()
  "Helper function to initialize Journalctl")

(define-derived-mode journalctl-mode comint-mode "Journalctl"
  "Major mode for `run-journalctl'.

\\<journalctl-mode-map>"
  ;; body here.  Does the previous line make any sense?
)
(add-hook 'journalctl-mode-hook 'journalctl--initialize)

(provide 'journalctl-mode)
;;; journalctl-mode.el ends here
