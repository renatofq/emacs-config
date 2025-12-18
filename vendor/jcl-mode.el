;;; jcl-mode.el --- IBM JCL Major Mode               -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Laurent Siksous

;; Author: Laurent Siksous <lss@free.fr>
;; Keywords: JCL

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

;;

;;; Code:

(defgroup jcl nil
  "Major mode for editing JCL."
  :prefix "jcl-"
  :group 'languages
  )

(defvar jcl-mode-hook nil)

(defvar jcl-mode-map
  (let ((map (make-keymap)))
;;    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for JCL major mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jcl\\'" . jcl-mode))

(defvar jcl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?' "$" st)
     st)
  "Syntax table for jcl-mode.")

;;; Keywords
(defconst jcl-keywords '("JOB" "EXEC" "DD")  )
(defconst jcl-keywords-regexp (regexp-opt jcl-keywords 'words))

;; Operators
(defconst jcl-operators
	'("=" "," "(" ")")
	"JCL operators."
	)
(defvar jcl-operators-regexp (regexp-opt jcl-operators))

(defvar jcl-font-lock-keywords
  `(
    ;; SYSINs
    ("^[^\/].*$" . font-lock-preprocessor-face)
    ;; Parms between quotes
    ("\'.*\'" 0 font-lock-string-face)
    ;; Comments
    ("^\/\/\\*.*$" . font-lock-comment-face)
    (,jcl-operators-regexp . font-lock-constant-face)
    (,jcl-keywords-regexp . font-lock-keyword-face)
   ))

;;(defvar font-lock-defaults '((jcl-font-lock-keywords)))

;; Preparing for commenting, uncommenting and indenting...
(defun jcl-comment-current-line ()
  "Add '*' in the third column."
  (interactive)
  (move-to-column 2)
  (delete-char 1)
  (insert "*")
  )

(defun jcl-mode ()
  "Major mode for editing JCL files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table jcl-mode-syntax-table)
  (use-local-map jcl-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(jcl-font-lock-keywords))
  (setq major-mode 'jcl-mode)
  (setq mode-name "JCL")
  (run-hooks 'jcl-mode-hook))

(provide 'jcl-mode)

;;; jcl-mode.el ends here
