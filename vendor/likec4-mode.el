;;; likec4-mode.el --- Major mode for editing LikeC4 dsl files -*- lexical-binding: t -*-

;; Copyright (c) 2024 Renato Fernandes de Queiroz <renatofq@gmail.com>

;; Author: Renato Fernandes de Queiroz <renatofq@gmail.com>
;; Created: 19 September 2024
;; Version: 0.1
;; Keywords: likec4 c4 dsl major-mode

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Derived from struturizr-mode by Giles Paterson. You can find the
;; original work at https://github.com/gilesp/structurizr-mode

;;; Code:

;; Define mode hook
(defvar likec4-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.c4\\'" . likec4-mode))

;; create the keyword list for font-lock
;; each category of keyword is given a particular face
(setq likec4-font-lock-keywords
      ;; define several categories of keywords
      (let* ((x-keywords '("specification"
                           "model"
                           "views"
                           "view"
                           "dynamic"
                           "element"
                           "configuration"
                           "tag"
                           "style"
                           "relationship"
                           "extend"
                           "with"
                           "where"
                           "kind"
                           "is"
                           "not"
                           "and"))
             (x-relationship '("->" "<-" "<->"))
             (x-properties '("include"
                             "description"
                             "title"
                             "shape"
                             "color"
                             "icon"
                             "opacity"
                             "border"
                             "technology"
                             "link"
                             "notation"))
             (x-consts '("rectangle"
                         "storage"
                         "cylinder"
                         "browser"
                         "mobile"
                         "person"
                         "queue"
                         "primary"
                         "blue"
                         "secondary"
                         "sky"
                         "muted"
                         "slate"
                         "amber"
                         "gray"
                         "green"
                         "indigo"
                         "red"
                         "dashed"
                         "solid"
                         "none"
                         "dotted"))

             ;; generate regex string for each category of keywords.
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-relationship-regexp (regexp-opt x-relationship))
             (x-properties-regexp (regexp-opt x-properties 'words))
             (x-consts-regexp (regexp-opt x-consts 'words)))

        ;; note: following order matters, because once coloured,
        ;; that part won't change. In general, put longer words first.
        `((,x-properties-regexp . 'font-lock-variable-name-face)
          (,x-keywords-regexp . 'font-lock-keyword-face)
          (,x-relationship-regexp . 'font-lock-builtin-face)
          (,x-consts-regexp . 'font-lock-constant-face))))

(defvar likec4-mode-syntax-table nil "Syntax table for `likec4-mode'.")

(setq likec4-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ; Comments start with //
    (modify-syntax-entry ?/ "< 12" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    (modify-syntax-entry ?\" "\"" syntax-table)
    (modify-syntax-entry ?{ "(" syntax-table)
    (modify-syntax-entry ?} ")" syntax-table)
    (modify-syntax-entry ?, "." syntax-table)
    syntax-table))

;; indentation
(defvar likec4-indent-offset 2
  "*Indentation offset for `likec4-mode'.")

(defun likec4-indent-line ()
  "Indent current line as LikeC4 dsl"
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "{")
              (setq indent-col (+ indent-col likec4-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "}") (>= indent-col likec4-indent-offset))
        (setq indent-col (- indent-col likec4-indent-offset))))
    (indent-line-to indent-col)))


;;;###autoload
(define-derived-mode likec4-mode fundamental-mode "LikeC4"
  "Major mode for editing LikeC4 dsl"
  ;; indentation
  (make-local-variable 'likec4-indent-offset)
  (set (make-local-variable 'indent-line-function) 'likec4-indent-line)
  ;; syntax table
  (set-syntax-table likec4-mode-syntax-table)
  ;; code for syntax highlighting
  (setq font-lock-defaults '((likec4-font-lock-keywords)))
  (run-hooks 'likec4-mode-hook))


;; add the mode to the `features' list
(provide 'likec4-mode)
;;; likec4-mode.el ends here
