;;; mermaid-ts-mode.el --- major mode for mermaid with tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2024, Renato Fernandes de Queiroz

;; Author: Renato Fernandes de Queiroz
;; Version: 0.1
;; Keywords: mermaid tree-sitter
;; License: GNU General Public License = 3
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Comentary:

;; Tree-sitter based major mode for working with mermaid graphs.
;; See https://mermaid-js.github.io/

;; Derived from mermaid-mode by Adrien Brochard. You can find the original work
;; at https://github.com/abrochard/mermaid-mode

;;; Code:
(require 'treesit)
(require 'ob)
(require 'ob-eval)

(defgroup mermaid-ts-mode nil
  "Major mode for working with mermaid. tree-sitter based"
  :group 'extensions)

(defcustom mermaid-ts-mmdc-location "mmdc"
  "Mmdc location."
  :type 'string
  :group 'mermaid-ts-mode)

(defcustom mermaid-ts-output-format ".png"
  "Mmdc output format."
  :group 'mermaid-ts-mode
  :type 'string)

(defcustom mermaid-ts-tmp-dir "/tmp/"
  "Dir for tmp files."
  :group 'mermaid-ts-mode
  :type 'string)

(defcustom mermaid-ts-flags ""
  "Additional flags to pass to the mermaid-cli."
  :group 'mermaid-ts-mode
  :type 'string)

(defvar org-babel-default-header-args:mermaid-ts
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a mermaid source block.")

(defun org-babel-execute:mermaid-ts (body params)
  "Execute command with BODY and PARAMS from src block."
  (let* ((out-file (or (cdr (assoc :file params))
                       (error "Mermaid requires a \":file\" header argument")))
         (temp-file (org-babel-temp-file "mermaid-"))
         (width (cdr (assoc :width params)))
         (height (cdr (assoc :height params)))
         (theme (cdr (assoc :theme params)))
         (background-color (cdr (assoc :background-color params)))
         (cmd (concat (shell-quote-argument mermaid-ts-mmdc-location)
                      " -o " (org-babel-process-file-name out-file)
                      " -i " temp-file
                      (when width (format " -w %s" width))
                      (when height (format " -H %s" height))
                      (when theme (concat " -t " theme))
                      (when background-color (concat " -b " background-color))
                      " " mermaid-ts-flags)))
    (with-temp-file temp-file (insert body))
    (org-babel-eval cmd "")
    nil))


(defvar mermaid-ts-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; Comment style "%% ..."
    (modify-syntax-entry ?% ". 124" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    syntax-table)
  "Syntax table for `mermaid-ts-mode'.")

(defun mermaid-ts-setup ()
  "Setup treesit for mermaid-ts-mode."

  (setq-local treesit-font-lock-feature-list
              '((comment)
                (declaration)
                (operator)
                (statement)
                (name)))
  (setq-local treesit-font-lock-level 5)

  ;; This handles font locking
  (setq-local treesit-font-lock-settings
              (treesit-font-lock-rules
               :language 'mermaid
               :override t
               :feature 'comment
               '((comment) @font-lock-comment-face)

               :language 'mermaid
               :override t
               :feature 'declaration
               "([\"sequenceDiagram\" \"stateDiagram-v2\" \"flowchart\"]
                 @font-lock-keyword-face)"

               :language 'mermaid
               :override t
               :feature 'operator
               "[(sequence_stmt_signal (sequence_signal_type)
                                       @font-lock-function-name-face
                                       .
                                       [(sequence_signal_plus_sign)
                                        (sequence_signal_minus_sign)]?
                                       @font-lock-function-name-face)
                 (state_stmt_arrow (state_arrow) @font-lock-function-name-face)
                 (flow_stmt_vertice [(flow_link_simplelink)
                                     (flow_link_arrowtext)
                                     (flow_link_middletext)]
                                    @font-lock-function-name-face)]"

               :language 'mermaid
               :override t
               :feature 'statement
               "[(sequence_stmt_participant .
                                            [\"participant\" \"actor\"]
                                            @font-lock-constant-face
                                            (sequence_actor)
                                            .
                                            \"as\"
                                            @font-lock-constant-face)
                 (sequence_stmt_autonumber) @font-lock-constant-face
                 (sequence_stmt_activate .
                                         \"activate\"
                                         @font-lock-constant-face)
                 (sequence_stmt_deactivate .
                                           \"deactivate\"
                                           @font-lock-constant-face)
                 (sequence_stmt_loop .
                                     \"loop\"
                                     @font-lock-constant-face
                                     _
                                     \"end\"
                                     @font-lock-constant-face
                                     .)
                 (sequence_stmt_alt .
                                    \"alt\"
                                    @font-lock-constant-face
                                    _
                                    \"else\"
                                    @font-lock-constant-face
                                    _
                                    \"end\"
                                    @font-lock-constant-face
                                    .)
                 (sequence_stmt_opt . \"opt\"  @font-lock-constant-face)
                 (sequence_stmt_par . \"par\"  @font-lock-constant-face)
                 (sequence_stmt_note .
                                     \"note
                                     \"
                                     @font-lock-constant-face
                                     .
                                     [(sequence_note_placement)
                                      @font-lock-constant-face
                                      \"over\"
                                      @font-lock-constant-face])]"

               :language 'mermaid
               :override t
               :feature 'name
               "([(state_name)
                  (sequence_actor)
                  (flow_vertex_id)]
                 @font-lock-variable-use-face)"))

  ;; This handles indentation
  (setq-local treesit-simple-indent-rules
              `((mermaid
                 ((parent-is ,(regexp-opt '("diagram_sequence"
                                            "diagram_state"
                                            "diagram_flow")))
                  parent
                  4)
                 (no-node parent 0))))

  ;; End with this
  (treesit-major-mode-setup))

(defun mermaid-ts-compile ()
  "Compile the current mermaid file using mmdc."
  (interactive)
  (mermaid-ts-compile-file (buffer-file-name)))

(defun mermaid-ts-compile-buffer ()
  "Compile the current mermaid buffer using mmdc."
  (interactive)
  (let* ((tmp-file-name (concat mermaid-ts-tmp-dir "current-buffer.mmd")))
    (write-region (point-min) (point-max) tmp-file-name)
    (mermaid-ts-compile-file tmp-file-name)))

(defun mermaid-ts-compile-region ()
  "Compile the current mermaid region using mmdc."
  (interactive)
  (let* ((tmp-file-name (concat mermaid-ts-tmp-dir "current-region.mmd")))
    (when (use-region-p)
      (write-region (region-beginning) (region-end) tmp-file-name)
      (mermaid-ts-compile-file tmp-file-name))))

(defun mermaid-ts-compile-file (file-name)
  "Compile the given mermaid file using mmdc."
  (interactive "fFilename: ")
  (let* ((input file-name)
         (output (concat (file-name-sans-extension input) mermaid-ts-output-format))
         (exit-code (apply #'call-process mermaid-ts-mmdc-location nil "*mmdc*" nil (append (split-string mermaid-ts-flags " ") (list "-i" input "-o" output)))))
    (if (zerop exit-code)
        (let ((buffer (find-file-noselect output t)))
          (display-buffer buffer)
          (save-excursion
            (set-buffer buffer)
            (auto-revert-mode)))
      (pop-to-buffer "*mmdc*"))))



(defvar mermaid-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'mermaid-ts-compile)
    (define-key map (kbd "C-c C-f") 'mermaid-ts-compile-file)
    (define-key map (kbd "C-c C-b") 'mermaid-ts-compile-buffer)
    (define-key map (kbd "C-c C-r") 'mermaid-ts-compile-region)
    map))


;;;###autoload
(define-derived-mode mermaid-ts-mode prog-mode "Mermaid[TS]"
  "Major mode for editing Mermaid with tree-sitter."
  :syntax-table mermaid-ts-syntax-table

  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'mermaid)
    (treesit-parser-create 'mermaid)
    (mermaid-ts-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-ts-mode))

(provide 'mermaid-ts-mode)
;;; mermaid-ts-mode.el ends here
