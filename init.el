;; package --- Emacs init file

(defvar savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; set package-user-dir to be relative to Prelude install path
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)


;;;; UX setup
;; disable tool-bar menu-bar and scroll-bar
(tool-bar-mode -1)

;; I'll experiment menu-bar for a while
;; (menu-bar-mode -1)

;; Disable scroll-bar
(scroll-bar-mode -1)

;; maximize the initial frame automatically
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; font
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-13"))
(add-to-list 'default-frame-alist '(line-spacing . 0.1))

;; ediff-split horizontally
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; highlight the current line
(global-hl-line-mode +1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; disable dialog box
(setq use-dialog-box nil)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Move through windows with Shift-<arrow keys>
(windmove-default-keybindings)

;;;; Editor setup
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)            ;; but maintain correct appearance

;; newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; save-as functions
(defun save-as-and-switch (filename)
  "Clone the current buffer and switch to the clone"
  (interactive "FCopy and switch to file: ")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename nil nil nil 'confirm))
  (find-file filename))

(defun save-as-do-not-switch (filename)
  "Clone the current buffer but don't switch to the clone"
  (interactive "FCopy (without switching) to file:")
  (write-region (point-min) (point-max) filename)
  (find-file-noselect filename))

(defun save-as (filename)
  "Prompt user whether to switch to the clone."
  (interactive "FCopy to file: ")
  (if (y-or-n-p "Switch to new file?")
    (save-as-and-switch filename)
    (save-as-do-not-switch filename)))

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :config
  (which-key-mode))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

;; savehist keeps track of some history
(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode +1))

;; save recent files
(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

;; bookmarks
(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" savefile-dir)
        bookmark-save-flag 1))

;; whitespace-mode config
(use-package whitespace
  :diminish whitespace-mode
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing)))

(use-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src". "#\\+end_src"))
  (add-hook 'text-org-hook #'flyspell-mode))

;; meaningful names for buffers with the same name
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t       ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*"))  ; don't muck with special buffers

;; tramp
(use-package tramp
  :config (setq tramp-default-method "ssh"))

;; re-nuilder
(use-package re-builder
  :init
  (setq reb-re-syntax 'string))

;; cleanup modeline
(use-package diminish
  :ensure t)

;; Vertico, Orderless and Marginalia
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

;; org-mode
(use-package org
  :init
  (setq org-directory "~/Documentos/org")
  (setq org-default-notes-file (concat org-directory "/notas.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline "~/Documentos/org/tarefas.org" "Tarefas")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry
           (file+datetree "~/Documentos/org/diario.org")
           "* %?\nEm %U\n  %i\n  %a")))

  ;; default identation on code blocks
  (setq org-edit-src-content-indentation 0)

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((scheme . t)
     (emacs-lisp . nil)
     (dot . t)))
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (member lang '("scheme" "dot"))))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  ;; fix mismatch of < and > inside code blocks
  (defun org-mode-<>-syntax-fix (start end)
    "Change syntax of characters ?< and ?> to symbol within source code blocks."
    (let ((case-fold-search t))
      (when (eq major-mode 'org-mode)
        (save-excursion
          (goto-char start)
          (while (re-search-forward "<\\|>" end t)
            (when (save-excursion
                    (and
                     (re-search-backward "[[:space:]]*#\\+\\(begin\\|end\\)_src\\_>" nil t)
                     (string-equal (downcase (match-string 1)) "begin")))
              ;; This is a < or > in an org-src block
              (put-text-property (point) (1- (point))
                                 'syntax-table (string-to-syntax "_"))))))))

  (defun org-setup-<>-syntax-fix ()
    "Setup for characters ?< and ?> in source code blocks."
    (make-local-variable 'syntax-propertize-function)
    (setq syntax-propertize-function 'org-mode-<>-syntax-fix)
    (syntax-propertize (point-max)))

  (add-hook 'org-mode-hook #'org-setup-<>-syntax-fix))

;; ;; denote - zettlekasten package
;; (use-package denote
;;   :ensure t
;;   :init
;;   (setq denote-directory (expand-file-name "~/Documentos/zettelkasten")
;;         denote-known-keywords '("emacs" "programação"))
;;   (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
;;   :config
;;   ;; org capture with denote
;;   (setq denote-org-capture-specifiers "%l\n%i\n%?")
;;   (add-to-list 'org-capture-templates
;;                '("n" "New note (with denote.el)" plain
;;                  (file denote-last-path)
;;                  #'denote-org-capture
;;                  :no-save t
;;                  :immediate-finish nil
;;                  :kill-buffer t
;;                  :jump-to-captured t)))

;; use settings from .editorconfig file when present
(use-package editorconfig
  :ensure t
  :init
  (editorconfig-mode 1)
  (diminish 'editorconfig-mode))

;; magit
(use-package magit
  :ensure t)

;; eglot
(use-package eglot
  :ensure t)

;; silver searcher
(use-package ag
  :ensure t)

;; diff-hl
(use-package diff-hl
  :ensure t
  :commands (diff-hl-dired-mode diff-hl-magit-post-refresh)
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode +1))

;; crux
(use-package crux
  :ensure t
  :bind (("C-k" . crux-smart-kill-line)
         ("C-S-k" . crux-kill-whole-line)
         ("C-S-RET" . crux-smart-open-line-above)
         ("S-RET" . crux-smart-open-line)
         ("C-a" . crux-move-beginning-of-line))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region))

;; theme
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi t))

;; Olivetti mode
(use-package olivetti
  :ensure t)

;; smartparens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (smartparens-global-mode))

;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (setq yas-snippet-dirs
      (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-global-mode 1))

;; Popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;;;; Language specific settings ------------------------------------------------
;; Lisp family ----------------
(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (diminish 'paredit-mode "()"))

(use-package rainbow-delimiters
  :ensure t)

;; Emacs Lisp
(use-package elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;; Scheme
(use-package geiser-guile
  :ensure t)

(use-package scheme
  :requires paredit
  :diminish paredit
  :config
  (add-hook 'scheme-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode))

;; Clojure
(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :requires clj-refactor
  :config
  (setq nrepl-log-messages t)
  (setq cider-use-overlays nil)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'clj-refactor-mode)
  ;; (add-hook 'cider-mode-hook
  ;;           #'(lambda ()
  ;;               (setq cider-eldoc-display-for-symbol-at-point nil)
  ;;               (remove-hook 'eldoc-documentation-functions #'cider-eldoc t)))
  )

;; Other languages --------------
;; Cobol
(use-package cobol-mode
  :mode "\\.cbl\\'")

;; Typescript
(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :config
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure))

;;;; cc-mode
(use-package cc-mode
  :config
  (add-hook 'c-mode-hook
            (lambda ()
              (setq c-default-style "k&r"
                    tab-width 4
                    c-basic-offset 4))))

;; yaml
(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'")

;; Java
(use-package java-ts-mode
  :mode "\\.java\\'")

;; PlantUML
(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :init
  (setq plantuml-default-exec-mode 'jar
        plantuml-jar-path (substitute-in-file-name "${HOME}/.local/lib/plantuml.jar")))

(provide 'init)
;;; init.el ends here
