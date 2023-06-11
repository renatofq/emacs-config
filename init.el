;;; package --- Emacs init file

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 10MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 10000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defvar savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; config path
(setenv "PATH"
        (concat (getenv "PATH")
                ":" (substitute-in-file-name "${HOME}/.local/bin")
                ":" (substitute-in-file-name "${VOLTA_HOME}/bin")))
; (setq exec-path (append exec-path (list (substitute-in-file-name "${VOLTA_HOME}/bin")))


;;;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; set package-user-dir to be relative to Prelude install path
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)


;;;; UX setup
;; disable tool-bar menu-bar and scroll-bar
(tool-bar-mode -1)
(menu-bar-mode -1)
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
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

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

;; org-mode
(use-package org
  :init
  (setq org-directory "~/Documentos/org")
  (setq org-default-notes-file (concat org-directory "/notas.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline (concat org-directory "/tarefas.org") "Tarefas")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry
           (file+datetree (concat org-directory "/diario.org"))
           "* %?\nEm %U\n  %i\n  %a"))))

;; denote - zettlekasten package
(use-package denote
  :ensure t
  :init
  (setq denote-directory (expand-file-name "~/Documentos/zettelkasten")
        denote-known-keywords '("emacs" "programação"))
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
  :config
  ;; org capture with denote
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("n" "New note (with denote.el)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

;; cleanup modeline
(use-package diminish
  :ensure t)

;; Vertico + Orderless
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

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

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
(load-theme 'modus-operandi t)
;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))

;; smartparens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (smartparens-global-mode))

;; company
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t))

;;;; Language specific settings

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
  :mode "\\.java\\'"
  :config
  (add-hook 'java-ts-mode-hook 'eglot-ensure))

;; PlantUML
(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :init
  (setq plantuml-default-exec-mode 'jar
        plantuml-jar-path (substitute-in-file-name "${HOME}/.local/lib/plantuml.jar")))

(provide 'init)
;;; init.el ends here
