;; init.el --- Emacs init file -*- lexical-binding: t -*-
;; configure custom file and load it's values first
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; load user defined
(load (expand-file-name "user.el" user-emacs-directory))

;; add melpa-stable
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;;;; Load "vendor" directory
(user/load-all-in-directory (expand-file-name "vendor" user-emacs-directory))
(provide 'init)

;;;; UX setup
;; disable tool-bar menu-bar and scroll-bar
(tool-bar-mode -1)

;; Disable scroll-bar
(scroll-bar-mode -1)

;; maximize the initial frame automatically
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; font
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))
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

;; keep a sigle dired buffer
(setq dired-kill-when-opening-new-dired-buffer t)

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
(setq tab-always-indent 'complete)    ;; smart tab behavior - indent or complete
(setq-default fill-column 80)         ;; wrap line at column 80

;; Emacs 30 and newer: Disable Ispell completion function.
(setq text-mode-ispell-word-completion nil)

;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current mode.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; end of sentence is not double spaces
(setq sentence-end-double-space nil)

;; newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-by-copying t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

;; dabbrev-expand
(global-set-key (kbd "M-/") 'dabbrev-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; define a unified place to all generated/data
(defun varfile-name (filename)
  "This function returns the file path for a 'varfile'. A generated/data file
   used by different modes that clutter 'user-emacs-directory' and should not be
   versioned."
  (file-name-concat user-emacs-directory "varfile" filename))

;; Move text M-up and down to
(use-package move-text
  :ensure t
  :init
  (move-text-default-bindings))

;; project
(use-package project
  :config
  (setq project-list-file (varfile-name "projects")))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (varfile-name "saveplace"))
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
        savehist-file (varfile-name "savehist"))
  (savehist-mode +1))

;; save recent files
(use-package recentf
  :config
  (setq recentf-save-file (varfile-name "recentf")
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

;; bookmarks
(use-package bookmark
  :config
  (setq bookmark-default-file (varfile-name "bookmarks")
        bookmark-save-flag 1))

;; tramp
(use-package tramp
  :config (setq tramp-default-method "ssh"
                tramp-persistency-file-name (varfile-name "tramp")))

;; whitespace-mode config
(use-package whitespace
  :diminish whitespace-mode
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing)))

(use-package jinx
  :ensure t
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; meaningful names for buffers with the same name
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t       ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*"))  ; don't muck with special buffers

;; cleanup modeline
(use-package diminish
  :ensure t)

;; Orderless, Vertico and Marginalia
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Enable rich annotations at minibuffer using the Marginalia package
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


;; Corfu
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-preselect 'directory)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  ;; (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)         ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find) ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history) ;; orig. next-matching-history-element
         ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  )

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;; markdown
(use-package markdown-mode
  :ensure t
  :requires edit-indirect
  :mode ("\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)
              ("C-c ." . markdown-edit-code-block))
  :init
  (setq markdown-command "pandoc")
  (setq markdown-fontify-code-blocks-natively t)
  (add-to-list 'markdown-code-lang-modes '("mermaid" . mermaid-ts-mode)))

;; org-mode
(use-package org
  :init
  (setq org-directory (file-name-concat (xdg-user-dir "DOCUMENTS") "org/"))
  (setq org-default-notes-file (file-name-concat org-directory "inbox.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline (file-name-concat org-directory "tasks.org") "Tarefas")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry
           (file+datetree (file-name-concat org-directory "journal.org"))
           "* %?\nEm %U\n  %i\n  %a")))

  ;; default indentation on code blocks
  (setq org-edit-src-content-indentation 0)

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((scheme . t)
     (emacs-lisp . nil)
     (dot . t)))

  (setq org-confirm-babel-evaluate 'user/org-confirm-babel-evaluate)

  (add-hook 'org-mode-hook #'olivetti-mode)
  (add-hook 'org-mode-hook #'jinx-mode)
  (add-hook 'org-mode-hook #'user/org-setup-<>-syntax-fix)

  :config
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "babel" t ("pdflatex" "xelatex" "lualatex"))))

;; use settings from .editorconfig file when present
(use-package editorconfig
  :ensure t
  :init
  (editorconfig-mode 1)
  (diminish 'editorconfig-mode))

;; magit + diff-hl
(use-package magit
  :ensure t)

(use-package diff-hl
  :ensure t
  :commands (diff-hl-dired-mode diff-hl-magit-post-refresh)
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode +1))

;; eglot
(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs
        `(,(expand-file-name "snippets" user-emacs-directory))))

(use-package eglot
  :ensure t
  :init
  (setq completion-category-overrides '((eglot (styles orderless))))
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (eglot-inlay-hints-mode -1)))
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
                 . ("clangd"
                       "-j=4"
                       "--log=error"
                       "--malloc-trim"
                       "--background-index"
                       "--clang-tidy"
                       "--cross-file-rename"
                       "--completion-style=detailed"
                       "--pch-storage=memory"))))

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
  (crux-with-region-or-line comment-or-uncomment-region))

;; theme
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi-tinted t))

;; rg.el -- ripgrep
(use-package rg
  :init
  (rg-enable-default-bindings))

;;;; Language specific settings ------------------------------------------------
;; Enable tree-sitter for some major modes
(use-package emacs
  :init
  (setq major-mode-remap-alist
        '((json-mode . json-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (c-mode . c-ts-mode)
          (java-mode . java-ts-mode))))

;; Lisp family ----------------
(use-package paredit
  :ensure t
  :diminish "()"
  :hook (emacs-lisp-mode lisp-interaction-mode eval-expression-minibuffer-setup))

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
  :config
  (add-hook 'scheme-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode))

;; Fennel
(use-package fennel-mode
  :config
  (add-hook 'fennel-mode-hook #'paredit-mode)
  (add-hook 'fennel-mode-hook #'rainbow-delimiters-mode)
  ;; (add-hook 'fennel-mode-hook #'fennel-proto-repl-minor-mode)
  (add-hook 'fennel-mode-hook
            (lambda ()
              (add-to-list 'eglot-server-programs '(fennel-mode . ("fennel-ls")))
              (eglot-ensure))))

;; Clojure
(use-package flymake-kondor
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (paredit-mode 1)
              (rainbow-delimiters-mode 1)
              (flymake-kondor-setup)
              (flymake-mode t))))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (setq cider-enrich-classpath t)
  (setq cider-use-overlays nil))

;; Other languages --------------
;; Cobol
(use-package cobol-mode
  :mode "\\.cbl\\'")

;; Javascript + Typescript
(use-package typescript-ts-mode
  :config
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure))

(use-package js-ts-mode
  :mode "\\.js\\'"
  :init
  (add-hook 'js-ts-mode-hook 'eglot-ensure))

(use-package nvm
  :config
  ;; default node version
  (nvm-use "v20.15.1"))

(use-package js-comint
  :config
  (js-do-use-nvm))

;;;; cc-mode
(use-package c-ts-mode
  :config
  (add-hook 'c-ts-mode-hook 'eglot-ensure))

(use-package c-mode
  :bind (:map c-mode-base-map
              (("TAB" . user/c-indent-complete)))
  :init
  (add-hook 'c-mode-hook 'eglot-ensure))

(use-package dockerfile-ts-mode
  :mode "\\(Dockerfile\\|Containerfile\\)")

(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'")

;; PlantUML
(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :init
  (setq plantuml-jar-path (expand-file-name "plantuml/plantuml.jar"
                                            (xdg-data-home)))
  (setq plantuml-exec-mode 'jar)
  (setq plantuml-java-args '("-Djava.awt.headless=true" "-jar"))
  (setq org-plantuml-jar-path plantuml-jar-path)
  (setq org-plantuml-exec-mode plantuml-exec-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t))))

(use-package text-mode
  :mode "\\.\\(txt|xml\\)\\'")

(use-package feature-mode
  :mode "\\.feature\\'"
  :init
  (setq feature-default-language "pt")
  (setq feature-default-i18n-file
        (expand-file-name "etc/gherkin-i18n.yml" user-emacs-directory)))

(use-package ebnf-mode
  :mode "\\.ebnf\\'")

;; eat + eshell
(use-package eat
  :ensure t
  :custom (eat-kill-buffer-on-exit t))

(use-package eshell
  :after eat
  :requires pcmpl-args
  :init
  ;; Use eat as terminal emulator
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
              (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
              (setq eshell-destroy-buffer-when-process-dies 't)))

  (defun user/eshell-new ()
    "Open a new shell instance"
    (interactive)
    (eshell 'N))
  (global-set-key (kbd "C-c .") #'user/eshell-new)


  (setq eshell-prompt-regexp " [Λλ] ")
  (setq eshell-prompt-function
        (lambda ()
          (concat (user/shortened-path (eshell/pwd) 32)
                  (if (= (user-uid) 0) " Λ " " λ ")))))
;;; init.el ends here

;; Experimental
;;; Experimental end
