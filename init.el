;; package --- Emacs init file
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

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

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

;; re-nuilder
(use-package re-builder
  :init
  (setq reb-re-syntax 'string))

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


;; Corfu, Cape and Tempel
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

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

; Configure Tempel
(use-package tempel
  :ensure t
  :bind
  (:map tempel-map
        ("<tab>" . tempel-next)
        ("<backtab>" . tempel-previous)
        ("<return>" . tempel-end))
  :init
  (setq tempel-path (file-name-concat user-emacs-directory "templates"))
  ;; Setup completion at point
  (defun user/tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'user/tempel-setup-capf)
  (add-hook 'prog-mode-hook 'user/tempel-setup-capf)
  (add-hook 'text-mode-hook 'user/tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
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
  (defun user/org-confirm-babel-evaluate (lang body)
    (not (member lang '("scheme" "dot"))))
  (setq org-confirm-babel-evaluate 'user/org-confirm-babel-evaluate)

  ;; fix mismatch of < and > inside code blocks
  (defun user/org-mode-<>-syntax-fix (start end)
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

  (defun user/org-setup-<>-syntax-fix ()
    "Setup for characters ?< and ?> in source code blocks."
    (make-local-variable 'syntax-propertize-function)
    (setq syntax-propertize-function 'user/org-mode-<>-syntax-fix)
    (syntax-propertize (point-max)))

  (add-hook 'org-mode-hook #'user/org-setup-<>-syntax-fix))

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
  :ensure t
  :init
  (setq completion-category-overrides '((eglot (styles orderless))))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (defun user/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-super-capf
                     #'eglot-completion-at-point
                     #'tempel-expand
                     #'cape-file))))

  (add-hook 'eglot-managed-mode-hook #'user/eglot-capf))

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
  (crux-with-region-or-line comment-or-uncomment-region))

;; theme
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi t))

;; smartparens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (smartparens-global-mode)
  :config
  (sp-pair "'" "'" :actions :rem)
  (sp-pair "\\[" "\\]" :actions '(insert wrap autoskip navigate)))

;; rg.el -- ripgrep
(use-package rg
  :init
  (rg-enable-default-bindings))

;; eat + eshell
(use-package eat
  :ensure t)

(use-package eshell
  :after eat
  :requires pcmpl-args
  :init
  ;; Use eat as terminal emulator
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  (defun user/eshell-new ()
    "Open a new shell instance"
    (interactive)
    (eshell 'N))
  (global-set-key (kbd "C-c .") #'user/eshell-new)

  (setq user/eshell-aliases
        '((o   . find-file)
          (oo  . find-file-other-window)
          (d   . dired)
          (l   . (lambda () (eshell/ls '-l)))
          (ll  . (lambda () (eshell/ls '-la)))
          (cl  . eshell/clear-scrollback)
          (v   . view-file)
          (vo  . view-file-other-window)
          ;; overwrites
          (less . view-file)
          (emacs . find-file)))

  (mapc (lambda (alias)
	      (defalias (car alias) (cdr alias)))
        user/eshell-aliases)

  (defun user/shortened-path (path max-len)
    "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
    (let* ((components (split-string (abbreviate-file-name path) "/"))
           (len (+ (1- (length components))
                   (reduce '+ components :key 'length)))
           (str ""))
      (while (and (> len max-len)
                  (cdr components))
        (setq str (concat str
                          (cond ((= 0 (length (car components))) "/")
                                ((= 1 (length (car components)))
                                 (concat (car components) "/"))
                                (t
                                 (if (string= "."
                                              (string (elt (car components) 0)))
                                     (concat (substring (car components) 0 2)
                                             "/")
                                   (string (elt (car components) 0) ?/)))))
              len (- len (1- (length (car components))))
              components (cdr components)))
      (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

  (setq eshell-prompt-regexp " [Λλ] ")
  (setq eshell-prompt-function
        (lambda ()
          (concat (user/shortened-path (eshell/pwd) 32)
                  (if (= (user-uid) 0) " Λ " " λ ")))))

;;;; Language specific settings ------------------------------------------------
;; Enable tree-sitter for some major modes
(use-package emacs
  :config
  (setq major-mode-remap-alist
        '((json-mode . json-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode))))

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

;; Fennel
(use-package fennel-mode
  :config
  ;; (add-hook 'fennel-mode-hook 'fennel-proto-repl-minor-mode)
  (add-hook 'fennel-mode-hook #'paredit-mode)
  (add-hook 'fennel-mode-hook #'rainbow-delimiters-mode))

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

(use-package dockerfile-ts-mode
  :mode "\\(Dockerfile\\|Containerfile\\)")

(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'")

;; PlantUML
(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :init
  (setq plantuml-default-exec-mode 'jar
        plantuml-jar-path (substitute-in-file-name "${HOME}/.local/lib/plantuml.jar")))
;;; init.el ends here
