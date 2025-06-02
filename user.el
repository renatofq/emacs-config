;; package --- Emacs user file -*- lexical-binding: t -*-
(defun user/move-begining-of-line ()
  "Move point to the beginning of line or beginning of indentation. Toggling between the two"
  (interactive)
  (let ((p (point)))
    (back-to-indentation)
    (when (= p (point))
      (move-beginning-of-line 1))))

(defun user/keyboard-quit ()
  "Smater version of the built-in `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it."
  (interactive)
  (if (> (minibuffer-depth) 0)
    (abort-recursive-edit)
    (keyboard-quit)))

(defun user/c-indent-complete ()
  (interactive)
  (let ((p (point)))
    (c-indent-line-or-region)
    (when (= p (point))
      (call-interactively 'complete-symbol))))

(defun user/proxy-bb-setup ()
  "Configura o proxy bb"
  (interactive)
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10.*\\)")
          ("http" . "127.0.0.1:3128")
          ("https" . "127.0.0.1:3128"))))

;; custom-scratch buffer
(defvar user/scratch-buffer-mode-alist
  '(("clojure"     . clojure-mode)
    ("cobol"       . cobol-mode)
    ("config"      . conf-mode)
    ("fundamental" . fundamental-mode)
    ("java"        . java-mode)
    ("javascript"  . js2-mode)
    ("json"        . js-json-mode)
    ("markdown"    . markdown-mode)
    ("org"         . org-mode)
    ("sql"         . sql-mode)
    ("typescript"  . typescript-mode)
    ("yaml"        . yaml-ts-mode))

  "Alist of NAME and MODE of the modes that can be used as major mode for
creating user/scratch-buffer.")

(defun user/select-scratch-buffer-mode ()
  "Selects a major mode for a scratch buffer.
Calls `completing-read' to select a mode from `user/scratch-buffer-mode-alist'."
  (assoc (completing-read "Major mode: "
                          user/scratch-buffer-mode-alist
                          nil t)
         user/scratch-buffer-mode-alist))

(defun user/get-scratch-buffer (mode arg)
  "Return a user/scratch-buffer specified by MODE, creating a new one if needed.

MODE must be a cons cell where CAR is the name and CDR is a symbol bound to a
major mode function."
  (let* ((buffer-name (concat "*" (car mode) "-scratch*"))
         (buffer (if arg
                     (generate-new-buffer buffer-name)
                   (get-buffer-create buffer-name))))
    (with-current-buffer buffer
      (funcall (cdr mode)))
    buffer))

(defun user/switch-to-scratch-buffer (arg)
  "Switch to a user/scratch-buffer with a major mode.

When called with prefix argument uses `user/scratch-buffer-mode-default'.
Otherwise, calls `completing-read' to select a mode from
`user/scratch-buffer-mode-alist'."
  (interactive "P")
  (switch-to-buffer
   (user/get-scratch-buffer
    (user/select-scratch-buffer-mode)
    arg)))

;; save-as functions
(defun user/save-as-and-switch (filename)
  "Clone the current buffer and switch to the clone"
  (interactive "FCopy and switch to file: ")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename nil nil nil 'confirm))
  (find-file filename))

(defun user/save-as-do-not-switch (filename)
  "Clone the current buffer but don't switch to the clone"
  (interactive "FCopy (without switching) to file:")
  (write-region (point-min) (point-max) filename)
  (find-file-noselect filename))

(defun user/save-as (filename)
  "Prompt user whether to switch to the clone."
  (interactive "FCopy to file: ")
  (if (y-or-n-p "Switch to new file?")
    (user/save-as-and-switch filename)
    (user/save-as-do-not-switch filename)))

(defun user/window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(defun user/load-all-in-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded."
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

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

(defun user/org-confirm-babel-evaluate (lang body)
  (not (member lang '("scheme" "dot"))))

;; some legacy but, pehaps useful functions
(defun user/--xml-try-move-up-element ()
  (condition-case nil
      (progn
        (nxml-backward-up-element)      ; always returns nil
        t)
    (error nil)))

(defun user/xpath (exclusions)
  (save-excursion
    (save-restriction
      (widen)
      (named-let rec ((path nil))
        (if (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                 (user/--xml-try-move-up-element))
            (if-let* ((tagname (xmltok-start-tag-local-name))
                      ((seq-contains-p exclusions tagname 'string=)))
              (rec path)
              (rec  (cons (xmltok-start-tag-local-name) path)))
          (format "/%s" (mapconcat 'identity path "/")))))))


(defun user/xpath-pix-auto ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let* ((xpath (user/xpath '("Envelope" "Document")))
         (result (format "XPath na PAIN.013: '/%s'" xpath)))
    (when (called-interactively-p t)
      (kill-new result)
      (message result))
    result))
;;; user.el ends here
