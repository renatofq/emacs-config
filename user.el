;; package --- Emacs user file
(defun user/proxy-bb-setup ()
  "Configura o proxy bb"
  (interactive)
  (setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "127.0.0.1:3128")
     ("https" . "127.0.0.1:3128"))))

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
;;; user.el ends here
