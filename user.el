;; package --- Emacs user file
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
    (save-as-and-switch filename)
    (save-as-do-not-switch filename)))

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
;;; user.el ends here
