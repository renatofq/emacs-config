;;; nvm.el --- Manage Node versions within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024 Renato Fernandes de Queiroz

;; Author: Renato Fernandes de Queiroz <renatofq@gmail.com>
;; Version: 0.1
;; Keywords: node, nvm, fish
;; License: GNU General Public License = 3
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Allows to activate a version of node installed with nvm-fish
;; see: https://github.com/jorgebucaran/nvm.fish

;; It has the same package name as https://github.com/rejeep/nvm.el
;; in order to be used by js-comint

;;; Code:
(require 'xdg)

(defvar nvm-current-version nil
  "Current active version.")

(defvar nvm-install-path (expand-file-name "nvm" (xdg-data-home))
  "nvm installation path.")

(defun nvm--version-path (version)
  (expand-file-name version nvm-install-path))

(defun nvm--installed-versions ()
  (mapcar (lambda (file) (cons file nil))
   (directory-files nvm-install-path nil "^v")))

(defun nvm--find-exact-version-for (version)
  "Returns the exact version of the node installation"
  nil)

;;;###autoload
(defun nvm-use (version)
  "Activate Node VERSION."
  (interactive
   (list (completing-read "Version: " (nvm--installed-versions))))
  (let* ((version-path (nvm--version-path version))
         (nvm-bin-path (expand-file-name "bin" version-path))
         (not-nvm-path-p (lambda (path-dir)
                           (not (string-prefix-p nvm-install-path path-dir)))))
    (setenv "NVM_BIN" nvm-bin-path)
    (setenv "NVM_PATH" (expand-file-name "lib" version-path))
    (setenv "PATH"
            (string-join (thread-last (getenv "PATH")
                                      (parse-colon-path)
                                      (seq-filter not-nvm-path-p)
                                      (cons nvm-bin-path))
                         path-separator))
    (setq exec-path
          (cons nvm-bin-path
                (seq-filter not-nvm-path-p
                            exec-path)))
    (setq nvm-current-version version)))

(provide 'nvm)
;;; nvm.el ends here
