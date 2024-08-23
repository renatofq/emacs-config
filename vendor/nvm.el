;;; nvm.el --- Manage Node versions within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024 Renato Fernandes de Queiroz

;; Author: Renato Fernandes de Queiroz <renatofq@gmail.com>
;; Maintainer: Renato Fernandes de Queiroz <renatofq@gmail.com>
;; Version: 0.3.0
;; Keywords: node, nvm, fish

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(require 'xdg)

(defgroup nvm nil
  "Manage Node versions within Emacs"
  :prefix "nvm-"
  :group 'tools)

(defvar nvm-current-version nil
  "Current active version.")

(defvar nvm-install-path (expand-file-name "nvm" (xdg-data-home))
  "nvm installation path.")

(defun nvm--version-path (version)
  (expand-file-name version nvm-install-path))

;;;###autoload
(defun nvm-use (version)
  "Activate Node VERSION."
  (let* ((version-path (nvm--version-path version))
        (nvm-bin-path (expand-file-name "bin" version-path)))
    (setenv "NVM_BIN" nvm-bin-path)
    (setenv "NVM_PATH" (expand-file-name "lib" version-path))
    (setenv "PATH" (string-join
                    (cons nvm-bin-path (parse-colon-path (getenv "PATH")))
                    path-separator))
    (add-to-list 'exec-path nvm-bin-path)
    (setq nvm-current-version version)))

(provide 'nvm)
;;; nvm.el ends here
