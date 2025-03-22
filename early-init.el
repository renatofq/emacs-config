;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 32MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 33554432)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Make compilation less verbose
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))

;; config path
(let ((local-bin (substitute-in-file-name "${HOME}/.local/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":" local-bin))
  (add-to-list 'exec-path local-bin))
