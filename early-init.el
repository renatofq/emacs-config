;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 10MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 10000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Make compilation less verbose
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))

;; config path
(setenv "PATH"
        (concat (getenv "PATH")
                ":" (substitute-in-file-name "${HOME}/.local/bin")))
