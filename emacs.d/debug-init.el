;; -*- lexical-binding: t; -*-

;; ============================
;; initializing package manager
;; ============================

(setq straight-repository-branch "develop"
      straight-use-package-by-default t
      straight-vc-git-default-protocol 'https)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; ======================================================
;; test code below
;; run with `emacs -q -l debug-init.el'
;; ======================================================
