(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://stable.melpa.org/packages/")
             t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;

(use-package conf-mode
  :config (add-hook 'conf-mode-hook
                    (lambda () (setq tab-width 4))))

(use-package dired
  :config
  ;; The default key for dired-up-directory is ^ -- wtf?
  ;; That might be literally the most inconvenient key.
  ;; Make it easy to go up from either side of the keyboard.
  (add-hook
   'dired-mode-hook
   (lambda ()
     (define-key dired-mode-map "3" 'dired-up-directory)
     (define-key dired-mode-map ";" 'dired-up-directory)
     (define-key dired-mode-map "`" 'dired-find-file))))

(use-package dired-x)

(use-package exec-path-from-shell
  :if (or *is-mac* *is-linux*)
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status))
  :config (setq magit-commit-arguments '("--signoff")
                magit-log-arguments '("--graph" "--color" "--decorate" "-n256")
                magit-rebase-arguments '("--autosquash" "--autostash")))

(use-package midnight
  :config (midnight-mode t))

(use-package org
  :config (progn
	    (org-babel-do-load-languages
	     'org-babel-load-languages
	     '((C . t)
	       (python . t)
	       (js . t)
	       (emacs-lisp . t)
	       (sh . t)
	       (scheme . t)
	       (lisp . t)))
	    (setq org-confirm-babel-evaluate nil)))

(use-package re-builder
  :bind (("C-c R" . re-builder))
  :config (setq reb-re-syntax 'string))

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

(use-package yaml-mode)

;(provide 'install-packages)
(use-package ido
  :init (ido-mode 'both)
  :config (setq
	   ido-enable-flex-matching t
	   ido-use-filename-at-point 'guess
	   ido-use-url-at-point t))

(use-package slime
  :config (setq inferior-lisp-program "sbcl")
  :defer (slime))

(use-package python
  :config (setq python-shell-interpreter "python3"))

