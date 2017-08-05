(use-package avy
  :ensure t
  :bind (("M-g M-g" . avy-goto-line)
	 ("M-g a" . avy-goto-char-timer)))

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

(use-package god-mode
  :ensure t
  :config
  (setq god-mod-alist '((nil . "C-")
			("m" . "M-")
			("u" . "s-")
			("y" . "H-"))
	god-literal-key " ")
  (defkey <escape> god-mode-all))

(use-package ido
  :init (ido-mode 'both)
  :config (setq
	   ido-enable-flex-matching t
	   ido-use-filename-at-point 'guess
	   ido-use-url-at-point t))

(use-package magit
  :ensure t
  :config (setq magit-log-arguments '("--graph" "--color" "--decorate" "-n256")
                magit-rebase-arguments '("--autosquash" "--autostash")))

(use-package midnight
  :config (midnight-mode t))

(use-package org
  :config
  (if (fboundp 'org-babel-load-languages)
      (org-babel-load-languages ; this uses a modification I made to org
       '(C python js emacs-lisp sh scheme lisp))
    (org-babel-do-load-languages ; this is how babel languages are normally set
     'org-babel-load-languages
     '((C . t)
       (python . t)
       (js . t)
       (emacs-lisp . t)
       (sh . t)
       (scheme . t)
       (lisp . t))))
  (setq org-confirm-babel-evaluate nil)
  (define-key org-mode-map (kbd "C-,") nil))

(use-package python
  :config (setq python-shell-interpreter "python3"))

(use-package re-builder
  :bind (("C-c R" . re-builder))
  :config (setq reb-re-syntax 'string))

(use-package slime
  :config (setq inferior-lisp-program "sbcl")
  :defer (slime))

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 2.0))

(use-package yaml-mode)

;(provide 'install-packages)
