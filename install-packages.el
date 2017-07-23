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
     (define-key dired-mode-map ";" 'dired-up-directory))))

(use-package elpy)

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

(use-package re-builder
  :bind (("C-c R" . re-builder))
  :config (setq reb-re-syntax 'string))

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

(use-package yaml-mode)

;(provide 'install-packages)
