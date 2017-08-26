(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/"))
      package-archive-priorities '(("gnu" . 3)
				   ("org" . 2)
				   ("melpa-stable" . 1)
				   ("melpa" . 0)))

(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;

(use-package ace-window
  :ensure t
  :config
  (defkey C-o ace-window)
  (ace-window-display-mode)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t)
  (setq aw-dispatch-alist
	'((?0 aw-delete-window " Ace - Delete Window")
	  (?m aw-swap-window " Ace - Swap Window")
	  (?n aw-flip-window)
	  (?2 aw-split-window-vert " Ace - Split Vert Window")
	  (?3 aw-split-window-horz " Ace - Split Horz Window")
	  (?1 delete-other-windows " Ace - Maximize Window"))))

(use-package avy
  :ensure t
  :bind (("M-g M-g" . avy-goto-line)
	 ("M-g a" . avy-goto-char-timer)))

(use-package beacon
  :ensure t
  :config (beacon-mode))

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
     (define-key dired-mode-map "`" 'dired-find-file)))
  (add-hook
   'dired-mode-hook
   #'auto-revert-mode)
  (defkey (C-x C-d) dired-jump))

(use-package dired-x)

(use-package engine-mode
  :ensure t
  :config
  (engine-mode)
  (engine/set-keymap-prefix (kbd "C-c ;"))
  (setq engine/browser-function 'eww-browse-url)
  (let ((engine-list (emacs-file "engine-list.el")))
    (if (file-exists-p engine-list)
	(load engine-list)
      (message "Failed to load engine-list.el"))))

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
  (setq god-literal-key " "
	god-mod-alist '((nil . "C-")
			("j" . "M-")
			("u" . "s-")
			("y" . "H-")
			("m" . "C-M-")))
  (defkeys
    ;; which one is best? maybe they all have their place
    <escape> god-mode-all
    C-\` god-mode-all
    C-\\ god-mode-all)
  (defun god-mode-update-cursor ()
    (setq cursor-type
	  (if (or god-local-mode
		  buffer-read-only)
	      'hollow
	    'box)))
  (add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-mode-update-cursor))

(use-package ido
  :config (setq
	   ido-enable-flex-matching t
	   ido-use-filename-at-point 'guess
	   ido-use-url-at-point t))

(use-package magit
  :ensure t
  :config
  (setq magit-log-arguments '("--graph" "--color" "--decorate" "-n256")
	magit-rebase-arguments '("--autosquash" "--autostash"))
  (defkey (C-x g) magit-status))

(use-package move-text
  :ensure t
  :config (move-text-default-bindings))

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

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook
	    (lambda ()
	      (auto-fill-mode)
	      (setq comment-auto-fill-only-comments t)
	      (hs-minor-mode))))

(use-package projectile
  :ensure t
  :config (projectile-global-mode))

(use-package python
  :config (setq python-shell-interpreter "python3"))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook
	    'rainbow-delimiters-mode))

(use-package re-builder
  :config (setq reb-re-syntax 'string))

(use-package rust-mode
  :ensure t)

(use-package slime
  :config (setq inferior-lisp-program "sbcl"))

(use-package shell
  :config
  (add-hook 'shell-mode-hook
	    (lambda ()
	      (yas-minor-mode -1))))

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  (defkeys
    M-x smex
    (C-x C-x) smex))

(use-package text-mode
  :preface (provide 'text-mode)
  :config (add-hook 'text-mode-hook 'auto-fill-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 2.0))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode))

(use-package yaml-mode)

;(provide 'install-packages)
