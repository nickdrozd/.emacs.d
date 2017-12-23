(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/"))
      package-archive-priorities
      '(("gnu" . 3)
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
        '((?0 aw-delete-window "Delete Window")
          (?m aw-swap-window "Swap Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?n aw-flip-window)
          (?2 aw-split-window-vert "Split Vert Window")
          (?3 aw-split-window-horz "Split Horz Window")
          (?1 delete-other-windows "Maximize Window")
          (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?? aw-show-dispatch-help))))

(use-package avy
  :ensure t
  :config
  (defkeys
    C-j avy-goto-char-timer
    M-j avy-goto-line))

(use-package beacon
  :ensure t
  :config (beacon-mode))

(use-package company
  :ensure t
  :config (global-company-mode))

(use-package conf-mode
  :config (add-hook 'conf-mode-hook
                    (lambda () (setq tab-width 4))))

(use-package dired
  :config
  ;; The default key for dired-up-directory is ^ -- wtf?
  ;; That might be literally the most inconvenient key.
  ;; Make it easy to go up from either side of the keyboard.
  (define-key dired-mode-map ";" 'dired-up-directory)
  (define-key dired-mode-map "3" 'dired-find-file)
  (define-key dired-mode-map (kbd "C-o") nil)
  (define-key dired-mode-map (kbd "C-t") nil)
  (add-hook
   'dired-mode-hook
   #'auto-revert-mode)
  (require 'dired-x)
  (defkey (C-x C-d) dired-jump))

(use-package dumb-jump
  :ensure t
  :config (dumb-jump-mode))

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
  (defkey C-\\ god-mode-all)
  (defun god-mode-update-cursor ()
    (setq cursor-type
	  (if (or god-local-mode
		  buffer-read-only)
	      'hollow
	    'box)))
  (add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-mode-update-cursor))

(use-package go-mode
  :ensure t)

(use-package helpful
  :ensure t)

(use-package ido
  :config (setq
	   ido-enable-flex-matching t
	   ido-use-filename-at-point 'guess
	   ido-use-url-at-point t))

(use-package info
  :config
  (add-to-list 'Info-directory-list
               (expand-file-name "~/info")))

(use-package magit
  :ensure t
  :config
  (setq magit-log-arguments '("--graph" "--color" "--decorate" "-n256")
	magit-rebase-arguments '("--autosquash" "--autostash"))
  (defkeys
    M-g magit-status
    s-b magit-blame))

(use-package markdown-mode
  :ensure t)

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
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
  (define-key org-mode-map (kbd "C-,") nil))

(use-package org-present
  :ensure t
  :config
  (setq
   org-present-mode-hook
   (lambda ()
     (delete-other-windows)
     (org-present-big)
     (org-display-inline-images)
     (org-present-hide-cursor)
     (org-present-read-only))
   org-present-mode-quit-hook
   (lambda ()
     (org-present-small)
     (org-remove-inline-images)
     (org-present-show-cursor)
     (org-present-read-write))))

(use-package paredit
  :ensure t)

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook
	    (lambda ()
	      (auto-fill-mode)
	      (setq comment-auto-fill-only-comments t)
	      (hs-minor-mode)
	      (setq indent-tabs-mode nil)
	      (setq tab-width 4))))

(use-package projectile
  :ensure t
  :config
  (setq projectile-switch-project-action 'projectile-dired)
  (projectile-global-mode))

(use-package python
  :config (setq python-shell-interpreter "python3"))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook
            'rainbow-delimiters-mode)
  ;; from yoo2080.wordpress.com/2013/12/21/small-rainbow-delimiters-tutorial
  (require 'cl-lib)
  (require 'color)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 30))))

(use-package re-builder
  :config (setq reb-re-syntax 'string))

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  (use-package cargo
    :ensure t
    :config
    (add-hook 'rust-mode-hook
              'cargo-minor-mode)))

(use-package slime
  :config (setq inferior-lisp-program "sbcl"))

(use-package shell
  :config
  (add-hook 'shell-mode-hook
	    (lambda ()
	      (yas-minor-mode -1))))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  (defkeys
    M-x smex
    (C-x C-x) smex))

(use-package suggest
  :ensure t)

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
