(require 'package)
(require 'keys)

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

;; Get delight first so other packages can use it
(use-package delight
  :ensure t)

;; Look at names in `minor-mode-alist' to see what exactly to delight

;;;

(use-package ace-window
  :ensure t
  :pin melpa
  :config
  (defkey C-o ace-window)
  (ace-window-display-mode)
  (setq aw-keys '(?a ?s ?d ?f ?z ?x ?c ?v ?b)) ;fix ?b
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
          (?h aw-execute-command-other-window "Execute Command Other Window")
          (?? aw-show-dispatch-help)))
  (setq aw-make-frame-char ?6))

(use-package avy
  :ensure t
  :config
  (defkeys
    C-j avy-goto-char-timer
    M-j avy-goto-line))

(use-package beacon
  :ensure t
  :delight beacon-mode
  :config (beacon-mode))

(use-package company
  :ensure t
  :delight company-mode
  :config (global-company-mode))

(use-package compile
  :config
  (setq
   compilation-scroll-output t))

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
  :config
  (dumb-jump-mode)
  (defkey M-g dumb-jump-go-prompt))

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

(use-package evil
  :ensure t
  ;; this is apparently needed because of dependencies
  :pin melpa
  :config
  (defkey C-\` evil-mode)
  ;; evil mode brings this in and activates it as a dependency
  (global-undo-tree-mode -1))

(use-package exec-path-from-shell
  :if (or *is-mac* *is-linux*)
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package fireplace
  :ensure t
  :pin melpa)

(use-package flycheck
  :ensure t
  :delight flycheck-mode
  :config
  (setq-default
   flycheck-disabled-checkers
   '(rust-cargo emacs-lisp-checkdoc))
  (global-flycheck-mode))

(use-package god-mode
  :ensure t
  :config
  (setq god-literal-key " "
        god-mod-alist '((nil . "C-")
                        ("m" . "M-")
                        ("i" . "s-")
                        ("w" . "H-")
                        ("[" . "C-M-")))
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
  :ensure t
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package hackernews
  :ensure t
  :config
  (setq
   hackernews-comments-format "* %3s |"
   hackernews-item-format "%c %t\n"
   hackernews-items-per-page 50)
  (set-face-attribute
   'hackernews-comment-count nil
   :foreground "lightblue")
  (add-to-list 'same-window-regexps "\*hackernews.*\*")

  ;; See https://github.com/clarete/hackernews.el/pull/45
  (define-advice hackernews (:around (fn &rest args) hackernews--dynamic-story-count)
    "Adapt `hackernews-items-per-page' to current window height."
    (let ((hackernews-items-per-page (1- (window-text-height))))
      (apply fn args))))

(use-package helpful
  :ensure t
  :pin melpa
  :config
  (defkeys
    (C-h f) helpful-callable
    (C-h v) helpful-variable
    (C-h k) helpful-key
    (C-h C-h) helpful-at-point
    (C-h C-q) helpful-kill-buffers))

(use-package hideshow
  :delight hs-minor-mode)

(use-package hl-line
  :config
  (global-hl-line-mode)
  (set-face-attribute
   'hl-line nil
   :background "midnight blue"
   :foreground nil))

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
    s-b magit-blame))

(use-package markdown-mode
  :ensure t)

(use-package move-text
  :ensure t
  :config (move-text-default-bindings))

(use-package multiple-cursors
  :ensure t)

(use-package nameless
  :ensure t
  :config
  (setq
   nameless-private-prefix t
   nameless-affect-indentation-and-filling nil)
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode))

(use-package org
  :config
  (defkeys
    (C-c n a) org-agenda
    (C-c n c) org-capture
    (C-c n l) org-store-link)

  ;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
  (setq org-agenda-files '("~/org/inbox.org"
                           "~/org/projects.org"
                           "~/org/tickler.org"))

  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/org/inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/org/tickler.org" "Tickler")
                                 "* %i%? \n %U")))

  (setq org-refile-targets '(("~/org/projects.org" :maxlevel . 3)
                             ("~/org/someday.org" :level . 1)
                             ("~/org/tickler.org" :maxlevel . 2)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (defkeys-in-map org-mode-map
    C-k kill-ring-save
    C-v org-yank
    C-y backward-kill-word
    M-y org-kill-line)

  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil)

  (define-key org-mode-map (kbd "C-,") nil)

  ;; I think the standard way of configuring babel languages is gross
  (defun org-babel-load-languages (&optional enable-list disable-list)
    "Enable the langs in ENABLE-LIST, disable those in DISABLE-LIST."
    (let ((enable-pairs (mapcar (lambda (lang) (cons lang t)) enable-list))
          (disable-pairs (mapcar (lambda (lang) (cons lang nil)) disable-list)))
      (org-babel-do-load-languages
       'org-babel-load-languages
       (append enable-pairs disable-pairs))))

  (org-babel-load-languages
   '(C python js emacs-lisp shell scheme lisp)))

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

(use-package ox-jekyll-md
  :ensure t)

(use-package paredit
  :ensure t)

(use-package pdf-tools
  :ensure t
  :pin melpa
  :config
  (setq pdf-view-midnight-colors '("white" . "black")))

(use-package php-mode
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
  :pin melpa
  :delight projectile-mode '(:eval (format " [%s]" (projectile-project-name)))
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (setq projectile-switch-project-action 'projectile-vc)
  (projectile-mode))

(use-package python
  :config (setq python-shell-interpreter "python3"))

(use-package racket-mode
  :ensure t)

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

(use-package refine
  :ensure t)

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
  :ensure t
  :config (setq inferior-lisp-program "sbcl"))

(use-package shell
  :config
  (setq comint-input-ignoredups t)
  (add-hook 'shell-mode-hook
            (lambda ()
              (yas-minor-mode -1))))

(use-package simple
  :delight auto-fill-function)

(use-package smartparens
  :ensure t
  :delight smartparens-mode
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
  :ensure t
  :pin melpa
  :config
  (setq suggest-insert-example-on-start nil))

(use-package text-mode
  :preface (provide 'text-mode)
  :config (add-hook 'text-mode-hook 'auto-fill-mode))

(use-package which-key
  :ensure t
  :delight which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 2.0))

(use-package yasnippet
  :ensure t
  :delight yas-minor-mode
  :config (yas-global-mode))

(use-package yaml-mode
  :ensure t)
