(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

(when *is-mac*
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'super))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;(set-frame-parameter nil 'fullscreen 'maximized)

;;;

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://stable.melpa.org/packages/")
             t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;

(defun emacs-file (file)
  (expand-file-name file user-emacs-directory))

(defvar emacs.d-files
  '("keys.el"
    "custom.el"
    "packages.el"))

;; load everything up
(dolist (file emacs.d-files)
  (load (emacs-file file)))

(setq custom-file (emacs-file "custom.el"))

(setq column-number-mode t)
(setq completions-format 'vertical)

;; makes windows always open vertically
(setq split-height-threshold nil)

(put 'upcase-region 'disabled nil)

(defalias 'qrr 'query-replace-regexp)
(defalias 'sh 'shell)

(setq uniquify-buffer-name-style 'forward)

(show-paren-mode 1)

(setq inhibit-startup-message t)
(tool-bar-mode 0)

;; Is there a way to get rid of that ugly line number stripe?
;; (global-linum-mode t)

(display-time-mode 0)

;; good idea?
;; (add-hook 'focus-out-hook #'garbage-collect)

(add-hook 'before-save-hook 'delete-trailing-whitespace)


(defun open-4-windows ()
  (interactive)
  (delete-other-windows)
  (while (< (count-windows) 4)
    (split-window-right))
  (balance-windows))

(add-hook 'window-setup-hook 'open-4-windows)


(setq tab-always-indent 'complete)

(add-to-list 'same-window-buffer-names "*shell*")
