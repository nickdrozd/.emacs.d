(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

(when *is-mac*
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'super))

(set-frame-parameter nil 'fullscreen 'maximized)


(defun emacs-file (file)
  (expand-file-name file user-emacs-directory))

(defvar emacs.d-files
  '("keys.el"
    "custom.el"
    "install-packages.el"))

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
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Is there a way to get rid of that ugly line number stripe?
;; (global-linum-mode t)

(display-time-mode 0)

;; good idea?
;; (add-hook 'focus-out-hook #'garbage-collect)
