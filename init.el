(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

(when *is-mac*
  (setq
   mac-command-modifier 'control
   mac-option-modifier 'meta
   mac-control-modifier 'super
   mac-function-modifier 'hyper))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;(set-frame-parameter nil 'fullscreen 'maximized)

(setq package--init-file-ensured t)

(defun emacs-file (file)
  (expand-file-name file user-emacs-directory))

(defvar emacs.d-files
  '("keys.el"
    "global.el"
    "custom.el"
    "packages.el"))

;; load everything up
(dolist (file emacs.d-files)
  (load (emacs-file file)))

(setq custom-file (emacs-file "custom.el"))




(defun open-windows ()
  "Fills frame with windows of width >= 80.
The message 'opening windows' seems to be
necessary to make it run at startup, but why?"
  (interactive)
  (message "opening windows...")
  (delete-other-windows)
  (while (< 80 (/ (window-width) 2))
    (split-window-right)
    (balance-windows)))

(add-hook 'window-setup-hook 'open-windows)
