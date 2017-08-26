(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

(when *is-mac*
  (setq
   mac-command-modifier 'control
   mac-option-modifier 'meta
   mac-control-modifier 'super
   mac-function-modifier 'hyper))

(load-theme 'manoj-dark)

(setq package--init-file-ensured t)

(defun emacs-file (file)
  (expand-file-name file user-emacs-directory))

(defvar emacs.d-files
  '("custom.el"
    "global.el"
    "keys.el"
    "packages.el"))

(dolist (file emacs.d-files)
  (load (emacs-file file)))

(setq custom-file (emacs-file "custom.el"))

;;;

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; This still doesn't work quite right -- on a wide monitor, it opens
;; either too many or too few windows.
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
