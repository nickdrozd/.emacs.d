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

(defun open-windows ()
  "Fills frame with windows of width >= 80."
  (interactive)
  (delete-other-windows)
  (let ((number-of-windows (/ (window-width) 80)))
    (dotimes (_ (1- number-of-windows))
      (split-window-right))
    (balance-windows)))

(add-hook 'window-setup-hook 'open-windows)

;;; Run at start

(shell)
