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
    "windows.el"
    "packages.el"))

(dolist (file emacs.d-files)
  (load (emacs-file file)))

(setq custom-file (emacs-file "custom.el"))
