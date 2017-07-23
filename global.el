
(column-number-mode 1)
;(setq column-number-mode t)
(setq completions-format 'vertical)

;; makes windows always open vertically
(setq split-height-threshold nil)

(put 'upcase-region 'disabled nil)

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

(setq tab-always-indent 'complete)
