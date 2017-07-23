(defmacro toggle-modes (flag &rest modes)
  `(progn
     ,@(mapcar (lambda (mode)
		 `(,mode ,flag))
	       modes)))

(defmacro enable-modes (&rest modes)
  `(toggle-modes 1 ,@modes))

(defmacro disable-modes (&rest modes)
  `(toggle-modes 0 ,@modes))

(enable-modes
 column-number-mode
 show-paren-mode
 )

(disable-modes
 tool-bar-mode

 ;; Time might be nice, but other stuff is included in the default
 display-time-mode

 ;; Is there a way to get rid of that ugly line number stripe?
 global-linum-mode
 )

;;;

(setq completions-format 'vertical)

;; makes windows always open vertically
(setq split-height-threshold nil)

(put 'upcase-region 'disabled nil)

(setq uniquify-buffer-name-style 'forward)

(setq inhibit-startup-message t)

;; good idea?
;; (add-hook 'focus-out-hook #'garbage-collect)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq tab-always-indent 'complete)
