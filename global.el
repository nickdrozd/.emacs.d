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
 ido-mode
 column-number-mode
 show-paren-mode
 global-hi-lock-mode
 delete-selection-mode
 subword-mode
 global-prettify-symbols-mode

 ;; assuming tooltips are sent to echo area
 tooltip-mode

 midnight-mode
 )

(disable-modes
 tool-bar-mode
 menu-bar-mode

 ;; Time might be nice, but other stuff is included in the default
 display-time-mode

 ;; Is there a way to get rid of that ugly line number stripe?
 global-linum-mode
 )

;;;

(setq
 completions-format 'vertical
 tab-always-indent 'complete

 ;; makes windows always open vertically
 split-height-threshold nil
 split-width-threshold 80

 uniquify-buffer-name-style 'forward
 inhibit-startup-message t
 sentence-end-double-space nil
 emacs-lisp-docstring-fill-column 50
 auto-revert-verbose nil
 jka-compr-verbose nil
 )

(defmacro enable-functions (&rest functions)
  `(dolist (func '(,@functions))
     (put func 'disabled nil)))

(enable-functions
 upcase-region
 downcase-region
 narrow-to-region
 list-timers
 )

;; good idea?
;; (add-hook 'focus-out-hook #'garbage-collect)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)

(dolist (regexp '("magit:.+" "\*shell\*"))
  (add-to-list 'same-window-regexps regexp t))

(when (boundp 'byte-metering-on)
  (setq byte-metering-on t))
