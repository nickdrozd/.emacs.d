;; This might be overkill, but occasionally it's useful
;; to pass in a function called with arguments.
;; For anything more complicated and for functions whose
;; names should be saved, use `defbind`.
(defmacro defkey (keyb func)
  "Assign a function to a keybinding."
  `(global-set-key
    (kbd ,keyb)
    (if (listp ',func)
	(lambda () (interactive) ,func)
      ',func)))

;; C-x C-s? yeah right
(defkey "M-s" save-buffer)

;; ibuffer is better than buffer-list
(defkey "C-x C-b" ibuffer)

;; Make regexp search the default
(defkey "C-s" isearch-forward-regexp)
(defkey "C-r" isearch-backward-regexp)
(defkey "C-M-s" isearch-forward)
(defkey "C-M-r" isearch-backward)

;; C-x o? please
(defkey "C-<right>" other-window)
(defkey "C-<left>" (other-window -1))
(defkey "C-." other-window)
(defkey "C-," (other-window -1))

;; Alternative to M-x (sites.google.com/site/steveyegge2/effective-emacs)
(defkey "C-c C-m" execute-extended-command)

(defkey "C-c q" query-replace-regexp)

(defkey "C-w" backward-kill-word)
(defkey "C-x C-k" kill-region)


(defmacro defbind (keyb name args &rest body)
  "Define a function and give it a keybinding."
  `(progn
     (defun ,name ,args
       (interactive)
       ,@body)
     (global-set-key (kbd ,keyb) ',name)))


(defbind "C-c s" save-and-kill-buffer ()
  (save-buffer)
  (kill-buffer))


(defbind "C-c i" init-edit ()
  (find-file user-init-file))


(provide 'keys)
