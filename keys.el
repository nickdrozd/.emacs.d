;; This might be overkill, but occasionally it's useful
;; to pass in a function called with arguments.
;; For anything more complicated and for functions whose
;; names should be saved, use `defbind`.
(defmacro defkey (keyb func)
  "Assign a function to a keybinding."
  (let ((keyb-string
	 (if (listp keyb)
	     (apply 'concat
		    (mapcar (lambda (kb)
			      (concat (symbol-name `,kb) " "))
			    keyb))
	   (symbol-name `,keyb))))
    `(global-set-key
      (kbd ,keyb-string)
      ,(if (listp func)
	   `(lambda () (interactive) ,func)
	 `',func))))

(defun break-args-into-pairs (args)
  (let ((result nil))
    (while (< 1 (length args))
      (setq result (cons (list (car args) (cadr args)) result))
      (setq args (cddr args)))
    (nreverse result)))

(defmacro defkeys (&rest keys-and-funcs)
  (let ((defkey-statements
	  (mapcar (lambda (pair)
		    (cons 'defkey pair))
		  (break-args-into-pairs keys-and-funcs))))
    `(progn ,@defkey-statements)))


(defkeys
  ;; C-x C-s? yeah right
  M-s save-buffer

  ;; ibuffer is better than buffer-list
  (C-x C-b) ibuffer
  s-b ido-switch-buffer

  ;; Make regexp search the default
  C-s isearch-forward-regexp
  C-r isearch-backward-regexp
  C-M-s isearch-forward
  C-M-r isearch-backward

  ;; C-x o? please
  C-<right> other-window
  C-<left> (other-window -1)
  C-. other-window
  C-\, (other-window -1)

  ;; Alternative to M-x (sites.google.com/site/steveyegge2/effective-emacs)
  (C-c C-m) execute-extended-command

  (C-c q) query-replace-regexp

  C-w backward-kill-word
  (C-x C-k) kill-region

  s-k kill-this-buffer

  H-s (switch-to-buffer "*scratch*")

  H-h shell
  )




(defmacro defbind (keyb name args &rest body)
  "Define a function and give it a keybinding."
  `(progn
     (defun ,name ,args
       (interactive)
       ,@body)
     (defkey ,keyb ,name)))


(defbind (C-x s) save-and-kill-buffer ()
  (save-buffer)
  (kill-buffer))

;; adapted from sites.google.com/site/steveyegge2/my-dot-emacs-file
(defbind (C-x w) swap-windows ()
  (let ((windows (window-list)))
    (let ((w1 (car windows))
	  (w2 (cadr windows)))
      (let ((b1 (window-buffer w1))
	    (b2 (window-buffer w2))
	    (s1 (window-start w1))
	    (s2 (window-start w2)))
	(set-window-buffer w1 b2)
	(set-window-buffer w2 b1)
	(set-window-start w1 s2)
	(set-window-start w2 s1)))))



(defmacro key-to-open-file (keyb file)
  `(defbind ,keyb ,(make-symbol (concat "edit-conf-" file)) ()
     (find-file (emacs-file ,(concat file ".el")))))

(defmacro keys-to-open-files (&rest keys-and-files)
  (let ((key-to-open-file-statements
	  (mapcar (lambda (pair)
		    (cons 'key-to-open-file pair))
		  (break-args-into-pairs keys-and-files))))
    `(progn ,@key-to-open-file-statements)))


(keys-to-open-files
 H-i "init"
 H-k "keys"
 H-g "global"
 H-p "packages")




(provide 'keys)
