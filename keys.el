;; This might be overkill, but occasionally it's useful
;; to pass in a function called with arguments.
;; For anything more complicated and for functions whose
;; names should be saved, use `defbind`.
(defmacro defkey (keyb func &optional keymap)
  "Assign a function to a keybinding."
  (let ((keyb-string
	 (if (listp keyb)
	     (apply 'concat
		    (mapcar (lambda (kb)
			      (concat (symbol-name `,kb) " "))
			    keyb))
	   (symbol-name `,keyb))))
    `(define-key ,(if keymap keymap 'global-map)
       (kbd ,keyb-string)
       ,(if (listp func)
	    `(lambda () (interactive) ,func)
	  `',func))))

(defun break-args-into-tuples (n args)
  (let ((result nil))
    (while (< (1- n) (length args))
      (let ((subseq (cl-subseq args 0 n)))
	(setq result (cons subseq result))
	(setq args (nthcdr n args))))
    (nreverse result)))

(defmacro defkeys (&rest keys-and-funcs)
  (let ((defkey-statements
	  (mapcar (lambda (pair)
		    (cons 'defkey pair))
		  (break-args-into-tuples 2 keys-and-funcs))))
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
  (C-x C-x) execute-extended-command

  (C-c q) query-replace-regexp

  ;; semi-cua keys

  C-k kill-ring-save
  M-k kill-region

  C-w kill-this-buffer

  C-v yank
  M-v yank-pop

  C-y backward-kill-word
  M-y kill-line

  C-z undo

  H-s (switch-to-buffer "*scratch*")

  H-h shell

  ;; should these be C- or s-?
  s-\; eval-expression ;; previously M-:
  s-\' eval-last-sexp

  C-0 delete-window
  C-1 delete-other-windows
  C-2 split-window-below
  C-3 split-window-right
  C-= balance-windows

  s-= text-scale-adjust
  s-- text-scale-adjust

  M-\, beginning-of-buffer
  M-. end-of-buffer

  s-\\ toggle-input-method
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



(defmacro key-to-open-file (keyb file)
  `(defbind ,keyb ,(make-symbol (concat "edit-conf-" file)) ()
     (find-file (emacs-file ,(concat file ".el")))))

(defmacro keys-to-open-files (&rest keys-and-files)
  (let ((key-to-open-file-statements
	 (mapcar (lambda (pair)
		   (cons 'key-to-open-file pair))
		 (break-args-into-tuples 2 keys-and-files))))
    `(progn ,@key-to-open-file-statements)))


(keys-to-open-files
 H-i "init"
 H-k "keys"
 H-g "global"
 H-w "windows"
 H-p "packages")




(defmacro defalq (symbol definition)
  `(defalias ',symbol ',definition))

(defmacro defaliases (&rest als-and-defs)
  (let ((defalq-statements
	  (mapcar (lambda (pair)
		    (cons 'defalq pair))
		  (break-args-into-tuples 2 als-and-defs))))
    `(progn ,@defalq-statements)))

(defaliases
  qrr query-replace-regexp
  sh shell
  )
