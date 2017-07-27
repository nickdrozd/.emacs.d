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

;; C-x C-s? yeah right
(defkey M-s save-buffer)

;; ibuffer is better than buffer-list
(defkey (C-x C-b) ibuffer)

;; Make regexp search the default
(defkey C-s isearch-forward-regexp)
(defkey C-r isearch-backward-regexp)
(defkey C-M-s isearch-forward)
(defkey C-M-r isearch-backward)

;; C-x o? please
(defkey C-<right> other-window)
(defkey C-<left> (other-window -1))
(defkey C-. other-window)
(defkey C-\, (other-window -1))

;; Alternative to M-x (sites.google.com/site/steveyegge2/effective-emacs)
(defkey (C-c C-m) execute-extended-command)

(defkey (C-c q) query-replace-regexp)

(defkey C-w backward-kill-word)
(defkey (C-x C-k) kill-region)

(defkey s-k kill-this-buffer)

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
     (find-file (emacs-file ,file))))

(key-to-open-file H-i "init.el")
(key-to-open-file H-k "keys.el")
(key-to-open-file H-g "global.el")
(key-to-open-file H-p "packages.el")



(provide 'keys)
