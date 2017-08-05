(add-to-list 'initial-frame-alist '(fullscreen . maximized))

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

;;;

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
