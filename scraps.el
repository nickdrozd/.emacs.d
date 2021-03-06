(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

;; yas
;; PropertySchema.${1:$(upcase yas-text)}: '$1_value',
;; $0

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defun expand-macro ()
  (interactive)
  (backward-kill-sexp)
  (prin1 (macroexpand-1 (read (current-kill 0)))
         (current-buffer)))


(defun to-underscore ()
  (interactive)
  (replace-regexp "\\([A-Z]\\)" "_\\1" nil (region-beginning) (region-end))
  (downcase-region (region-beginning) (region-end)))


(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column 100000))
    (fill-paragraph)))


(defun dedicate-window (&optional arg)
  "Dedicate the current window."
  (interactive)
  (set-window-dedicated-p (selected-window) (not arg)))
