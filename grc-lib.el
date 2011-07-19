;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General purpose functions
(defun grc-list (thing)
  "Return THING if THING is a list, or a list with THING as its element."
  (if (listp thing)
      thing
    (list thing)))

(defun grc-string (thing)
  (if (stringp thing)
      thing
    (prin1-to-string thing t)))

(defun grc-flatten (x)
  (cond ((null x) nil)
        ((listp x) (append (grc-flatten (car x)) (grc-flatten (cdr x))))
        (t (list x))))

(defun grc-get-in (alist seq &optional not-found)
  (let ((val (reduce (lambda (a k)
                       (aget a k)) seq :initial-value alist)))
    (if (and (null val) not-found)
        not-found
      val)))

(provide 'grc-lib)
