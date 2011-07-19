;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request parsing
(defun grc-parse-get-categories (json-entry)
  (remove-if 'null
             (mapcar (lambda (c)
                       (let ((label-idx (string-match "/label/" c))
                             (state-idx (string-match "/state/com.google/" c)))
                         (cond
                          (state-idx (substring c (+ state-idx 18)))
                          (label-idx (substring c (+ label-idx 7))))))
                     (aget json-entry 'categories))))

(defun grc-parse-process-entry (json-entry)
  `((id         . ,(aget json-entry 'id))
    (date       . ,(aget json-entry 'published))
    (title      . ,(aget json-entry 'title))
    ;; TODO: could be many links here...
    (link       . ,(aget (first (aget json-entry 'alternate t)) 'href))
    (source     . ,(or (aget (first (aget json-entry 'via)) 'title)
                       (grc-get-in json-entry '(origin title))))
    (feed       . ,(grc-get-in json-entry '(origin streamId)))
    (summary    . ,(grc-get-in json-entry '(summary content)))
    (content    . ,(or (grc-get-in json-entry '(content content))))
    (categories . ,(grc-parse-get-categories json-entry))))

(defun grc-parse-parse-response (root)
  (setq grc-raw-response root)
  (let ((entries (aget root 'items)))
    (mapcar 'grc-parse-process-entry entries)))

(provide 'grc-parse)
