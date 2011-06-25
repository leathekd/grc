(require 'html2text)

(defun grc-xml-get-child (node child-name)
  (car (last (assq child-name node))))

(defvar grc-html-entity-list
  '(("&amp;" "&")
    ("&apos;" "'")
    ("&gt;" ">")
    ("&lt;" "<")
    ("&quot;" "\"")))

(defun grc-replace-string (from-string to-string)
  (while (search-forward from-string nil t)
    (replace-match to-string nil t)))

(defun grc-replace-regexp (regexp to-string)
  (while (search-forward-regexp regexp nil t)
    (replace-match to-string nil t)))

(defun grc-strip-html ()
  (save-excursion
    (mapcar '(lambda (pair)
               (goto-char 1)
               (grc-replace-string (car pair) (cadr pair)))
            grc-html-entity-list)
    (goto-char 1)
    (grc-replace-regexp "<.*?>" "")
    (goto-char 1)
    (while (not (eobp))
      (beginning-of-line)
      (delete-horizontal-space)
      (forward-line 1))
    (goto-char 1)
    (grc-replace-regexp "^\n+" "\n")))

(defun grc-process-entry (entry)
  `((title . ,(grc-xml-get-child entry 'title))
    (date . ,(grc-xml-get-child entry 'published))
    (link . ,(xml-get-attribute (assq 'link entry) 'href))
    (source . ,(grc-xml-get-child
                (first (xml-get-children entry 'source))
                'title))
    (summary . ,(g-using-scratch
                 (let ((summary (grc-xml-get-child entry 'summary))
                       (left-margin 4))
                   (if summary
                       (progn
                         (insert summary)
                         (html2text)
                         (goto-char (point-max))
                         (delete-blank-lines)
                         (fill-region (point-min) (point-max))
                         (buffer-substring (point-min) (point-max)))
                     ""))))))

(defun grc-print-entry (entry)
  (insert
   (format "  %s : %s\n    %s\n%s"
           (aget entry 'title t)
           (format-time-string "%F %R"
                               (date-to-time (aget entry 'date t)))
           (aget entry 'link t)
           (aget entry 'summary t))))

(defun grc-group-by (field entries)
  (let* ((groups (remq nil (remove-duplicates
                             (mapcar (lambda (x) (aget x field t)) entries)
                             :test 'string=)))
         (ret-list '()))
    (amake 'ret-list groups)
    (mapcar (lambda (entry)
              (let* ((k (aget entry field t))
                     (v (aget ret-list k t)))
                (aput 'ret-list k (cons entry v))))
            entries)
    ret-list))

(defun grc-sort-by (field entries)
  (sort entries
        (lambda (a b)
          (string<
           (aget a field)
           (aget b field)))))

(defvar grc-entries nil)



(defun grc-parse-response (buffer)
  (let* ((root (car (xml-parse-region (point-min) (point-max))))
         (xml-entries (xml-get-children root 'entry))
         (entries (mapcar 'grc-process-entry xml-entries)))
    (setq grc-entries entries)
    (with-current-buffer (get-buffer-create "*Google Reader*")
      (erase-buffer)
      (mapcar (lambda (group)
                (insert (car group) "\n")
                (mapcar 'grc-print-entry
                        (grc-sort-by 'date (cdr group))))
              (grc-group-by 'source entries)))))

(defun grc-refresh-remote-entries ()
  (let ((g-atom-view-xsl nil)
        (g-html-handler `grc-parse-response)
        (greader-state-url-pattern (concat greader-state-url-pattern
                                           "&xt=user/-/state/com.google/read")))
    (greader-reading-list)))

(defun grc-reading-list ()
  (interactive)
  (let ((g-atom-view-xsl nil)
        (g-html-handler `grc-parse-response)
        (greader-state-url-pattern (concat greader-state-url-pattern
                                           "&xt=user/-/state/com.google/read")))
    (greader-reading-list)))




