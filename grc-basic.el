(autoload 's-trim "s.el")

(defvar grc-basic-html-entity-list nil)

(setq grc-basic-html-entity-list
      '(("&nbsp;" "") ;; do I need these spaces?
        ("&amp;" "&")
        ("&apos;" "'")
        ("&#39;" "'")
        ("&#8217;" "'")
        ("&prime;" "'")
        ("&gt;" ">")
        ("&lt;" "<")
        ("&quot;" "\"")
        ("&#8220;" "\"")
        ("&#8221;" "\"")
        ("&ldquo;" "\"")
        ("&rdquo;" "\"")
        ("&mdash;" "—")
        ("&#9670;" "©")))

(defun grc-basic-convert-entities ()
  "Searches through the buffer replacing common HTML entities with their chars"
  (mapcar '(lambda (pair)
             (goto-char (point-min))
             (grc-replace-string (car pair) (cadr pair)))
          grc-basic-html-entity-list))

(defun grc-basic-trim-left-in-buffer ()
  "Removes all leading whitespace from all lines in the buffer"
  (goto-char (point-min))
  (while (not (eobp))
    (beginning-of-line)
    (delete-horizontal-space)
    (forward-line 1)))

(defun grc-basic-normalize-newlines ()
  "Reduces multiple blank lines down to one"
  (goto-char (point-min))
  (grc-replace-regexp "^\n+" "\n")
  (when (and (> (point-max) (point-min))
             (equal "\n" (buffer-substring (point-min) (1+ (point-min)))))
    (goto-char (point-min))
    (delete-char 1)))

(defun grc-basic-strip-tag (tag)
  (goto-char (point-min))
  (let ((pt (search-forward (concat "<" tag) nil t)))
    (while pt
      (delete-region (- pt (1+ (length tag)))
                     (search-forward (concat "</" tag ">") nil t))
      (setq pt (search-forward (concat "<" tag) nil t)))))

(defun grc-basic-strip-html ()
  "Converts some HTML entities and naively removes HTML tags."
  (grc-basic-convert-entities)
  (-each '("head" "style" "script" "input" "select" "textarea") 'grc-basic-strip-tag)
  (goto-char (point-min))
  (grc-replace-regexp "<.*?>" "")
  (grc-basic-trim-left-in-buffer)
  (grc-basic-normalize-newlines)
  (goto-char (point-min)))

(defun grc-basic-strip-html-to-string (str)
  "Takes a string and returns it stripped of HTML"
  (with-temp-buffer
    (insert str)
    (grc-basic-strip-html)
    (buffer-string)))

(defun grc-basic-insert-newlines ()
  (goto-char (point-min))
  (grc-replace-regexp "<br.*?>" "\n")
  (goto-char (point-min))
  (grc-replace-regexp "</.?p>" "\n"))

(defun grc-basic-buttonify-anchors ()
  "Walks through a buffer of html and removes the anchor tags,
  replacing them with a button that will browse the link"
  (goto-char (point-min))
  (while (search-forward-regexp "<a" nil t)
    (let* ((p1 (point))
           (p2 (search-forward-regexp ">" nil t))
           (p3 (search-forward-regexp "</a>" nil t))
           (attrs (html2text-get-attr p1 p2))
           (href (html2text-attr-value attrs "href"))
           (href (substring href 1 (1- (length href))))
           (text (grc-basic-strip-html-to-string
                  (buffer-substring-no-properties p2 (- p3 4)))))
      (when (and text (not (equal "" (s-trim text))))
        (progn
          (delete-region (- p1 2) p3)
          (let ((pt (point)))
            (insert text)
            (make-button pt
                         (point)
                         'url href
                         'help-echo href
                         'type 'grc-basic-link-button)))))))

(defun grc-button-browse-url (overlay)
  (browse-url (overlay-get overlay 'url)))

(define-button-type 'grc-basic-link-button
  'follow-link t
  'face 'link
  'action #'grc-basic-button-browse-url)

(defun grc-basic-prepare-text (text)
  (grc-basic-strip-html-to-string text))

(defun grc-basic-next-anchor ()
  (interactive)
  (forward-button 1 t t))

(defun grc-basic-previous-anchor ()
  (interactive)
  (backward-button 1 t t))

(defun grc-show-basic-renderer ()
  (grc-basic-buttonify-anchors)
  (grc-basic-insert-newlines)
  (goto-char (point-min))
  (grc-basic-strip-html)
  (fill-region (point-min) (point-max)))

(setq grc-prepare-text-fn 'grc-basic-prepare-text
      grc-show-summary-renderer 'grc-show-basic-renderer
      grc-show-external-link-viewer 'grc-basic-external-view-this-url
      grc-show-next-anchor-fn 'grc-basic-next-anchor
      grc-show-previous-anchor-fn 'grc-basic-previous-anchor)

(provide 'grc-basic)
