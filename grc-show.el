;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show functions
(defvar grc-show-buffer "*grc show*" "Name of the buffer for the grc show view")

(defun grc-show-print-comment (comment)
  (insert (format "%s - %s<br/>%s<br/><br/>"
                  (aget comment 'author)
                  (format-time-string
                   "%a %m/%d %l:%M %p"
                   (seconds-to-time (aget comment 'createdTime)))
                  (or (aget comment 'htmlContent)
                      (aget comment 'plainContent)))))

(defun grc-show-entry (entry)
  (let ((buffer (get-buffer-create grc-show-buffer)))
    (with-current-buffer buffer
      (grc-show-mode)
      (let ((inhibit-read-only t)
            (next-entry (cadr (member entry grc-entry-cache)))
            (prev-entry (cadr (member entry (reverse grc-entry-cache))))
            (summary (or (aget entry 'content t)
                         (aget entry 'summary t)
                         "No summary provided."))
            (title (or (aget entry 'title))))
        (erase-buffer)
        (insert "<html><head><title></title></head><body>")
        (mapcar (lambda (lst) (insert (format "%s:  %s<br/>"
                                         (car lst) (cadr lst))))
                `(("Title"  ,(aget entry 'title))
                  ("Link"   ,(aget entry 'link))
                  ("Date"   ,(format-time-string
                              "%a %m/%d %l:%M %p"
                              (seconds-to-time (aget entry 'date))))
                  ("Source" ,(aget entry 'source))
                  ("Next Story"
                   ,(if next-entry
                        (concat (aget next-entry 'title)
                                " from "
                                (aget next-entry 'source))
                      "None"))
                  ("Previous Story"
                   ,(if prev-entry
                        (concat (aget prev-entry 'title)
                                " from "
                                (aget prev-entry 'source))
                      "None"))))
        (insert "<br/>" summary)

        (when (aget entry 'comments t)
          (insert "<br/><br/>Comments:<br/>")
          (mapcar 'grc-show-print-comment
                  (grc-sort-by 'createdTime (aget entry 'comments))))

        (insert "</body></html>")
        (if (featurep 'w3m)
            (let ((w3m-display-inline-images t))
              (w3m-buffer))
          (html2text))
        (grc-highlight-keywords (grc-keywords grc-entry-cache))))
    (setq grc-current-entry (grc-mark-read entry))
    (switch-to-buffer buffer)
    (grc-list-refresh)))

(defun grc-show-help ()
  ;;TODO
  (interactive)
  )

(defun grc-show-mark-kept-unread (remove)
  (interactive "P")
  (funcall (grc-mark-fn "kept-unread") grc-current-entry remove))

(defun grc-show-mark-starred (remove)
  (interactive "P")
  (funcall (grc-mark-fn "starred") grc-current-entry remove))

(defun grc-show-kill-this-buffer ()
  (interactive)
  (when (get-buffer grc-list-buffer)
    (switch-to-buffer (get-buffer grc-list-buffer))
    (kill-buffer grc-show-buffer)))

(defun grc-show-next-entry ()
  (interactive)
  (let ((entry (cadr (member grc-current-entry grc-entry-cache))))
    (if entry
        (progn
          (grc-show-entry entry)
          (with-current-buffer grc-list-buffer
            (grc-list-refresh)
            (forward-line)))
      (error "No more entries"))))

(defun grc-show-previous-entry ()
  (interactive)
  (let ((entry (cadr (member grc-current-entry (reverse grc-entry-cache)))))
    (if entry
        (progn
          (grc-show-entry entry)
          (with-current-buffer grc-list-buffer
            (grc-list-refresh)
            (forward-line -1)))
      (error "No previous entries"))))

(defun grc-show-view-external ()
  (interactive)
  (grc-view-external grc-current-entry))

(defun grc-show-advance-or-show-next-entry ()
  (interactive)
  (if (eobp)
      (grc-show-next-entry)
    (let ((scroll-error-top-bottom t))
      (scroll-up-command 25))))

(defun grc-show-external-view-url ()
  (interactive)
  (when (featurep 'w3m)
    (w3m-external-view-this-url)))

(defun grc-show-next-anchor ()
  (interactive)
  (when (featurep 'w3m)
    (w3m-next-anchor)))

(defun grc-show-previous-anchor ()
  (interactive)
  (when (featurep 'w3m)
    (w3m-previous-anchor)))

(defvar grc-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " "           'grc-show-advance-or-show-next-entry)
    (define-key map "?"           'grc-show-help)
    (define-key map "q"           'grc-show-kill-this-buffer)
    (define-key map "k"           'grc-show-mark-kept-unread)
    (define-key map "s"           'grc-show-mark-starred)
    (define-key map "n"           'grc-show-next-entry)
    (define-key map "p"           'grc-show-previous-entry)
    (define-key map "v"           'grc-show-view-external)
    (define-key map (kbd "RET")   'grc-show-external-view-url)
    (define-key map (kbd "TAB")   'grc-show-next-anchor)
    (define-key map (kbd "S-TAB") 'grc-show-previous-anchor)
    map)
  "Keymap for \"grc show\" buffers.")
(fset 'grc-show-mode-map grc-show-mode-map)

(defun grc-show-mode ()
  "Major mode for viewing a feed entry in grc

\\{grc-show-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map grc-show-mode-map)
  (setq major-mode 'grc-show-mode
        mode-name "grc-show")
  (setq buffer-read-only t))

(provide 'grc-show)
