(require 'html2text)

(defvar grc-entry-cache nil)
(defvar grc-current-entry nil)

(defun grc-xml-get-child (node child-name)
  (car (last (assq child-name node))))

(defun grc-strip-html (text)
  (when text
    (g-using-scratch
     (insert text)

     ;; There should be a better way...
     (html2text-replace-string "’" "'" (point-min) (point-max))

     (html2text)
     (buffer-substring (point-min) (point-max)))))


;;(xml-get-children (first grc-xml-entries) 'id)
;;(grc-xml-get-child (first grc-xml-entries) 'id)
(defun grc-process-entry (entry)
  `((title . ,(grc-strip-html (grc-xml-get-child entry 'title)))
    (date . ,(grc-xml-get-child entry 'published))
    (link . ,(xml-get-attribute (assq 'link entry) 'href))
    (source . ,(grc-strip-html (grc-xml-get-child
                                (first (xml-get-children entry 'source))
                                'title)))
    (summary . ,(grc-strip-html (grc-xml-get-child entry 'summary)))))


(defun grc-parse-response (buffer)
  (let* ((root (car (xml-parse-region (point-min) (point-max))))
         (xml-entries (xml-get-children root 'entry))
         (entries (grc-sort-by 'date (mapcar 'grc-process-entry xml-entries))))
    entries))

(defun grc-remote-entries ()
  (let ((g-atom-view-xsl nil)
        (g-html-handler `grc-parse-response)
        (greader-state-url-pattern (concat greader-state-url-pattern
                                           "&xt=user/-/state/com.google/read")))
    (greader-reading-list)))

(defun grc-truncate-text (text &optional max elide)
  (if text
      (let* ((max (or max 20))
             (len (length text))
             (str (replace-regexp-in-string
                   "\\(\\W\\)*$" "" (substring text
                                               0
                                               (if (> max len) len max)))))
        (if (and (< max len) elide)
            (concat str "...")
          str))
    ""))

(defun grc-print-entry (entry)
  (let ((source ))
    (insert
     (format "%-12s    %-25s   %s\n"
             (format-time-string "%a %l:%M %p"
                                 (date-to-time (aget entry 'date t)))
             (grc-truncate-text (aget entry 'source t) 22 t)
             (aget entry 'title t)))))

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

;; TODO - sort seems destructive somehow, need to sort up-front and
;; maintain integrity, rather than always creating a copy to sort...
(defun grc-sort-by (field entries)
  (let ((sorted (sort (copy-alist entries)
                      (lambda (a b)
                        (string<
                         (aget a field)
                         (aget b field))))))
    (setq grc-entry-cache sorted)
    sorted))

(defun grc-reading-list ()
  (interactive)
  (with-current-buffer (get-buffer-create "*grc list*")
    (grc-list-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (mapcar 'grc-print-entry (grc-remote-entries)))))

(defun grc-get-current-item ()
  (nth (- (line-number-at-pos) 1) grc-entry-cache))

(defun grc-help ()
  ;;TODO
  (interactive)
  )

(defun grc-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun grc-view-external ()
  "Open the current rss entry in the default emacs browser"
  (interactive)
  (let* ((entry (grc-get-current-item))
         (link (aget entry 'link t)))
    (if link
        (browse-url link)
      (message "Unable to view this item"))))

(defun grc-mark-read-and-remove ()
  (interactive)
  (let ((entry (grc-get-current-item)))
    ;;TODO
    ))

(defun grc-next-item ()
  (interactive)
  (next-line)
  (move-beginning-of-line nil))

(defun grc-previous-item ()
  (interactive)
  (previous-line)
  (move-beginning-of-line nil))

(defun grc-advance-or-show-next-item ()
  ;;TODO
  (interactive)
  )

(defun grc-show-item ()
  ;;TODO
  (interactive)
  )

(defvar grc-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'grc-help)
    (define-key map "q" 'grc-kill-this-buffer)
    (define-key map "v" 'grc-view-external)
    (define-key map "x" 'grc-mark-read-and-remove)
    (define-key map "n" 'grc-next-item)
    (define-key map "p" 'grc-previous-item)
    (define-key map " " 'grc-advance-or-show-next-item)
    (define-key map (kbd "RET") 'grc-show-item)
    map)
  "Keymap for \"grc list\" buffers.")
(fset 'grc-list-mode-map grc-list-mode-map)

(defun grc-list-mode ()
  "Major mode for viewing feeds with grc

This buffer contains the results of the \"grc-reading-list\" command
for displaying unread feeds from Google Reader.

All currently available key bindings:

\\{grc-list-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map grc-list-mode-map)
  (setq major-mode 'grc-list-mode
        mode-name "grc-list")
  (setq buffer-read-only t))









(defvar grc-viewn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'grc-help)
    (define-key map "q" 'grc-kill-this-buffer)
    (define-key map "v" 'grc-view-external)
    (define-key map "x" 'grc-mark-read-and-remove)
    (define-key map "n" 'grc-next-item)
    (define-key map "p" 'grc-previous-item)
    (define-key map " " 'grc-advance-or-show-next-item)
    (define-key map (kbd "RET") 'grc-show-item)
    map)
  "Keymap for \"grc view\" buffers.")
(fset 'grc-view-mode-map grc-view-mode-map)

(defun grc-view-mode ()
  "Major mode for viewing feeds with grc

This buffer contains the results of the \"grc-reading-list\" command
for displaying unread feeds from Google Reader.

All currently available key bindings:

\\{grc-list-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map grc-view-mode-map)
  (setq major-mode 'grc-view-mode
        mode-name "grc-view")
  (setq buffer-read-only t))
