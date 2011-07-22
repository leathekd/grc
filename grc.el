(require 'cl)
(require 'html2text)

(require 'grc-lib)
(require 'grc-auth)
(require 'grc-req)
(require 'grc-parse)
(require 'grc-highlight)
(require 'grc-list)
(require 'grc-show)


(defgroup grc nil "Google Reader Client for Emacs")
(defcustom grc-enable-hl-line t
  "Turn on hl-line-mode in the grc list buffer"
  :type  'boolean
  :group 'grc)

(defcustom grc-fetch-count 100
  "The count of items to fetch.  The larger the count the slower the request."
  :type 'integer
  :group 'grc)

(defvar grc-google-categories
  '(("broadcast"               . "Shared")
    ("broadcast-friends"       . "Shared")
    ("fresh"                   . "Fresh")
    ("kept-unread"             . "Kept Unread")
    ("like"                    . "Liked")
    ("read"                    . "Read")
    ("reading-list"            . "Reading List")
    ("starred"                 . "Starred")
    ("tracking-body-link-used" . "Tracking Body Link Used")
    ("tracking-emailed"        . "Tracking Email")
    ("tracking-item-link-used" . "Tracking Item Link Used")
    ("tracking-kept-unread"    . "Tracking Kept Unread")
    ("tracking-mobile-read"    . "Tracking Mobile Read"))
  "list of the categories that google adds to entries")

(defvar grc-state-alist '("Shared" "Kept Unread" "Read"
                          "Reading List" "Starred"))
(defvar grc-current-state "reading-list")

(defvar grc-entry-cache nil)
(defvar grc-current-entry nil)

(defun grc-set-current-entry (entry)
  (setq grc-current-entry entry))

(defun grc-set-entry-cache (entries)
  (setq grc-entry-cache entries))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display functions
(defun grc-prepare-text (text)
  (when text
    (with-temp-buffer
      (insert (decode-coding-string text 'utf-8))
      (when (featurep 'w3m)
        (w3m-decode-entities))
      (html2text)
      (buffer-substring (point-min) (point-max)))))

(defun grc-truncate-text (text &optional max elide)
  (if text
      (let* ((max (or max 20))
             (len (length text))
             (max (if (and elide (< max len))
                      (- max 3)
                    max))
             (str (replace-regexp-in-string
                   "\\(\\W\\)*$"
                   ""
                   (substring text 0 (if (> max len) len max)))))
        (if (and (< max len) elide)
            (concat str "...")
          str))
    ""))

(defun grc-format-categories (entry)
  (let* ((cats (aget entry 'categories t)))
    (mapconcat (lambda (c) (or (aget grc-google-categories c t) c))
               (reduce (lambda (categories c)
                         (remove c categories))
                       '("broadcast" "fresh" "reading-list"
                         "tracking-body-link-used" "tracking-emailed"
                         "tracking-item-link-used" "tracking-kept-unread"
                         "tracking-mobile-read")
                       :initial-value cats)
               " ")))

(defun grc-title-for-printing (entry title-width)
  (let ((title (aget entry 'title t))
        (streamId (aget entry 'feed))
        (summary (or (aget entry 'content t)
                     (aget entry 'summary t)))
        (case-fold-search t))
    (if title
        title
      (if (string-match "post$" streamId)
          (grc-truncate-text (substring summary 0 (string-match "<br" summary))
                             title-width t)
        "No title provided."))))

(defun grc-keywords (entries)
  ;; TODO: too convoluted- simplify
  ;;       this gets all the cats across entries, flattens to one
  ;;       list, dedups, then translates to what the user sees
  (let ((categories
         (mapcar (lambda (c) (or (aget grc-google-categories c t) c))
                 (delete-dups (grc-flatten
                               (mapcar (lambda (e) (aget e 'categories t))
                                       entries))))))
    (delete-dups
     (append categories
             (mapcar (lambda (e) (grc-truncate-text
                             (aget e 'source) 22 t)) entries)))))

(defun grc-read-state (prompt)
  "Return state name read from minibuffer."
  (let ((grc-read-history '())
        (choices (sort grc-state-alist 'string<))
        (completing-read-fn (if (featurep 'ido)
                                'ido-completing-read
                              'completing-read)))
    (cdr (rassoc (apply completing-read-fn prompt choices
                        nil 'require-match nil grc-read-history)
                 grc-google-categories))))

;; Main entry function
(defun grc (&optional state)
  (interactive "P")
  (setq grc-current-state (if (and state (interactive-p))
                              (grc-read-state "State: ")
                            grc-current-state))
  (grc-list-display (grc-req-remote-entries grc-current-state))
  (switch-to-buffer grc-list-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General view functions
(defun grc-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun grc-entry-index (entry)
  (- (length grc-entry-cache)
     (length (member entry grc-entry-cache))))

(defun grc-add-category (entry category)
  (let ((mem (member entry grc-entry-cache)))
    (when (null (member category (aget entry 'categories t)))
      (aput 'entry 'categories
            (cons category (aget entry 'categories t))))
    (setcar mem entry)
    entry))

(defun grc-remove-category (entry category)
  (let ((mem (member entry grc-entry-cache)))
    (when (member category (aget entry 'categories t))
      (aput 'entry 'categories
            (delete category (aget entry 'categories t))))
    (setcar mem entry)
    entry))

(defun grc-mark-fn (tag)
  `(lambda (entry &optional remove)
     (let ((mem (member ,tag (aget entry 'categories))))
       (cond
        ((and mem (null remove)) entry)
        ((and (null mem) remove) entry)
        (t (condition-case err
               (progn
                 (grc-req-send-edit-request (grc-req-tag-request entry
                                                                 ,tag remove))
                 (if (null remove)
                     (grc-add-category entry ,tag)
                   (grc-remove-category entry ,tag)))
             (error (message "There was a problem marking the entry as read: %s"
                             err))))))))

(defun grc-mark-read (entry)
  (funcall (grc-mark-fn "read") entry nil))

(defun grc-mark-kept-unread (entry)
  (funcall (grc-mark-fn "kept-unread") entry nil))

(defun grc-mark-read-and-remove (entry)
  (delete (grc-mark-read entry) grc-entry-cache))

(defun grc-mark-starred (entry &optional remove)
  (funcall (grc-mark-fn "starred") entry remove))

(defun grc-view-external (entry)
  "Open the current rss entry in the default emacs browser"
  (interactive)
  (let ((link (aget entry 'link t)))
    (if link
        (progn
          (browse-url link)
          (grc-mark-read entry))
      (message "Unable to view this entry"))))

(provide 'grc)
