;; general
;; TODO: need to access https://www.google.com/reader/api/0/friend/list to
;; get to the encodedSharersList to send in...
;; This might necessitate leaving greader behind...
;; TODO: Oauth

;; TODO: Docstrings && help commands
;; TODO: refactor downloading and showing - ie, initial dl should call
;;       grc-refresh-view
;; TODO: split this file into smaller files

;; both list and show
;; TODO: All, not just unread
;; TODO: share
;; TODO: adding note - edit w/ snippet=note
;; TODO: emailing
;; TODO: feed icons?
;; TODO: comment icon, user pictures with comments?

;; List view
;; TODO: pagination?
;; TODO: add counts to header line
;; TODO: add years when looking at older posts
;; TODO: operations on regions (read, etc)
;; TODO: flexible columns? - calc max col sizes upfront
;; TODO: investigate other ways of refreshing view (delete lines, etc)
;;       for refreshing a line - modify entry, delete line, redraw
;; TODO: secondary sort
;; TODO: search

;; Show view
;; TODO: fill-buffer to prevent long lines (does w3m have something
;;       for this? or is generic "fill")
;; TODO: user comments
;; TODO: add comment

(require 'cl)
(require 'html2text)
(require 'g-auth)
(require 'greader)

(require 'grc-lib)
(require 'grc-req)
(require 'grc-parse)
(require 'grc-highlight)
(require 'grc-list)
(require 'grc-show)

;; The default of 4 hours seems to be too long
(setq g-auth-lifetime "1 hour")

(defgroup grc nil "Google Reader Client for Emacs")
(defcustom grc-enable-hl-line t
  "Turn on hl-line-mode in the grc list buffer"
  :type  'boolean
  :group 'grc)

(defcustom grc-fetch-count 100
  "The count of items to fetch.  The larger the count the slower the request."
  :type 'integer
  :group 'grc)

(defcustom grc-shell-file-name "/bin/bash"
  "Greader, as is, has issues with zsh.  This is my workaround."
  :type 'string
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

(defvar grc-entry-cache nil)
(defvar grc-current-entry nil)
(defvar grc-current-state "reading-list")

(defvar grc-sort-columns '(date source))
(defvar grc-current-sort nil)
(defvar grc-current-sort-reversed nil)
(defcustom grc-default-sort-column 'date
  "Default column by which to sort the list view"
  :group 'grc
  :type '(choice (const :tag "Date" 'date)
                 (const :tag "Source" 'source)))

(defvar grc-list-buffer "*grc list*" "Name of the buffer for the grc list view")

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



(defvar grc-state-alist '(("Shared"       . "broadcast-friends")
                          ("Kept Unread"  . "kept-unread")
                          ("Read"         . "read")
                          ("Reading List" . "reading-list")
                          ("Starred"      . "starred")))

(defun grc-read-state (prompt)
  "Return state name read from minibuffer."
  (let* ((grc-read-history '())
         (greader-state-alist grc-state-alist)
         (choices (sort (mapcar 'car greader-state-alist) 'string<))
         (completing-read-fn (if (featurep 'ido)
                                 'ido-completing-read
                               'completing-read))
         (selection (apply completing-read-fn prompt choices
                           nil 'require-match nil grc-read-history)))
    (aget greader-state-alist selection)))

;; Main entry function
(defun grc-reading-list (&optional state)
  (interactive "P")
  (grc-req-ensure-authenticated)
  (let ((buffer (get-buffer-create grc-list-buffer))
        (state (if (and state (interactive-p))
                   (grc-read-state "State: ")
                 grc-current-state)))
    (setq grc-current-state state)
    (with-current-buffer buffer
      (grc-list-mode)
      (grc-list-display (grc-req-remote-entries grc-current-state))
      (grc-list-header-line)
      (goto-char (point-min))
      (switch-to-buffer buffer))))
(defalias 'grc 'grc-reading-list)

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
                 (grc-req-send-edit-request (grc-req-google-tag-request entry ,tag
                                                                        remove))
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
