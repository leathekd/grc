;; general
;; TODO: investigate what it would take to remove reliance on g-client
;;       if needed
;; TODO: requests need to be much more async.  It's unacceptable to
;;       freeze emacs when fetching feeds -
;;       start-process-shell-command and sentinels?

;; both list and show
;; TODO: header line?
;; TODO: mark unread, star, unstar, share(?), email(?)
;;       (greader-star)?
;; TODO: adding note - edit w/ snippet=note
;; TODO: emailing, sharing

;; List view
;; TODO: add years when looking at older posts
;; TODO: operations on regions (read, etc)
;; TODO: flexible columns? - calc max col sizes upfront
;; TODO: investigate other ways of refreshing view (delete lines, etc)
;;       for refreshing a line - modify entry, delete line, redraw
;; TODO: sorting/grouping list view
;; TODO: mark all as read: http://www.google.com/reader/api/0/mark-all-as-read
;; TODO: search

;; Show view
;; TODO:

(require 'cl)
(require 'html2text)
(require 'g-auth)
(require 'greader)

(defvar grc-entry-cache nil)
(defvar grc-current-entry nil)
(defvar grc-list-buffer "*grc list*" "Name of the buffer for the grc list view")
(defvar grc-show-buffer "*grc show*" "Name of the buffer for the grc show view")

(defgroup grc nil "Google Reader Client for Emacs")
(defcustom grc-enable-hl-line t
  "Turn on hl-line-mode in the grc list buffer"
  :type  'boolean
  :group 'grc)

(defface grc-highlight-nick-base-face
  '((t nil))
  "Base face used for highlighting nicks in erc. (Before the nick
color is added)"
  :group 'grc-faces)

(defvar grc-highlight-face-table
  (make-hash-table :test 'equal)
  "The hash table that contains unique grc faces.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General purpose functions
(defun grc-list (thing)
  "Return THING if THING is a list, or a list with THING as its element."
  (if (listp thing)
      thing
    (list thing)))

(defun grc-flatten (x)
  (cond ((null x) nil)
        ((listp x) (append (grc-flatten (car x)) (grc-flatten (cdr x))))
        (t (list x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google reader requests
(defun grc-remote-entries (&optional state)
  "This overrides and hooks into greader.el to get the job done."
  (let ((g-atom-view-xsl nil)
        (g-html-handler `grc-parse-response)
        (greader-state-url-pattern
         (if (null state)
             (concat greader-state-url-pattern
                     "&xt=user/-/state/com.google/read")
           greader-state-url-pattern))
        (greader-number-of-articles 100))
    (greader-reading-list state)))

(defun grc-send-request (request)
  (declare (special g-curl-program g-curl-common-options
                    greader-auth-handle))
  (g-auth-ensure-token greader-auth-handle)
  (with-temp-buffer
    (shell-command
     (format "%s %s %s  -X POST -d '%s' '%s' "
             g-curl-program g-curl-common-options
             (g-authorization greader-auth-handle)
             request
             "http://www.google.com/reader/api/0/edit-tag?client=emacs-g-client")
     (current-buffer))
    (goto-char (point-min))
    (cond
     ((looking-at "OK") (message "OK"))
     (t (error "Error %s: " request)))))

(defun grc-mark-read-request (entry)
  (format "a=user/-/state/com.google/read&async=true&s=%s&i=%s&T=%s"
          (aget entry 'feed)
          (aget entry 'id)
          (g-auth-token greader-auth-handle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request parsing
(defun grc-xml-get-child (node child-name)
  (car (last (assq child-name node))))

(defun grc-xml-get-categories (xml-entry)
  (remove-if 'empty-string-p
             (mapcar (lambda (e) (xml-get-attribute e 'label))
                     (xml-get-children xml-entry 'category))))

(defun grc-xml-get-source (xml-entry)
  "Will extract the souce from the xml-entry.  If it is a shared item, it will
  extract the source from the link with the title X's shared items"
  (let ((categories (grc-xml-get-categories xml-entry)))
    (if (or (member "broadcast" categories)
            (member "broadcast-friends" categories))
        (let ((link (first
                     (remove-if-not
                      (lambda (e) (string= "via" (xml-get-attribute e 'rel)))
                      (xml-get-children xml-entry 'link)))))
          (xml-get-attribute link 'title))
      (grc-xml-get-child (first (xml-get-children xml-entry 'source)) 'title))))

(defun grc-process-entry (xml-entry)
  `((id         . ,(grc-xml-get-child xml-entry 'id))
    (date       . ,(grc-xml-get-child xml-entry 'published))
    (title      . ,(grc-xml-get-child xml-entry 'title))
    (link       . ,(xml-get-attribute (assq 'link xml-entry) 'href))
    (source     . ,(grc-xml-get-source xml-entry))
    (feed       . ,(xml-get-attribute (assq 'source xml-entry) 'gr:stream-id))
    (summary    . ,(grc-xml-get-child xml-entry 'summary))
    (content    . ,(grc-xml-get-child xml-entry 'content))
    (categories . ,(grc-xml-get-categories xml-entry))))

(defun grc-parse-response (buffer)
  (let* ((root (car (xml-parse-region (point-min) (point-max))))
         (xml-entries (xml-get-children root 'entry))
         (entries (grc-sort-by 'date
                               (mapcar 'grc-process-entry xml-entries)
                               t)))
    (setq grc-xml-entries xml-entries)
    entries))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting keywords
(defun grc-hexcolor-luminance (color)
  "Returns the luminance of color COLOR. COLOR is a string \(e.g.
\"#ffaa00\", \"blue\"\) `color-values' accepts. Luminance is a
value of 0.299 red + 0.587 green + 0.114 blue and is always
between 0 and 255."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (car (cdr values)))
         (b (car (cdr (cdr values)))))
    (floor (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)) 256)))

(defun grc-invert-color (color)
  "Returns the inverted color of COLOR."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (car (cdr values)))
         (b (car (cdr (cdr values)))))
    (format "#%04x%04x%04x"
            (- 65535 r) (- 65535 g) (- 65535 b))))

;;;###autoload
(defun grc-highlight-keywords (keywords)
  "Searches for nicknames and highlights them. Uses the first
twelve digits of the MD5 message digest of the nickname as
color (#rrrrggggbbbb)."
  (let (bounds word color new-kw-face kw (case-fold-search nil))
    (while keywords
      (goto-char (point-min))
      (setq kw (car keywords))
      (while (search-forward kw nil t)
        (setq bounds `(,(point) . ,(- (point) (length kw))))
        (setq word (buffer-substring-no-properties
                    (car bounds) (cdr bounds)))
        (setq new-kw-face (gethash word grc-highlight-face-table))
        (unless new-kw-face
          (setq color (concat "#" (substring (md5 (downcase word)) 0 12)))
          (if (equal (cdr (assoc 'background-mode (frame-parameters))) 'dark)
              ;; if too dark for background
              (when (< (grc-hexcolor-luminance color) 85)
                (setq color (grc-invert-color color)))
            ;; if to bright for background
            (when (> (grc-hexcolor-luminance color) 170)
              (setq color (grc-invert-color color))))
          (setq new-kw-face (make-symbol (concat "grc-highlight-nick-"
                                                 word "-face")))
          (copy-face 'grc-highlight-nick-base-face new-kw-face)
          (set-face-foreground new-kw-face color)
          (puthash word new-kw-face grc-highlight-face-table))
        (put-text-property (car bounds) (cdr bounds) 'face new-kw-face))
      (setq keywords (cdr keywords))))
  )

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

;; "list of the categories that google adds to entries"
(setq grc-google-categories '(("broadcast"         . "Shared")
                              ("broadcast-friends" . "Shared")
                              ("read"              . "Read")
                              ("kept-unread"       . "Kept Unread")
                              ("like"              . "Liked")
                              ("reading-list"      . "Reading List")
                              ("starred"           . "Starred")))

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

(defun grc-print-entry (entry)
  "Takes an entry and formats it into the line that'll appear on the list view"
  (let* ((source (grc-prepare-text (aget entry 'source t)))
         (title (grc-prepare-text (aget entry 'title t)))
         (cats (grc-format-categories entry))
         (date (date-to-time (aget entry 'date t)))
         (one-week (- (float-time (current-time))
                      (* 60 60 24 7)))
         (static-width (+ 14 2 23 2 2 (length cats) 1))
         (title-width (- (window-width) static-width)))
    (insert
     (format "%-14s  %-23s  %s"
             (format-time-string
              (if (> one-week (float-time date))
                  "%m/%d %l:%M %p"
                "  %a %l:%M %p")
              date)
             (grc-truncate-text source 23 t)
             (grc-truncate-text title title-width t)))

    (when (< 0 (length cats))
      (insert (format " (%s)" cats)))
    (insert "\n")))

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

(defun grc-sort-by (field entries &optional reverse-result)
  (let* ((sorted (sort (copy-alist entries)
                       (lambda (a b)
                         (string<
                          (aget a field)
                          (aget b field)))))
         (sorted (if reverse-result (reverse sorted) sorted)))
    (setq grc-entry-cache sorted)
    sorted))

(defun grc-display-list (entries)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (mapcar 'grc-print-entry entries)
    ;; TODO: too convoluted- simplify
    ;;       this gets all the cats across entries, flattens to one
    ;;       list, dedups, then translates to what the user sees
    (let* ((categories
            (mapcar (lambda (c) (or (aget grc-google-categories c t) c))
                    (delete-dups (grc-flatten
                                  (mapcar (lambda (e) (aget e 'categories t))
                                          entries)))))
           (keywords
            (delete-dups
             (append categories
                     (mapcar (lambda (e) (grc-truncate-text
                                     (aget e 'source) 22 t)) entries)))))
      (grc-highlight-keywords keywords))))

(defun grc-read-state (prompt)
  "Return state name read from minibuffer."
  (let* ((grc-read-history '())
         (greader-state-alist
          '(("Shared" . "broadcast-friends")
            ("Kept Unread" . "kept-unread")
            ("Read" . "read")
            ("Reading List" . "reading-list")
            ("Starred" . "starred")))
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
  (g-auth-ensure-token greader-auth-handle)
  (let ((buffer (get-buffer-create grc-list-buffer)))
    (with-current-buffer buffer
      (grc-list-mode)
      (grc-display-list
       (grc-remote-entries (when (and state (interactive-p))
                             (grc-read-state "State: "))))
      (goto-char (point-min))
      (switch-to-buffer buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General view functions
(defun grc-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun grc-entry-index (entry)
  (- (length grc-entry-cache)
     (length (member entry grc-entry-cache))))

(defun grc-show-entry (entry)
  (let ((buffer (get-buffer-create grc-show-buffer)))
    (with-current-buffer buffer
      (grc-show-mode)
      (let ((inhibit-read-only t)
            (summary (or (aget entry 'content t)
                         (aget entry 'summary t)
                         "No summary provided.")))
        (erase-buffer)
        (mapcar (lambda (lst) (insert (format "%s:  %s<br/>"
                                         (car lst) (cadr lst))))
                `(("Title"  ,(aget entry 'title))
                  ("Link"   ,(aget entry 'link) )
                  ("Date"   ,(aget entry 'date) )
                  ("Source" ,(aget entry 'source))))
        (insert "<br/>" summary)
        (if (featurep 'w3m)
            (w3m-buffer)
          (html2text))))
    (setq grc-current-entry (grc-mark-read entry))
    (switch-to-buffer buffer)
    (grc-list-refresh)))

(defun grc-mark-read (entry)
  (condition-case nil
      (progn
        ;;(grc-send-request (grc-mark-read-request entry))
        (let ((mem (member entry grc-entry-cache)))
          (when (null (member "read" (aget entry 'categories t)))
            (aput 'entry 'categories
                  (cons "read" (aget entry 'categories t))))
          (setcar mem entry)
          entry))
    (error "There was a problem marking the entry as read")))

(defun grc-mark-read-and-remove (entry)
  (delete (grc-mark-read entry) grc-entry-cache))

(defun grc-view-external (entry)
  "Open the current rss entry in the default emacs browser"
  (interactive)
  (let ((link (aget entry 'link t)))
    (if link
        (progn
          (browse-url link)
          (grc-mark-read entry))
      (message "Unable to view this entry"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List view functions
(defun grc-list-get-current-entry ()
  "utility function to get the entry from the current line in list view"
  (nth (- (line-number-at-pos) 1) grc-entry-cache))

(defun grc-list-next-entry ()
  (interactive)
  (next-line)
  (move-beginning-of-line nil))

(defun grc-list-previous-entry ()
  (interactive)
  (previous-line)
  (move-beginning-of-line nil))

(defun grc-list-refresh ()
  (with-current-buffer grc-list-buffer
    (let ((line (1- (line-number-at-pos))))
      (grc-display-list grc-entry-cache)
      (goto-char (point-min))
      (forward-line line))))

(defun grc-list-help ()
  ;;TODO
  (interactive)
  )

(defun grc-list-view-external ()
  "Open the current rss entry in the default emacs browser"
  (interactive)
  (grc-view-external (grc-list-get-current-entry))
  (grc-list-refresh))

(defun grc-list-mark-read ()
  (interactive)
  (grc-mark-read (grc-list-get-current-entry))
  (grc-list-next-entry)
  (grc-list-refresh)
  (forward-line))

(defun grc-list-mark-read-and-remove ()
  (interactive)
  (grc-mark-read-and-remove (grc-list-get-current-entry))
  (grc-list-refresh))

(defun grc-list-show-entry ()
  (interactive)
  (grc-show-entry (grc-list-get-current-entry)))

(defvar grc-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'grc-list-help)
    (define-key map "q" 'grc-kill-this-buffer)
    (define-key map "v" 'grc-list-view-external)
    (define-key map "r" 'grc-list-mark-read)
    (define-key map "x" 'grc-list-mark-read-and-remove)
    (define-key map "n" 'grc-list-next-entry)
    (define-key map "p" 'grc-list-previous-entry)
    (define-key map " " 'grc-list-show-entry)
    (define-key map "g" 'grc-reading-list)
    (define-key map (kbd "RET") 'grc-list-show-entry)
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
  (setq buffer-read-only t)
  (hl-line-mode grc-enable-hl-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show functions
(defun grc-show-help ()
  ;;TODO
  (interactive)
  )

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
  (grc-view-external grc-current-entry))

(defun grc-show-advance-or-show-next-entry ()
  (interactive)
  (if (eobp)
      (grc-show-next-entry)
    (let ((scroll-error-top-bottom t))
      (scroll-up-command 25))))

(defvar grc-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'grc-show-help)
    (define-key map "q" 'grc-show-kill-this-buffer)
    (define-key map "v" 'grc-show-view-external)
    (define-key map "n" 'grc-show-next-entry)
    (define-key map "p" 'grc-show-previous-entry)
    (define-key map " " 'grc-show-advance-or-show-next-entry)
    (when (featurep 'w3m)
      (define-key map (kbd "TAB") 'w3m-next-anchor))
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
  (setq buffer-read-only t)
  (when (featurep 'w3m)
    (setq w3m-display-inline-images t)))
