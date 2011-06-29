;; TODO: Refactor: the code has gotten a bit muddled and hard to follow
;; TODO: requests need to be much more async.  It's unacceptable to
;;       freeze emacs when fetching feeds -
;;       start-process-shell-command and sentinels?
;; TODO: investigate other ways of refreshing view (delete lines, etc)
;;       for refreshing a line - modify entry, delete line, redraw
;; TODO: mark unread, star, unstar, share(?), email(?)
;;       (greader-star)?
;; TODO: investigate what it would take to remove reliance on g-client
;; TODO: streamline processing- print whole buffer and colorize it
;;       once, strip html once, etc

(require 'cl)
(require 'html2text)
(require 'grc-highlight)

(defvar grc-entry-cache nil)
(defvar grc-current-entry nil)

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
(defun grc-remote-entries ()
  "Currently this overrides and hooks into greader.el to get the job done."
  (let ((g-atom-view-xsl nil)
        (g-html-handler `grc-parse-response)
        (greader-state-url-pattern (concat greader-state-url-pattern
                                           "&xt=user/-/state/com.google/read"))
        (greader-number-of-articles 100))
    (greader-reading-list)))

(defun grc-send-request (request)
  (declare (special g-curl-program g-curl-common-options
                    greader-auth-handle))
  (g-auth-ensure-token greader-auth-handle)
  (g-using-scratch
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

;;TODO: figure out why unicode works except in the g-using-scratch buffer...
(defun grc-prepare-text (text &optional keywords)
  (when text
    (g-using-scratch
     (insert text)

     (when keywords
       (grc-highlight-keywords keywords))

     ;; There must be a better way...
     (html2text-replace-string "’" "'" (point-min) (point-max))
     (html2text-replace-string "–" "--" (point-min) (point-max))
     (html2text-replace-string "—" "--" (point-min) (point-max))
     (html2text)
     (buffer-substring (point-min) (point-max)))))

(defun grc-extract-categories (entry filter-string)
  (mapcar (lambda (e) (xml-get-attribute e 'label))
          (remove-if-not (lambda (e) (string-match filter-string
                                              (xml-get-attribute e 'term)))
                         (xml-get-children entry 'category))))

(defun grc-process-entry (entry)
  `((id         . ,(grc-xml-get-child entry 'id))
    (title      . ,(grc-xml-get-child entry 'title))
    (date       . ,(grc-xml-get-child entry 'published))
    (link       . ,(xml-get-attribute (assq 'link entry) 'href))
    (source     . ,(grc-xml-get-child (first (xml-get-children entry 'source))
                                      'title))
    (feed       . ,(xml-get-attribute (assq 'source entry) 'gr:stream-id))
    (summary    . ,(grc-xml-get-child entry 'summary))
    (content    . ,(grc-xml-get-child entry 'content))
    (label      . ,(grc-extract-categories entry "label"))
    (categories . ,(grc-extract-categories entry "state"))))

(defun grc-parse-response (buffer)
  (let* ((root (car (xml-parse-region (point-min) (point-max))))
         (xml-entries (xml-get-children root 'entry))
         (entries (grc-sort-by 'date (mapcar 'grc-process-entry xml-entries))))
    (setq grc-xml-entries xml-entries)
    entries))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display functions
(defun grc-truncate-text (text &optional max elide)
  (if text
      (let* ((max (or max 20))
             (len (length text))
             (str (replace-regexp-in-string
                   "\\(\\W\\)*$"
                   ""
                   (substring text 0 (if (> max len) len max)))))
        (if (and (< max len) elide)
            (concat str "...")
          str))
    ""))

(defun grc-transform-category (category)
  (let ((cat-names '(("read" . "Read")
                     ("broadcast" . "Shared")
                     ("kept-unread" . "Kept Unread")
                     ("starred" . "Starred"))))
    (or (aget cat-names category t) category)))

(defun grc-format-categories (entry)
  (let* ((labelz (aget entry 'label t))
         (categories (aget entry 'categories t))
         (cats (intersection categories
                             '("read" "broadcast"
                               "kept-unread" "starred")
                             :test 'string=)))
    (if cats
        (concat "("
                (when labelz (mapconcat 'identity labelz " "))
                (when labelz " ")
                (mapconcat 'grc-transform-category cats " ")
                ")")
      (concat "("
              (when labelz (mapconcat 'identity labelz " "))
              (when labelz " ")
              "Unread)"))))

(defun grc-print-entry (entry)
  (let ((source (grc-truncate-text (aget entry 'source t) 22 t))
        (title (aget entry 'title t))
        (cats (grc-format-categories entry)))
    (insert
     (format "%-12s   %-25s   %s%s\n"
             (format-time-string "%a %l:%M %p"
                                 (date-to-time (aget entry 'date t)))
             source
             title
             (if (or (< 0 (length (aget entry 'categories)))
                     (< 0 (length (aget entry 'labels))))
                 (format " %s" cats)
               "")))))

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
  (let ((sorted (sort (copy-alist entries)
                      (lambda (a b)
                        (string<
                         (aget a field)
                         (aget b field))))))
    (setq grc-entry-cache sorted)
    sorted))

(defun grc-display-list (entries)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (mapcar 'grc-print-entry entries)
    (let ((keywords
           (delete-dups
            (append (grc-flatten (mapcar (lambda (e) (aget e 'categories t))
                                         entries))
                    (grc-flatten (mapcar (lambda (e) (aget e 'label t))
                                         entries))
                    (mapcar (lambda (e) (grc-truncate-text
                                    (aget e 'source) 22 t)) entries)))))
      (grc-highlight-keywords keywords))))

;; Main entry function
(defun grc-reading-list ()
  (interactive)
  (greader-re-authenticate)
  (let ((buffer (get-buffer-create "*grc list*")))
    (with-current-buffer buffer
      (grc-list-mode)
      (grc-display-list (grc-remote-entries))
      (switch-to-buffer buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General view functions
(defun grc-get-current-item ()
  "utility function to get the item from the current line in list view"
  (with-current-buffer (get-buffer "*grc list*")
    (nth (- (line-number-at-pos) 1) grc-entry-cache)))

(defun grc-next-item ()
  (interactive)
  (next-line)
  (move-beginning-of-line nil))

(defun grc-previous-item ()
  (interactive)
  (previous-line)
  (move-beginning-of-line nil))

(defun grc-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun grc-show-item (entry)
  ;; save entry as grc-current-entry
  (setq grc-current-entry entry)
  (let ((buffer (get-buffer-create "*grc show*")))
    (with-current-buffer buffer
      (grc-view-mode)
      (let ((inhibit-read-only t)
            (summary (or (aget entry 'content t)
                         (aget entry 'summary t)
                         "No summary provided.")))
        (erase-buffer)
        (insert "Title: "  (aget entry 'title) "<br/>")
        (insert "Link: "   (aget entry 'link) "<br/>")
        (insert "Date: "   (aget entry 'date) "<br/>")
        (insert "Source: " (aget entry 'source) "<br/>")
        (insert "<br/>" summary)
        (if (featurep 'w3m)
            (w3m-buffer)
          (html2text))))
    (switch-to-buffer buffer)))

(defun grc-mark-read (entry)
  (condition-case nil
      (progn
        (grc-send-request (grc-mark-read-request entry))
        (let ((mem (member entry grc-entry-cache))
              (new-entry (aput 'entry 'categories
                               (cons "read" (aget entry 'categories t)))))
          (setcar mem new-entry)
          new-entry))
    (error "There was a problem marking the item as read")))

(defun grc-mark-read-and-remove (entry)
  (delete (grc-mark-read entry) grc-entry-cache))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List view functions
(defun grc-list-next-item ()
  (interactive)
  (with-current-buffer (get-buffer "*grc list*")
    (grc-next-item)))

(defun grc-list-previous-item ()
  (interactive)
  (with-current-buffer (get-buffer "*grc list*")
    (grc-previous-item)))

(defun grc-list-refresh ()
  (with-current-buffer (get-buffer "*grc list*")
    (let ((line (line-number-at-pos)))
      (grc-display-list grc-entry-cache)
      (goto-line line)
      (beginning-of-line))))

(defun grc-list-help ()
  ;;TODO
  (interactive)
  )

(defun grc-list-view-external ()
  "Open the current rss entry in the default emacs browser"
  (interactive)
  (let* ((entry (grc-get-current-item))
         (link (aget entry 'link t)))
    (if link
        (progn
          (browse-url link)
          (grc-mark-read entry)
          (grc-list-refresh))
      (message "Unable to view this item"))))

(defun grc-list-mark-read ()
  (interactive)
  (grc-mark-read (grc-get-current-item))
  (grc-list-refresh)
  (grc-list-next-item))

(defun grc-list-mark-read-and-remove ()
  (interactive)
  (grc-mark-read-and-remove (grc-get-current-item))
  (grc-list-refresh))

(defun grc-advance-or-show-next-item ()
  ;;TODO - handle when we're out of items
  (interactive)
  ;; check to see if we're on the list page
  (condition-case nil
      (scroll-up-command)
    (progn
      (grc-list-next-item)
      (grc-show-item))))

(defun grc-list-show-item ()
  (interactive)
  (let ((entry (grc-get-current-item)))
    (grc-show-item entry)
    (grc-mark-read entry))
  (grc-list-refresh))

(defvar grc-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'grc-list-help)
    (define-key map "q" 'grc-kill-this-buffer)
    (define-key map "v" 'grc-list-view-external)
    (define-key map "r" 'grc-list-mark-read)
    (define-key map "x" 'grc-list-mark-read-and-remove)
    (define-key map "n" 'grc-list-next-item)
    (define-key map "p" 'grc-list-previous-item)
    (define-key map " " 'grc-advance-or-show-next-item)
    (define-key map (kbd "RET") 'grc-list-show-item)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View mode functions
(defvar grc-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'grc-help)
    (define-key map "q" 'grc-kill-this-buffer)
    (define-key map "v" 'grc-view-external)
    (define-key map "n" 'grc-next-item)
    (define-key map "p" 'grc-previous-item)
    (define-key map " " 'grc-advance-or-show-next-item)
    (define-key map (kbd "RET") 'grc-show-item)
    map)
  "Keymap for \"grc view\" buffers.")
(fset 'grc-view-mode-map grc-view-mode-map)

(defun grc-view-mode ()
  "Major mode for viewing a feed entry in grc

\\{grc-view-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map grc-view-mode-map)
  (setq major-mode 'grc-view-mode
        mode-name "grc-view")
  (setq buffer-read-only t)
  (when (featurep 'w3m)
    (setq w3m-display-inline-images t)))
