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

(defun grc-list-header-line ()
  (setq header-line-format
        (format "Google Reader Client for %s  Viewing: %s  Sort: %s %s"
                greader-user-email (car (rassoc grc-current-state
                                                grc-state-alist))
                (capitalize (symbol-name (or grc-current-sort
                                             grc-default-sort-column)))
                (if grc-current-sort-reversed
                    "Descending" "Ascending"))))

(defun grc-list-refresh ()
  (with-current-buffer grc-list-buffer
    (grc-list-header-line)
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

(defun grc-list-mark-fn (tag)
  `(lambda (&optional remove)
     (funcall (grc-mark-fn ,tag) (grc-list-get-current-entry) remove)
     (grc-list-next-entry)
     (grc-list-refresh)))

(defun grc-list-mark-read ()
  (interactive)
  (funcall (grc-list-mark-fn "read")))

(defun grc-list-mark-read-and-remove ()
  (interactive)
  (grc-mark-read-and-remove (grc-list-get-current-entry))
  (grc-list-refresh))

(defun grc-list-mark-kept-unread (remove)
  (interactive "P")
  (funcall (grc-list-mark-fn "kept-unread") remove))

(defun grc-list-mark-starred (remove)
  (interactive "P")
  (funcall (grc-list-mark-fn "starred") remove))

(defun grc-list-mark-all-read (feed)
  (interactive "P")
  (let* ((feed-name (when (and feed (interactive-p))
                      (ido-completing-read "Feed: "
                                           (mapcar (lambda (e) (aget e
                                                                'source t))
                                                   grc-entry-cache)
                                           nil t)))
         (items (remove-if-not (lambda (e) (string= feed-name
                                               (aget e 'source t)))
                               grc-entry-cache))
         (src (aget (first items) 'feed t)))

    (grc-req-ensure-authenticated)
    (grc-req-post-request "http://www.google.com/reader/api/0/mark-all-as-read"
                          (format "s=%s&ts=%s&T=%s"
                                  (or src "user/-/state/com.google/reading-list")
                                  (floor (* 1000000 (float-time)))
                                  (g-auth-token greader-auth-handle)))
    (mapcar (lambda (e) (grc-add-category e "read"))
            (or items grc-entry-cache)))
  (grc-list-refresh))

(defun grc-list-show-entry ()
  (interactive)
  (grc-show-entry (grc-list-get-current-entry)))

(defun grc-list-sort ()
  (interactive)
  (let ((next-sort (or (cadr (member grc-current-sort grc-sort-columns))
                       grc-default-sort-column)))
    (setq grc-current-sort-reversed (not grc-current-sort-reversed))
    (when (not grc-current-sort-reversed)
      (setq grc-current-sort next-sort))
    (grc-sort-by grc-current-sort grc-entry-cache grc-current-sort-reversed)
    (grc-list-refresh)))

(defvar grc-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q"         'grc-kill-this-buffer)
    (define-key map "?"         'grc-list-help)
    (define-key map "k"         'grc-list-mark-kept-unread)
    (define-key map "r"         'grc-list-mark-read)
    (define-key map "x"         'grc-list-mark-read-and-remove)
    (define-key map "s"         'grc-list-mark-starred)
    (define-key map "n"         'grc-list-next-entry)
    (define-key map "p"         'grc-list-previous-entry)
    (define-key map " "         'grc-list-show-entry)
    (define-key map (kbd "RET") 'grc-list-show-entry)
    (define-key map "o"         'grc-list-sort)
    (define-key map "v"         'grc-list-view-external)
    (define-key map "g"         'grc-reading-list)
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

(defun grc-list-print-entry (entry)
  "Takes an entry and formats it into the line that'll appear on the list view"
  (let* ((source (grc-prepare-text (aget entry 'source t)))
         (cats (grc-format-categories entry))
         (date (seconds-to-time (aget entry 'date t)))
         (one-week (- (float-time (current-time))
                      (* 60 60 24 7)))
         (static-width (+ 14 2 23 2 2 (length cats) 1))
         (title-width (- (window-width) static-width))
         (title (grc-prepare-text (grc-title-for-printing entry title-width))))
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

(defun grc-list-display (entries)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (mapcar 'grc-list-print-entry
            (grc-sort-by 'date entries t))
    (grc-highlight-keywords (grc-keywords entries))))

(provide 'grc-list)