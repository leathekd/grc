;;; grc-list.el --- Google Reader Mode for Emacs
;;
;; Copyright (c) 2011 David Leatherman
;;
;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/google-reader-client
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the code for showing the list of Reader entries

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(defvar grc-sort-columns '(crawl-date source))
(defvar grc-current-sort nil)
(defvar grc-current-sort-reversed t)
(defcustom grc-default-sort-column 'crawl-date
  "Default column with which to sort the list view"
  :group 'grc
  :type '(choice (symbol :tag "Date" 'crawl-date)
                 (symbol :tag "Source" 'source)))

(defvar grc-list-buffer "*grc list*" "Name of the buffer for the grc list view")

(defun grc-list-print-entry (entry)
  "Takes an entry and formats it into the line that'll appear on the list view"
  (let* ((source (grc-prepare-text (aget entry 'source t)))
         (cats (grc-format-categories entry))
         (date (seconds-to-time (aget entry 'crawl-date t)))
         (one-week (- (float-time (current-time))
                      (* 60 60 24 7)))
         (static-width (+ 14 2 23 2 2
                          (length cats)
                          (if (aget entry 'comments t) 4 0)
                          1))
         (title-width (- (window-width) static-width))
         (title (grc-prepare-text (grc-title-for-printing entry))))
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
    (when (aget entry 'comments t)
      (insert (format " [C]")))
    (insert "\n")))

(defun grc-list-display (entries)
  (with-current-buffer (get-buffer-create grc-list-buffer)
    (let ((inhibit-read-only t))
      (grc-list-mode)
      (erase-buffer)
      (setq grc-entry-cache entries)
      (grc-list-header-line)
      (mapcar 'grc-list-print-entry grc-entry-cache)

      ;; remove final trailing newline
      (goto-char (point-max))
      (delete-horizontal-space)
      (when (> 1 (point))
        (delete-backward-char 1))

      (grc-highlight-keywords (grc-keywords entries))
      (goto-char (point-min)))))

(defun grc-list-incremental-display ()
  (setq grc-entry-cache (append (grc-req-incremental-fetch) grc-entry-cache))
  (grc-list-refresh))

(defun grc-list-get-current-entry ()
  "utility function to get the entry from the current line in list view"
  (let ((entry (nth (- (line-number-at-pos) 1) grc-entry-cache)))
    (if entry
        entry
      (error "No entry at point."))))

(defun grc-list-next-entry ()
  "Move the point to the next entry."
  (interactive)
  (next-line)
  (move-beginning-of-line nil))

(defun grc-list-previous-entry ()
  "Move the point to the previous entry."
  (interactive)
  (previous-line)
  (move-beginning-of-line nil))

(defun grc-list-header-line ()
  (setq header-line-format
        (format "Google Reader Client -- Viewing: %s (%s) %s  Sort: %s %s"
                (cdr (assoc grc-current-state
                            grc-google-categories))
                (length grc-entry-cache)
                (if (grc-req-unread-comment-count) "[C]" "")
                (capitalize (symbol-name (or grc-current-sort
                                             grc-default-sort-column)))
                (if grc-current-sort-reversed
                    "Descending" "Ascending"))))

(defun grc-list-refresh ()
  (with-current-buffer (get-buffer-create grc-list-buffer)
    (let ((line (1- (line-number-at-pos))))
      (grc-list-display grc-entry-cache)
      (goto-char (point-min))
      (forward-line line))))

(defun grc-list-help ()
  "Show the help message for the grc list view"
  (interactive)
  (grc-help))

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

(defun grc-list-mark-read (remove)
  "Mark the current entry as Read.  Use the prefix operator to unmark."
  (interactive "P")
  (funcall (grc-list-mark-fn "read") remove))

(defun grc-list-mark-read-and-remove ()
  "Mark the current entry as Read and remove it immediately from the list."
  (interactive)
  (funcall (grc-mark-fn "read") (grc-list-get-current-entry))
  (setq grc-entry-cache (delete (grc-list-get-current-entry) grc-entry-cache))
  (grc-list-refresh))

(defun grc-list-mark-kept-unread (remove)
  "Mark the current entry as Kept Unread.  Use the prefix operator to unmark."
  (interactive "P")
  (funcall (grc-list-mark-fn "kept-unread") remove))

(defun grc-list-mark-starred (remove)
  "Star the current entry.  Use the prefix operator to un-star."
  (interactive "P")
  (funcall (grc-list-mark-fn "starred") remove))

(defun grc-list-share (remove)
  "Share the current entry.  Use the prefix operator to un-share."
  (interactive "P")
  (funcall (grc-list-mark-fn "broadcast") remove))

(defun grc-list-mark-all-read (feed)
  "Mark all as Read."
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
    (grc-req-mark-all-read src)
    (mapcar (lambda (e) (grc-add-category e "read"))
            (or items grc-entry-cache)))
  (grc-list-display grc-entry-cache)
  (goto-char (point-min))
  (grc-list-refresh))

(defun grc-list-show-entry ()
  "View the current entry."
  (interactive)
  (grc-show-entry (grc-list-get-current-entry)))

(defun grc-list-sort ()
  "Cycle through sort states.

  The defined states are:

  Date Asc
  Date Desc
  Source Asc
  Source Desc"
  (interactive)
  (let ((next-sort (or (cadr (member grc-current-sort grc-sort-columns))
                       grc-default-sort-column)))
    (setq grc-current-sort-reversed (not grc-current-sort-reversed))
    (when (not grc-current-sort-reversed)
      (setq grc-current-sort next-sort))
    (message "%s %s" grc-current-sort grc-current-sort-reversed)
    (setq grc-entry-cache (grc-sort-by grc-current-sort grc-entry-cache
                                       grc-current-sort-reversed 'title))

    (grc-list-refresh)))

(defvar grc-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q"         'grc-kill-this-buffer)
    (define-key map "?"         'grc-list-help)
    (define-key map "k"         'grc-list-mark-kept-unread)
    (define-key map "r"         'grc-list-mark-read)
    (define-key map "x"         'grc-list-mark-read-and-remove)
    (define-key map "*"         'grc-list-mark-starred)
    (define-key map "n"         'grc-list-next-entry)
    (define-key map "p"         'grc-list-previous-entry)
    (define-key map "!"         'grc-list-share)
    (define-key map " "         'grc-list-show-entry)
    (define-key map (kbd "RET") 'grc-list-show-entry)
    (define-key map "o"         'grc-list-sort)
    (define-key map "v"         'grc-list-view-external)
    (define-key map "g"         'grc)
    map)
  "Keymap for \"grc list\" buffers.")
(fset 'grc-list-mode-map grc-list-mode-map)

(defun grc-list-mode ()
  "Major mode for viewing feeds with grc

  This buffer contains the results of the \"grc\" command
  for displaying unread feeds from Google Reader.

  All currently available key bindings:

  g    Display or refresh the grc reading list.
  v    Open the current rss entry in the default emacs browser
  o    Cycle through sort states.
  RET  View the current entry.
  SPC  View the current entry.
  p    Move the point to the previous entry.
  n    Move the point to the next entry.
  *    Star the current entry.  Use the prefix operator to un-star.
  !    Share the current entry.  Use the prefix operator to un-share.
  x    Mark the current entry as Read and remove it immediately from the list.
  r    Mark the current entry as Read.  Use the prefix operator to unmark.
  k    Mark the current entry as Kept Unread.  Use the prefix operator to unmark
  ?    Show the help message for the grc list screen
  q    Kill the current buffer."
  (interactive)
  (kill-all-local-variables)
  (use-local-map grc-list-mode-map)
  (setq major-mode 'grc-list-mode
        mode-name "grc-list")
  (setq buffer-read-only t)
  (hl-line-mode grc-enable-hl-line))

(provide 'grc-list)
;;; grc-list.el ends here
