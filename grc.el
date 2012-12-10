;;; grc.el --- Google Reader Mode for Emacs
;;
;; Copyright (c) 2011 David Leatherman
;;
;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/grc
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the main entry function (grc) as well as various
;; shared functions

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

(require 'assoc)
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

(defgroup grc-faces
  nil "Group for grc related faces"
  :group 'grc)

(defface grc-read-face '((t (:foreground "dim gray")))
  "grc face for read items"
  :group 'grc-faces)

(defcustom grc-enable-hl-line t
  "Turn on hl-line-mode in the grc list buffer"
  :type  'boolean
  :group 'grc)

(defcustom grc-fetch-count 100
  "The count of items to fetch.  The larger the count the slower the request."
  :type 'integer
  :group 'grc)

(defcustom grc-instapaper-username ""
  ""
  :group 'grc
  :type 'string)

(defcustom grc-instapaper-password ""
  ""
  :group 'grc
  :type 'string)

(defvar grc-google-categories
  '(("fresh"                      . "Fresh")
    ("kept-unread"                . "Kept Unread")
    ("read"                       . "Read")
    ("reading-list"               . "Reading List")
    ("starred"                    . "Starred")
    ("tracking-body-link-used"    . "Tracking Body Link Used")
    ("tracking-emailed"           . "Tracking Email")
    ("tracking-item-link-used"    . "Tracking Item Link Used")
    ("tracking-kept-unread"       . "Tracking Kept Unread")
    ("tracking-mobile-read"       . "Tracking Mobile Read"))
  "list of the categories that google adds to entries")

(defvar grc-state-alist '("Kept Unread" "Read" "Reading List" "Starred"))
(defvar grc-current-state "reading-list")

(defvar grc-entry-cache nil)
(defvar grc-current-entry nil)

(defvar grc-html-entity-list
  '(("&amp;" "&")
    ("&apos;" "'")
    ("&gt;" ">")
    ("&lt;" "<")
    ("&quot;" "\"")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display functions
(defun grc-replace-string (from-string to-string)
  "Replaces all occurrences of from-string with to-string"
  (while (search-forward from-string nil t)
    (replace-match to-string nil t)))

(defun grc-replace-regexp (regexp to-string)
  "Replaces all regexp matches with to-string"
  (while (search-forward-regexp regexp nil t)
    (replace-match to-string nil t)))

(defun grc-convert-entities ()
  "Searches through the buffer replacing common HTML entities with their chars"
  (mapcar '(lambda (pair)
             (goto-char (point-min))
             (grc-replace-string (car pair) (cadr pair)))
          grc-html-entity-list))

(defun grc-trim-left-in-buffer ()
  "Removes all leading whitespace from all lines in the buffer"
  (goto-char (point-min))
  (while (not (eobp))
    (beginning-of-line)
    (delete-horizontal-space)
    (forward-line 1)))

(defun grc-normalize-newlines ()
  "Reduces multiple blank lines down to one"
  (goto-char (point-min))
  (grc-replace-regexp "^\n+" "\n")
  (when (and (> (point-max) (point-min))
             (equal "\n" (buffer-substring (point-min) (1+ (point-min)))))
    (goto-char (point-min))
    (delete-char 1)))

;; TODO: this is doing more than just stripping html, should it be
;; refactored to just strip html and move the entities, trimming, and
;; normalizing to some other function?
(defun grc-strip-html ()
  "Converts some HTML entities and removes HTML tags."
  (save-excursion
    (grc-convert-entities)
    (goto-char (point-min))
    (grc-replace-regexp "<.*?>" "")
    (grc-trim-left-in-buffer)
    (grc-normalize-newlines)))

(defun grc-strip-html-to-string (str)
  "Takes a string and returns it stripped of HTML"
  (with-temp-buffer
    (insert str)
    (grc-strip-html)
    (buffer-string)))

(defun grc-footnote-anchors (&optional use-annotations links)
  "Walks through a buffer of html and removes the anchor tags,
  replacing them with the body of the anchor followed by the url in
  brackets.  Alternatively, if use-annotations is true, it will put a
  number in place of the link and list the links at the bottom.

  links isn't meant to be passed in, it's used for recursive calls"
  (goto-char (point-min))
  (if (search-forward-regexp "<a" nil t)
      (let* ((p1 (point))
             (p2 (search-forward-regexp ">" nil t))
             (p3 (search-forward-regexp "</a>" nil t))
             (attrs (html2text-get-attr p1 p2))
             (href (html2text-attr-value attrs "href"))
             (href (substring href 1 (1- (length href))))
             (text (grc-strip-html-to-string
                    (buffer-substring-no-properties p2 (- p3 4)))))
        (if (and text (not (equal "" (grc-trim text))))
            (progn
              (delete-region (- p1 2) p3)
              (insert text)
              (when (not (string= text href))
                (insert (format " [%s]"
                                (if use-annotations
                                    (+ 1 (length links))
                                  href))))
              (grc-footnote-anchors use-annotations
                                    (append links (list href))))
          (grc-footnote-anchors use-annotations links)))
    (when use-annotations
      (goto-char (point-max))
      (insert "\n\nLinks:\n")
      (reduce (lambda (n l)
                (insert "[" (prin1-to-string n) "] " l "\n")
                (+ 1 n)) links :initial-value 1))))

(defun grc-prepare-title (title)
  (with-temp-buffer
    (insert title)
    (when (featurep 'w3m)
      (w3m-decode-entities))
    (goto-char (point-min))
    (html2text)
    (buffer-string)))

(defun grc-clean-buffer ()
  "Runs grc-clean-text over the entire buffer"
  (let ((cleaned (grc-clean-text (buffer-string))))
    (erase-buffer)
    (insert cleaned)))

(defun grc-button-browse-url (overlay)
  (browse-url-at-point))

(define-button-type 'grc-link-button
  'follow-link t
  'face 'link
  'action #'grc-button-browse-url)

(defun grc-buttonfiy-links ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward thing-at-point-url-regexp nil t)
      (make-button (match-beginning 0)
                   (match-end 0)
                   'url (buffer-substring-no-properties (match-beginning 0) (match-end 0))
                   'type 'grc-link-button))))

(defun grc-clean-text (text &optional skip-anchor-annotations)
  "Meant for entry text, will footnote links and strip HTML"
  (when text
    (with-temp-buffer
      (insert text)
      (when (featurep 'w3m)
        (w3m-decode-entities))
      (goto-char (point-min))
      (unless skip-anchor-annotations
        (grc-footnote-anchors grc-use-anchor-annotations)
        (grc-buttonfiy-links))

      (goto-char (point-min))
      (grc-replace-regexp "<br.*?>" "\n")
      (goto-char (point-min))
      (grc-replace-regexp "</.?p>" "\n")
      (goto-char (point-min))
      (grc-strip-html)
      (buffer-substring (point-min) (point-max)))))

(defun grc-prepare-text (text)
  "Meant for shorter strings (where link annotation isn't desired), strips HTML
  and decodes entities"
  (grc-clean-text text t))

(defun grc-truncate-text (text &optional max elide)
  "Will truncate text down to max or 20 characters.

  Optional elide will replace the last character with …"
  (if text
      (let* ((max (or max 20))
             (len (length text))
             (max (if (and elide (< max len))
                      (1- max)
                    max))
             (str (replace-regexp-in-string
                   "\\(\\W\\)*$"
                   ""
                   (substring text 0 (if (> max len) len max)))))
        (if (and (< max len) elide)
            (concat str "…")
          str))
    ""))

(defun grc-format-categories (entry)
  "Remove internal categories and convert the remaining to be human readable"
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

(defun grc-title-for-printing (entry)
  "Given an entry, extract a title"
  (let ((title (aget entry 'title t))
        (streamId (aget entry 'src-id))
        (summary (or (aget entry 'content t)
                     (aget entry 'summary t)))
        (case-fold-search t))
    (if title
        title
      (if (string-match "post$" streamId)
          (substring summary 0 (string-match "<br" summary))
        "No title provided."))))

(defun grc-keywords (entries)
  "Keywords determines what will be highlighted.  For now this is only the
  source of the entry."
  (mapcar (lambda (e) (aget e 'src-title t)) entries))

(defun grc-read-state (prompt)
  "Return state name read from minibuffer."
  (let ((grc-read-history '())
        (choices (sort (copy-list grc-state-alist) 'string<))
        (completing-read-fn (if (featurep 'ido)
                                'ido-completing-read
                              'completing-read)))
    (car (rassoc (apply completing-read-fn prompt choices
                        nil 'require-match nil grc-read-history)
                 grc-google-categories))))

;;;###autoload
(defun grc (&optional state)
  "Display or refresh the grc reading list.  Main entry function."
  (interactive "P")
  (setq grc-current-state (if (and state (interactive-p))
                              (grc-read-state "State: ")
                            grc-current-state))
  (if (get-buffer grc-list-buffer)
      (switch-to-buffer grc-list-buffer)
    (progn (grc-list-make-buffer "Fetching entries...")
           (switch-to-buffer grc-list-buffer)
           (grc-req-unread-entries
            '(lambda (response headers)
               (let ((response (grc-req-parse-response response)))
                 (grc-list-display response)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General view functions
(defun grc-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun grc-help ()
  "Show the help message for the grc view"
  (interactive)
  (let ((mode major-mode))
    (with-current-buffer (get-buffer-create (format "*%s help*" mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (documentation mode t))
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (view-buffer (current-buffer) 'kill-buffer-if-not-modified)))))

(defun grc-mark-read (entries)
  "Marks the entry as read on Google Reader"
  (grc-req-mark-read
   (mapcar (lambda (i) (cdr (assoc 'id i))) entries)
   (mapcar (lambda (i) (cdr (assoc 'src-id i))) entries)))

(defun grc-mark-kept (entries)
  "Marks the entry as kept unread on Google Reader"
  (grc-req-mark-kept-unread
   (mapcar (lambda (i) (cdr (assoc 'id i))) entries)
   (mapcar (lambda (i) (cdr (assoc 'src-id i))) entries)))

(defun grc-mark-starred (entries &optional remove-p)
  "Marks the entry as starred on Google Reader"
  (grc-req-mark-kept-unread
   (mapcar (lambda (i) (cdr (assoc 'id i))) entries)
   (mapcar (lambda (i) (cdr (assoc 'src-id i))) entries)
   remove-p))

(defun grc-view-external (entry)
  "Open the current rss entry in the default emacs browser"
  (interactive)
  (let ((link (cdr (assoc 'link entry))))
    (browse-url link)
    (grc-mark-read (list entry))))

(defun grc-send-to-instapaper (entries)
  (dolist (entry (grc-list entries))
    (grapnel-retrieve-url "https://www.instapaper.com/api/add"
                          '((complete . (message "Sent to Instapaper")))
                          `(("username" . ,grc-instapaper-username)
                            ("password" . ,grc-instapaper-password)
                            ("url"      . ,(aget entry 'link t))))))

(provide 'grc)
;;; grc.el ends here
