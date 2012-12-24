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
(require 'cl)
(require 'dash)
(require 's)
(require 'html2text)

(require 'grc-lib)
(require 'grc-auth)
(require 'grc-req)
(require 'grc-parse)
(require 'grc-highlight)
(require 'grc-list)
(require 'grc-show)
(require 'grc-basic)

(defgroup grc nil "Google Reader Client for Emacs")

(defgroup grc-faces
  nil "Group for grc related faces"
  :group 'grc)

(defface grc-read-face '((t (:foreground "dim gray")))
  "grc face for read items"
  :group 'grc-faces)

(defcustom grc-fetch-count 50
  "The count of items to fetch.  The larger the count the slower the request."
  :type 'integer
  :group 'grc)

(defcustom grc-max-fetch-count 250
  "The maximum number of items to fetch."
  :type 'integer
  :group 'grc)

(defvar grc-state-alist
  '(("Unread" . ((name . "Unread")
                 (id . "reading-list")
                 (fn . grc-req-unread-entries)))
    ("Starred" . ((name . "Starred")
                  (id . "starred")))
    ("Kept Unread" . ((name . "Kept Unread")
                      (id . "kept-unread")))
    ("Read" . ((name . "Read")
               (id . "read")))))

(defvar grc-current-state (cdr (assoc "Unread" grc-state-alist)))
(defvar grc-prepare-text-fn 'grc-basic-prepare-text)
(defvar grc-read-history nil)

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

(defun grc-prepare-title (title)
  (replace-regexp-in-string
   "[\n]" " " (funcall grc-prepare-text-fn title)))

(defun grc-prepare-text (text)
  (funcall grc-prepare-text-fn text))

(defun grc-read-state ()
  "Return state name read from minibuffer."
  (let ((grc-read-history '())
        (choices (sort (mapcar
                        (lambda (state-alist) (car state-alist))
                        grc-state-alist)
                       'string<)))
    (cdr (assoc (completing-read "State: " choices
                                 nil 'require-match nil grc-read-history)
                grc-state-alist))))

(defun grc-fetch-entries (&optional params)
  (let ((fn (or (cdr (assoc 'fn grc-current-state)) 'grc-req-fetch-entries)))
    (grc-list-make-buffer "Fetching entries...")
    (switch-to-buffer (get-buffer-create grc-list-buffer))
    (funcall fn (cdr (assoc 'id grc-current-state))
             '(lambda (response headers)
                (let ((response (grc-req-parse-response response)))
                  (grc-list-display response))))))

;;;###autoload
(defun grc (&optional prefixed-p)
  "Display or refresh the grc reading list.  Main entry function."
  (interactive "P")
  (setq grc-current-state (if prefixed-p
                              (grc-read-state)
                            grc-current-state))
  (if (and (get-buffer grc-list-buffer)
           (not prefixed-p))
      (switch-to-buffer grc-list-buffer)
    (grc-fetch-entries)))

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

(provide 'grc)
;;; grc.el ends here
