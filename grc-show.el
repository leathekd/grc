;;; grc-show.el --- Google Reader Mode for Emacs
;;
;; Copyright (c) 2011 David Leatherman
;;
;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/grc
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the code for showing an individual Reader entry

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
  "Show the help message for the grc show view"
  (interactive)
  (grc-help))

(defun grc-show-mark-kept-unread (remove)
  "Mark the current entry as Keep Unread."
  (interactive "P")
  (funcall (grc-mark-fn "kept-unread") grc-current-entry remove))

(defun grc-show-mark-starred (remove)
  "Star the current entry."
  (interactive "P")
  (funcall (grc-mark-fn "starred") grc-current-entry remove))

(defun grc-show-kill-this-buffer ()
  "Close the show buffer and return to the list buffer."
  (interactive)
  (when (get-buffer grc-list-buffer)
    (switch-to-buffer (get-buffer grc-list-buffer))
    (kill-buffer grc-show-buffer)))

(defun grc-show-next-entry ()
  "View the next entry."
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
  "View the previous entry."
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
  "Load the current entry in an external browser."
  (interactive)
  (grc-view-external grc-current-entry))

(defun grc-show-advance-or-show-next-entry ()
  "Will move down 25 lines or load the next entry once at the bottom."
  (interactive)
  (if (eobp)
      (grc-show-next-entry)
    (let ((scroll-error-top-bottom t))
      (scroll-up-command 25))))

(defun grc-show-external-view-url ()
  "Load the URL/anchor under point in an external browser."
  (interactive)
  (when (featurep 'w3m)
    (w3m-external-view-this-url)))

(defun grc-show-next-anchor ()
  "Move the point to the next anchor."
  (interactive)
  (when (featurep 'w3m)
    (w3m-next-anchor)))

(defun grc-show-previous-anchor ()
  "Move the point to the previous anchor."
  (interactive)
  (when (featurep 'w3m)
    (w3m-previous-anchor)))

(defvar grc-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " "           'grc-show-advance-or-show-next-entry)
    (define-key map "?"           'grc-show-help)
    (define-key map "q"           'grc-show-kill-this-buffer)
    (define-key map "k"           'grc-show-mark-kept-unread)
    (define-key map "*"           'grc-show-mark-starred)
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

  All currently available key bindings:

  S-TAB  Move the point to the previous anchor.
  TAB    Move the point to the next anchor.
  RET    Load the URL/anchor under point in an external browser.
  v      Load the current entry in an external browser.
  p      View the previous entry.
  n      View the next entry.
  s      Star the current entry.
  k      Mark the current entry as Keep Unread.
  q      Close the show buffer and return to the list buffer.
  ?      Show the help message for the grc show view
  SPC    Will move down 25 lines or load the next entry once at the bottom."
  (interactive)
  (kill-all-local-variables)
  (use-local-map grc-show-mode-map)
  (setq major-mode 'grc-show-mode
        mode-name "grc-show")
  (setq buffer-read-only t))

(provide 'grc-show)
;;; grc-show.el ends here
