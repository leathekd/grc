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
;; TODO- add light colors
(defface grc-show-header-face
  '((t :foreground "green" :bold t))
  "Face used for displaying header names."
  :group 'grc-faces)

(defface grc-show-context-face
  '((t (:foreground "DeepSkyBlue1" :bold t)))
  "Face used for displaying next/previous story headers."
  :group 'grc-faces)

(defvar grc-show-buffer "*grc show*" "Name of the buffer for the grc show view")

(defvar grc-show-summary-renderer 'grc-show-basic-renderer
  "The function to call to render the summary (text) of the entry. Is called
with the show buffer active, the summary already inserted, and the view narrowed
to just the summary.")

(defvar grc-show-external-link-viewer 'grc-basic-external-view-this-url
  "The function that is called to view the current line's entry in an external
browser")

(defvar grc-show-next-anchor-fn 'grc-basic-next-anchor
  "The function called to move the cursor to the next anchor")

(defvar grc-show-previous-anchor-fn 'grc-basic-previous-anchor
  "The function called to move the cursor to the previous anchor")

(defun grc-propertize-keyword (keyword)
  "Takes a string, KEYWORD, and returns it propertized with a generated
foreground color"
  (propertize keyword 'face (grc-highlight-make-face keyword)))

(defun grc-show-render-header (entry)
  "Render the header with colored labels and some info about next and previous
stories"
  (let ((next-entry (with-current-buffer grc-list-buffer
                      (save-excursion
                        (forward-line)
                        (tabulated-list-get-id))))
        (prev-entry (with-current-buffer grc-list-buffer
                      (save-excursion
                        (goto-char (line-beginning-position))
                        (unless (bobp)
                          (forward-line -1)
                          (tabulated-list-get-id))))))
    (mapcar (lambda (lst)
              (insert (format "%s: %s\n"
                              (car lst)
                              (grc-prepare-text (cadr lst)))))
            `((,(propertize "Title" 'face 'grc-show-header-face)
               ,(cdr (assoc 'title entry)))
              (,(propertize "Date" 'face 'grc-show-header-face)
               ,(format-time-string
                 "%a %m/%d %l:%M %p"
                 (seconds-to-time (cdr (assoc 'date entry)))))
              (,(propertize "Source" 'face 'grc-show-header-face)
               ,(grc-propertize-keyword (cdr (assoc 'src-title entry))))
              (,(propertize "Next Story" 'face 'grc-show-context-face)
               ,(if next-entry
                    (concat (cdr (assoc 'title next-entry))
                            " [" (grc-propertize-keyword
                                  (cdr (assoc 'src-title next-entry))) "]")
                  "None"))
              (,(propertize "Previous Story" 'face 'grc-show-context-face)
               ,(if prev-entry
                    (concat (cdr (assoc 'title prev-entry))
                            " [" (grc-propertize-keyword
                                  (cdr (assoc 'src-title prev-entry))) "]")
                  "None"))))
    (insert "\n")))

(defun grc-show-render-summary ()
  (funcall grc-show-summary-renderer))

(defun grc-show-entry (entry)
  "Print the given entry in the grc-show-buffer"
  (let ((buffer (get-buffer-create grc-show-buffer)))
    (with-current-buffer buffer
      (grc-show-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (grc-show-render-header entry)
        (save-restriction
          (narrow-to-region (point) (point-max))
          (insert (or (cdr (assoc 'content entry))
                      (cdr (assoc 'summary entry))
                      "No summary provided."))
          (grc-show-render-summary))
        (put-text-property (point-min) (point-max) 'grc-current-entry entry)
        (goto-char (point-min))
        (switch-to-buffer buffer))
      (run-hooks 'grc-show-entry-hook))))

(defun grc-show-current-entry ()
  (get-text-property (point) 'grc-current-entry))

(defun grc-show-help ()
  "Show the help message for the grc show view"
  (interactive)
  (grc-help))

(defun grc-show-mark-kept-unread ()
  "Mark the current entry as Keep Unread."
  (interactive)
  (grc-mark-kept-unread (list (grc-show-current-entry))))

(defun grc-show-mark-read ()
  "Mark the current entry as Read"
  (interactive)
  (grc-mark-read (list (grc-show-current-entry))))

(defun grc-show-mark-starred (remove)
  "Star the current entry."
  (interactive)
  (grc-mark-starred (list (grc-show-current-entry))))

(defun grc-show-kill-this-buffer ()
  "Close the show buffer and return to the list buffer."
  (interactive)
  (when (get-buffer grc-list-buffer)
    (switch-to-buffer grc-list-buffer)
    (kill-buffer grc-show-buffer)))

(defun grc-show-next-entry ()
  "View the next entry."
  (interactive)
  (with-current-buffer grc-list-buffer
    (forward-line)
    (if (eobp)
        (error "No more entries")
      (grc-list-show-entry))))

(defun grc-show-previous-entry ()
  "View the previous entry."
  (interactive)
  (let ((entry (with-current-buffer grc-list-buffer
                 (forward-line -1)
                 (tabulated-list-get-id))))
    (if entry
        (grc-show-entry entry)
      (error "No previous entries"))))

(defun grc-show-view-external ()
  "Load the current entry in an external browser."
  (interactive)
  (grc-view-external (grc-show-current-entry))
  (run-hooks 'grc-show-external-hook))

(defun grc-show-advance-or-show-next-entry ()
  "Will move down 25 lines or load the next entry once at the bottom."
  (interactive)
  (if (eobp)
      (grc-show-next-entry)
    (let ((scroll-error-top-bottom t))
      (scroll-up-command 25)
      (when (eobp)
        (grc-show-next-entry)))))

(defun grc-show-next-anchor ()
  "Move the point to the next anchor."
  (interactive)
  (funcall grc-show-next-anchor-fn))

(defun grc-show-previous-anchor ()
  "Move the point to the previous anchor."
  (interactive)
  (funcall grc-show-previous-anchor-fn))

(defvar grc-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " "               'grc-show-advance-or-show-next-entry)
    (define-key map "?"               'grc-show-help)
    (define-key map "q"               'grc-show-kill-this-buffer)
    (define-key map "k"               'grc-show-mark-kept-unread)
    (define-key map "r"               'grc-show-mark-read)
    (define-key map "*"               'grc-show-mark-starred)
    (define-key map "n"               'grc-show-next-entry)
    (define-key map "p"               'grc-show-previous-entry)
    (define-key map "v"               'grc-show-view-external)
    (define-key map (kbd "TAB")       'grc-show-next-anchor)
    (define-key map (kbd "<backtab>") 'grc-show-previous-anchor)
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
  *      Star the current entry.  Use the prefix operator to un-star.
  r      Mark the current entry as Read.
  k      Mark the current entry as Keep Unread.
  q      Close the show buffer and return to the list buffer.
  ?      Show the help message for the grc show view
  SPC    Will move down 25 lines or load the next entry once at the bottom."
  (interactive)
  (kill-all-local-variables)
  (use-local-map grc-show-mode-map)
  (setq major-mode 'grc-show-mode
        mode-name "grc-show"
        truncate-lines t
        buffer-read-only t))

(provide 'grc-show)
;;; grc-show.el ends here
