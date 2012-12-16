;;; grc-w3m.el --- Google Reader Mode for Emacs
;;
;; Copyright (c) 2011 David Leatherman
;;
;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/grc
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains several functions that handle rendering using
;; w3m and related libraries

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
(defun grc-w3m-prepare-text (text)
  "Prepares text for display by decoding entities and stripping HTML. Takes
TEXT as an arg and returns the processed text."
  (with-temp-buffer
    (insert text)
    (w3m-decode-entities)
    (goto-char (point-min))
    (html2text)
    (buffer-string)))

(defun grc-show-w3m-header-renderer (entry)
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

(defun grc-show-w3m-renderer ()
  (let ((w3m-display-inline-images t)
        (w3m-fill-column 72))
    (w3m-region (point-min) (point-max))))

(provide 'grc-w3m)
;;; grc-w3m.el ends here
