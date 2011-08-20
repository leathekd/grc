;;; grc-parse.el --- Google Reader Mode for Emacs
;;
;; Copyright (c) 2011 David Leatherman
;;
;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/google-reader-client
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the code for parsing the response from Google
;; into the format used internally by grc

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
(defun grc-parse-get-categories (json-entry)
  (remove-if 'null
             (mapcar (lambda (c)
                       (let ((label-idx (string-match "/label/" c))
                             (state-idx (string-match "/state/com.google/" c)))
                         (cond
                          (state-idx (substring c (+ state-idx 18)))
                          (label-idx (substring c (+ label-idx 7))))))
                     (aget json-entry 'categories))))

(defun grc-parse-process-entry (json-entry)
  `((id         . ,(aget json-entry 'id))
    (date       . ,(aget json-entry 'published))
    (crawl-date . ,(string-to-int (substring
                                   (aget json-entry 'crawlTimeMsec)
                                   0 -3)))
    (title      . ,(aget json-entry 'title))
    ;; TODO: could be many links here...
    (link       . ,(aget (first (aget json-entry 'alternate t)) 'href))
    (src-title  . ,(or (aget (first (aget json-entry 'via)) 'title)
                       (grc-get-in json-entry '(origin title))
                       (format "%s's Shared Items" (aget json-entry 'author))))
    (src-url    . ,(grc-get-in json-entry '(origin htmlUrl)))
    (src-id     . ,(grc-get-in json-entry '(origin streamId)))

    (summary    . ,(grc-get-in json-entry '(summary content)))
    (content    . ,(grc-get-in json-entry '(content content)))
    (categories . ,(grc-parse-get-categories json-entry))
    (comments   . ,(aget json-entry 'comments t))))

(defun grc-parse-parse-response (root)
  (setq grc-raw-response root)
  (let ((entries (aget root 'items t)))
    (mapcar 'grc-parse-process-entry entries)))

(provide 'grc-parse)
;; grc-parse.el ends here
