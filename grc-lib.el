;;; grc-lib.el --- Google Reader Mode for Emacs
;;
;; Copyright (c) 2011 David Leatherman
;;
;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/grc
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains general purpose elisp functions that I find
;; useful

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
(defun grc-list (thing)
  "Return THING if THING is a list or a list with THING as its element."
  (if (listp thing)
      thing
    (list thing)))

(defun grc-string (thing)
  "Return THING if THING is a string or convert THING to a string and return it"
  (if (stringp thing)
      thing
    (prin1-to-string thing t)))

(defun grc-get-in (alist seq &optional not-found)
  "Return the value in a nested alist structure.

  seq is a list of keys
  Returns nil or the not-found value if the key is not present"
  (let ((val (-reduce-from
              (lambda (a k)
                (cdr (assoc k a)))
              alist
              seq)))
    (or val not-found)))

(provide 'grc-lib)
;;; grc-lib.el ends here
