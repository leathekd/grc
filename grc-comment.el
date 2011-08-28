;;; grc-comment.el --- Google Reader Mode for Emacs
;;
;; Copyright (c) 2011 David Leatherman
;;
;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/grc
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the code for entering comments and notes

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
(defvar grc-comment-buffer "*grc comment*")
(defvar grc-comment-callback-fn nil)
(defvar grc-comment-callback-args nil)

(defun grc-comment-commit ()
  (interactive)
  (apply grc-comment-callback-fn (buffer-string) grc-comment-callback-args)
  (grc-comment-cancel))

(defun grc-comment-cancel ()
  (interactive)
  (kill-buffer grc-comment-buffer))

(defvar grc-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'grc-comment-commit)
    (define-key map (kbd "C-c C-k") 'grc-comment-cancel)
    map)
  "Keymap for \"grc comment\" buffers.")
(fset 'grc-comment-mode-map grc-comment-mode-map)

(defun grc-comment-mode ()
  ""
  (interactive)
  (kill-all-local-variables)
  (make-variable-buffer-local 'grc-comment-callback-fn)
  (make-variable-buffer-local 'grc-comment-callback-args)
  (use-local-map grc-comment-mode-map)
  (setq header-line-format
        "Google Reader Client -- C-c C-c to submit comment, C-c C-k to cancel")
  (setq major-mode 'grc-comment-mode
        mode-name "grc-comment"))

(defun grc-comment-open-buffer (cb-fn &rest cb-args)
  (save-excursion
    (with-current-buffer (get-buffer-create grc-comment-buffer)
      (grc-comment-mode)
      (setq grc-comment-callback-fn cb-fn)
      (setq grc-comment-callback-args cb-args)
      (pop-to-buffer grc-comment-buffer))))

(provide 'grc-comment)
;;; grc-comment.el ends here
