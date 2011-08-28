;;; grc-lib.el --- Google Reader Mode for Emacs
;;
;; Copyright (C) 2011  David Leatherman
;;
;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/grc
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file was originally erc-highlight-nicknames.  It was modified
;; to take a list of keywords to highlight as well as to save the new
;; faces in a grc specific variable.  Loads of refactoring, too.

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
(defface grc-highlight-nick-base-face
  '((t nil))
  "Base face used for highlighting keywords in grc. (Before the keyword
  color is added)"
  :group 'grc)

(defvar grc-highlight-face-table
  (make-hash-table :test 'equal)
  "The hash table that contains unique grc faces.")

(defun grc-highlight-hexcolor-luminance (color)
  "Returns the luminance of color COLOR. COLOR is a string \(e.g.
  \"#ffaa00\", \"blue\"\) `color-values' accepts. Luminance is a
  value of 0.299 red + 0.587 green + 0.114 blue and is always
  between 0 and 255."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (car (cdr values)))
         (b (car (cdr (cdr values)))))
    (floor (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)) 256)))

(defun grc-highlight-invert-color (color)
  "Returns the inverted color of COLOR."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (car (cdr values)))
         (b (car (cdr (cdr values)))))
    (format "#%04x%04x%04x"
            (- 65535 r) (- 65535 g) (- 65535 b))))

(defun grc-highlight-color-for-word (word)
  (let ((color (concat "#" (substring (md5 (downcase word)) 0 12))))
    (if (equal (cdr (assoc 'background-mode (frame-parameters))) 'dark)
        ;; if too dark for background
        (when (< (grc-highlight-hexcolor-luminance color) 85)
          (grc-highlight-invert-color color))
      ;; if too bright for background
      (when (> (grc-highlight-hexcolor-luminance color) 170)
        (grc-highlight-invert-color color)))))

(defun grc-highlight-make-face (word)
  (or (gethash word grc-highlight-face-table)
      (let ((color (grc-highlight-color-for-word word))
            (new-kw-face
             (make-symbol (concat "grc-highlight-nick-" word "-face"))))
        (copy-face 'grc-highlight-nick-base-face new-kw-face)
        (set-face-foreground new-kw-face color)
        (puthash word new-kw-face grc-highlight-face-table))))

(defun grc-highlight-keyword (kw)
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (search-forward kw nil t)
      (let ((start (- (point) (length kw)))
            (end (point)))
        (put-text-property start end
                           'face
                           (grc-highlight-make-face kw))))))

(defun grc-highlight-keywords (keywords)
  "Searches for nicknames and highlights them. Uses the first
  twelve digits of the MD5 message digest of the nickname as
  color (#rrrrggggbbbb)."
  (let ((kw (car keywords)))
    (when (and kw (not (empty-string-p kw)))
      (grc-highlight-keyword kw)
      (grc-highlight-keywords (cdr keywords)))))

(provide 'grc-highlight)
;;; grc-highlight.el ends here
