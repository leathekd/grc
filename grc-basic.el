;;; grc-basic.el --- Google Reader Mode for Emacs
;;
;; Copyright (c) 2011 David Leatherman
;;
;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/grc
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the 'basic' view renderer. The renderer makes a
;; naive attempt at stripping HTML and then transforms the links into
;; buttons.

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
(defvar grc-basic-html-entity-list
  '(("&nbsp;" " ") ;; do I need these spaces?
    ("&#xa0;" " ")
    (" " " ") ;; this is the literal unicode nbsp char
    ("&amp;" "&")
    ;; so many quotes - I'm ignoring left vs right
    ("&apos;" "'")
    ("&#39;" "'")
    ("&#8216;" "'")
    ("&#8217;" "'")
    ("&prime;" "'")
    ("&quot;" "\"")
    ("&rsquo;" "'")
    ("&#8220;" "\"")
    ("&#8221;" "\"")
    ("&ldquo;" "\"")
    ("&rdquo;" "\"")
    ("&gt;" ">")
    ("&lt;" "<")
    ("&mdash;" "—")
    ("&#9670;" "©")
    ("&#8230;" "…")))

(defun grc-basic-convert-entities ()
  "Searches through the buffer replacing common HTML entities with their chars"
  (mapcar '(lambda (pair)
             (goto-char (point-min))
             (grc-replace-string (car pair) (cadr pair)))
          grc-basic-html-entity-list))

(defun grc-basic-trim-left-in-buffer ()
  "Removes all leading whitespace from all lines in the buffer"
  (goto-char (point-min))
  (while (not (eobp))
    (beginning-of-line)
    (delete-horizontal-space)
    (forward-line 1)))

(defun grc-basic-normalize-newlines ()
  "Reduces multiple blank lines down to one"
  (goto-char (point-min))
  (grc-replace-regexp "^\n+" "\n")
  (when (and (> (point-max) (point-min))
             (equal "\n" (buffer-substring (point-min) (1+ (point-min)))))
    (goto-char (point-min))
    (delete-char 1)))

(defun grc-basic-strip-tag (tag)
  (goto-char (point-min))
  (let ((pt (search-forward (concat "<" tag) nil t)))
    (while pt
      (delete-region (- pt (1+ (length tag)))
                     (search-forward (concat "</" tag ">") nil t))
      (setq pt (search-forward (concat "<" tag) nil t)))))

(defun grc-basic-strip-html ()
  "Converts some HTML entities and naively removes HTML tags."
  (grc-basic-convert-entities)
  (-each '("head" "style" "script" "input" "select" "textarea")
         'grc-basic-strip-tag)
  (goto-char (point-min))
  (grc-replace-regexp "<.*?>" "")
  (grc-basic-trim-left-in-buffer)
  (grc-basic-normalize-newlines)
  (goto-char (point-min)))

(defun grc-basic-strip-html-to-string (str)
  "Takes a string and returns it stripped of HTML"
  (with-temp-buffer
    (insert str)
    (grc-basic-strip-html)
    (s-trim (buffer-string))))

(defun grc-basic-insert-newlines ()
  (goto-char (point-min))
  (grc-replace-regexp "<br.*?>" "\n")
  (goto-char (point-min))
  (grc-replace-regexp "</.?p>" "\n"))

(defun grc-extract-href-from-region (start end)
  (let ((text (buffer-substring start end)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (re-search-forward "href[ ]?=['\" ]?" (point-max) t)
      (browse-url-url-at-point))))

(defun grc-basic-buttonify-anchors ()
  "Walks through a buffer of html and removes the anchor tags,
  replacing them with a button that will browse the link"
  (goto-char (point-min))
  (while (search-forward-regexp "<a" nil t)
    (let* ((p1 (point))
           (p2 (search-forward-regexp ">" nil t))
           (p3 (search-forward-regexp "</a>" nil t))
           (href (grc-extract-href-from-region p1 p3))
           (text (grc-basic-strip-html-to-string
                  (buffer-substring-no-properties p2 (- p3 4)))))
      (when (and href text (not (equal "" (s-trim text))))
        (progn
          (delete-region (- p1 2) p3)
          (let ((pt (point)))
            (insert text)
            (make-button pt
                         (point)
                         'url href
                         'help-echo href
                         'type 'grc-basic-link-button)))))))

(defun grc-basic-button-browse-url (overlay)
  (browse-url (overlay-get overlay 'url)))

(defun grc-basic-convert-lists ()
  (goto-char (point-max))
  (while (search-backward-regexp "<li.*?>\\(.*?\\)</li>" nil t)
    (let ((beg (match-beginning 0))
          (end (match-end 0))
          (txt (match-string 1)))
      (delete-region beg end)
      (goto-char beg)
      (insert "\n- " txt "\n"))))

(define-button-type 'grc-basic-link-button
  'follow-link t
  'face 'link
  'action #'grc-basic-button-browse-url)

(defun grc-basic-prepare-text (text)
  (grc-basic-strip-html-to-string text))

(defun grc-basic-next-anchor ()
  (interactive)
  (forward-button 1 t t))

(defun grc-basic-previous-anchor ()
  (interactive)
  (backward-button 1 t t))

(defun grc-show-basic-renderer ()
  (run-hooks 'grc-basic-before-render-hook)
  (grc-basic-buttonify-anchors)
  (grc-basic-insert-newlines)
  (grc-basic-convert-lists)
  (goto-char (point-min))
  (grc-basic-strip-html)
  (fill-region (point-min) (point-max))
  (run-hooks 'grc-basic-after-render-hook))

(setq grc-prepare-text-fn 'grc-basic-prepare-text
      grc-show-summary-renderer 'grc-show-basic-renderer
      grc-show-external-link-viewer 'grc-basic-external-view-this-url
      grc-show-next-anchor-fn 'grc-basic-next-anchor
      grc-show-previous-anchor-fn 'grc-basic-previous-anchor)

(provide 'grc-basic)
;;; grc.el ends here
