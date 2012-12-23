;;; grc-list.el --- Google Reader Mode for Emacs
;;
;; Copyright (c) 2011 David Leatherman
;;
;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/grc
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the code for showing the list of Reader entries

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
(defvar grc-list-buffer "*grc list*" "Name of the buffer for the grc list view")

(defun grc-list-header-line (&optional unread-count)
  "Set the header line for the grc-list-buffer"
  (setq header-line-format
        (format "%s  (%s)  Sort: %s %s"
                (cdr (assoc 'title grc-raw-response))
                (let ((lines (count-lines (point-min) (point-max))))
                  (if unread-count
                      (if (> unread-count lines)
                          (format "%s/%s" lines unread-count)
                        unread-count)
                    lines))
                (car tabulated-list-sort-key)
                (if (cdr tabulated-list-sort-key) "▲" "▼")))
  ;; fetch the actual count
  (unless unread-count
    (grc-req-unread-count 'grc-list-header-line)))

(defun grc-list-entry-data (e)
  "Calculate the data to print"
  (let ((id (cdr (assoc 'id e))))
    (list e (let* ((date (seconds-to-time (cdr (assoc 'date e))))
                   (one-week (- (float-time (current-time))
                                (* 60 60 24 7)))
                   (d-str (format-time-string
                           (if (> one-week (float-time date))
                               "%m/%d %H:%M"
                             "%a %H:%M")
                           date))
                   (src (grc-prepare-text (cdr (assoc 'src-title e))))
                   (title (grc-prepare-title (cdr (assoc 'title e)))))
              (vector d-str
                      (propertize src 'face (grc-highlight-make-face src))
                      title)))))

(defun grc-list-print-entries (entries)
  "Prints all the entries"
  (setq tabulated-list-entries (mapcar 'grc-list-entry-data entries))
  (tabulated-list-print t))

(defun grc-list-make-buffer (&optional msg)
  "Create the list buffer and add a message if provided."
  (with-current-buffer (get-buffer-create grc-list-buffer)
    (let ((inhibit-read-only t))
      (grc-list-mode)
      (erase-buffer)
      (when msg
        (insert msg))
      (buffer-name))))

(defun grc-list-apply-marks (marks)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((id (tabulated-list-get-id))
             (tag (some
                   (lambda (l)
                     (let ((tag (car l))
                           (lst (cdr l)))
                       (when (member id lst)
                         tag)))
                   marks)))
        (when tag
          (tabulated-list-put-tag tag)))
      (forward-line))))

(defun grc-list-fetch-more ()
  (let ((fn (or (cdr (assoc 'fn grc-current-state)) 'grc-req-fetch-entries)))
    (funcall fn (cdr (assoc 'id grc-current-state))
             '(lambda (raw-response headers)
                (when (get-buffer grc-list-buffer)
                  (with-current-buffer grc-list-buffer
                    (let ((response (grc-req-parse-response raw-response))
                          (marked-items (grc-list-marked-lines)))
                      ;; append to tab-list-entries and display
                      (setq tabulated-list-entries
                            (append tabulated-list-entries
                                    (mapcar 'grc-list-entry-data response)))
                      (tabulated-list-print t)
                      (grc-list-header-line)
                      ;; keep going if there are more
                      (if (cdr (assoc 'continuation grc-raw-response))
                          (grc-list-fetch-more)
                        (grc-list-apply-marks marked-items))))))
             nil nil
             `(("c" . ,(cdr (assoc 'continuation grc-raw-response)))))))

(defun grc-list-display (&optional entries)
  "Display the given entries in the grc-list-buffer"
  (interactive)
  (with-current-buffer (grc-list-make-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if (< 0 (length entries))
          (progn
            (grc-list-print-entries entries)
            (when (cdr (assoc 'continuation grc-raw-response))
              (grc-list-fetch-more)))
        (insert "No unread entries."))
      (grc-list-header-line)
      (goto-char (point-min)))))

(defun grc-list-refresh ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Fetching entries...")
    (grc-fetch-entries)))

(defun grc-list-next-entry ()
  "Move the point to the next entry."
  (interactive)
  (forward-line)
  (beginning-of-line))

(defun grc-list-previous-entry ()
  "Move the point to the previous entry."
  (interactive)
  (forward-line -1)
  (beginning-of-line))

(defun grc-list-help ()
  "Show the help message for the grc list view"
  (interactive)
  (grc-help))

(defun grc-list-has-read-overlay-p ()
  "Tests to see if the current line has the grc-read overlay"
  (-any? (lambda (o) (equal 'grc-read-face (overlay-get o 'face)))
         (overlays-at (point))))

(defun grc-list-add-read-overlay (&optional begin end)
  "Adds an overlay to the line that applies the grc-read-face to the text"
  (let* ((begin (or begin (line-beginning-position)))
         (end (or end (line-end-position)))
         (overlay (make-overlay begin end)))
    (overlay-put overlay 'face 'grc-read-face)))

(defun grc-list-remove-read-overlay ()
  "Removes the grc-read overlay from the line"
  (-each (overlays-at (point))
         (lambda (overlay)
           (when (equal 'grc-read-face (overlay-get overlay 'face))
             (delete-overlay overlay)))))

(defun grc-list-mark-region (tag begin end)
  (save-excursion
    (goto-char begin)
    (while (and (< (point) end)
                (not (eobp)))
      (when (tabulated-list-get-id)
        (tabulated-list-put-tag tag)
        (run-hook-with-args 'grc-line-tagged-hook
                            tag (tabulated-list-get-id)
                            (line-beginning-position) (line-end-position)))
      (forward-line))))

(defun grc-list-on-tagged (tag id beginning end)
  "Add or remove the grc-read overlay when marks are added or removed"
  (save-excursion
    (goto-char beginning)
    (if (or (equal tag " ") (equal tag "k"))
        (grc-list-remove-read-overlay)
      (grc-list-add-read-overlay))))

(add-hook 'grc-line-tagged-hook 'grc-list-on-tagged)

(defun grc-list-marked-lines ()
  "Collects all the lines that have marks and returns the associated data"
  (let (marked)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((cmd (char-to-string (char-after))))
          (unless (equal cmd " ")
            (let* ((alist (assoc cmd marked))
                   (lst (cdr alist)))
              (setq marked
                    (cons
                     (cons cmd (cons (tabulated-list-get-id) lst))
                     (delete alist marked))))))
        (forward-line)))
    marked))

(defun grc-list-execute-marks ()
  "Apply the appropriate actions to the marked lines"
  (interactive)
  (unless (derived-mode-p 'grc-list-mode)
    (error "The current buffer is not in grc list mode"))
  (run-hooks 'grc-list-before-execute-marks-hook)
  (when (yes-or-no-p "Execute marks?")
    (let ((marked (grc-list-marked-lines)))
      (-each
       marked
       (lambda (mark-alist)
         (let ((mark-type (car mark-alist))
               (items (cdr mark-alist)))
           (run-hook-with-args 'grc-list-execute-marks-hook mark-type items)))))
    (run-hooks 'grc-list-after-execute-marks-hook)))

(defun grc-list-delete-read-lines ()
  "Deletes lines that have the grc-read overlay"
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (while (not (eobp))
      (if (grc-list-has-read-overlay-p)
          (delete-region (line-beginning-position) (1+ (line-end-position)))
        (forward-line))))
  (goto-char (point-min)))

(add-hook 'grc-list-after-execute-marks-hook 'grc-list-delete-read-lines)

(defun grc-list-clear-marks ()
  (grc-list-mark-region " " (point-min) (point-max)))

(add-hook 'grc-list-after-execute-marks-hook 'grc-list-clear-marks t)
(add-hook 'grc-list-after-execute-marks-hook 'grc-list-header-line t)

(defmacro grc-list-defun-mark-for (name tag)
  "Define a function to mark entries"
  (let ((fn-name (intern (format "grc-list-mark-%s" name)))
        (doc (format "Mark entry at point as %s."
                     (replace-regexp-in-string "[-]" " " name))))
    `(progn
       (defun ,fn-name () ,doc
         (interactive)
         (let ((beginning (line-beginning-position))
               (end (line-end-position)))
           (tabulated-list-put-tag ,tag t)
           (run-hook-with-args 'grc-line-tagged-hook
                               ,tag (tabulated-list-get-id)
                               beginning end)))
       (put ',fn-name 'definition-name ',name))))

(defmacro grc-list-execute-marks-for (name tag fn)
  "Define a function to execute the marks for a particular tag."
  (let ((fn-name (intern (format "grc-list-execute-%s-marks" name)))
        (doc (format "Execute entries marked as %s."
                     (replace-regexp-in-string "[-]" " " name))))
    `(progn
       (defun ,fn-name (mark-type items) ,doc
         (when (equal ,tag mark-type)
           (funcall ,fn items)))
       (put ',fn-name 'definition-name ',name))))

(defmacro grc-list-def-fns (name tag &optional handler)
  "Defines both the mark and execute functions and adds the appropriate hook
to wire it all together"
  (let ((hook-name (intern (format "grc-list-execute-%s-marks" name))))
    `(progn
       (grc-list-defun-mark-for ,name ,tag)
       (when ,handler
         (grc-list-execute-marks-for ,name ,tag ,handler)
         (add-hook 'grc-list-execute-marks-hook ',hook-name)))))

;; Declare all the mark fns and execution handlers
(grc-list-def-fns "unmark" " ")
(grc-list-def-fns "read" "r" 'grc-mark-read)
(grc-list-def-fns "kept-unread" "k" 'grc-mark-kept)
(grc-list-def-fns "starred" "s" 'grc-mark-starred)

(defun grc-list-show-entry ()
  "View the current entry."
  (interactive)
  (when (tabulated-list-get-id)
    (grc-show-entry (tabulated-list-get-id))))

(defun grc-list-show-entry-external ()
  "Open the current rss entry in the default emacs browser"
  (interactive)
  (when (tabulated-list-get-id)
    (grc-view-external (tabulated-list-get-id))))

(defun grc-list-after-show ()
  "Add the read overlay and mark an entry as read when it is opened in the
view buffer or externally in the browser"
  (with-current-buffer grc-list-buffer
    (let ((id (tabulated-list-get-id)))
      (grc-list-add-read-overlay)
      (grc-mark-read (list id)))))

(add-hook 'grc-show-entry-hook 'grc-list-after-show)
(add-hook 'grc-show-external-hook 'grc-list-after-show)

(defvar grc-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "q"         'grc-kill-this-buffer)
    (define-key map "?"         'grc-list-help)
    (define-key map "k"         'grc-list-mark-kept-unread)
    (define-key map "r"         'grc-list-mark-read)
    (define-key map "*"         'grc-list-mark-starred)
    (define-key map "n"         'grc-list-next-entry)
    (define-key map "p"         'grc-list-previous-entry)
    (define-key map " "         'grc-list-show-entry)
    (define-key map (kbd "RET") 'grc-list-show-entry)
    (define-key map "v"         'grc-list-show-entry-external)
    (define-key map "u"         'grc-list-mark-unmark)
    (define-key map "x"         'grc-list-execute-marks)
    (define-key map "g"         'grc-list-refresh)
    map)
  "Keymap for \"grc-list\" buffers.")
(fset 'grc-list-mode-map grc-list-mode-map)

(define-derived-mode grc-list-mode tabulated-list-mode "grc-list"
  "Major mode for viewing feeds with grc

  This buffer contains the results of the \"grc\" command
  for displaying unread feeds from Google Reader.

  All currently available key bindings:

  g    Display or refresh the grc reading list.
  v    Open the current rss entry in the default emacs browser
  RET  View the current entry.
  SPC  View the current entry.
  p    Move the point to the previous entry.
  n    Move the point to the next entry.
  *    Star the current entry.
  r    Mark the current entry as Read.
  k    Mark the current entry as Kept Unread.
  u    Unmark the current entry.
  x    Execute the marked actions.
  ?    Show the help message for the grc list screen
  q    Kill the current buffer."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'grc-list-mode
        mode-name "grc-list"
        buffer-read-only t
        lexical-binding t
        tabulated-list-format [("Date" 11
                                (lambda (a b)
                                  (> (cdr (assoc 'date (car a)))
                                     (cdr (assoc 'date (car b))))))
                               ("Source" 15 t)
                               ("Title" 0 t)]
        tabulated-list-sort-key (cons "Date" nil)
        tabulated-list-padding 2
        truncate-lines t)
  (use-local-map grc-list-mode-map))

(defun grc-list-enable-hl-line ()
  (hl-line-mode t))

(add-hook 'grc-list-mode-hook 'grc-list-enable-hl-line)

(provide 'grc-list)
;;; grc-list.el ends here
