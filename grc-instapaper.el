;;; grc-instapaper.el --- Google Reader Mode for Emacs
;;
;; Copyright (c) 2011 David Leatherman
;;
;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/grc
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Experimental support for sendint entries to Instapaper

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

 ;; Credential management

(defun grc-instapaper-creds-ok-p (username password)
  "Call the authenticate endpoint to verify the provided creds"
  (let ((grapnel-options
         (concat grapnel-options " --user " username ":" password)))
    (grapnel-retrieve-url-sync
     "https://www.instapaper.com/api/authenticate"
     '((success . (lambda (r h)
                    (message "resp: %s headers: %s" r h)
                    t))
       (failure . (lambda (_ _) nil))
       (error
        . (lambda (r exit-code)
            (error "Curl failed with %s while checking the instapaper creds %s"
                   exit-code r)))))))

(defun grc-instapaper-cred-plist ()
  "Retrieve the credentials from the secure plstore"
  (let* ((plstore (plstore-open grc-token-file))
         (id "grc-instapaper")
         (plist (cdr (plstore-get plstore id)))
         (username (plist-get plist :username))
         (password (plist-get plist :password)))
    (if (and username password)
        plist
      (let*
          ((username (read-string "Enter your Instapaper username: "))
           (password (read-string
                      "Enter your Instapaper password (if you have one): ")))
        (if (grc-instapaper-creds-ok-p username password)
            (progn
              (plstore-put plstore id nil (plist-put
                                           (plist-put plist :username username)
                                           :password password))
              (plstore-save plstore)
              plist)
          (error (concat "Auth failed. Check your Instapaper username "
                         "and password and try again.")))))))

 ;; Main save link functionality

(defun grc-instapaper-save-link (entries)
  "Send the given ENTRIES to Instapaper"
  (let* ((plist (grc-instapaper-cred-plist))
         (username (plist-get plist :username))
         (password (plist-get plist :password)))
    (dolist (entry (grc-list entries))
      (grapnel-retrieve-url "https://www.instapaper.com/api/add"
                            '((complete . (message "Sent to Instapaper")))
                            `(("username" . ,username)
                              ("password" . ,password)
                              ("url"      . ,(cdr (assoc 'link entry))))))))

;; Wire up the grc-list buffer
(grc-list-def-fns "instapaper" "i" 'grc-instapaper-save-link)
(define-key grc-list-mode-map "i" 'grc-list-mark-instapaper)

;; and the grc-show buffer
(defun grc-show-send-to-instapaper ()
  "Send the current entry to Instapaper"
  (interactive)
  (grc-instapaper-save-link (list (grc-show-current-entry))))

(define-key grc-show-mode-map "i" 'grc-show-send-to-instapaper)

 ;; Kindle support - experimental

;; TODO: Hopefully this can go away someday if send-to-kindle support
;; is ever added to the API
(defun grc-instapaper-bookmarklet-key ()
  "Retrieve the bookmarklet key.  See `grc-instapaper-set-bookmarklet-key'
for more information"
  (let* ((plstore (plstore-open grc-token-file))
         (id "grc-instapaper")
         (plist (cdr (plstore-get plstore id)))
         (bookmarklet-key (plist-get plist :bookmarklet-key)))
    (or bookmarklet-key
        (error "bookmarklet key is not set. %s %s"
               "See the documentation for"
               "`grc-instapaper-set-bookmarklet-key'"))))

(defun grc-instapaper-set-bookmarklet-key (key)
  "The bookmarklet-key is a name that I've given to a little segment of the
Instapaper bookmarklet URL that identifies your account. Go to the Extras page
on instapaper.com and install the 'Read Later' bookmarklet.  Right click on it
to view the source.  You are looking for something like:

www.instapaper.com/X/<BOOKMARKLET_ID_IS_HERE>?a=...

Take the contents of the bookmarklet-id and use this function to enter it in."
  (let* ((plstore (plstore-open grc-token-file))
         (id "grc-instapaper")
         (plist (cdr (plstore-get plstore id))))
    (plstore-put plstore id nil (plist-put :bookmarklet-key key))
    (plstore-save plstore)))

(defun grc-instapaper-send-to-kindle (entries)
  "Send the given ENTRIES to a Kindle via Instapaper.  Kindle support must be
configured on instapaper.com before this will work."
  (let ((key (grc-instapaper-bookmarklet-key)))
    (dolist (entry (grc-list entries))
      (grapnel-retrieve-url
       (concat "https://www.instapaper.com/j/" key)
       '((complete . (message "Sent to Kindle via Instapaper")))
       `(("a" . "send-to-kindle")
         ("u" . ,(cdr (assoc 'link entry)))
         ("t" . (ceiling (* (float-time) 100000))))))))

(grc-list-def-fns "instapaper-kindle" "l" 'grc-instapaper-send-to-kindle)
(define-key grc-list-mode-map "l" 'grc-list-mark-instapaper-kindle)

(defun grc-show-send-to-instapaper-kindle ()
  "Send the current entry to a Kindle via Instapaper.  Kindle support must be
configured on instapaper.com before this will work."
  (interactive)
  (grc-instapaper-send-to-kindle (list (grc-show-current-entry))))

(define-key grc-show-mode-map "l" 'grc-show-send-to-instapaper-kindle)

 ;; and finally...

(provide 'grc-instapaper)
