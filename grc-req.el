;;; grc-req.el --- Google Reader Mode for Emacs
;;
;; Copyright (c) 2011 David Leatherman
;;
;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/grc
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains code for getting and posting from and to Google

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
(require 'json)
(require 'oauth2)

(defvar grc-req-client-name "grc-emacs-client"
  "Arbitrary client string for various reqeuests")

(defvar grc-auth-header-format
  "--header 'Authorization: OAuth %s'"
  "HTTP authorization headers to send.")

(defvar grc-req-base-url "http://www.google.com/reader/"
  "Base URL for Google Reader  API.")

(defvar grc-req-subscribed-feed-list-url
  (concat grc-req-base-url "api/0/subscription/list")
  "URL for retrieving list of subscribed feeds.")

(defvar grc-req-preference-set-url
  (concat grc-req-base-url "api/0/preference/set")
  "URL for setting a preference")

(defvar grc-req-edit-tag-url
  (concat grc-req-base-url "api/0/edit-tag")
  "URL for editing a tag")

(defvar grc-req-edit-item-url
  (concat grc-req-base-url "api/0/item/edit")
  "URL for editing an entry")

(defvar grc-req-stream-url-pattern
  "http://www.google.com/reader/api/0/stream/contents/%s")

(defvar grc-req-reading-list-url (grc-req-stream-url "reading-list"))

(defun grc-req-stream-url (&optional state)
  "Get the url for Google Reader entries, optionally limited to a specified
  state- e.g., kept-unread"
  (let ((stream-state (if (null state)
                          ""
                        (concat "user/-/state/com.google/" state))))
    (format grc-req-stream-url-pattern stream-state)))

(defun grc-req-parse-response (raw-resp)
  (cond
   ((string-match "^{" raw-resp)
    (grc-parse-parse-response
     (let ((json-array-type 'list))
       (json-read-from-string
        (decode-coding-string raw-resp 'utf-8)))))
   ((string-match "^OK" raw-resp)
    "OK")
   (t nil)))

(defun grc-req-format-url-params (params)
  "Convert an alist of params into an & delimeted string suitable for curl"
  (mapconcat
   (lambda (p)
     (cond
      ((consp p)
       (concat (format "%s" (car p))
               "="
               (url-hexify-string (format "%s" (cdr p)))))
      (t
       (url-hexify-string (format "%s" p)))))
   params "&"))

(defun grc-req-request (url callback &optional method params retryp)
  (setq grc-token (or grc-token (grc-auth)))
  (let ((failure-cb (lambda (response headers)
                      ;; try reauthenticating
                      (if (cdr (assoc "X-Reader-Google-Bad-Token" headers))
                          (setq grc-token (grc-refresh-action-token grc-token))
                        (setq grc-token (grc-refresh-access-token grc-token)))
                      (if retryp
                          (error "Failed with: %s for %s"
                                 (cadr (assoc "response-code" headers))
                                 url)
                        (grc-req-request url callback method params t)))))
    (grapnel-retrieve-url
     url
     `((success . ,callback)
       (failure . ,failure-cb)
       (error . (lambda (response) (error "Error: %s" response))))
     method
     (append (or params '())
             `((access_token . ,(plist-get grc-token :access-token))
               ("T" . ,(plist-get grc-token :action-token))
               (client . ,grc-req-client-name)
               (ck . ,(grc-string (floor
                                   (* 1000000 (float-time)))))
               (output . "json"))))))

(defun grc-req-unread-entries (callback &optional limit since)
  (let* (;; the number of items to return
         (params (when limit
                   `(("n" . ,(format "%s" limit)))))
         ;; the oldest in which I'm interested
         (params (if since
                     (cons `("ot" . ,(format "%s" since)) params)
                   params)))
    (grc-req-request grc-req-reading-list-url callback
                     "GET"
                     (append
                      params
                      `(;; exclude read entries
                        ("xt" . "user/-/state/com.google/read")
                        ;; ranking method- newest first
                        ("r" . "n"))))))

(defun grc-req-mark (ids feeds params)
  (grc-req-request
   grc-req-edit-item-url '(lambda (&rest x)) "POST"
   (append params
           (mapcar (lambda (p i) (cons `("i" . ,i) p)) ids)
           (mapcar (lambda (p s) (cons `("s" . ,s) p)) feeds))))

(defun grc-req-mark-kept-unread (ids feeds)
  "Send a request to mark an entry as kept-unread.  Will also remove the read
  category"
  (grc-req-mark ids feeds '(("a" . "user/-/state/com.google/kept-unread")
                            ("r" . "user/-/state/com.google/read"))))

(defun grc-req-mark-read (ids feeds)
  "Send a request to mark an entry as read.  Will also remove the kept-unread
  category"
  (grc-req-mark ids feeds '(("r" . "user/-/state/com.google/kept-unread")
                            ("a" . "user/-/state/com.google/read"))))

(defun grc-req-mark-starred (ids feeds &optional remove-p)
  (grc-req-mark ids feeds
                `(((if remove-p "r" "a") . "user/-/state/com.google/starred"))))

(provide 'grc-req)
;;; grc-req.el ends here
