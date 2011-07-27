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

(defvar grc-auth-header-format
  "--header 'Authorization: OAuth %s'"
  "HTTP authorization headers to send.")

(defvar grc-req-base-url
  "http://www.google.com/reader/"
  "Base URL for Google Reader  API.")

(defvar grc-req-subscribed-feed-list-url
  (concat grc-req-base-url
          "api/0/subscription/list?output=json")
  "URL for retrieving list of subscribed feeds.")

(defvar grc-req-unread-count-url
  (concat grc-req-base-url
          "api/0/unread-count?all=true&output=json")
  "URL for retrieving unread counts for subscribed  feeds.")

(defvar grc-req-last-fetch-time nil)

(defcustom grc-curl-program "/usr/bin/curl"
  "Full path of the curl executable"
  :group 'grc
  :type 'string)

(defcustom grc-curl-options
  "--compressed --silent --location --location-trusted"
  "Options to pass to all grc curl requests"
  :group 'grc
  :type 'string)

(defun grc-req-auth-header ()
  (format grc-auth-header-format
          (aget grc-auth-access-token 'token)))

(defun grc-req-get-request (endpoint &optional request no-auth raw-get)
  (unless no-auth (grc-auth-ensure-authenticated))
  (let* ((command (format
                   "%s %s %s -X GET '%s' "
                   grc-curl-program grc-curl-options
                   (if no-auth "" (grc-req-auth-header))
                   (if request
                       (concat endpoint "?" request)
                     endpoint)))
         (resp (shell-command-to-string command)))
    (cond
     (raw-get resp)
     ((string-match "^{" resp) (let ((json-array-type 'list))
                                 (json-read-from-string resp)))
     ((string-match "^OK" resp) "OK")
     (t (error "Error fetching: %s?%s\nFull command: %s\nResponse: %s"
               endpoint request command resp)))))

(defun grc-req-post-request (endpoint request &optional no-auth)
  (unless no-auth (grc-auth-ensure-authenticated))
  (let* ((command (format
                   "%s %s %s  -X POST -d '%s' '%s' "
                   grc-curl-program grc-curl-options
                   (if no-auth "" (grc-req-auth-header))
                   request
                   endpoint))
         (resp (shell-command-to-string command)))
    (cond
     ((string-match "^{" resp) (let ((json-array-type 'list))
                                 (json-read-from-string resp)))
     ((string-match "^OK" resp) "OK")
     (t (error "Error fetching: %s?%s\nFull command: %s\nResponse: %s"
               endpoint request command resp)))))

(defun grc-req-send-edit-request (request)
  (grc-req-post-request
   "http://www.google.com/reader/api/0/edit-tag?client=emacs-g-client"
   request))

(defvar grc-req-stream-url-pattern
  "http://www.google.com/reader/api/0/stream/contents/%s")

;; TODO: sharers, n=100, output=json
(defun grc-req-stream-url (&optional state)
  (let ((stream-state (if (null state)
                          ""
                        (concat "user/-/state/com.google/" state))))
    (format grc-req-stream-url-pattern stream-state)))

(defun grc-req-format-params (params)
  (mapconcat (lambda (p) (concat (car p) "=" (cdr p)))
             params "&"))

(defun grc-req-incremental-fetch ()
  (when (string= grc-current-state "reading-list")
    (grc-req-remote-entries grc-current-state grc-req-last-fetch-time)))

(defun grc-req-remote-entries (&optional state since)
  (let ((params `(("n"       . ,(grc-string grc-fetch-count))
                  ("sharers" . ,(grc-req-sharers-hash))
                  ("client"  . "emacs-grc-client"))))
    (when (string= state "reading-list")
      (setq grc-req-last-fetch-time (floor (float-time)))
      (aput 'params "xt" "user/-/state/com.google/read"))
    (when since
      (aput 'params "ot" (prin1-to-string since)))
    (grc-parse-parse-response
     (grc-req-get-request (grc-req-stream-url state)
                          (grc-req-format-params params)))))

(defun grc-req-friends ()
  (grc-req-get-request "http://www.google.com/reader/api/0/friend/list"
                       "output=json"))

(defvar grc-req-sharers-hash-val nil "caches the value of the sharers hash")
(defun grc-req-sharers-hash ()
  (or grc-req-sharers-hash-val
      (setq grc-req-sharers-hash-val
            (aget (grc-req-friends) 'encodedSharersList))))

(defun grc-req-tag-request (entry tag remove)
  (format "%s=user/-/state/com.google/%s&async=true&s=%s&i=%s&T=%s"
          (if remove "r" "a")
          tag
          (aget entry 'feed)
          (aget entry 'id)
          (grc-auth-get-action-token)))

(defun grc-req-mark-all-read (src)
  (grc-req-post-request
   "http://www.google.com/reader/api/0/mark-all-as-read"
   (format "s=%s&ts=%s&T=%s"
           (or src "user/-/state/com.google/reading-list")
           (floor (* 1000000 (float-time)))
           (grc-auth-get-action-token))))

(defun grc-req-subscriptions ()
  (grc-req-get-request grc-req-subscribed-feed-list-url))

(provide 'grc-req)
;;; grc-req.el ends here
