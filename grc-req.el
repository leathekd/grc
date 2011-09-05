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

(defvar grc-req-client-name "grc-emacs-client"
  "Arbitrary client string for various reqeuests")

(defvar grc-auth-header-format
  "--header 'Authorization: OAuth %s'"
  "HTTP authorization headers to send.")

(defvar grc-req-base-url
  "http://www.google.com/reader/"
  "Base URL for Google Reader  API.")

(defvar grc-req-subscribed-feed-list-url
  (concat grc-req-base-url
          "api/0/subscription/list")
  "URL for retrieving list of subscribed feeds.")

(defvar grc-req-unread-count-url
  (concat grc-req-base-url
          "api/0/unread-count")
  "URL for retrieving unread counts for subscribed feeds.")

(defvar grc-req-preference-list-url
  (concat grc-req-base-url
          "api/0/preference/list")
  "URL for retrieving preferences stored by Google Reader.
  For the most part, this is only used by the Google Reader UI,
  grc just gets and sets the last-allcomments-view pref.")

(defvar grc-req-preference-set-url
  (concat grc-req-base-url
          "api/0/preference/set")
  "URL for setting a preference")

(defvar grc-req-edit-tag-url
  (concat grc-req-base-url
          "api/0/edit-tag")
  "URL for editing a tag")

(defvar grc-req-edit-item-url
  (concat grc-req-base-url
          "api/0/item/edit")
  "URL for editing an entry")

(defvar grc-req-edit-comment-url
  (concat grc-req-base-url
          "api/0/comment/edit")
  "URL for adding/editing a comment")

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

(defvar grc-req-async-cb-fns (make-hash-table :test 'equal))
(defvar grc-req-async-responses (make-hash-table :test 'equal))

(defun grc-req-process-sentinel (process event)
  (when (string-match "^finished" event)
    (let ((raw-resp (gethash process grc-req-async-responses)))
      (funcall
       (gethash process grc-req-async-cb-fns)
       (let ((raw-resp
              (cond ((string-match "^{" raw-resp)
                     (let ((json-array-type 'list))
                       (json-read-from-string
                        (decode-coding-string raw-resp 'utf-8))))
                    (t (error "Error in async request. Cmd: %s\nResponse: %s"
                              (process-command process) raw-resp)))))
         (grc-parse-parse-response raw-resp))))
    (remhash process grc-req-async-cb-fns)))

(defun grc-req-process-filter (process string)
  (let ((resp (gethash process grc-req-async-responses "")))
    (puthash process (concat resp string) grc-req-async-responses)))

(defun grc-req-do-async-request (cb-fn verb endpoint
                                       &optional params no-auth raw-response)
  (unless no-auth (grc-auth-ensure-authenticated))
  (let* ((endpoint (concat endpoint
                           "?client=" grc-req-client-name
                           "&ck=" (grc-string (floor (* 1000000 (float-time))))
                           "&output=json"))
         (params (if (listp params) (grc-req-format-params params) params))
         (process (start-process "grc request"
                                 nil
                                 grc-curl-program
                                 "--compressed"
                                 "--silent" "--location" "--location-trusted"
                                 "--header"
                                 (format "Authorization: OAuth %s"
                                         (aget grc-auth-access-token 'token))
                                 verb
                                 (if (string= "POST" verb)
                                     (format "-d \"%s\"" params)
                                   "")
                                 (if (and (not (equal "" params))
                                          (string= "GET" verb))
                                     (concat endpoint "&" params)
                                   endpoint))))
    (puthash process cb-fn grc-req-async-cb-fns)
    (set-process-filter process 'grc-req-process-filter)
    (set-process-sentinel process 'grc-req-process-sentinel)))

(defun grc-req-async-get-request (cb-fn endpoint
                                        &optional params no-auth raw-response)
  (grc-req-do-async-request cb-fn "GET" endpoint params no-auth raw-response))

(defun grc-req-async-post-request (cb-fn endpoint params
                                         &optional no-auth raw-response)
  (grc-req-do-async-request cb-fn "POST" endpoint params no-auth raw-response))

(defun grc-req-do-request (verb endpoint &optional params no-auth raw-response)
  (unless no-auth (grc-auth-ensure-authenticated))
  (let* ((endpoint (concat endpoint
                           "?client=" grc-req-client-name
                           "&ck=" (grc-string (floor (* 1000000 (float-time))))
                           "&output=json"))
         (params (if (listp params) (grc-req-format-params params) params))
         (command (format
                   "%s %s %s -X %s %s '%s' "
                   grc-curl-program
                   grc-curl-options
                   (if no-auth "" (grc-req-auth-header))
                   verb
                   (if (string= "POST" verb)
                       (format "-d \"%s\"" params)
                     "")
                   (if (and (not (empty-string-p params)) (string= "GET" verb))
                       (concat endpoint "&" params)
                     endpoint)))
         (raw-resp (shell-command-to-string command)))
    (cond
     (raw-response raw-resp)
     ((string-match "^{" raw-resp)
      (let ((json-array-type 'list))
        (json-read-from-string
         (decode-coding-string raw-resp 'utf-8))))
     ((string-match "^OK" raw-resp) "OK")
     (t (error "Error: %s?%s\nFull command: %s\nResponse: %s"
               endpoint params command raw-resp)))))

(defun grc-req-get-request (endpoint &optional params no-auth raw-response)
  (grc-req-do-request "GET" endpoint params no-auth raw-response))

(defun grc-req-post-request (endpoint params &optional no-auth raw-response)
  (grc-req-do-request "POST" endpoint params no-auth raw-response))


(defvar grc-req-stream-url-pattern
  "http://www.google.com/reader/api/0/stream/contents/%s")

(defun grc-req-stream-url (&optional state)
  (let ((stream-state (if (null state)
                          ""
                        (concat "user/-/state/com.google/" state))))
    (format grc-req-stream-url-pattern stream-state)))

(defun grc-req-format-params (params)
  (mapconcat (lambda (p) (concat (car p) "=" (url-hexify-string (cdr p))))
             params "&"))

(defun grc-req-incremental-fetch (cb-fn)
  (when (string= grc-current-state "reading-list")
    (grc-req-remote-entries cb-fn grc-current-state grc-req-last-fetch-time)))

;; TODO: Need to factor out the state specific voodoo into something
;; less kludgy
(defun grc-req-remote-entries (cb-fn &optional state since)
  (let ((params `(("n"       . ,(grc-string grc-fetch-count))
                  ("sharers" . ,(grc-req-sharers-hash))
                  ("client"  . "emacs-grc-client"))))
    (cond
     ((string= state "reading-list")
      (setq grc-req-last-fetch-time (floor (float-time)))
      (aput 'params "xt" "user/-/state/com.google/read")
      (aput 'params "r" "n"))
     ((string= state "broadcast-friends-comments")
      (aput 'params "co" "true")
      (aput 'params "r" "c")))
    (when since
      (aput 'params "ot" (prin1-to-string since)))
    (grc-req-async-get-request
     cb-fn
     (grc-req-stream-url state)
     (grc-req-format-params params))
    (if (string= state "broadcast-friends-comments")
        (grc-req-set-preference "last-allcomments-view"
                                (floor (* 1000000 (float-time)))))))

(defun grc-req-edit-tag (id feed tag remove-p &optional extra-params)
  (grc-req-post-request
   grc-req-edit-tag-url
   (format "%s=user/-/state/com.google/%s&async=true&s=%s&i=%s&T=%s%s"
           (if remove-p
               "r"
             "a")
           tag feed id
           (grc-auth-get-action-token)
           (if extra-params
               (concat "&" extra-params)
             ""))))

(defun grc-req-add-comment (entry-id src-id comment)
  (let ((params `(("s"       . ,src-id)
                  ("i"       . ,entry-id)
                  ("T"       . ,(grc-string (grc-auth-get-action-token)))
                  ("action"  . "addcomment")
                  ("comment" . ,comment))))
    (grc-req-post-request grc-req-edit-comment-url params)))

(defun grc-req-friends ()
  (grc-req-get-request "http://www.google.com/reader/api/0/friend/list"))

(defun grc-req-unread-comment-count ()
  (let* ((unread-counts
          (aget (grc-req-get-request
                 grc-req-unread-count-url
                 `(("n"           . ,(grc-string grc-fetch-count))
                   ("all"         . "true")
                   ("allcomments" . "true")
                   ("sharers"     . ,(grc-req-sharers-hash))))
                'unreadcounts t))
         (unread-comments
          (first (remove-if-not (lambda (c)
                                  (string-match "broadcast-friends-comments"
                                                (aget c 'id)))
                                unread-counts))))
    (aget unread-comments 'count t)))

(defvar grc-req-sharers-hash-val nil "caches the value of the sharers hash")
(defun grc-req-sharers-hash ()
  (or grc-req-sharers-hash-val
      (setq grc-req-sharers-hash-val
            (aget (grc-req-friends) 'encodedSharersList))))

(defun grc-req-mark-all-read (src)
  (grc-req-post-request
   "http://www.google.com/reader/api/0/mark-all-as-read"
   (format "s=%s&ts=%s&T=%s"
           (or src "user/-/state/com.google/reading-list")
           (floor (* 1000000 (float-time)))
           (grc-auth-get-action-token))))

(defun grc-req-subscriptions ()
  (grc-req-get-request grc-req-subscribed-feed-list-url))

(defun grc-req-set-preference (key val)
  (grc-req-post-request grc-req-preference-set-url
                        `(("k" . ,(grc-string key))
                          ("v" . ,(grc-string val))
                          ("T" . ,(grc-string (grc-auth-get-action-token))))))

(defun grc-req-share-with-comment (comment title snippet src-title src-url
                                           entry-url)
  (let ((params `(("share"      . "true")
                  ("linkify"    . "true")
                  ("T"          . ,(grc-string (grc-auth-get-action-token)))
                  ("annotation" . ,comment)
                  ("title"      . ,(aget entry 'title))
                  ("snippet"    . ,(or (aget entry 'summary t)
                                       (aget entry 'content t)))
                  ("srcTitle"   . ,(aget entry 'src-title))
                  ("srcUrl"     . ,(aget entry 'src-url))
                  ("url"        . ,(aget entry 'link)))))
    (grc-req-post-request grc-req-edit-item-url params)))

(provide 'grc-req)
;;; grc-req.el ends here
