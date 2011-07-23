;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google reader requests
(defvar grc-auth-header-format
  "--header 'Authorization: OAuth %s'"
  "HTTP authorization headers to send.")

(defvar grc-req-base-url
  "http://www.google.com/reader/"
  "Base URL for Google Reader  API.")

(defvar grc-req-subscribed-feed-list-url
  (concat greader-base-url
          "api/0/subscription/list?output=json")
  "URL for retrieving list of subscribed feeds.")

(defvar grc-req-unread-count-url
  (concat greader-base-url
          "api/0/unread-count?all=true&output=json")
  "URL for retrieving unread counts for subscribed  feeds.")

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
  (with-temp-buffer
    (unless no-auth (grc-auth-ensure-authenticated))
    (let ((command (format
                   "%s %s %s -X GET '%s' "
                   grc-curl-program grc-curl-options
                   (if no-auth "" (grc-req-auth-header))
                   (if request
                       (concat endpoint "?" request)
                     endpoint))))
     (shell-command command (current-buffer))
     (goto-char (point-min))
     (cond
      (raw-get (buffer-substring-no-properties (point-min) (point-max)))
      ((looking-at "{") (let ((json-array-type 'list))
                          (json-read)))
      ((looking-at "OK") "OK")
      (t (error "Error fetching: %s?%s\nFull command: %s"
                endpoint request command))))))

(defun grc-req-post-request (endpoint request &optional no-auth)
  (with-temp-buffer
    (unless no-auth (grc-auth-ensure-authenticated))
    (let ((command (format
                    "%s %s %s  -X POST -d '%s' '%s' "
                    grc-curl-program grc-curl-options
                    (if no-auth "" (grc-req-auth-header))
                    request
                    endpoint)))
      (shell-command command (current-buffer))
      (goto-char (point-min))
      (cond
       ((looking-at "{") (let ((json-array-type 'list))
                           (json-read)))
       ((looking-at "OK") "OK")
       (t (error "Error requesting: %s\nFull command: %s " request command))))))

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

(defun grc-req-remote-entries (&optional state)
  (let ((params `(("n"       . "100")
                  ("sharers" . ,(grc-req-sharers-hash))
                  ("client"  . "emacs-grc-client"))))
    (when (string= state "reading-list")
      (aput 'params "xt" "user/-/state/com.google/read"))
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

;; TODO: replace greader stuff
(defun grc-req-subscriptions ()
  (grc-req-get-request grc-req-subscribed-feed-list-url))

(provide 'grc-req)
