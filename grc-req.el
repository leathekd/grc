;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google reader requests
(defun grc-req-ensure-authenticated ()
  (when (or
         (null (g-auth-token greader-auth-handle))
         (null (g-auth-cookie-alist greader-auth-handle))
         (time-less-p (g-auth-lifetime greader-auth-handle)
                      (time-since (g-auth-timestamp greader-auth-handle))))
    (greader-re-authenticate)))

(defun grc-req-get-request (endpoint &optional request)
  (grc-req-ensure-authenticated)
  (with-temp-buffer
    (let ((shell-file-name grc-shell-file-name))
      (shell-command
       (format
        "%s %s %s -X GET '%s' "
        g-curl-program g-curl-common-options
        (g-authorization greader-auth-handle)
        (if (null request)
            endpoint
          (concat endpoint "?" request)))
       (current-buffer)))
    (goto-char (point-min))
    (cond
     ((looking-at "{") (let ((json-array-type 'list))
                         (json-read)))
     (t (error "Error %s?%s: " endpoint request)))))

(defun grc-req-post-request (endpoint request)
  (grc-req-ensure-authenticated)
  (with-temp-buffer
    (let ((shell-file-name grc-shell-file-name))
      (shell-command
       (format
        "%s %s %s  -X POST -d '%s' '%s' "
        g-curl-program g-curl-common-options
        (g-authorization greader-auth-handle)
        request
        endpoint)
       (current-buffer)))
    (goto-char (point-min))
    (cond
     ((looking-at "OK") (message "OK"))
     (t (error "Error %s: " request)))))

(defun grc-req-send-edit-request (request)
  (grc-req-post-request
   "http://www.google.com/reader/api/0/edit-tag?client=emacs-g-client"
   request))

(defvar grc-req-stream-url-pattern
  "https://www.google.com/reader/api/0/stream/contents/%s")

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

;; TODO: doit
(defun grc-req-sharers-hash ()
  "CNeog8beARCElIvC_AoQ6IGRkbgCEKvg1JcWELrV6PyrAxCIz_rcggIQo5z87ZcBENe5rra6Ag")

(defun grc-req-tag-request (entry tag remove)
  (format "%s=user/-/state/com.google/%s&async=true&s=%s&i=%s&T=%s"
          (if remove "r" "a")
          tag
          (aget entry 'feed)
          (aget entry 'id)
          (g-auth-token greader-auth-handle)))

(defun grc-req-total-unread-count ()
  (reduce (lambda (x y)
            (let ((yval (cdr (assoc 'count y))))
              (if (> x yval) x yval)))
          (greader-unread-count) :initial-value 0))

(defun grc-req-subscriptions ()
  (let ((shell-file-name grc-shell-file-name))
    (greader-subscriptions)))

(provide 'grc-req)
