;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auth functions - generally oauth2
(require 'grc)
(require 'grc-req)

(defcustom grc-auth-client-id ""
  "The client id provided by Google"
  :group 'grc
  :type 'string)

(defcustom grc-auth-client-secret ""
  "The client secret provided by Google"
  :group 'grc
  :type 'string)

(defcustom grc-auth-refresh-token-file "~/.grc-refresh-token"
  "The file where grc will cache the Google Oauth refresh token"
  :group 'grc
  :type 'file)

(defvar grc-auth-authorization-code-url
  (concat "https://accounts.google.com/o/oauth2/auth?"
          "client_id=%s"
          "&redirect_uri=urn:ietf:wg:oauth:2.0:oob"
          "&scope=http://www.google.com/reader/api"
          "&response_type=code"))

(defvar grc-auth-access-token nil)
(defvar grc-auth-refresh-token nil)
(defvar grc-auth-action-token nil)

(defun grc-auth-verify-config ()
  (unless (and grc-auth-client-id grc-auth-client-secret)
    (error "Missing Client Id and/or Client Secret.  See README for info.")))

(defun grc-auth-get-auth-code ()
  (grc-auth-verify-config)
  (browse-url (format grc-auth-authorization-code-url grc-auth-client-id))
  (read-from-minibuffer "Authorization Code: "))

(defun grc-auth-save-refresh-token (refresh-token)
  (with-current-buffer (get-buffer-create "*grc refresh token*")
    (erase-buffer)
    (insert refresh-token)
    (write-region (point-min) (point-max) grc-auth-refresh-token-file)
    (kill-buffer (current-buffer))))

(defun grc-auth-restore-refresh-token ()
  (when (and (file-exists-p grc-auth-refresh-token-file)
             (< 0 (nth 7 (file-attributes grc-auth-refresh-token-file))))
    (with-current-buffer (find-file-noselect grc-auth-refresh-token-file t)
      (setq grc-auth-refresh-token
            (buffer-substring-no-properties (point-min) (point-max)))
      (kill-buffer (current-buffer)))
    grc-auth-refresh-token))

(defun grc-auth-set-access-token (resp)
  (setq grc-auth-access-token
        `((token   . ,(aget resp 'access_token t))
          (expires . ,(seconds-to-time (+ (aget resp 'expires_in)
                                          (float-time)))))))

(defun grc-auth-get-access-token () (aget grc-auth-access-token 'token))

(defun grc-auth-set-refresh-token (resp)
  (setq grc-auth-refresh-token (aget resp 'refresh_token))
  (grc-auth-save-refresh-token (aget resp 'refresh_token)))

(defun grc-auth-request-refresh-token ()
  (grc-auth-verify-config)
  (let* ((auth-code (grc-auth-get-auth-code))
         (resp (grc-req-post-request
                "https://accounts.google.com/o/oauth2/token"
                (grc-req-format-params
                 `(("client_id"     . ,grc-auth-client-id)
                   ("client_secret" . ,grc-auth-client-secret)
                   ("code"          . ,auth-code)
                   ("redirect_uri"  . "urn:ietf:wg:oauth:2.0:oob&")
                   ("grant_type"    . "authorization_code")))
                t)))
    ;; TODO: handle errors
    (grc-auth-set-access-token resp)
    (grc-auth-set-refresh-token resp)))

(defun grc-auth-request-action-token ()
  `((token   . ,(grc-req-get-request
                 (concat grc-req-base-url "api/0/token")
                 nil nil t))
    (expires . ,(seconds-to-time (+ (* 60 25) (float-time))))))

(defun grc-auth-set-action-token ()
  (setq grc-auth-action-token (grc-auth-request-action-token)))

(defun grc-auth-get-action-token ()
  (when (or (null grc-auth-action-token)
            (time-less-p (aget grc-auth-action-token 'expires) (current-time)))
    (grc-auth-set-action-token))
  (aget grc-auth-action-token 'token t))

(defun grc-auth-get-refresh-token ()
  (or grc-auth-refresh-token
      (grc-auth-restore-refresh-token)
      (grc-auth-request-refresh-token)))

(defun grc-auth-re-authenticate ()
  (let ((refresh-token (grc-auth-get-refresh-token)))
    ;; on first call, get-refresh-token will set everything
    (if (and grc-auth-access-token
             (time-less-p (current-time) (aget grc-auth-access-token 'expires)))
        grc-auth-access-token
      (grc-auth-set-access-token
       (grc-req-post-request
        "https://accounts.google.com/o/oauth2/token"
        (grc-req-format-params
         `(("client_id"     . ,grc-auth-client-id)
           ("client_secret" . ,grc-auth-client-secret)
           ("refresh_token" . ,refresh-token)
           ("grant_type"    . "refresh_token")))
        t)))))

(defun grc-auth-ensure-authenticated ()
  (if (or (null grc-auth-access-token)
          (time-less-p (aget grc-auth-access-token 'expires) (current-time)))
      (grc-auth-re-authenticate)
    grc-auth-access-token))

(provide 'grc-auth)
