;;; grc-auth.el --- Google Reader Mode for Emacs
;;
;; Copyright (c) 2011 David Leatherman
;;
;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/grc
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains code to authenticate against Google's Oauth2
;; implementation.  In addition it will cache the refresh token on
;; disk and keep the access and action tokens up-to-date.

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
  "Verify that the client id and client secret are set"
  (when (or (and (string= "" grc-auth-client-id)
                   (string= "" grc-auth-client-secret))
              (and (null grc-auth-client-id)
                   (null grc-auth-client-secret)))
    (error (concat "Missing Client Id and/or Client Secret."
                   "See https://github.com/leathekd/grc for details."))))

(defun grc-auth-get-auth-code ()
  "Opens the browser to get the authorization code from Google."
  (grc-auth-verify-config)
  (browse-url (format grc-auth-authorization-code-url grc-auth-client-id))
  (read-from-minibuffer "Authorization Code: "))

(defun grc-auth-save-refresh-token (refresh-token)
  "Saves the refresh token to grc-auth-refresh-token-file"
  (with-current-buffer (get-buffer-create "*grc refresh token*")
    (erase-buffer)
    (insert refresh-token)
    (write-region (point-min) (point-max) grc-auth-refresh-token-file)
    (kill-buffer (current-buffer))))

(defun grc-auth-restore-refresh-token ()
  "Reads in the refresh token from grc-auth-refresh-token-file"
  (when (and (file-exists-p grc-auth-refresh-token-file)
             (< 0 (nth 7 (file-attributes grc-auth-refresh-token-file))))
    (with-current-buffer (find-file-noselect grc-auth-refresh-token-file t)
      (setq grc-auth-refresh-token
            (buffer-substring-no-properties (point-min) (point-max)))
      (kill-buffer (current-buffer)))
    grc-auth-refresh-token))

(defun grc-auth-set-access-token (resp)
  "Caches the short-lived access token from the server response"
  (setq grc-auth-access-token
        `((token   . ,(aget resp 'access_token t))
          (expires . ,(- (+ (aget resp 'expires_in)
                            (float-time))
                         300)))))

(defun grc-auth-get-access-token ()
  "Gets the token value from the saved access token alist"
  (aget grc-auth-access-token 'token))

(defun grc-auth-set-refresh-token (resp)
  "Caches and saves the refresh token from the server response"
  (setq grc-auth-refresh-token (aget resp 'refresh_token))
  (grc-auth-save-refresh-token (aget resp 'refresh_token)))

(defun grc-auth-request-refresh-token ()
  "Make the request to Google to retrieve the refresh token"
  (grc-auth-verify-config)
  (let* ((auth-code (grc-auth-get-auth-code))
         (resp (grc-req-post-request
                "https://accounts.google.com/o/oauth2/token"
                `(("client_id"     . ,grc-auth-client-id)
                  ("client_secret" . ,grc-auth-client-secret)
                  ("code"          . ,auth-code)
                  ("redirect_uri"  . "urn:ietf:wg:oauth:2.0:oob")
                  ("grant_type"    . "authorization_code"))
                t)))
    ;; TODO: handle errors
    (grc-auth-set-access-token resp)
    (grc-auth-set-refresh-token resp)))

(defun grc-auth-request-action-token ()
  "Fetch the action token, this is the T attribute in many requests"
  `((token   . ,(grc-req-get-request
                 (concat grc-req-base-url "api/0/token")
                 nil nil t))
    (expires . ,(+ (* 60 25) (float-time)))))

(defun grc-auth-set-action-token ()
  "Cache the token from the action token alist"
  (setq grc-auth-action-token (grc-auth-request-action-token)))

(defun grc-auth-get-action-token ()
  "Return or refresh (based on the expiry timestamp) the action token"
  (when (or (null grc-auth-action-token)
            (<= (aget grc-auth-action-token 'expires) (floor (float-time))))
    (grc-auth-set-action-token))
  (aget grc-auth-action-token 'token t))

(defun grc-auth-get-refresh-token ()
  "Return the refresh token from the file or fetch it if the file doesn't exist"
  (or grc-auth-refresh-token
      (grc-auth-restore-refresh-token)
      (grc-auth-request-refresh-token)))

(defun grc-auth-re-authenticate ()
  "Get the refresh and access tokens as needed based on expiry"
  (let ((refresh-token (grc-auth-get-refresh-token)))
    ;; on first call, get-refresh-token will set everything
    (if (and grc-auth-access-token
             (<= (floor (float-time)) (aget grc-auth-access-token 'expires)))
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
  "Make sure that all of the tokens are available for requests"
  (if (or (null grc-auth-access-token)
          (<= (floor (aget grc-auth-access-token 'expires))
              (floor (float-time))))
      (grc-auth-re-authenticate)
    grc-auth-access-token))

(defun grc-auth-logout ()
  "Scrub the cached variables and delete the refresh token from disk"
  (when (file-exists-p grc-auth-refresh-token-file)
    (delete-file grc-auth-refresh-token-file))
  (setq grc-auth-access-token nil)
  (setq grc-auth-refresh-token nil)
  (setq grc-auth-action-token nil))

(provide 'grc-auth)
;;; grc-auth.el ends here
