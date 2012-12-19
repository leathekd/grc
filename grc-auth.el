;;; grc-auth.el --- Google Reader Mode for Emacs
;;
;; Copyright (c) 2012 David Leatherman
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
(defvar grc-auth-url "https://accounts.google.com/o/oauth2/auth")
(defvar grc-token-url "https://accounts.google.com/o/oauth2/token")
(defvar grc-resource-url "https://www.google.com/reader/api")
(defvar grc-action-token-url "http://www.google.com/reader/api/0/token")
(defvar grc-token-file (concat user-emacs-directory "grc.plstore"))
(defvar grc-token nil)

(defun grc-auth ()
  (flet ((plstore-progress-callback-function (&rest _))) ;; No messages, please
    (let* ((plstore (plstore-open grc-token-file))
           (id "grc-auth-client")
           (plist (cdr (plstore-get plstore id)))
           (client-id (plist-get plist :client-id))
           (client-secret (plist-get plist :client-secret)))
      (or plist
          (let*
              ((client-id (read-string "Enter your Google OAuth2 client id: "))
               (client-secret (read-string
                               "Enter your Google OAuth2 client secret: "))
               (token (oauth2-auth grc-auth-url grc-token-url
                                   client-id client-secret grc-resource-url)))
            (if (oauth2-token-access-token token)
                (let ((plist `(:client-id
                               ,client-id
                               :client-secret
                               ,client-secret
                               :refresh-token
                               ,(oauth2-token-refresh-token token))))
                  (plstore-put plstore id nil plist)
                  (plstore-save plstore)
                  plist)
              (error "Auth failed. client id or secret may be wrong")))))))

(defun grc-refresh-access-token (callback)
  "Refresh the access token from Google. Updates grc-token."
  (let ((grapnel-options grc-req-curl-options))
    (grapnel-retrieve-url
     grc-token-url
     `((success . (lambda (response headers)
                    (setq grc-token
                          (plist-put
                           grc-token :access-token
                           (cdr (assoc 'access_token
                                       (json-read-from-string response)))))
                    (funcall ',callback)))
       (failure . (lambda (response headers)
                    (error "Failed with: %s for %s"
                           (cadr (assoc "response-code" headers))
                           ,grc-token-url)))
       (error . (lambda (resp exit-code) (error "Error: %s %s"
                                           response exit-code))))
     "POST" nil
     `(("client_id" . ,(plist-get grc-token :client-id))
       ("client_secret" . ,(plist-get grc-token :client-secret))
       ("refresh_token" . ,(plist-get grc-token :refresh-token))
       ("grant_type" . "refresh_token")))))

(defun grc-refresh-action-token (callback)
  "Refresh the action token from Google. Updates grc-token."
  (let ((grapnel-options grc-req-curl-options))
    (grapnel-retrieve-url
     grc-action-token-url
     `((success . (lambda (response headers)
                    (setq grc-token
                          (plist-put grc-token :action-token response))
                    (funcall ',callback)))
       (failure . (lambda (response headers)
                    (error "grc-refresh-action-token failed with: %s for %s"
                           (cadr (assoc "response-code" headers))
                           ,grc-action-token-url)))
       (error . (lambda (resp exit-code) (error "Error: %s %s"
                                           response exit-code))))
     nil nil nil
     `(("Authorization" .
        ,(concat "OAuth " (plist-get grc-token :access-token)))))))

(provide 'grc-auth)
;;; grc-auth.el ends here
