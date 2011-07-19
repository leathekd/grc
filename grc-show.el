;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show functions
(defun grc-show-help ()
  ;;TODO
  (interactive)
  )

(defun grc-show-mark-kept-unread (remove)
  (interactive "P")
  (funcall (grc-mark-fn "kept-unread") grc-current-entry remove))

(defun grc-show-mark-starred (remove)
  (interactive "P")
  (funcall (grc-mark-fn "starred") grc-current-entry remove))

(defun grc-show-kill-this-buffer ()
  (interactive)
  (when (get-buffer grc-list-buffer)
    (switch-to-buffer (get-buffer grc-list-buffer))
    (kill-buffer grc-show-buffer)))

(defun grc-show-next-entry ()
  (interactive)
  (let ((entry (cadr (member grc-current-entry grc-entry-cache))))
    (if entry
        (progn
          (grc-show-entry entry)
          (with-current-buffer grc-list-buffer
            (grc-list-refresh)
            (forward-line)))
      (error "No more entries"))))

(defun grc-show-previous-entry ()
  (interactive)
  (let ((entry (cadr (member grc-current-entry (reverse grc-entry-cache)))))
    (if entry
        (progn
          (grc-show-entry entry)
          (with-current-buffer grc-list-buffer
            (grc-list-refresh)
            (forward-line -1)))
      (error "No previous entries"))))

(defun grc-show-view-external ()
  (interactive)
  (grc-view-external grc-current-entry))

(defun grc-show-advance-or-show-next-entry ()
  (interactive)
  (if (eobp)
      (grc-show-next-entry)
    (let ((scroll-error-top-bottom t))
      (scroll-up-command 25))))

(defvar grc-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'grc-show-advance-or-show-next-entry)
    (define-key map "?" 'grc-show-help)
    (define-key map "q" 'grc-show-kill-this-buffer)
    (define-key map "k" 'grc-show-mark-kept-unread)
    (define-key map "s" 'grc-show-mark-starred)
    (define-key map "n" 'grc-show-next-entry)
    (define-key map "p" 'grc-show-previous-entry)
    (define-key map "v" 'grc-show-view-external)
    (when (featurep 'w3m)
      (define-key map (kbd "RET")   'w3m-external-view-this-url)
      (define-key map (kbd "TAB")   'w3m-next-anchor)
      (define-key map (kbd "S-TAB") 'w3m-previous-anchor))
    map)
  "Keymap for \"grc show\" buffers.")
(fset 'grc-show-mode-map grc-show-mode-map)

(defun grc-show-mode ()
  "Major mode for viewing a feed entry in grc

\\{grc-show-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map grc-show-mode-map)
  (setq major-mode 'grc-show-mode
        mode-name "grc-show")
  (setq buffer-read-only t)
  (when (featurep 'w3m)
    (setq w3m-display-inline-images t)))

(provide 'grc-show)
