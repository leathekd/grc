;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting keywords
(defface grc-highlight-nick-base-face
  '((t nil))
  "Base face used for highlighting nicks in erc. (Before the nick
color is added)"
  :group 'grc-faces)

(defvar grc-highlight-face-table
  (make-hash-table :test 'equal)
  "The hash table that contains unique grc faces.")

(defun grc-hexcolor-luminance (color)
  "Returns the luminance of color COLOR. COLOR is a string \(e.g.
\"#ffaa00\", \"blue\"\) `color-values' accepts. Luminance is a
value of 0.299 red + 0.587 green + 0.114 blue and is always
between 0 and 255."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (car (cdr values)))
         (b (car (cdr (cdr values)))))
    (floor (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)) 256)))

(defun grc-invert-color (color)
  "Returns the inverted color of COLOR."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (car (cdr values)))
         (b (car (cdr (cdr values)))))
    (format "#%04x%04x%04x"
            (- 65535 r) (- 65535 g) (- 65535 b))))

(defun grc-highlight-keywords (keywords)
  "Searches for nicknames and highlights them. Uses the first
twelve digits of the MD5 message digest of the nickname as
color (#rrrrggggbbbb)."
  (let (bounds word color new-kw-face kw (case-fold-search nil))
    (while keywords
      (goto-char (point-min))
      (setq kw (car keywords))
      (while (search-forward kw nil t)
        (setq bounds `(,(point) . ,(- (point) (length kw))))
        (setq word (buffer-substring-no-properties
                    (car bounds) (cdr bounds)))
        (setq new-kw-face (gethash word grc-highlight-face-table))
        (unless new-kw-face
          (setq color (concat "#" (substring (md5 (downcase word)) 0 12)))
          (if (equal (cdr (assoc 'background-mode (frame-parameters))) 'dark)
              ;; if too dark for background
              (when (< (grc-hexcolor-luminance color) 85)
                (setq color (grc-invert-color color)))
            ;; if to bright for background
            (when (> (grc-hexcolor-luminance color) 170)
              (setq color (grc-invert-color color))))
          (setq new-kw-face (make-symbol (concat "grc-highlight-nick-"
                                                 word "-face")))
          (copy-face 'grc-highlight-nick-base-face new-kw-face)
          (set-face-foreground new-kw-face color)
          (puthash word new-kw-face grc-highlight-face-table))
        (put-text-property (car bounds) (cdr bounds) 'face new-kw-face))
      (setq keywords (cdr keywords)))))

(provide 'grc-highlight)
