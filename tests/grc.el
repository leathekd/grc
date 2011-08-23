(require 'ert)
(require 'grc)

(ert-deftest grc-test-well-documented ()
  (let ((needs-docs nil))
    (mapatoms (lambda (x)
                (when (and (fboundp x)
                           (string-match "^grc" (symbol-name x))
                           (or (not (documentation x t))
                               (string= "" (grc-trim (documentation x t)))))
                  (setq needs-docs (cons (symbol-name x) needs-docs)))))
    (should (eql nil (sort needs-docs 'string<)))))

(ert-deftest grc-test-replace-string ()
  (with-temp-buffer
    (insert "now is the time for all good men\n")
    (insert "to come to the aid of their country")
    (goto-char (point-min))
    (grc-replace-string "the" "teh")
    (should (string= (concat "now is teh time for all good men\n"
                             "to come to teh aid of tehir country")
                     (buffer-string)))))

(ert-deftest grc-test-replace-regexp ()
  (with-temp-buffer
    (insert "now is the time for all good men\n")
    (insert "to come to the aid of their country")
    (goto-char (point-min))
    (grc-replace-regexp "\\bthe\\b" "teh")
    (should (string= (concat "now is teh time for all good men\n"
                             "to come to teh aid of their country")
                     (buffer-string)))))

(ert-deftest grc-test-convert-entities ()
  (with-temp-buffer
    (insert "&amp; &apos; &gt; &lt; &quot;")
    (grc-convert-entities)
    (should (string= "& ' > < \"" (buffer-string)))))

(ert-deftest grc-test-trim-left-in-buffer ()
  (with-temp-buffer
    (insert "   hello\nworld,\n\thow's it\n\ngoing")
    (grc-trim-left-in-buffer)
    (should (string= "hello\nworld,\nhow's it\n\ngoing" (buffer-string)))))

(ert-deftest grc-test-normalize-newlines ()
  (with-temp-buffer
    (insert "hello\n\n\n\nworld,\nhow's\n\n\nit\n\ngoing")
    (grc-normalize-newlines)
    (should (string= "hello\n\nworld,\nhow's\n\nit\n\ngoing" (buffer-string)))))

(ert-deftest grc-test-strip-html ()
  (with-temp-buffer
    (insert "<html><head></head><body>")
    (insert "<div>Hello <b>THERE</b></div>\n")
    (insert "<p>Hello <a href=\"#\">THERE</a></a>\n")
    (insert "</body></html>")
    (grc-strip-html)
    (should (string= "Hello THERE\nHello THERE\n"
                     (buffer-string)))))

(ert-deftest grc-test-footnote-anchors ())
(ert-deftest grc-test-clean-buffer ())
(ert-deftest grc-test-clean-text ())
(ert-deftest grc-test-prepare-text ())
(ert-deftest grc-test-truncate-text ())
(ert-deftest grc-test-format-categories ())
(ert-deftest grc-test-title-for-printing ())
(ert-deftest grc-test-keywords ())
(ert-deftest grc-test-read-state ())
(ert-deftest grc ())
(ert-deftest grc-test-logout ())
(ert-deftest grc-test-kill-this-buffer ())
(ert-deftest grc-test-help ())
(ert-deftest grc-test-entry-index ())
(ert-deftest grc-test-add-category ())
(ert-deftest grc-test-remove-category ())
(ert-deftest grc-test-mark-fn ())
(ert-deftest grc-test-mark-read ())
(ert-deftest grc-test-mark-kept-unread ())
(ert-deftest grc-test-mark-starred ())
(ert-deftest grc-test-view-external ())
(ert-deftest grc-test-shared-p ())
(ert-deftest grc-test-share ())
(ert-deftest grc-test-add-comment ())
