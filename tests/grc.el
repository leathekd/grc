(require 'ert)
(require 'grc)

(ert-deftest grc-well-documented ()
  (let ((needs-docs nil))
    (mapatoms (lambda (x)
                (when (and (fboundp x)
                           (string-match "^grc" (symbol-name x))
                           (or (not (documentation x t))
                               (string= "" (grc-trim (documentation x t)))))
                  (setq needs-docs (cons (symbol-name x) needs-docs)))))
    (should (eql nil (sort needs-docs 'string<)))))


