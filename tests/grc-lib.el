(require 'ert)
(require 'grc-lib)

(ert-deftest test-grc-trim ()
  (should (string= "" (grc-trim " ")))
  (should (string= "before" (grc-trim "
before")))
  (should (string= "after" (grc-trim "after
"))))

(ert-deftest test-grc-list ()
  (equal '("hello") (grc-list "hello"))
  (equal '("hello") (grc-list '("hello"))))

(ert-deftest test-grc-string ()
  (string= "hello" (grc-string "hello"))
  (string= "123" (grc-string 123))
  (string= "(123)" (grc-string '(123))))

(ert-deftest test-grc-flatten ())
(ert-deftest test-grc-get-in ())
(ert-deftest test-grc-sort-by ())
(ert-deftest test-grc-group-by ())
