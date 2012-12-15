(require 'ert)
(require 'grc-lib)

(ert-deftest test-grc-trim ()
  (should (string= "" (grc-trim " ")))
  (should (string= "before" (grc-trim "
before")))
  (should (string= "after" (grc-trim "after
"))))

(ert-deftest test-grc-list ()
  (should (equal '("hello") (grc-list "hello")))
  (should (equal '("hello") (grc-list '("hello")))))

(ert-deftest test-grc-string ()
  (should (string= "hello" (grc-string "hello")))
  (should (string= "123" (grc-string 123)))
  (should (string= "(123)" (grc-string '(123)))))

(ert-deftest test-grc-flatten ()
  (should (equal '(a b c 1 2 3) (grc-flatten '((a b c) (1 2 3)))))
  (should (equal '(a b c 1 2 3) (grc-flatten '((a (b c)) (1 (2 (3))))))))

(ert-deftest test-grc-get-in ()
  (let ((alst '((a . "a")
                (b . ((c . "c")))
                (d . ((e . ((f . "f"))))))))
    (should (string= "a" (grc-get-in alst '(a))))
    (should (string= "c" (grc-get-in alst '(b c))))
    (should (string= "f" (grc-get-in alst '(d e f))))
    (should (eq nil (grc-get-in alst '(b z))))))

(ert-deftest test-grc-sort-by ()
  (let ((lst '(((str . "a") (num . 10))
               ((str . "b") (num . 1))
               ((str . "c") (num . 16))
               ((str . "c") (num . 2)))))
    (should (string= "a" (cdr (assoc 'str (first (grc-sort-by 'str lst))))))
    (should (string= "c" (cdr (assoc 'str
                                     (car (last (grc-sort-by 'str lst)))))))
    (should (string= "c" (cdr (assoc 'str (first (grc-sort-by 'str lst t))))))
    (should (= 2 (cdr (assoc 'num (car (last (grc-sort-by 'str lst)))))))
    (should (= 16 (cdr (assoc 'num
                              (car (last (grc-sort-by 'str lst nil 'num)))))))
    (should (= 16 (cdr (assoc 'num (first (grc-sort-by 'str lst t 'num))))))))

(ert-deftest test-grc-group-by ()
  (should (equal (grc-group-by 'age '(((name . "fred") (age . 25))
                                      ((name . "barney") (age . 25))
                                      ((name . "wilma") (age . 22))))
                 '((22 ((name . "wilma")  (age . 22)))
                   (25 ((name . "barney") (age . 25))
                       ((name . "fred")   (age . 25)))))))
