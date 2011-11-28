(require 'ert)
(require 'grc)

(setq grc-test-parse-json-entry
      '((origin (htmlUrl . "url") (title . "title") (streamId . "id"))
        (annotations ((profileCardParams . "params") (profileId . "profile-id")
                      (userId . "user-id") (author . "author")
                      (content . "content")))
        (likingUsers) (author . "author")
        (summary (content . "content") (direction . "ltr"))
        (alternate ((type . "text/html") (href . "href")))
        (updated . 1313949960) (published . 1313949960)
        (title . "title")
        (categories "/state/com.google/reading-list" "/label/category" "stuff")
        (id . "id") (timestampUsec . "1313949974072640")
        (crawlTimeMsec . "1313949974072")))

(setq grc-test-parse-json-entry-parsed
      '((id . "id") (date . 1313949960) (crawl-date . 1313949974)
        (title . "title") (link . "href") (src-title . "title")
        (src-url . "url") (src-id . "id") (summary . "content")
        (content) (categories "reading-list" "category")))

(ert-deftest grc-parse-get-categories ()
  "Tests extracting and deduping categories from parsed json"
  (let ((entry '((categories . ("/label/foobar"
                                "/state/com.google/foobaz"
                                "stuff"
                                "/state/com.google/foobar")))))
    (should (equal '("foobar" "foobaz") (grc-parse-get-categories entry)))))

(ert-deftest grc-parse-process-entry ()
  "Tests parsing all the used values from the parsed json entry"
  (should (equal grc-test-parse-json-entry-parsed
                 (grc-parse-process-entry grc-test-parse-json-entry))))

(ert-deftest grc-parse-process-response ()
  "Tests parsing all entries in the json"
  (let ((root `((items . (,grc-test-parse-json-entry
                          ,grc-test-parse-json-entry))))
        (result `(,grc-test-parse-json-entry-parsed
                  ,grc-test-parse-json-entry-parsed)))
    (should (equal result  (grc-parse-parse-response root)))))


