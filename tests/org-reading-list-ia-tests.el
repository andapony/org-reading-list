;;; org-reading-list-ia-tests.el --- Tests for IA discovery -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-reading-list)
(require 'org-reading-list-ia)

(require 'cl-lib)


(ert-deftest org-reading-list-ia-test-title-tokens ()
  (should (equal (org-reading-list-ia--title-tokens
                  "Reminiscences and Incidents of Early Days")
                 '("reminiscences" "incidents" "early" "days")))
  (should (null (org-reading-list-ia--title-tokens nil))))

(ert-deftest org-reading-list-ia-test-search-url ()
  (let ((org-reading-list-ia-max-candidates 25)
        (url (org-reading-list-ia--search-url "Soulé, Frank" "The Annals" 10)))
    (should (string-prefix-p
             "https://archive.org/advancedsearch.php?q=" url))
    (should (string-match-p "rows=10" url))
    (should (string-match-p "output=json" url))
    ;; q is URL-encoded; decode and check the assembled query.
    (let ((q (url-unhex-string
              (progn (string-match "q=\\([^&]+\\)" url) (match-string 1 url)))))
      (should (string-match-p "creator:(soule)" q))
      (should (string-match-p "title:(annals)" q))
      (should (string-match-p "mediatype:texts" q)))))

(ert-deftest org-reading-list-ia-test-search-parses-docs ()
  (cl-letf (((symbol-function 'org-reading-list--fetch-json)
             (lambda (_url)
               '((response
                  . ((numFound . 2)
                     (docs
                      . (((identifier . "annals1855")
                          (title . "The Annals of San Francisco")
                          (creator . "Soulé, Frank")
                          (year . "1855")
                          (publisher . "Appleton")
                          (collection . ("americana" "googlebooks")))
                         ((identifier . "annals1966goog")
                          (title . "The Annals of San Francisco")
                          (creator . ("Soulé, Frank" "Gihon, John"))
                          (year . "1966"))))))))))
    (let ((rows (org-reading-list-ia--search "Soulé, Frank" "Annals" 10)))
      (should (= (length rows) 2))
      (should (equal (plist-get (nth 0 rows) :identifier) "annals1855"))
      (should (equal (plist-get (nth 0 rows) :year) "1855"))
      (should (equal (plist-get (nth 0 rows) :collection) "americana; googlebooks"))
      ;; A list-valued creator is joined.
      (should (equal (plist-get (nth 1 rows) :creator)
                     "Soulé, Frank; Gihon, John")))))


(provide 'org-reading-list-ia-tests)
;;; org-reading-list-ia-tests.el ends here
