;;; org-reading-list-ia-tests.el --- Tests for IA discovery -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-reading-list)
(require 'org-reading-list-ia)

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

(provide 'org-reading-list-ia-tests)
;;; org-reading-list-ia-tests.el ends here
