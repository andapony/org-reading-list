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

(ert-deftest org-reading-list-ia-test-match-guard ()
  (let ((good '(:creator "Soulé, Frank" :title "The Annals of San Francisco"))
        (wrong-author '(:creator "Justice, Donald"
                        :title "The Annals of San Francisco"))
        (thin '(:creator "Soulé, Frank" :title "Annals")))
    (should (org-reading-list-ia--candidate-match-p
             good "Soulé, Frank" "The Annals of San Francisco"))
    ;; Same title, wrong author — rejected (the Donald-Justice lesson).
    (should-not (org-reading-list-ia--candidate-match-p
                 wrong-author "Soulé, Frank" "The Annals of San Francisco"))
    ;; Too few overlapping title tokens — rejected.
    (should-not (org-reading-list-ia--candidate-match-p
                 thin "Soulé, Frank" "The Annals of San Francisco"))))

(ert-deftest org-reading-list-ia-test-metadata-google-scan ()
  (cl-letf (((symbol-function 'org-reading-list--fetch-json)
             (lambda (_url)
               '((metadata
                  . ((identifier . "annals00souggoog")
                     (contributor . "Google")
                     (collection . ("americana" "googlebooks"))
                     (ppi . "300")
                     (imagecount . "560")
                     (openlibrary_edition . "OL1234M")
                     (scandate . "20080101")))))))
    (let ((md (org-reading-list-ia--metadata "annals00souggoog")))
      (should (eq (plist-get md :google) t))
      (should (eq (plist-get md :open) t))
      (should (= (plist-get md :ppi) 300))
      (should (= (plist-get md :imagecount) 560))
      (should (equal (plist-get md :olid) "OL1234M"))
      (should (eq (plist-get md :scan) t)))))

(ert-deftest org-reading-list-ia-test-metadata-lending-and-stub ()
  ;; Lending item: access-restricted-item true → :open nil but a scan.
  (cl-letf (((symbol-function 'org-reading-list--fetch-json)
             (lambda (_url)
               '((metadata . ((imagecount . "200")
                              (access-restricted-item . "true")
                              (scandate . "20100101")))))))
    (let ((md (org-reading-list-ia--metadata "x")))
      (should (eq (plist-get md :open) nil))
      (should (eq (plist-get md :scan) t))
      (should (eq (plist-get md :google) nil))))
  ;; Catalog stub: no images, no scandate → nil (no readable scan).
  (cl-letf (((symbol-function 'org-reading-list--fetch-json)
             (lambda (_url)
               '((metadata . ((identifier . "stub")
                              (mediatype . "texts")))))))
    (should (null (org-reading-list-ia--metadata "stub")))))

(ert-deftest org-reading-list-ia-test-year-int ()
  (should (= (org-reading-list-ia--year-int "1855") 1855))
  (should (= (org-reading-list-ia--year-int "[1845]") 1845))
  (should (null (org-reading-list-ia--year-int nil))))

(ert-deftest org-reading-list-ia-test-rank ()
  (let* ((rows (list
                '(:id "c" :year-int 1933 :google nil :open t :ppi 400)
                '(:id "a" :year-int 1855 :google t   :open t :ppi 150)
                '(:id "b" :year-int 1855 :google nil :open t :ppi 300)
                '(:id "d" :year-int nil  :google nil :open t :ppi 500)))
         (ranked (org-reading-list-ia--rank rows)))
    ;; Earliest year first; non-Google breaks the 1855 tie; year-less last.
    (should (equal (mapcar (lambda (r) (plist-get r :id)) ranked)
                   '("b" "a" "c" "d")))))

(ert-deftest org-reading-list-ia-test-editions-pipeline ()
  (cl-letf (((symbol-function 'org-reading-list-ia--search)
             (lambda (_a _t _rows)
               (list '(:identifier "i1855" :title "The Annals of San Francisco"
                       :creator "Soulé, Frank" :year "1855")
                     '(:identifier "iwrong" :title "The Annals of San Francisco"
                       :creator "Justice, Donald" :year "1850")
                     '(:identifier "i1966" :title "The Annals of San Francisco"
                       :creator "Soulé, Frank" :year "1966")
                     '(:identifier "istub" :title "The Annals of San Francisco"
                       :creator "Soulé, Frank" :year "1900"))))
            ((symbol-function 'org-reading-list-ia--metadata)
             (lambda (id)
               (pcase id
                 ("i1855" '(:ppi 300 :imagecount 500 :open t :google nil
                            :olid "OL1M" :scan t))
                 ("i1966" '(:ppi 150 :imagecount 400 :open t :google t
                            :olid nil :scan t))
                 ("istub" nil)             ; no readable scan → dropped
                 (_ (error "unexpected metadata fetch: %s" id))))))
    (let* ((res (org-reading-list-ia--editions
                 "Soulé, Frank" "The Annals of San Francisco"))
           (rows (plist-get res :rows)))
      ;; Wrong-author filtered before metadata; stub dropped; 1855 first.
      (should (equal (mapcar (lambda (r) (plist-get r :identifier)) rows)
                     '("i1855" "i1966")))
      (should (= (plist-get (car rows) :year-int) 1855))
      (should (null (plist-get res :truncated))))))

(provide 'org-reading-list-ia-tests)
;;; org-reading-list-ia-tests.el ends here
