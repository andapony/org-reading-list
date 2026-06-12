;;; org-reading-list-tests.el --- Tests for org-reading-list -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Offline unit tests for the pure functions: no network access.
;; Run with:  make test
;; or:        emacs -Q --batch -L . -l tests/org-reading-list-tests.el \
;;                  -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-reading-list)

;;;; Bibkey normalization

(ert-deftest org-reading-list-test-bibkey-isbn13 ()
  (should (equal (org-reading-list--bibkey "9780252066313")
                 "ISBN:9780252066313")))

(ert-deftest org-reading-list-test-bibkey-isbn-hyphens ()
  (should (equal (org-reading-list--bibkey "978-0-252-06631-3")
                 "ISBN:9780252066313")))

(ert-deftest org-reading-list-test-bibkey-isbn10-x ()
  (should (equal (org-reading-list--bibkey "097522980X")
                 "ISBN:097522980X")))

(ert-deftest org-reading-list-test-bibkey-olid-bare ()
  (should (equal (org-reading-list--bibkey "OL5851208M")
                 "OLID:OL5851208M")))

(ert-deftest org-reading-list-test-bibkey-olid-url-encoded ()
  ;; Work id present too; the edition id must win.
  (should (equal (org-reading-list--bibkey
                  "https://openlibrary.org/works/OL1618059W/Mountains_and_molehills?edition=key%3A/books/OL5851208M")
                 "OLID:OL5851208M")))

(ert-deftest org-reading-list-test-bibkey-prefixed-passthrough ()
  (should (equal (org-reading-list--bibkey "LCCN:62011340")
                 "LCCN:62011340")))

(ert-deftest org-reading-list-test-bibkey-work-id-rejected ()
  (should-error (org-reading-list--bibkey
                 "https://openlibrary.org/works/OL1618059W")
                :type 'user-error))

(ert-deftest org-reading-list-test-bibkey-garbage-rejected ()
  (should-error (org-reading-list--bibkey "not a book")
                :type 'user-error))

;;;; Name inversion

(ert-deftest org-reading-list-test-invert-simple ()
  (should (equal (org-reading-list--invert-name "Roger W. Lotchin")
                 "Lotchin, Roger W.")))

(ert-deftest org-reading-list-test-invert-suffix ()
  (should (equal (org-reading-list--invert-name "Martin Luther King Jr.")
                 "King, Jr., Martin Luther")))

(ert-deftest org-reading-list-test-invert-already-inverted ()
  (should (equal (org-reading-list--invert-name "Lotchin, Roger W.")
                 "Lotchin, Roger W.")))

(ert-deftest org-reading-list-test-invert-mononym ()
  (should (equal (org-reading-list--invert-name "Voltaire") "Voltaire")))

;;;; Tag normalization

(ert-deftest org-reading-list-test-tagify-basic ()
  (should (equal (org-reading-list--tagify "San Francisco (Calif.)")
                 "san_francisco_calif")))

(ert-deftest org-reading-list-test-tagify-trailing-punct ()
  (should (equal (org-reading-list--tagify "History.") "history")))

(ert-deftest org-reading-list-test-tagify-overlong-nil ()
  (should-not (org-reading-list--tagify (make-string 60 ?x))))

;;;; Cite keys

(ert-deftest org-reading-list-test-citekey-base ()
  (should (equal (org-reading-list--citekey-base "Lotchin, Roger W." "1997")
                 "lotchin1997")))

(ert-deftest org-reading-list-test-citekey-base-anon ()
  (should (equal (org-reading-list--citekey-base nil "1855") "anon1855")))

(ert-deftest org-reading-list-test-citekey-unique-suffixing ()
  (should (equal (org-reading-list--citekey-unique
                  "smith2020" '("smith2020" "smith2020a"))
                 "smith2020b"))
  (should (equal (org-reading-list--citekey-unique "smith2020" '())
                 "smith2020")))

;;;; MARC extraction (hand-built DOMs, no network)

(defconst org-reading-list-test--marc-record
  '(record nil
           (datafield ((tag . "010") (ind1 . " ") (ind2 . " "))
                      (subfield ((code . "a")) "   62011340 "))
           (datafield ((tag . "020") (ind1 . " ") (ind2 . " "))
                      (subfield ((code . "a")) "9781984882004")
                      (subfield ((code . "q")) "ebook"))
           (datafield ((tag . "035") (ind1 . " ") (ind2 . " "))
                      (subfield ((code . "a")) "(DLC)in00024365782"))
           (datafield ((tag . "035") (ind1 . " ") (ind2 . " "))
                      (subfield ((code . "a")) "(OCoLC)1384557"))
           (datafield ((tag . "050") (ind1 . "0") (ind2 . "0"))
                      (subfield ((code . "a")) "F865")
                      (subfield ((code . "b")) ".M3 1962"))
           (datafield ((tag . "650") (ind1 . " ") (ind2 . "0"))
                      (subfield ((code . "a")) "Gold mines and mining")
                      (subfield ((code . "z")) "California")
                      (subfield ((code . "y")) "19th century"))
           (datafield ((tag . "651") (ind1 . " ") (ind2 . "0"))
                      (subfield ((code . "a")) "San Francisco (Calif.)")
                      (subfield ((code . "x")) "History")))
  "A minimal MARC record DOM for extraction tests.")

(ert-deftest org-reading-list-test-marc-field-010 ()
  (should (equal (org-reading-list--marc-field
                  org-reading-list-test--marc-record "010")
                 "62011340")))

(ert-deftest org-reading-list-test-marc-field-050-joined ()
  (should (equal (org-reading-list--marc-field
                  org-reading-list-test--marc-record "050" "a" "b")
                 "F865 .M3 1962")))

(ert-deftest org-reading-list-test-marc-oclc-skips-dlc ()
  (should (equal (org-reading-list--marc-oclc
                  org-reading-list-test--marc-record)
                 "1384557")))

(ert-deftest org-reading-list-test-marc-isbns ()
  (should (equal (org-reading-list--marc-isbns
                  org-reading-list-test--marc-record "a")
                 '("9781984882004"))))

(ert-deftest org-reading-list-test-marc-subject-tags ()
  (should (equal (org-reading-list--marc-subject-tags
                  (list org-reading-list-test--marc-record))
                 '("gold_mines_and_mining" "california" "19th_century"
                   "san_francisco_calif" "history"))))

(ert-deftest org-reading-list-test-loc-records-isbn-preference ()
  (let* ((ebook org-reading-list-test--marc-record)
         (print-rec
          '(record nil
                   (datafield ((tag . "020"))
                              (subfield ((code . "a")) "9781984881991"))))
         (dom `(zs:searchRetrieveResponse nil
                (zs:records nil
                            (zs:record nil (zs:recordData nil ,ebook))
                            (zs:record nil (zs:recordData nil ,print-rec))))))
    ;; Querying the print ISBN should sort the print record first.
    (should (equal (car (org-reading-list--loc-records dom "9781984881991"))
                   print-rec))
    ;; Without an ISBN, response order stands.
    (should (equal (car (org-reading-list--loc-records dom nil))
                   ebook))))

;;;; Filing entries under a headline

(ert-deftest org-reading-list-test-demote ()
  (should (equal (org-reading-list--demote "* A\ntext\n** B\n" 1)
                 "** A\ntext\n*** B\n"))
  (should (equal (org-reading-list--demote "* A\n" 0) "* A\n")))

(defconst org-reading-list-test--entry
  "* TOREAD New\n:PROPERTIES:\n:CUSTOM_ID: new\n:END:\n"
  "A minimal top-level entry string for filing tests.")

(ert-deftest org-reading-list-test-insert-under-existing-headline ()
  (with-temp-buffer
    (org-mode)
    (insert "* Books\n** TOREAD Old\n:PROPERTIES:\n:CUSTOM_ID: old\n"
            ":END:\n* Reference\nnote\n")
    (org-reading-list--insert-under-headline
     org-reading-list-test--entry "Books")
    ;; The new entry is demoted to a child of Books (level 2).
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\* TOREAD New$" nil t))
    (let ((new-pos (match-beginning 0))
          (books-pos (progn (goto-char (point-min))
                            (re-search-forward "^\\* Books$" nil t)
                            (match-beginning 0)))
          (ref-pos (progn (goto-char (point-min))
                          (re-search-forward "^\\* Reference$" nil t)
                          (match-beginning 0))))
      ;; It is filed inside the Books subtree, before the next heading.
      (should (< books-pos new-pos ref-pos)))
    ;; The pre-existing entry is untouched.
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\* TOREAD Old$" nil t))))

(ert-deftest org-reading-list-test-insert-creates-headline ()
  (with-temp-buffer
    (org-mode)
    (insert "* Reference\nnote\n")
    (org-reading-list--insert-under-headline
     org-reading-list-test--entry "Books")
    (goto-char (point-min))
    (should (re-search-forward "^\\* Books$" nil t))
    ;; The entry is filed beneath the freshly created headline, with its
    ;; property drawer intact and readable as an entry property.
    (should (re-search-forward "^\\*\\* TOREAD New$" nil t))
    (should (equal (org-entry-get nil "CUSTOM_ID") "new"))))

;;;; Capture target (shared headline)

(ert-deftest org-reading-list-test-goto-headline-existing ()
  (with-temp-buffer
    (org-mode)
    (insert "* Books\n** TOREAD Old\n:PROPERTIES:\n:CUSTOM_ID: old\n"
            ":END:\n* Reference\nnote\n")
    (goto-char (point-max))
    (let ((org-reading-list-headline "Books"))
      (org-reading-list-goto-headline))
    ;; Leaves point on the heading, as a `file+function' target must.
    (should (org-at-heading-p))
    (should (equal (org-get-heading t t t t) "Books"))))

(ert-deftest org-reading-list-test-goto-headline-honors-custom ()
  (with-temp-buffer
    (org-mode)
    (insert "* Reference\nnote\n")
    ;; Reads `org-reading-list-headline', creating it when absent.
    (let ((org-reading-list-headline "Reading"))
      (org-reading-list-goto-headline))
    (should (org-at-heading-p))
    (should (equal (org-get-heading t t t t) "Reading"))))

;;;; Entry rendering

(ert-deftest org-reading-list-test-entry-string ()
  (should (equal (org-reading-list--entry-string
                  '(:title "One: A Tale" :tags ("a" "b")
                    :props (("CUSTOM_ID" . "smith2020")
                            ("TITLE" . "One: A Tale")
                            ("LCCN" . nil))))
                 (concat "* TOREAD One: A Tale :a:b:\n"
                         ":PROPERTIES:\n"
                         ":CUSTOM_ID: smith2020\n"
                         ":TITLE: One: A Tale\n"
                         ":END:\n")))
  (should (equal (org-reading-list--entry-string
                  '(:title "One: A Tale" :tags nil
                    :props (("CUSTOM_ID" . "smith2020"))))
                 (concat "* TOREAD One: A Tale\n"
                         ":PROPERTIES:\n"
                         ":CUSTOM_ID: smith2020\n"
                         ":END:\n"))))

(provide 'org-reading-list-tests)
;;; org-reading-list-tests.el ends here
