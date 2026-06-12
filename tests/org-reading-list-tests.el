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
(require 'cl-lib)

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

(ert-deftest org-reading-list-test-marc-strip-punct ()
  (should (equal (org-reading-list--marc-strip-punct
                  "Buried ships of San Francisco /")
                 "Buried ships of San Francisco"))
  (should (equal (org-reading-list--marc-strip-punct
                  "San Francisco, California :")
                 "San Francisco, California"))
  (should (equal (org-reading-list--marc-strip-punct "Researchity,")
                 "Researchity"))
  ;; Trailing period: accepted that initials lose theirs ("Filion,
  ;; Ron S." -> "Filion, Ron S"), matching the OL path's rendering.
  (should (equal (org-reading-list--marc-strip-punct "Filion, Ron S.")
                 "Filion, Ron S"))
  (should-not (org-reading-list--marc-strip-punct nil))
  (should-not (org-reading-list--marc-strip-punct " /")))

(ert-deftest org-reading-list-test-marc-pub-field ()
  ;; 264 wins over 260 when both are present.
  (let ((rec '(record nil
                      (datafield ((tag . "260"))
                                 (subfield ((code . "b")) "Old Publisher,"))
                      (datafield ((tag . "264"))
                                 (subfield ((code . "b")) "New Publisher,")))))
    (should (equal (org-reading-list--marc-pub-field rec "b")
                   "New Publisher,")))
  ;; Pre-RDA record: only 260.
  (let ((rec '(record nil
                      (datafield ((tag . "260"))
                                 (subfield ((code . "c")) "1962.")))))
    (should (equal (org-reading-list--marc-pub-field rec "c") "1962.")))
  ;; Neither field present.
  (should-not (org-reading-list--marc-pub-field '(record nil) "c")))

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

(defconst org-reading-list-test--loc-buried-ships
  '(record nil
           (datafield ((tag . "010") (ind1 . " ") (ind2 . " "))
                      (subfield ((code . "a")) "  2023911799"))
           (datafield ((tag . "020") (ind1 . " ") (ind2 . " "))
                      (subfield ((code . "a")) "9798393569716")
                      (subfield ((code . "q")) "pbk"))
           (datafield ((tag . "035") (ind1 . " ") (ind2 . " "))
                      (subfield ((code . "a")) "(OCoLC)on1406777487"))
           (datafield ((tag . "050") (ind1 . "0") (ind2 . "0"))
                      (subfield ((code . "a")) "F869.S347")
                      (subfield ((code . "b")) "F555 2023"))
           (datafield ((tag . "100") (ind1 . "1") (ind2 . " "))
                      (subfield ((code . "a")) "Filion, Ron S.")
                      (subfield ((code . "e")) "author"))
           (datafield ((tag . "245") (ind1 . "1") (ind2 . "0"))
                      (subfield ((code . "a")) "Buried ships of San Francisco /")
                      (subfield ((code . "c")) "Ron S. Filion."))
           (datafield ((tag . "264") (ind1 . " ") (ind2 . "1"))
                      (subfield ((code . "a")) "San Francisco, California :")
                      (subfield ((code . "b")) "Researchity,")
                      (subfield ((code . "c")) "[2023]"))
           (datafield ((tag . "300") (ind1 . " ") (ind2 . " "))
                      (subfield ((code . "a")) "i, 643 pages"))
           (datafield ((tag . "650") (ind1 . " ") (ind2 . "0"))
                      (subfield ((code . "a")) "Historic ships")
                      (subfield ((code . "z")) "California")
                      (subfield ((code . "z")) "San Francisco"))
           (datafield ((tag . "650") (ind1 . " ") (ind2 . "0"))
                      (subfield ((code . "a")) "Urban archaeology")
                      (subfield ((code . "z")) "California")
                      (subfield ((code . "z")) "San Francisco")))
  "LoC MARC record for ISBN 9798393569716, trimmed from the real SRU response.
Deliberately lacks an 082 field (the real record has none), covering the
DDC-nil case.")

(ert-deftest org-reading-list-test-marc-entry-data ()
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (data (org-reading-list--marc-entry-data
                (list org-reading-list-test--loc-buried-ships)
                "ISBN:9798393569716" nil))
         (props (plist-get data :props)))
    (should (equal (plist-get data :title)
                   "Buried ships of San Francisco"))
    (should (equal (plist-get data :isbns) '("9798393569716")))
    (should (equal (plist-get data :tags)
                   '("historic_ships" "california" "san_francisco"
                     "urban_archaeology")))
    (dolist (kv '(("CUSTOM_ID" . "filion2023")
                  ("BTYPE"     . "book")
                  ("AUTHOR"    . "Filion, Ron S")
                  ("TITLE"     . "Buried ships of San Francisco")
                  ("ADDRESS"   . "San Francisco, California")
                  ("PUBLISHER" . "Researchity")
                  ("DATE"      . "2023")
                  ("PAGES"     . "643")
                  ("ISBN"      . "9798393569716")
                  ("LCCN"      . "2023911799")
                  ("OCLC"      . "on1406777487")
                  ("LCC"       . "F869.S347 F555 2023")
                  ("DDC"       . nil)))
      (should (equal (cdr (assoc (car kv) props)) (cdr kv))))
    (should (equal (cdr (assoc "FOUND" props)) nil))))

(ert-deftest org-reading-list-test-marc-entry-data-fall-through ()
  ;; Author only in the second record; the first still supplies title.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (bare '(record nil
                        (datafield ((tag . "245") (ind1 . "0") (ind2 . "0"))
                                   (subfield ((code . "a")) "Anon title /"))))
         (data (org-reading-list--marc-entry-data
                (list bare org-reading-list-test--loc-buried-ships)
                "ISBN:1111111111" nil))
         (props (plist-get data :props)))
    (should (equal (plist-get data :title) "Anon title"))
    (should (equal (cdr (assoc "AUTHOR" props)) "Filion, Ron S"))
    ;; Queried ISBN unioned with every record's 020s.
    (should (equal (plist-get data :isbns)
                   '("1111111111" "9798393569716")))))

(ert-deftest org-reading-list-test-marc-entry-data-tags-cap ()
  ;; :tags is truncated to `org-reading-list-max-tags'.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (org-reading-list-max-tags 2)
         (data (org-reading-list--marc-entry-data
                (list org-reading-list-test--loc-buried-ships)
                "ISBN:9798393569716" nil)))
    (should (equal (plist-get data :tags)
                   '("historic_ships" "california")))))

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

;;;; Duplicate detection

(ert-deftest org-reading-list-test-scan-entries ()
  (with-temp-buffer
    (org-mode)
    (insert "* Books\n"
            "** TOREAD One: A Tale\n:PROPERTIES:\n"
            ":ISBN: 978-0-252-06631-3, 0252066316\n"
            ":TITLE: One: A Tale\n:AUTHOR: Smith, Ann\n:END:\n"
            "** TOREAD Two\n:PROPERTIES:\n:OLID: OL1M\n:END:\n")
    (let ((es (org-reading-list--scan-entries)))
      ;; One element per heading, in order, parent "Books" included
      ;; (its nil fields can never match).
      (should (= (length es) 3))
      (should (equal (plist-get (nth 0 es) :heading) "Books"))
      (should (= (plist-get (nth 0 es) :pos) 1))
      (should (< (plist-get (nth 0 es) :pos)
                 (plist-get (nth 1 es) :pos)
                 (plist-get (nth 2 es) :pos)))
      (let ((one (nth 1 es)))
        (should (equal (plist-get one :isbns)
                       '("9780252066313" "0252066316")))
        (should (equal (plist-get one :title) "One: A Tale"))
        (should (equal (plist-get one :author) "Smith, Ann"))
        ;; TOREAD is not a TODO keyword in a default Org buffer, so
        ;; `org-get-heading' keeps it; in a configured file it would
        ;; be stripped.  Either way the heading identifies the entry.
        (should (equal (plist-get one :heading) "TOREAD One: A Tale")))
      (should (equal (plist-get (nth 2 es) :olid) "OL1M"))
      (should (null (plist-get (nth 2 es) :isbns))))))

(ert-deftest org-reading-list-test-scan-entries-empty ()
  (with-temp-buffer
    (org-mode)
    (should (null (org-reading-list--scan-entries)))))

(defconst org-reading-list-test--dup-entries
  '((:pos 1 :heading "Books"
     :isbns nil :olid nil :title nil :author nil)
    (:pos 10 :heading "One: A Tale"
     :isbns ("0252066316") :olid "OL1M"
     :title "One: A Tale" :author "Smith, Ann")
    (:pos 99 :heading "Other Book"
     :isbns nil :olid nil :title "Other Book" :author "Smith, Ann"))
  "Scanner output stand-in for matcher tests.")

(ert-deftest org-reading-list-test-find-dup-isbn-cross-form ()
  ;; The fetched set and the stored entry share only the ISBN-10;
  ;; matching is set intersection, no 10↔13 conversion exists.
  (let ((dup (org-reading-list--find-duplicate
              '(:title "Unrelated" :isbns ("0252066316")
                :props (("AUTHOR" . nil)))
              org-reading-list-test--dup-entries)))
    (should (eq (car dup) 'exact))
    (should (equal (plist-get (cdr dup) :heading) "One: A Tale"))))

(ert-deftest org-reading-list-test-find-dup-olid ()
  (let ((dup (org-reading-list--find-duplicate
              '(:title "X" :isbns nil
                :props (("OLID" . "OL1M") ("AUTHOR" . nil)))
              org-reading-list-test--dup-entries)))
    (should (eq (car dup) 'exact))))

(ert-deftest org-reading-list-test-find-dup-similar-edition ()
  ;; Different subtitle, no shared identifier: publisher/edition change.
  (let ((dup (org-reading-list--find-duplicate
              '(:title "One: Revised and Expanded" :isbns ("1111111111")
                :props (("AUTHOR" . "Smith, Ann B.")))
              org-reading-list-test--dup-entries)))
    (should (eq (car dup) 'similar))
    (should (equal (plist-get (cdr dup) :heading) "One: A Tale"))))

(ert-deftest org-reading-list-test-find-dup-miss-different-title ()
  (should-not (org-reading-list--find-duplicate
               '(:title "Third Thing" :isbns ("2222222222")
                 :props (("AUTHOR" . "Smith, Ann")))
               org-reading-list-test--dup-entries)))

(ert-deftest org-reading-list-test-find-dup-no-author-no-similar ()
  ;; Anonymous works must not soft-match on title alone.
  (should-not (org-reading-list--find-duplicate
               '(:title "One: A Tale" :isbns nil
                 :props (("AUTHOR" . nil)))
               org-reading-list-test--dup-entries)))

(ert-deftest org-reading-list-test-find-dup-exact-beats-similar ()
  (let ((dup (org-reading-list--find-duplicate
              '(:title "One: A Tale" :isbns ("0252066316")
                :props (("AUTHOR" . "Smith, Ann")))
              org-reading-list-test--dup-entries)))
    (should (eq (car dup) 'exact))))

(defconst org-reading-list-test--dup-file-content
  (concat "* Books\n** TOREAD One: A Tale\n:PROPERTIES:\n"
          ":ISBN: 0252066316\n:TITLE: One: A Tale\n"
          ":AUTHOR: Smith, Ann\n:END:\n")
  "Reading-list file contents for duplicate command tests.")

(defconst org-reading-list-test--dup-data
  '(:title "One: A Tale" :tags nil
    :isbns ("9780252066313" "0252066316")
    :props (("TITLE" . "One: A Tale") ("AUTHOR" . "Smith, Ann")))
  "Stubbed --entry-data result matching the file's entry exactly.")

(defmacro org-reading-list-test--with-dup-file (&rest body)
  "Run BODY with `org-reading-list-file' bound to a fresh temp file.
BODY may reference `file', the temp file's path."
  `(let* ((file (make-temp-file "orl-dup" nil ".org"
                                org-reading-list-test--dup-file-content))
          (org-reading-list-file file))
     (unwind-protect
         (progn ,@body)
       (when-let* ((b (get-file-buffer file)))
         (with-current-buffer b (set-buffer-modified-p nil))
         (kill-buffer b))
       (delete-file file))))

(ert-deftest org-reading-list-test-insert-duplicate-declined-jumps ()
  (org-reading-list-test--with-dup-file
   (cl-letf (((symbol-function 'org-reading-list--entry-data)
              (lambda (_id &optional _source)
                org-reading-list-test--dup-data))
             ((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
     (org-reading-list-insert "9780252066313")
     ;; Jumped: point on the existing heading in the list buffer.
     (should (equal buffer-file-name file))
     (should (looking-at-p "\\*\\* TOREAD One: A Tale"))
     ;; Nothing inserted.
     (should (= 1 (count-matches "TOREAD" (point-min) (point-max)))))))

(ert-deftest org-reading-list-test-insert-duplicate-accepted-files ()
  (org-reading-list-test--with-dup-file
   (cl-letf (((symbol-function 'org-reading-list--entry-data)
              (lambda (_id &optional _source)
                org-reading-list-test--dup-data))
             ((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
     (org-reading-list-insert "9780252066313")
     (should (= 2 (count-matches "TOREAD" (point-min) (point-max)))))))

(ert-deftest org-reading-list-test-capture-duplicate-aborts-not-manual ()
  ;; Declining a duplicate must abort the capture, NOT fall through to
  ;; the manual-entry prompt (the condition-case fallback).
  (org-reading-list-test--with-dup-file
   (cl-letf (((symbol-function 'org-reading-list--entry-data)
              (lambda (_id &optional _source)
                org-reading-list-test--dup-data))
             ((symbol-function 'y-or-n-p) (lambda (_prompt) nil))
             ((symbol-function 'read-string)
              (lambda (prompt &rest _)
                (if (string-prefix-p "Lookup failed" prompt)
                    (ert-fail "fell through to manual-entry fallback")
                  ""))))
     (should-error (org-reading-list-capture) :type 'user-error))))

(ert-deftest org-reading-list-test-capture-duplicate-accepted-returns-entry ()
  (org-reading-list-test--with-dup-file
   (cl-letf (((symbol-function 'org-reading-list--entry-data)
              (lambda (_id &optional _source)
                org-reading-list-test--dup-data))
             ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
             ((symbol-function 'read-string) (lambda (_p &rest _) "x")))
     (should (string-prefix-p "* TOREAD One: A Tale"
                              (org-reading-list-capture))))))

(provide 'org-reading-list-tests)
;;; org-reading-list-tests.el ends here
