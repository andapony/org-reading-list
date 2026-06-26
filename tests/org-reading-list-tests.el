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

(ert-deftest org-reading-list-test-lccn-normalize ()
  ;; Catalog-card form: hyphen dropped, serial padded to six digits.
  (should (equal (org-reading-list--lccn-normalize "61-10539")
                 "61010539"))
  (should (equal (org-reading-list--lccn-normalize "2003-12345")
                 "2003012345"))
  ;; Alpha-prefixed card numbers pad the same way.
  (should (equal (org-reading-list--lccn-normalize "agr07-496")
                 "agr07000496"))
  ;; Already-canonical and space-padded values: spaces stripped only.
  (should (equal (org-reading-list--lccn-normalize "  2023911799")
                 "2023911799"))
  (should (equal (org-reading-list--lccn-normalize "61010539")
                 "61010539"))
  (should-not (org-reading-list--lccn-normalize nil)))

(ert-deftest org-reading-list-test-bibkey-lccn-normalized ()
  (should (equal (org-reading-list--bibkey "LCCN:61-10539")
                 "LCCN:61010539"))
  (should (equal (org-reading-list--bibkey "LCCN:61010539")
                 "LCCN:61010539"))
  ;; Other prefixes still pass through verbatim.
  (should (equal (org-reading-list--bibkey "OCLC:123-456")
                 "OCLC:123-456")))

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

;;;; Controlled-vocabulary tags

(ert-deftest org-reading-list-test-rewrites-safe-p ()
  (should (org-reading-list--tag-rewrites-safe-p
           '(("gold_discoveries" . "gold_rush") ("history" . nil))))
  (should (org-reading-list--tag-rewrites-safe-p nil))
  (should-not (org-reading-list--tag-rewrites-safe-p '(("ok" . 5))))
  (should-not (org-reading-list--tag-rewrites-safe-p '((1 . "x"))))
  (should-not (org-reading-list--tag-rewrites-safe-p "nope")))

(ert-deftest org-reading-list-test-tag-vocabulary ()
  (with-temp-buffer
    (insert "#+TAGS: gold_rush vigilance waterfront\n")
    (org-mode)
    (org-set-regexps-and-options)
    (let ((org-reading-list-tag-vocabulary '("extra")))
      (let ((v (org-reading-list--tag-vocabulary)))
        (should (member "gold_rush" v))
        (should (member "vigilance" v))
        (should (member "waterfront" v))
        (should (member "extra" v))))))

(ert-deftest org-reading-list-test-tag-vocabulary-filters-groups ()
  (with-temp-buffer
    (insert "#+TAGS: { waterfront : ships harbor }\n")
    (org-mode)
    (org-set-regexps-and-options)
    (let ((v (org-reading-list--tag-vocabulary)))
      (should (seq-every-p #'stringp v))
      (should (member "ships" v))
      (should (member "harbor" v)))))

(ert-deftest org-reading-list-test-rewrite-basic ()
  (let* ((vocab '("gold_rush" "vigilance" "waterfront"))
         (rw '(("gold_discoveries" . "gold_rush")
               ("vigilance_committees" . "vigilance")
               ("history" . nil)))
         (res (org-reading-list--rewrite-tags
               '("gold_discoveries" "history" "vigilance_committees" "waterfront")
               vocab rw)))
    ;; vocab passthrough + mapped, sorted alphabetically
    (should (equal (car res) '("gold_rush" "vigilance" "waterfront")))
    (should (equal (plist-get (cdr res) :dropped-explicit) '("history")))
    (should (equal (plist-get (cdr res) :rewritten)
                   '(("gold_discoveries" . "gold_rush")
                     ("vigilance_committees" . "vigilance"))))))

(ert-deftest org-reading-list-test-rewrite-unresolved ()
  ;; unmapped non-vocab tag, and a rewrite target not in vocab, both unresolved
  (let* ((vocab '("gold_rush"))
         (rw '(("bad_target" . "not_in_vocab")))
         (res (org-reading-list--rewrite-tags '("mystery" "bad_target") vocab rw)))
    (should (equal (car res) nil))
    (should (equal (plist-get (cdr res) :dropped-unresolved)
                   '("mystery" "bad_target")))))

(ert-deftest org-reading-list-test-rewrite-dedup-cap ()
  (let* ((org-reading-list-max-tags 2)
         (vocab '("a" "b" "c"))
         (res (org-reading-list--rewrite-tags '("c" "a" "a" "b") vocab nil)))
    ;; dedup, sort, cap at 2 -> ("a" "b")
    (should (equal (car res) '("a" "b")))))

(defmacro org-reading-list-test--with-list (&rest body)
  "Set up a temp reading-list buffer with two book entries, then run BODY."
  `(with-temp-buffer
     (insert "#+TAGS: gold_rush vigilance\n* Books\n"
             "** TOREAD A\n:PROPERTIES:\n:BTYPE: book\n:SUBJECTS: gold_discoveries; history\n:END:\n"
             "** TOREAD B\n:PROPERTIES:\n:BTYPE: book\n:SUBJECTS: unmapped_thing\n:END:\n")
     (org-mode)
     (org-set-regexps-and-options)
     (let ((org-reading-list-tag-rewrites
            '(("gold_discoveries" . "gold_rush") ("history" . nil))))
       ,@body)))

(ert-deftest org-reading-list-test-lint-collect ()
  (org-reading-list-test--with-list
   (let ((data (org-reading-list--lint-collect)))
     (should (= (length data) 2))
     (let ((a (car data)) (b (cadr data)))
       (should (equal (plist-get a :new) '("gold_rush")))
       (should-not (plist-get a :thin))
       ;; B: unmapped_thing has no home -> dropped-unresolved, 0 tags, thin
       (should (equal (plist-get b :new) nil))
       (should (plist-get b :thin))
       (should (equal (plist-get (plist-get b :record) :dropped-unresolved)
                      '("unmapped_thing")))))))

(ert-deftest org-reading-list-test-preen-entry ()
  (org-reading-list-test--with-list
   (goto-char (point-min))
   (re-search-forward "^\\*\\* TOREAD A")
   (let ((vocab (org-reading-list--tag-vocabulary)))
     (org-reading-list--preen-entry vocab org-reading-list-tag-rewrites)
     (should (equal (org-get-tags nil t) '("gold_rush"))))))

(ert-deftest org-reading-list-test-preen-buffer-confirm ()
  (org-reading-list-test--with-list
   ;; Decline: nothing changes.
   (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
     (org-reading-list-preen-tags t))
   (goto-char (point-min))
   (re-search-forward "^\\*\\* TOREAD A")
   (should (equal (org-get-tags nil t) nil))
   ;; Accept: both entries preen.
   (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
     (org-reading-list-preen-tags t))
   (goto-char (point-min))
   (re-search-forward "^\\*\\* TOREAD A")
   (should (equal (org-get-tags nil t) '("gold_rush")))))

(ert-deftest org-reading-list-test-preen-infer-hook ()
  (org-reading-list-test--with-list
   (goto-char (point-min))
   (re-search-forward "^\\*\\* TOREAD B")   ; B preens to zero tags -> thin
   (let ((vocab (org-reading-list--tag-vocabulary))
         (org-reading-list-tag-min 1)
         ;; Hook returns one in-vocab tag and one bogus tag (dropped).
         (org-reading-list-tag-infer-function
          (lambda (_ctx) '("vigilance" "bogus_not_in_vocab"))))
     (org-reading-list--preen-entry vocab org-reading-list-tag-rewrites)
     (should (equal (org-get-tags nil t) '("vigilance"))))))

(ert-deftest org-reading-list-test-derive-tags-projects-and-materializes ()
  (with-temp-buffer
    (insert "#+TAGS: gold_rush\n")
    (org-mode)
    (org-set-regexps-and-options)
    (let ((org-reading-list-tag-rewrites
           '(("gold_discoveries" . "gold_rush") ("history" . nil)))
          (data (list :title "X" :props nil
                      :subjects '("gold_discoveries" "history"))))
      (let* ((out (org-reading-list--derive-tags data))
             (props (plist-get out :props)))
        ;; Tags are the projection; SUBJECTS property holds the full source.
        (should (equal (plist-get out :tags) '("gold_rush")))
        (should (equal (cdr (assoc "SUBJECTS" props))
                       "gold_discoveries; history"))))))

(ert-deftest org-reading-list-test-derive-tags-empty-vocab-passthrough ()
  (with-temp-buffer
    (org-mode)                          ; no #+TAGS: -> empty vocabulary
    (let ((data (list :title "X" :props nil :subjects '("a" "b"))))
      (should (equal (plist-get (org-reading-list--derive-tags data) :tags)
                     '("a" "b"))))))

(ert-deftest org-reading-list-test-derive-tags-all-dropped-keeps-subjects ()
  (with-temp-buffer
    (insert "#+TAGS: only_this\n")
    (org-mode)
    (org-set-regexps-and-options)
    (let ((data (list :title "X" :props nil :subjects '("sailors" "whaling"))))
      (let* ((out (org-reading-list--derive-tags data))
             (props (plist-get out :props)))
        (should (null (plist-get out :tags)))          ; all unresolved -> dropped
        (should (equal (cdr (assoc "SUBJECTS" props))  ; source preserved
                       "sailors; whaling"))))))

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
                 '("9781984882004")))
  ;; Pre-RDA records embed qualifiers in $a itself.
  (should (equal (org-reading-list--marc-isbns
                  '(record nil
                           (datafield ((tag . "020"))
                                      (subfield ((code . "a"))
                                                "0689817479 (hc.)")))
                  "a")
                 '("0689817479"))))

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

(ert-deftest org-reading-list-test-loc-entry-query-lccn-normalized ()
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD Old Book\n:PROPERTIES:\n:LCCN: 61-10539\n:END:\n")
    (goto-char (point-min))
    (should (equal (org-reading-list--loc-entry-query)
                   '("bath.lccn=61010539")))))

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

(ert-deftest org-reading-list-test-marc-entry-data-subjects-uncapped ()
  ;; :subjects carries the full set; :tags stays capped at max-tags.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (org-reading-list-max-tags 2)
         (rec '(record nil
                       (datafield ((tag . "245") (ind1 . "0") (ind2 . "0"))
                                  (subfield ((code . "a")) "T /"))
                       (datafield ((tag . "650") (ind1 . " ") (ind2 . "0"))
                                  (subfield ((code . "a")) "Alpha"))
                       (datafield ((tag . "650") (ind1 . " ") (ind2 . "0"))
                                  (subfield ((code . "a")) "Beta"))
                       (datafield ((tag . "650") (ind1 . " ") (ind2 . "0"))
                                  (subfield ((code . "a")) "Gamma"))))
         (data (org-reading-list--marc-entry-data (list rec) "MI" nil)))
    (should (equal (plist-get data :subjects) '("alpha" "beta" "gamma")))
    (should (= (length (plist-get data :tags)) 2))))


(defconst org-reading-list-test--loc-kemble
  '(record nil
           (datafield ((tag . "010") (ind1 . " ") (ind2 . " "))
                      (subfield ((code . "a")) "   61010539 "))
           (datafield ((tag . "100") (ind1 . "1") (ind2 . " "))
                      (subfield ((code . "a")) "Kemble, Edward C."))
           (datafield ((tag . "245") (ind1 . "1") (ind2 . "0"))
                      (subfield ((code . "a"))
                                "A history of California newspapers, 1846-1858."))
           (datafield ((tag . "260") (ind1 . " ") (ind2 . " "))
                      (subfield ((code . "a")) "Los Gatos, Calif. :")
                      (subfield ((code . "b")) "Talisman Press,")
                      (subfield ((code . "c")) "1962."))
           (datafield ((tag . "650") (ind1 . " ") (ind2 . "0"))
                      (subfield ((code . "a")) "American newspapers")
                      (subfield ((code . "z")) "California")))
  "Pre-ISBN LoC MARC record (LCCN 61010539, no 020 field), trimmed.")

(ert-deftest org-reading-list-test-marc-entry-data-lccn-bibkey ()
  ;; LCCN bibkey: no queried ISBN; identifiers come from the record.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (data (org-reading-list--marc-entry-data
                (list org-reading-list-test--loc-buried-ships)
                "LCCN:2023911799" nil))
         (props (plist-get data :props)))
    (should (equal (plist-get data :isbns) '("9798393569716")))
    (should (equal (cdr (assoc "ISBN" props)) "9798393569716"))))

(ert-deftest org-reading-list-test-marc-entry-data-no-isbn ()
  ;; Pre-ISBN book: no 020 anywhere -> no :ISBN:, LCCN identifies it.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (data (org-reading-list--marc-entry-data
                (list org-reading-list-test--loc-kemble)
                "LCCN:61010539" nil))
         (props (plist-get data :props)))
    (should (null (plist-get data :isbns)))
    (should (null (cdr (assoc "ISBN" props))))
    (should (equal (cdr (assoc "LCCN" props)) "61010539"))
    (should (equal (cdr (assoc "CUSTOM_ID" props)) "kemble1962"))
    (should (equal (cdr (assoc "DATE" props)) "1962"))))

(ert-deftest org-reading-list-test-marc-entry-data-multi-isbn ()
  ;; LCCN bibkey, record with several 020s: all collected, first wins
  ;; the :ISBN: prop (cataloging order).
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (rec '(record nil
                       (datafield ((tag . "010"))
                                  (subfield ((code . "a")) "  2005921845"))
                       (datafield ((tag . "020"))
                                  (subfield ((code . "a")) "9780747581086"))
                       (datafield ((tag . "020"))
                                  (subfield ((code . "a")) "0747581088 (pbk.)"))
                       (datafield ((tag . "245") (ind1 . "0") (ind2 . "0"))
                                  (subfield ((code . "a")) "Multi /"))))
         (data (org-reading-list--marc-entry-data
                (list rec) "LCCN:2005921845" nil))
         (props (plist-get data :props)))
    (should (equal (plist-get data :isbns)
                   '("9780747581086" "0747581088")))
    (should (equal (cdr (assoc "ISBN" props)) "9780747581086"))))

;;;; LoC fallback in entry-data / entry

(defconst org-reading-list-test--loc-buried-ships-dom
  `(zs:searchRetrieveResponse
    nil
    (zs:records nil
                (zs:record nil
                           (zs:recordData
                            nil
                            ,org-reading-list-test--loc-buried-ships))))
  "SRU response DOM wrapping the Buried Ships record.")

(ert-deftest org-reading-list-test-entry-loc-fallback ()
  ;; Open Library misses; the entry is built from LoC instead.
  (let ((org-reading-list-file "/nonexistent/orl-test.org")
        (xml-calls 0))
    (cl-letf (((symbol-function 'org-reading-list--fetch-json)
               (lambda (_url) nil))
              ((symbol-function 'org-reading-list--fetch-xml)
               (lambda (url)
                 (cl-incf xml-calls)
                 (should (string-match-p "bath\\.isbn=9798393569716" url))
                 org-reading-list-test--loc-buried-ships-dom)))
      (let ((entry (org-reading-list-entry "9798393569716" "a friend")))
        (should (string-prefix-p
                 (concat "* TOREAD Buried ships of San Francisco"
                         " :historic_ships:california:san_francisco"
                         ":urban_archaeology:\n")
                 entry))
        (should (string-match-p "^:CUSTOM_ID: filion2023$" entry))
        (should (string-match-p "^:AUTHOR: Filion, Ron S$" entry))
        (should (string-match-p "^:ISBN: 9798393569716$" entry))
        (should (string-match-p "^:LCCN: 2023911799$" entry))
        (should (string-match-p "^:FOUND: a friend$" entry))
        ;; No LoC equivalents -> these props must be absent.
        (should-not (string-match-p ":OLID:" entry))
        (should-not (string-match-p ":DDC:" entry))
        (should (= xml-calls 1))))))

(ert-deftest org-reading-list-test-entry-both-miss ()
  ;; Both sources miss: the error names both.
  (cl-letf (((symbol-function 'org-reading-list--fetch-json)
             (lambda (_url) nil))
            ((symbol-function 'org-reading-list--fetch-xml)
             (lambda (_url) nil)))
    (let ((err (should-error (org-reading-list-entry "9798393569716")
                             :type 'user-error)))
      (should (string-match-p "No Open Library or LoC record"
                              (cadr err))))))

(ert-deftest org-reading-list-test-entry-olid-miss-no-loc ()
  ;; Non-ISBN input: LoC cannot be queried; old error, no SRU call.
  (cl-letf (((symbol-function 'org-reading-list--fetch-json)
             (lambda (_url) nil))
            ((symbol-function 'org-reading-list--fetch-xml)
             (lambda (_url)
               (ert-fail "LoC queried for a non-ISBN bibkey"))))
    (let ((err (should-error (org-reading-list-entry "OL5851208M")
                             :type 'user-error)))
      (should (string-match-p "No Open Library record" (cadr err))))))

(ert-deftest org-reading-list-test-entry-ol-hit-skips-loc ()
  ;; An Open Library hit carrying an identifier must not query LoC.
  (let ((org-reading-list-file "/nonexistent/orl-test.org"))
    (cl-letf (((symbol-function 'org-reading-list--fetch-json)
               (lambda (_url)
                 '(("ISBN:9780252066313"
                    . ((title . "One")
                       (authors . (((name . "Ann Smith"))))
                       (publish_date . "1997")
                       (identifiers . ((isbn_13 . ("9780252066313")))))))))
              ((symbol-function 'org-reading-list--fetch-xml)
               (lambda (_url)
                 (ert-fail "LoC queried despite an Open Library hit"))))
      (let ((entry (org-reading-list-entry "9780252066313")))
        (should (string-prefix-p "* TOREAD One\n" entry))
        (should (string-match-p "^:AUTHOR: Smith, Ann$" entry))))))

(defconst org-reading-list-test--loc-kemble-dom
  `(zs:searchRetrieveResponse
    nil
    (zs:records nil
                (zs:record nil
                           (zs:recordData
                            nil
                            ,org-reading-list-test--loc-kemble))))
  "SRU response DOM wrapping the Kemble record.")

(ert-deftest org-reading-list-test-entry-lccn-fallback ()
  ;; Open Library misses the LCCN; the entry is built from LoC.
  (let ((org-reading-list-file "/nonexistent/orl-test.org"))
    (cl-letf (((symbol-function 'org-reading-list--fetch-json)
               (lambda (_url) nil))
              ((symbol-function 'org-reading-list--fetch-xml)
               (lambda (url)
                 (should (string-match-p "bath\\.lccn=61010539" url))
                 org-reading-list-test--loc-kemble-dom)))
      (let ((entry (org-reading-list-entry "LCCN:61-10539")))
        (should (string-prefix-p
                 "* TOREAD A history of California newspapers, 1846-1858"
                 entry))
        (should (string-match-p "^:AUTHOR: Kemble, Edward C$" entry))
        (should (string-match-p "^:LCCN: 61010539$" entry))
        (should (string-match-p "^:DATE: 1962$" entry))
        (should (string-match-p "^:PUBLISHER: Talisman Press$" entry))
        ;; Pre-ISBN book: no ISBN property at all.
        (should-not (string-match-p ":ISBN:" entry))))))

(ert-deftest org-reading-list-test-entry-oclc-miss-no-loc ()
  ;; OCLC bibkeys have no SRU index configured: OL-only error, no call.
  (cl-letf (((symbol-function 'org-reading-list--fetch-json)
             (lambda (_url) nil))
            ((symbol-function 'org-reading-list--fetch-xml)
             (lambda (_url)
               (ert-fail "LoC queried for an OCLC bibkey"))))
    (let ((err (should-error (org-reading-list-entry "OCLC:1234567")
                             :type 'user-error)))
      (should (string-match-p "No Open Library record" (cadr err))))))

(ert-deftest org-reading-list-test-entry-lowercase-prefix-no-loc ()
  ;; A lowercase explicit bibkey passes through --bibkey unchanged and
  ;; must not reach the LoC fallback's case-sensitive prefix parsing.
  (cl-letf (((symbol-function 'org-reading-list--fetch-json)
             (lambda (_url) nil))
            ((symbol-function 'org-reading-list--fetch-xml)
             (lambda (_url)
               (ert-fail "LoC queried for a lowercase prefix bibkey"))))
    (should-error (org-reading-list-entry "isbn:9798393569716")
                  :type 'user-error)))

(ert-deftest org-reading-list-test-entry-lccn-both-miss ()
  ;; LCCN both-miss names both sources, with the full bibkey.
  (cl-letf (((symbol-function 'org-reading-list--fetch-json)
             (lambda (_url) nil))
            ((symbol-function 'org-reading-list--fetch-xml)
             (lambda (_url) nil)))
    (let ((err (should-error (org-reading-list-entry "LCCN:61-10539")
                             :type 'user-error)))
      (should (string-match-p
               "No Open Library or LoC record for LCCN:61010539"
               (cadr err))))))

;;;; Title/author LoC bridge (OLID / pre-ISBN books OL holds no id for)

(defconst org-reading-list-test--loc-men-memories
  '(record nil
           (datafield ((tag . "010") (ind1 . " ") (ind2 . " "))
                      (subfield ((code . "a")) "   02026842 "))
           (datafield ((tag . "050") (ind1 . "0") (ind2 . "0"))
                      (subfield ((code . "a")) "F869.S3")
                      (subfield ((code . "b")) "B18"))
           (datafield ((tag . "100") (ind1 . "1") (ind2 . " "))
                      (subfield ((code . "a")) "Barry, T. A.")
                      (subfield ((code . "q")) "(Theodore Augustus),")
                      (subfield ((code . "d")) "1825-1881."))
           (datafield ((tag . "245") (ind1 . "1") (ind2 . "0"))
                      (subfield ((code . "a"))
                                "Men and memories of San Francisco, in the \"spring of '50.\""))
           (datafield ((tag . "260") (ind1 . " ") (ind2 . " "))
                      (subfield ((code . "a")) "San Francisco,")
                      (subfield ((code . "b")) "A.L. Bancroft & company,")
                      (subfield ((code . "c")) "1873."))
           (datafield ((tag . "520") (ind1 . " ") (ind2 . " "))
                      (subfield ((code . "a")) "Sketches of early San Francisco."))
           (datafield ((tag . "651") (ind1 . " ") (ind2 . "0"))
                      (subfield ((code . "a")) "San Francisco (Calif.)")
                      (subfield ((code . "x")) "History")))
  "LoC MARC record for the Men-and-memories title (LCCN 02026842).")

(defconst org-reading-list-test--loc-men-memories-dom
  `(zs:searchRetrieveResponse
    nil
    (zs:records nil
                (zs:record nil
                           (zs:recordData
                            nil
                            ,org-reading-list-test--loc-men-memories))))
  "SRU response DOM wrapping the Men-and-memories record.")

(defconst org-reading-list-test--ol-men-memories
  '((title . "Men and memories of San Francisco, in the \"spring of '50\"")
    (authors . (((name . "Barry, T. A."))))
    (publish_date . "1873")
    (number_of_pages . 296)
    (publishers . (((name . "A.L. Bancroft & Company"))))
    (publish_places . (((name . "San Francisco"))))
    (ebooks . (((preview_url
                 . "https://archive.org/details/menandmemoriess00pattgoog"))))
    (url . "http://openlibrary.org/books/OL23402322M/Men_and_memories")
    (identifiers . ((openlibrary . ("OL23402322M")))))
  "Open Library `data' record lacking every identifier but the OLID.")

(ert-deftest org-reading-list-test-loc-title-author-cql ()
  ;; Leading title phrase (before subtitle/comma, quotes dropped) and
  ;; author surname, url-encoded for the SRU query string.
  (should (equal
           (org-reading-list--loc-title-author-cql
            "Men and memories of San Francisco, in the \"spring of '50\""
            "Barry, T. A.")
           (url-hexify-string
            (concat "bath.title=\"Men and memories of San Francisco\""
                    " and bath.author=\"Barry\""))))
  ;; Both a title and an author are required.
  (should-not (org-reading-list--loc-title-author-cql "Title only" nil))
  (should-not (org-reading-list--loc-title-author-cql nil "Author, A.")))

(ert-deftest org-reading-list-test-marc-abstract ()
  ;; The 520 summary, when present; nil for records without one.
  (should (equal (org-reading-list--marc-abstract
                  (list org-reading-list-test--loc-men-memories))
                 "Sketches of early San Francisco."))
  (should-not (org-reading-list--marc-abstract
               (list org-reading-list-test--loc-kemble))))

(ert-deftest org-reading-list-test-marc-entry-data-abstract ()
  ;; The LoC-fallback entry data carries the 520 as :ABSTRACT:.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (data (org-reading-list--marc-entry-data
                (list org-reading-list-test--loc-men-memories)
                "LCCN:02026842" nil))
         (props (plist-get data :props)))
    (should (equal (cdr (assoc "ABSTRACT" props))
                   "Sketches of early San Francisco.")))
  ;; A record without a 520 omits the property.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (data (org-reading-list--marc-entry-data
                (list org-reading-list-test--loc-kemble)
                "LCCN:61010539" nil))
         (props (plist-get data :props)))
    (should (null (cdr (assoc "ABSTRACT" props))))))

(ert-deftest org-reading-list-test-loc-match-p ()
  (let ((rec org-reading-list-test--loc-men-memories))
    ;; Author surname and year agree.
    (should (org-reading-list--loc-match-p rec "Barry, T. A." "1873"))
    ;; Unknown date on our side still matches on the author surname.
    (should (org-reading-list--loc-match-p rec "Barry, T. A." nil))
    ;; Wrong author surname: reject even with the right year.
    (should-not (org-reading-list--loc-match-p rec "Patten, B. A." "1873"))
    ;; Year mismatch: reject.
    (should-not (org-reading-list--loc-match-p rec "Barry, T. A." "1999"))))

(ert-deftest org-reading-list-test-entry-olid-augments-from-loc ()
  ;; OLID/pre-ISBN book: OL supplies description, LoC fills identifiers.
  (let ((org-reading-list-file "/nonexistent/orl-test.org")
        (xml-calls 0))
    (cl-letf (((symbol-function 'org-reading-list--fetch-json)
               (lambda (_url)
                 (list (cons "OLID:OL23402322M"
                             org-reading-list-test--ol-men-memories))))
              ((symbol-function 'org-reading-list--fetch-xml)
               (lambda (url)
                 (cl-incf xml-calls)
                 (should (string-match-p "bath\\.title" url))
                 (should (string-match-p "bath\\.author" url))
                 org-reading-list-test--loc-men-memories-dom)))
      (let ((entry (org-reading-list-entry
                    "https://openlibrary.org/works/OL228795W/x?edition=key%3A/books/OL23402322M")))
        ;; OL fields survive...
        (should (string-match-p "^:AUTHOR: Barry, T. A.$" entry))
        (should (string-match-p "^:IA: menandmemoriess00pattgoog$" entry))
        ;; ...and LoC fills the identifiers and summary OL lacked.
        (should (string-match-p "^:LCCN: 02026842$" entry))
        (should (string-match-p "^:LCC: F869.S3 B18$" entry))
        (should (string-match-p
                 "^:ABSTRACT: Sketches of early San Francisco.$" entry))
        (should (= xml-calls 1))))))

(ert-deftest org-reading-list-test-entry-olid-augment-guard ()
  ;; LoC returns a non-matching book: nothing is merged.
  (let ((org-reading-list-file "/nonexistent/orl-test.org")
        (xml-calls 0))
    (cl-letf (((symbol-function 'org-reading-list--fetch-json)
               (lambda (_url)
                 (list (cons "OLID:OL1M"
                             org-reading-list-test--ol-men-memories))))
              ((symbol-function 'org-reading-list--fetch-xml)
               (lambda (_url)
                 (cl-incf xml-calls)
                 org-reading-list-test--loc-kemble-dom)))
      (let ((entry (org-reading-list-entry "OL1M")))
        (should (string-match-p "^:AUTHOR: Barry, T. A.$" entry))
        (should-not (string-match-p ":LCCN:" entry))
        (should (= xml-calls 1))))))

(ert-deftest org-reading-list-test-loc-entry-query-title-author ()
  ;; No ISBN/LCCN on the entry: query by title and author instead.
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD Men and memories\n:PROPERTIES:\n"
            ":TITLE: Men and memories of San Francisco,"
            " in the \"spring of '50\"\n"
            ":AUTHOR: Barry, T. A.\n:END:\n")
    (goto-char (point-min))
    (should (equal (org-reading-list--loc-entry-query)
                   (list (org-reading-list--loc-title-author-cql
                          (concat "Men and memories of San Francisco,"
                                  " in the \"spring of '50\"")
                          "Barry, T. A."))))))

(ert-deftest org-reading-list-test-loc-enrich-by-title-author ()
  ;; enrich-loc works from a title/author-only entry, guarded by author.
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD Men and memories\n:PROPERTIES:\n"
            ":TITLE: Men and memories of San Francisco\n"
            ":AUTHOR: Barry, T. A.\n:END:\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'org-reading-list--fetch-xml)
               (lambda (url)
                 (should (string-match-p "bath\\.title" url))
                 org-reading-list-test--loc-men-memories-dom)))
      (org-reading-list-enrich-loc)
      (should (equal (org-entry-get nil "LCCN") "02026842"))
      (should (equal (org-entry-get nil "LCC") "F869.S3 B18"))
      (should (equal (org-entry-get nil "ABSTRACT")
                     "Sketches of early San Francisco.")))))

(ert-deftest org-reading-list-test-loc-enrich-title-author-guard ()
  ;; A title/author search whose records fail the author guard errors,
  ;; rather than applying a wrong record's identifiers.
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD Men and memories\n:PROPERTIES:\n"
            ":TITLE: Men and memories of San Francisco\n"
            ":AUTHOR: Patten, B. A.\n:END:\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'org-reading-list--fetch-xml)
               (lambda (_url)
                 org-reading-list-test--loc-men-memories-dom)))
      (should-error (org-reading-list-enrich-loc) :type 'user-error))))

;;;; Internet Archive PDF download

(ert-deftest org-reading-list-test-download-pdf-records-localfile ()
  ;; The download command must record the local copy in a settable
  ;; property and flag the holding, without erroring.  :FILE: is one of
  ;; Org's reserved special properties and cannot hold this value.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (dir (make-temp-file "orl-pdf" t))
         (org-reading-list-pdf-directory dir))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* TOREAD A Book\n:PROPERTIES:\n"
                  ":CUSTOM_ID: barry1873\n:IA: someiaitem\n:END:\n")
          (goto-char (point-min))
          (cl-letf (((symbol-function 'url-copy-file)
                     (lambda (_url newname &rest _)
                       (with-temp-file newname (insert "%PDF-1.4\n")))))
            (org-reading-list-download-pdf))
          (should (equal (org-entry-get nil "LOCALFILE")
                         (format "[[file:%s]]"
                                 (abbreviate-file-name
                                  (expand-file-name "barry1873.pdf" dir)))))
          (should-not (org-entry-get nil "FILE"))
          (should (string-match-p "OWN (pdf)"
                                  (or (org-entry-get nil "HOLDINGS") ""))))
      (delete-directory dir t))))

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

(ert-deftest org-reading-list-test-insert-before-local-variables ()
  ;; A trailing file local-variables block must stay last: the new child
  ;; is filed inside Books, before the local-variables section, not after
  ;; it (`org-end-of-subtree' otherwise sweeps the block into the subtree).
  (with-temp-buffer
    (org-mode)
    (insert "* Books\n** TOREAD Old\n:PROPERTIES:\n:CUSTOM_ID: old\n:END:\n"
            "\n# Local Variables:\n# org-foo: bar\n# End:\n")
    (org-reading-list--insert-under-headline
     org-reading-list-test--entry "Books")
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\* TOREAD New$" nil t))
    (let ((new-pos (match-beginning 0))
          (lv-pos (progn (goto-char (point-min))
                         (re-search-forward "^# Local Variables:$" nil t)
                         (match-beginning 0))))
      ;; The new entry precedes the local-variables block.
      (should (< new-pos lv-pos)))
    ;; The local-variables block is intact.
    (goto-char (point-min))
    (should (re-search-forward "^# End:$" nil t))))


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

(ert-deftest org-reading-list-test-find-dup-loc-data ()
  ;; MARC-derived data matches an existing entry by ISBN (exact) even
  ;; though it carries no OLID.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (data (org-reading-list--marc-entry-data
                (list org-reading-list-test--loc-buried-ships)
                "ISBN:9798393569716" nil))
         (entries '((:pos 1 :heading "TOREAD Buried ships of San Francisco"
                     :isbns ("9798393569716") :olid nil
                     :title "Buried ships of San Francisco"
                     :author "Filion, Ron S"))))
    (should (eq (car (org-reading-list--find-duplicate data entries))
                'exact))))

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

(ert-deftest org-reading-list-test-preen-entry-from-subjects ()
  (with-temp-buffer
    (insert "#+TAGS: gold_rush\n"
            "* TOREAD X\n:PROPERTIES:\n:SUBJECTS: gold_discoveries; history\n:END:\n")
    (org-mode)
    (org-set-regexps-and-options)
    (let ((org-reading-list-tag-rewrites
           '(("gold_discoveries" . "gold_rush") ("history" . nil))))
      (goto-char (point-min))
      (re-search-forward "^\\* TOREAD X")
      (org-reading-list--preen-entry (org-reading-list--tag-vocabulary)
                                     org-reading-list-tag-rewrites)
      (should (equal (org-get-tags nil t) '("gold_rush"))))))

(ert-deftest org-reading-list-test-preen-entry-no-subjects ()
  (with-temp-buffer
    (insert "#+TAGS: gold_rush\n* TOREAD X\n:PROPERTIES:\n:TITLE: X\n:END:\n")
    (org-mode)
    (org-set-regexps-and-options)
    (goto-char (point-min))
    (re-search-forward "^\\* TOREAD X")
    (should (eq (org-reading-list--preen-entry
                 (org-reading-list--tag-vocabulary) nil)
                'no-subjects))))

(ert-deftest org-reading-list-test-fetch-subjects-unions-sources ()
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD X\n:PROPERTIES:\n:ISBN: 0375505415\n:END:\n")
    (goto-char (point-min))
    (re-search-forward "^\\* TOREAD X")
    (let ((org-reading-list-subject-functions
           (list (lambda () '("sailors" "whaling"))
                 (lambda () '("whaling" "sea_stories")))))
      (org-reading-list--fetch-entry-subjects)
      ;; Cross-source union + dedup, then normalized/alphabetized.
      (should (equal (org-entry-get nil "SUBJECTS")
                     "sailors; sea_stories; whaling")))))

(ert-deftest org-reading-list-test-fetch-subjects-appends-normalizes-sorts ()
  ;; :SUBJECTS: is authoritative and append-only: existing tokens survive,
  ;; new ones are normalized and unioned, the result is deduped and sorted.
  (let ((org-reading-list-subject-functions
         (list (lambda () '("gold_rush" "Mining")))))
    (with-temp-buffer
      (org-mode)
      (insert "* TOREAD X\n:PROPERTIES:\n"
              ":SUBJECTS: primary_source; gold_rush\n:END:\n")
      (goto-char (point-min))
      (let ((result (org-reading-list--fetch-entry-subjects)))
        (should (equal result '("gold_rush" "mining" "primary_source")))
        (should (equal (org-entry-get nil "SUBJECTS")
                       "gold_rush; mining; primary_source"))))))

(ert-deftest org-reading-list-test-resolve-identifiers-fills-from-loc ()
  ;; No identifier: a guarded LoC title/author lookup fills :ISBN: and :LCCN:.
  (cl-letf (((symbol-function 'org-reading-list--loc-entry-records)
             (lambda ()
               '((record nil
                         (datafield ((tag . "020") (ind1 . "") (ind2 . ""))
                                    (subfield ((code . "a")) "0375505415"))
                         (datafield ((tag . "010") (ind1 . "") (ind2 . ""))
                                    (subfield ((code . "a")) "  2001012345 ")))))))
    (with-temp-buffer
      (org-mode)
      (insert "* TOREAD T\n:PROPERTIES:\n:TITLE: T\n:AUTHOR: Doe, Jane\n:DATE: 1990\n:END:\n")
      (goto-char (point-min))
      (let ((added (org-reading-list--resolve-identifiers)))
        (should (equal added '("ISBN" "LCCN")))
        (should (equal (org-entry-get nil "ISBN") "0375505415"))
        (should (equal (org-entry-get nil "LCCN") "2001012345"))))))

(ert-deftest org-reading-list-test-resolve-identifiers-skips-when-identified ()
  ;; An existing identifier short-circuits: no LoC query, no change.
  (cl-letf (((symbol-function 'org-reading-list--loc-entry-records)
             (lambda () (error "should not query LoC when an identifier exists"))))
    (with-temp-buffer
      (org-mode)
      (insert "* TOREAD T\n:PROPERTIES:\n:ISBN: 0375505415\n:TITLE: T\n:END:\n")
      (goto-char (point-min))
      (should-not (org-reading-list--resolve-identifiers))
      (should (equal (org-entry-get nil "ISBN") "0375505415")))))

(ert-deftest org-reading-list-test-resolve-identifiers-failsafe-no-match ()
  ;; No confident match: leave the entry untouched (fail-safe).
  (cl-letf (((symbol-function 'org-reading-list--loc-entry-records)
             (lambda () (user-error "No LoC catalog record found"))))
    (with-temp-buffer
      (org-mode)
      (insert "* TOREAD T\n:PROPERTIES:\n:TITLE: Obscure\n:AUTHOR: Doe, Jane\n:END:\n")
      (goto-char (point-min))
      (should-not (org-reading-list--resolve-identifiers))
      (should-not (org-entry-get nil "ISBN"))
      (should-not (org-entry-get nil "LCCN")))))

(ert-deftest org-reading-list-test-enrich-entry-pipeline ()
  ;; The per-entry pipeline aggregates each stage and passes NO-PROMPT
  ;; through to the registered enrich functions.
  (let ((org-reading-list-enrich-functions
         (list (lambda (np) (if np '("HOLDINGS") '("PROMPTED"))))))
    (cl-letf (((symbol-function 'org-reading-list--resolve-identifiers)
               (lambda () '("ISBN")))
              ((symbol-function 'org-reading-list--loc-entry-records)
               (lambda () 'recs))
              ((symbol-function 'org-reading-list--loc-apply-fields)
               (lambda (_recs &optional _force) '("LCCN")))
              ((symbol-function 'org-reading-list--fetch-entry-subjects)
               (lambda () '("sea" "ships")))
              ((symbol-function 'org-reading-list--reproject-entry-tags)
               (lambda () nil)))
      (let ((r (org-reading-list--enrich-entry t)))
        (should (equal (plist-get r :ids) '("ISBN")))
        (should (equal (plist-get r :loc) '("LCCN")))
        (should (equal (plist-get r :ext) '("HOLDINGS")))
        (should (equal (plist-get r :subjects) '("sea" "ships")))))))

(ert-deftest org-reading-list-test-enrich-entry-failsafe-loc ()
  ;; A LoC no-match (user-error) does not abort the pipeline.
  (let ((org-reading-list-enrich-functions nil))
    (cl-letf (((symbol-function 'org-reading-list--resolve-identifiers)
               (lambda () nil))
              ((symbol-function 'org-reading-list--loc-entry-records)
               (lambda () (user-error "no record")))
              ((symbol-function 'org-reading-list--fetch-entry-subjects)
               (lambda () nil))
              ((symbol-function 'org-reading-list--reproject-entry-tags)
               (lambda () nil)))
      (let ((r (org-reading-list--enrich-entry)))
        (should-not (plist-get r :loc))
        (should-not (plist-get r :ext))))))

(ert-deftest org-reading-list-test-enrich-report ()
  (should (equal (org-reading-list--enrich-report
                  '(:ids ("ISBN") :loc ("LCCN" "LCC") :ext ("HOLDINGS") :subjects ("a" "b")))
                 "filled ISBN, LCCN, LCC, HOLDINGS; subjects: 2"))
  (should (equal (org-reading-list--enrich-report
                  '(:ids nil :loc nil :ext nil :subjects ("a")))
                 "no new fields; subjects: 1"))
  (should (equal (org-reading-list--enrich-report
                  '(:ids nil :loc nil :ext nil :subjects nil))
                 "nothing new")))

(ert-deftest org-reading-list-test-enrich-changed-p ()
  (should (org-reading-list--enrich-changed-p '(:loc ("LCCN"))))
  (should (org-reading-list--enrich-changed-p '(:ext ("HOLDINGS"))))
  (should-not (org-reading-list--enrich-changed-p
               '(:ids nil :loc nil :ext nil :subjects ("a")))))

(ert-deftest org-reading-list-test-enrich-all-reports-progress ()
  ;; The whole-file pass confirms, enriches each entry non-interactively,
  ;; and reports how many changed.
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD A\n:PROPERTIES:\n:BTYPE: book\n:END:\n"
            "* TOREAD B\n:PROPERTIES:\n:BTYPE: book\n:END:\n")
    (let ((calls 0) (msgs '()))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'org-reading-list--enrich-entry)
                 (lambda (&optional _np)
                   (setq calls (1+ calls))
                   (if (= calls 1) '(:loc ("LCCN"))
                     '(:ids nil :loc nil :ext nil :subjects nil))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (ignore-errors (apply #'format fmt args)) msgs)
                   nil)))
        (org-reading-list-enrich t))
      (should (= calls 2))
      (should (seq-find (lambda (m) (and m (string-match-p "1/2" m))) msgs))
      (should (seq-find (lambda (m) (and m (string-match-p "Enriched 1 of 2" m))) msgs)))))

(ert-deftest org-reading-list-test-trust-ol-defcustom ()
  (should (boundp 'org-reading-list-trust-openlibrary-subjects))
  (should-not (default-value 'org-reading-list-trust-openlibrary-subjects)))

(ert-deftest org-reading-list-test-subjects-upstream-gates-openlibrary ()
  ;; LoC MARC subjects are always included; Open Library's only when trusted.
  (cl-letf (((symbol-function 'org-reading-list--loc-entry-records)
             (lambda ()
               '((record nil
                         (datafield ((tag . "650") (ind1 . " ") (ind2 . "0"))
                                    (subfield ((code . "a")) "California"))
                         (datafield ((tag . "651") (ind1 . " ") (ind2 . "0"))
                                    (subfield ((code . "a")) "To 1846"))))))
            ((symbol-function 'org-reading-list--entry-data)
             (lambda (&rest _) (list :subjects '("twelve_step_programs")))))
    (with-temp-buffer
      (org-mode)
      (insert "* TOREAD X\n:PROPERTIES:\n:ISBN: 9780299149741\n:END:\n")
      (goto-char (point-min))
      (let ((org-reading-list-trust-openlibrary-subjects nil))
        (let ((s (org-reading-list--subjects-from-upstream)))
          (should (member "california" s))
          (should (member "to_1846" s))
          (should-not (member "twelve_step_programs" s))))
      (let ((org-reading-list-trust-openlibrary-subjects t))
        (let ((s (org-reading-list--subjects-from-upstream)))
          (should (member "california" s))
          (should (member "twelve_step_programs" s)))))))













(ert-deftest org-reading-list-test-entry-label ()
  "Label is \"Surname, Title (Year)\", degrading on missing parts."
  (should (equal (org-reading-list--entry-label
                  "Foote, Shelby" "The Civil War" "1963-01-01")
                 "Foote, The Civil War (1963)"))
  (should (equal (org-reading-list--entry-label nil "Untitled Work" nil)
                 "Untitled Work"))
  (should (equal (org-reading-list--entry-label "Foote, Shelby" nil "1963")
                 "Foote (1963)")))

(ert-deftest org-reading-list-test-disambiguate-labels ()
  "Colliding labels gain a [citekey] suffix; unique ones are untouched."
  (should (equal (org-reading-list--disambiguate-labels
                  '(("Foote (1963)" . "foote1963")
                    ("Foote (1963)" . "foote1963a")
                    ("Lotchin (1997)" . "lotchin1997")))
                 '(("Foote (1963) [foote1963]" . "foote1963")
                   ("Foote (1963) [foote1963a]" . "foote1963a")
                   ("Lotchin (1997)" . "lotchin1997")))))

(ert-deftest org-reading-list-test-entries-pure-and-omits-keyless ()
  "Entries reads keyed headings only and never writes."
  (let* ((org-reading-list-file (make-temp-file "rl" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file org-reading-list-file
            (insert "* Books\n"
                    "** TOREAD The Civil War\n:PROPERTIES:\n"
                    ":AUTHOR: Foote, Shelby\n:TITLE: The Civil War\n"
                    ":DATE: 1963\n:CUSTOM_ID: foote1963\n:END:\n"
                    "** TOREAD Keyless\n:PROPERTIES:\n:AUTHOR: Nobody, A\n"
                    ":TITLE: Keyless\n:DATE: 2000\n:END:\n"))
          (let ((before (with-temp-buffer
                          (insert-file-contents org-reading-list-file)
                          (buffer-string))))
            (should (equal (org-reading-list-entries)
                           '(("Foote, The Civil War (1963)" . "foote1963"))))
            (should (equal before
                           (with-temp-buffer
                             (insert-file-contents org-reading-list-file)
                             (buffer-string))))))
      (delete-file org-reading-list-file))))

(ert-deftest org-reading-list-test-ensure-citekey-idempotent ()
  "An entry that already has a citekey keeps it; a keyless one gets one."
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD Kept\n:PROPERTIES:\n:CUSTOM_ID: keep1\n:END:\n"
            "* TOREAD New\n:PROPERTIES:\n:AUTHOR: Foote, Shelby\n"
            ":DATE: 1963\n:END:\n")
    (goto-char (point-min))
    (should (equal (org-reading-list-ensure-citekey) "keep1"))
    (re-search-forward "New")
    (should (equal (org-reading-list-ensure-citekey) "foote1963"))
    (goto-char (point-min))
    (re-search-forward "New")
    (should (equal (org-entry-get nil "CUSTOM_ID") "foote1963"))))

(ert-deftest org-reading-list-test-ensure-citekeys-stamps-all ()
  "The file-wide command stamps every keyless entry, leaving keyed ones."
  (with-temp-buffer
    (org-mode)
    (insert "* Books\n"
            "** TOREAD Kept\n:PROPERTIES:\n:CUSTOM_ID: keep1\n:END:\n"
            "** TOREAD A\n:PROPERTIES:\n:AUTHOR: Foote, Shelby\n"
            ":DATE: 1963\n:END:\n"
            "** TOREAD B\n:PROPERTIES:\n:AUTHOR: Foote, Shelby\n"
            ":DATE: 1963\n:END:\n")
    (should (= (org-reading-list-ensure-citekeys) 2))
    (let (keys)
      (org-map-entries
       (lambda () (let ((k (org-entry-get nil "CUSTOM_ID")))
                    (when k (push k keys)))))
      ;; keep1 untouched; the two keyless entries get distinct keys.
      (should (member "keep1" keys))
      (should (member "foote1963" keys))
      (should (member "foote1963a" keys))
      (should (= (length keys) 3)))))

(ert-deftest org-reading-list-test-ensure-citekeys-stamps-authorless ()
  "An authorless book entry is stamped; the list container is left alone."
  (with-temp-buffer
    (org-mode)
    (insert "* Books\n"
            "** TOREAD Anon Pamphlet\n:PROPERTIES:\n:TITLE: Anon Pamphlet\n"
            ":DATE: 1850\n:END:\n")
    (should (= (org-reading-list-ensure-citekeys) 1))
    (goto-char (point-min))
    (should-not (org-entry-get nil "CUSTOM_ID"))
    (re-search-forward "Anon Pamphlet")
    (should (equal (org-entry-get nil "CUSTOM_ID") "anon1850"))))

(ert-deftest org-reading-list-test-link-export ()
  "An orl: link exports as its description, else the entry label, else key."
  (should (equal (org-reading-list-link-export "foote1963" "Foote" 'html)
                 "Foote"))
  (cl-letf (((symbol-function 'org-reading-list-entries)
             (lambda () '(("Foote, The Civil War (1963)" . "foote1963")))))
    (should (equal (org-reading-list-link-export "foote1963" nil 'html)
                   "Foote, The Civil War (1963)"))
    (should (equal (org-reading-list-link-export "ghost" nil 'html)
                   "ghost"))))

(ert-deftest org-reading-list-test-link-registered ()
  "The orl link type is registered with org."
  (should (org-link-get-parameter "orl" :follow))
  (should (org-link-get-parameter "orl" :export))
  (should (org-link-get-parameter "orl" :complete)))

(ert-deftest org-reading-list-test-link-complete ()
  "Completion returns an orl: link for the chosen entry."
  (cl-letf (((symbol-function 'org-reading-list-entries)
             (lambda () '(("Foote, The Civil War (1963)" . "foote1963"))))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "Foote, The Civil War (1963)")))
    (should (equal (org-reading-list-link-complete) "orl:foote1963"))))

(provide 'org-reading-list-tests)
;;; org-reading-list-tests.el ends here
