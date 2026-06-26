;;; org-reading-list-mi-tests.el --- Tests for org-reading-list-mi -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Offline unit tests for org-reading-list-mi: no network access.
;; Run with: make test

;;; Code:

(require 'ert)
(require 'org-reading-list-mi)
(require 'cl-lib)

(defun org-reading-list-mi-test--parse-xml (s)
  "Parse string S as XML and return its DOM."
  (with-temp-buffer
    (insert s)
    (libxml-parse-xml-region (point-min) (point-max))))

(defun org-reading-list-mi-test--parse-html (s)
  "Parse string S as HTML and return its DOM."
  (with-temp-buffer
    (insert s)
    (libxml-parse-html-region (point-min) (point-max))))

(ert-deftest org-reading-list-mi-test-defcustoms ()
  (should (stringp org-reading-list-mi-search-url))
  (should (string-match-p "searchtype=X" org-reading-list-mi-search-url))
  (should (string-match-p "%s" org-reading-list-mi-search-url))
  (should (string-match-p "search.milibrary.org" org-reading-list-mi-xrecord-url))
  (should (equal org-reading-list-mi-holdings-code "MILIB"))
  (should (integerp org-reading-list-mi-max-results)))

(ert-deftest org-reading-list-mi-test-fetch-html-parses ()
  ;; --fetch-html turns a fetched body buffer into a DOM.
  (cl-letf (((symbol-function 'org-reading-list--http-body-buffer)
             (lambda (&rest _)
               (let ((b (generate-new-buffer " *mi-test*")))
                 (with-current-buffer b
                   (insert "<html><body><p id=\"x\">hi</p></body></html>"))
                 b))))
    (let ((dom (org-reading-list-mi--fetch-html "https://example/")))
      (should (equal (dom-text (dom-by-id dom "x")) "hi")))))

;;;; xrecord XML -> MARC record mapping

(defconst org-reading-list-mi-test--xrecord-xml
  "<IIIRECORD><TYPEINFO><BIBLIOGRAPHIC>
<VARFLD><MARCINFO><MARCTAG>001</MARCTAG></MARCINFO><MARCFIXDATA>44669515</MARCFIXDATA></VARFLD>
<VARFLD><MARCINFO><MARCTAG>020</MARCTAG><INDICATOR1> </INDICATOR1><INDICATOR2> </INDICATOR2></MARCINFO><MARCSUBFLD><SUBFIELDINDICATOR>a</SUBFIELDINDICATOR><SUBFIELDDATA>0375505415</SUBFIELDDATA></MARCSUBFLD></VARFLD>
<VARFLD><MARCINFO><MARCTAG>092</MARCTAG></MARCINFO><MARCSUBFLD><SUBFIELDINDICATOR>a</SUBFIELDINDICATOR><SUBFIELDDATA>973.92</SUBFIELDDATA></MARCSUBFLD><MARCSUBFLD><SUBFIELDINDICATOR>b</SUBFIELDINDICATOR><SUBFIELDDATA>N53</SUBFIELDDATA></MARCSUBFLD></VARFLD>
<VARFLD><MARCINFO><MARCTAG>245</MARCTAG><INDICATOR1>1</INDICATOR1><INDICATOR2>4</INDICATOR2></MARCINFO><MARCSUBFLD><SUBFIELDINDICATOR>a</SUBFIELDINDICATOR><SUBFIELDDATA>The new gilded age :</SUBFIELDDATA></MARCSUBFLD><MARCSUBFLD><SUBFIELDINDICATOR>b</SUBFIELDINDICATOR><SUBFIELDDATA>the New Yorker looks at the culture of affluence /</SUBFIELDDATA></MARCSUBFLD></VARFLD>
<VARFLD><MARCINFO><MARCTAG>520</MARCTAG></MARCINFO><MARCSUBFLD><SUBFIELDINDICATOR>a</SUBFIELDINDICATOR><SUBFIELDDATA>Thirty-three essays explore the culture of affluence.</SUBFIELDDATA></MARCSUBFLD></VARFLD>
</BIBLIOGRAPHIC></TYPEINFO></IIIRECORD>"
  "Trimmed Innovative Interfaces xrecord XML for one bib record.")

(ert-deftest org-reading-list-mi-test-xrecord-to-record ()
  (let ((rec (org-reading-list-mi--xrecord-to-record
              (org-reading-list-mi-test--parse-xml
               org-reading-list-mi-test--xrecord-xml))))
    (should (equal (org-reading-list--marc-field rec "245" "a" "b")
                   "The new gilded age : the New Yorker looks at the culture of affluence /"))
    (should (member "0375505415" (org-reading-list--marc-isbns rec "a")))
    (should (equal (org-reading-list--marc-field rec "092" "a" "b")
                   "973.92 N53"))
    ;; A control field (no subfields) is skipped.
    (should-not (org-reading-list--marc-field rec "001"))))

(ert-deftest org-reading-list-mi-test-xrecord-indicators ()
  (let* ((rec (org-reading-list-mi--xrecord-to-record
               (org-reading-list-mi-test--parse-xml
                org-reading-list-mi-test--xrecord-xml)))
         (df (seq-find (lambda (d) (equal (dom-attr d 'tag) "245"))
                       (dom-by-tag rec 'datafield))))
    (should (equal (dom-attr df 'ind1) "1"))
    (should (equal (dom-attr df 'ind2) "4"))))

(ert-deftest org-reading-list-mi-test-bib-record-fetches-xrecord ()
  (cl-letf (((symbol-function 'org-reading-list--fetch-xml)
             (lambda (url)
               (should (string-match-p "xrecord=b1146522" url))
               (org-reading-list-mi-test--parse-xml
                org-reading-list-mi-test--xrecord-xml))))
    (let ((rec (org-reading-list-mi--bib-record "b1146522")))
      (should (equal (org-reading-list--marc-field rec "245" "a")
                     "The new gilded age :")))))

;;;; Keyword search-results parsing

(defconst org-reading-list-mi-test--results-html
  "<div class=\"briefcitCell\">
     <div class=\"briefcitMark\"><input type=\"checkbox\" name=\"save\" value=\"b1146522\"></div>
     <div class=\"briefcitTitle\"><a href=\"/search?/X/frameset\">The new gilded age</a></div>
     <div class=\"briefcitAuthor\"><a href=\"/x\">Remnick, David</a></div>
     <div class=\"briefcitMedia\"><img src=\"/x\" alt=\"Books\"></div>
     <div class=\"briefcitPublisher\">Random House, 2000.</div>
   </div>
   <div class=\"briefcitCell\">
     <div class=\"briefcitMark\"><input type=\"checkbox\" name=\"save\" value=\"b1243827\"></div>
     <div class=\"briefcitTitle\"><a href=\"/x\">Moby-Dick, or, The whale</a></div>
     <div class=\"briefcitAuthor\"><a href=\"/x\">Melville, Herman</a></div>
     <div class=\"briefcitMedia\"><img src=\"/x\" alt=\"DVD\"></div>
     <div class=\"briefcitPublisher\">Warner, 2001.</div>
   </div>"
  "Trimmed MILibrary keyword brief-citation results, two rows.")

(ert-deftest org-reading-list-mi-test-results-candidates ()
  (let* ((dom (org-reading-list-mi-test--parse-html
               org-reading-list-mi-test--results-html))
         (cands (org-reading-list-mi--results-candidates dom)))
    (should (= (length cands) 2))
    (should (equal (plist-get (car cands) :bibid) "b1146522"))
    (should (equal (plist-get (car cands) :title) "The new gilded age"))
    (should (equal (plist-get (car cands) :author) "Remnick, David"))
    (should (equal (plist-get (car cands) :format) "Books"))
    (should (equal (plist-get (car cands) :year) "2000"))
    (should (equal (plist-get (cadr cands) :bibid) "b1243827"))
    (should (equal (plist-get (cadr cands) :title) "Moby-Dick, or, The whale"))
    (should (equal (plist-get (cadr cands) :format) "DVD"))))

(ert-deftest org-reading-list-mi-test-candidate-label ()
  (should (equal (org-reading-list-mi--candidate-label
                  '(:title "Moby Dick" :author "Peck, Gregory"
                    :format "DVD" :year "2001"))
                 "Moby Dick - Peck, Gregory  [DVD, 2001]"))
  ;; Missing pieces are omitted cleanly.
  (should (equal (org-reading-list-mi--candidate-label
                  '(:title "Just a title" :author nil :format nil :year nil))
                 "Just a title")))


(ert-deftest org-reading-list-mi-test-results-candidates-cap ()
  (let ((org-reading-list-mi-max-results 1)
        (dom (org-reading-list-mi-test--parse-html
              org-reading-list-mi-test--results-html)))
    (should (= (length (org-reading-list-mi--results-candidates dom)) 1))))

(defconst org-reading-list-mi-test--single-html
  "<html><body>
     <td class=\"bibInfoLabel\">Title</td>
     <td class=\"bibInfoData\"><strong>Buried ships of San Francisco</strong></td>
     <a href=\"/record=b1275806\">Permalink</a>
   </body></html>"
  "Trimmed single-record page (a unique search jumps straight to it).")

(ert-deftest org-reading-list-mi-test-results-candidates-single-record ()
  ;; No brief-citation rows: fall back to the single-record page.
  (let* ((dom (org-reading-list-mi-test--parse-html
               org-reading-list-mi-test--single-html))
         (cands (org-reading-list-mi--results-candidates dom)))
    (should (= (length cands) 1))
    (should (equal (plist-get (car cands) :bibid) "b1275806"))
    (should (equal (plist-get (car cands) :title)
                   "Buried ships of San Francisco"))))

;;;; Data build and update

(ert-deftest org-reading-list-mi-test-better-abstract ()
  (should (equal (org-reading-list-mi--better-abstract nil "x") "x"))
  (should (equal (org-reading-list-mi--better-abstract "y" nil) "y"))
  (should-not (org-reading-list-mi--better-abstract nil nil))
  (should-not (org-reading-list-mi--better-abstract "" "   "))
  (should (equal (org-reading-list-mi--better-abstract "short" "much longer one")
                 "much longer one"))
  (should (equal (org-reading-list-mi--better-abstract "much longer one" "short")
                 "much longer one")))

(ert-deftest org-reading-list-mi-test-callno ()
  (let ((rec '(record nil
                      (datafield ((tag . "092") (ind1 . "") (ind2 . ""))
                                 (subfield ((code . "a")) "973.92")
                                 (subfield ((code . "b")) "N53")))))
    (should (equal (org-reading-list-mi--callno rec) "973.92 N53"))))

(ert-deftest org-reading-list-mi-test-entry-data-mi-only ()
  ;; No ISBN/LCCN -> build straight from MI MARC; holdings + callno added.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (rec '(record nil
                       (datafield ((tag . "092") (ind1 . "") (ind2 . ""))
                                  (subfield ((code . "a")) "979.461")
                                  (subfield ((code . "b")) "L88"))
                       (datafield ((tag . "100") (ind1 . "1") (ind2 . ""))
                                  (subfield ((code . "a")) "Lewis, Oscar."))
                       (datafield ((tag . "245") (ind1 . "1") (ind2 . "0"))
                                  (subfield ((code . "a")) "San Francisco /"))
                       (datafield ((tag . "520") (ind1 . "") (ind2 . ""))
                                  (subfield ((code . "a")) "A local history."))))
         (data (org-reading-list-mi--entry-data rec "found-url"))
         (props (plist-get data :props)))
    (should (equal (plist-get data :title) "San Francisco"))
    (should (equal (cdr (assoc "HOLDINGS" props)) "MILIB"))
    (should (equal (cdr (assoc "CALLNO" props)) "MILIB 979.461 L88"))
    (should (equal (cdr (assoc "ABSTRACT" props)) "A local history."))
    (should (equal (cdr (assoc "FOUND" props)) "found-url"))))

(ert-deftest org-reading-list-mi-test-entry-data-bridges-on-isbn ()
  ;; MI MARC is the base; the ISBN bridge only fills fields MI lacks.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (rec '(record nil
                       (datafield ((tag . "020") (ind1 . "") (ind2 . ""))
                                  (subfield ((code . "a")) "0375505415"))
                       (datafield ((tag . "100") (ind1 . "1") (ind2 . ""))
                                  (subfield ((code . "a")) "Remnick, David."))
                       (datafield ((tag . "245") (ind1 . "1") (ind2 . "4"))
                                  (subfield ((code . "a")) "The New Gilded Age /"))
                       (datafield ((tag . "092") (ind1 . "") (ind2 . ""))
                                  (subfield ((code . "a")) "973.92")
                                  (subfield ((code . "b")) "N53"))))
         (called-with nil))
    (cl-letf (((symbol-function 'org-reading-list--entry-data)
               (lambda (id &optional source)
                 (setq called-with (list id source))
                 (list :title "OL Edition Title" :tags '("oltag")
                       :isbns '("0375505415")
                       :props (list (cons "TITLE" "OL Edition Title")
                                    (cons "ISBN" "0375505415")
                                    (cons "OLID" "OL123M")
                                    (cons "DDC" "813.54")
                                    (cons "ABSTRACT" "An OL abstract."))))))
      (let* ((data (org-reading-list-mi--entry-data rec "src"))
             (props (plist-get data :props)))
        ;; The bridge was consulted by ISBN.
        (should (equal (car called-with) "ISBN:0375505415"))
        ;; MI's title wins over Open Library's.
        (should (equal (plist-get data :title) "The New Gilded Age"))
        (should (equal (cdr (assoc "TITLE" props)) "The New Gilded Age"))
        ;; Fields MI lacks are filled from Open Library.
        (should (equal (cdr (assoc "OLID" props)) "OL123M"))
        (should (equal (cdr (assoc "DDC" props)) "813.54"))
        (should (equal (cdr (assoc "ABSTRACT" props)) "An OL abstract."))
        ;; MI holdings/callno attached.
        (should (equal (cdr (assoc "HOLDINGS" props)) "MILIB"))
        (should (equal (cdr (assoc "CALLNO" props)) "MILIB 973.92 N53"))))))

(ert-deftest org-reading-list-mi-test-enrich-unions-subjects ()
  ;; MI subjects and OL subjects are unioned into :subjects.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (rec '(record nil
                       (datafield ((tag . "020") (ind1 . "") (ind2 . ""))
                                  (subfield ((code . "a")) "0375505415"))
                       (datafield ((tag . "650") (ind1 . " ") (ind2 . "0"))
                                  (subfield ((code . "a")) "Sailors"))
                       (datafield ((tag . "245") (ind1 . "1") (ind2 . "0"))
                                  (subfield ((code . "a")) "T /")))))
    (cl-letf (((symbol-function 'org-reading-list--entry-data)
               (lambda (&rest _)
                 (list :title "T" :subjects '("whaling") :tags '("whaling")
                       :isbns '("0375505415") :props '(("TITLE" . "T"))))))
      (let ((data (org-reading-list-mi--entry-data rec "src")))
        (should (member "sailors" (plist-get data :subjects)))
        (should (member "whaling" (plist-get data :subjects)))))))


(ert-deftest org-reading-list-mi-test-entry-data-bridges-on-lccn ()
  ;; With a 010 (LCCN) and no 020 (ISBN), the bridge uses an LCCN id;
  ;; MI's title still wins and Open Library fills only the gaps.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (rec '(record nil
                       (datafield ((tag . "010") (ind1 . "") (ind2 . ""))
                                  (subfield ((code . "a")) "  00059095"))
                       (datafield ((tag . "245") (ind1 . "1") (ind2 . "0"))
                                  (subfield ((code . "a")) "MI Title /"))
                       (datafield ((tag . "092") (ind1 . "") (ind2 . ""))
                                  (subfield ((code . "a")) "973.92")
                                  (subfield ((code . "b")) "N53"))))
         (called-with nil))
    (cl-letf (((symbol-function 'org-reading-list--entry-data)
               (lambda (id &optional source)
                 (setq called-with (list id source))
                 (list :title "OL Title" :tags nil :isbns nil
                       :props (list (cons "TITLE" "OL Title")
                                    (cons "OLID" "OL999M"))))))
      (let* ((data (org-reading-list-mi--entry-data rec "src"))
             (props (plist-get data :props)))
        (should (equal (car called-with) "LCCN:00059095"))
        (should (equal (plist-get data :title) "MI Title"))
        (should (equal (cdr (assoc "OLID" props)) "OL999M"))
        (should (equal (cdr (assoc "HOLDINGS" props)) "MILIB"))
        (should (equal (cdr (assoc "CALLNO" props)) "MILIB 973.92 N53"))))))

(ert-deftest org-reading-list-mi-test-entry-data-bridge-falls-back-to-mi ()
  ;; ISBN present but neither Open Library nor LoC has it (an MI-only
  ;; edition): fall back to MI's own MARC instead of erroring.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (rec '(record nil
                       (datafield ((tag . "020") (ind1 . "") (ind2 . ""))
                                  (subfield ((code . "a")) "0792850149"))
                       (datafield ((tag . "092") (ind1 . "") (ind2 . ""))
                                  (subfield ((code . "a")) "FIC")
                                  (subfield ((code . "b")) "M"))
                       (datafield ((tag . "245") (ind1 . "1") (ind2 . "0"))
                                  (subfield ((code . "a")) "Moby Dick /")))))
    (cl-letf (((symbol-function 'org-reading-list--entry-data)
               (lambda (&rest _)
                 (user-error "No Open Library or LoC record for ISBN:0792850149"))))
      (let* ((data (org-reading-list-mi--entry-data rec "src"))
             (props (plist-get data :props)))
        (should (equal (plist-get data :title) "Moby Dick"))
        (should (equal (cdr (assoc "HOLDINGS" props)) "MILIB"))
        (should (equal (cdr (assoc "CALLNO" props)) "MILIB FIC M"))))))


(ert-deftest org-reading-list-mi-test-apply-update-enrich-empty ()
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD A Book\n:PROPERTIES:\n:TITLE: A Book\n:END:\n")
    (goto-char (point-min))
    (let ((data (list :title "A Book"
                      :props '(("TITLE" . "A Book")
                               ("ISBN" . "0375505415")
                               ("HOLDINGS" . "MILIB")
                               ("CALLNO" . "MILIB 973.92 N53")))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (org-reading-list-mi--apply-update data))
      ;; Empty field filled; holdings/callno applied.
      (should (equal (org-entry-get nil "ISBN") "0375505415"))
      (should (string-match-p "MILIB" (org-entry-get nil "HOLDINGS")))
      (should (equal (org-entry-get nil "CALLNO") "MILIB 973.92 N53")))))

(ert-deftest org-reading-list-mi-test-callno-merge-appends-new-code ()
  ;; Entry already has SFPL; applying MILIB appends it.
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD A Book\n:PROPERTIES:\n:CALLNO: SFPL 979.461 L88\n:END:\n")
    (goto-char (point-min))
    (let ((data (list :title "A Book"
                      :props '(("HOLDINGS" . "MILIB")
                               ("CALLNO" . "MILIB 222 A1")))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (org-reading-list-mi--apply-update data))
      (let ((callno (org-entry-get nil "CALLNO")))
        (should (string-match-p "SFPL 979.461 L88" callno))
        (should (string-match-p "MILIB 222 A1" callno))))))

(ert-deftest org-reading-list-mi-test-callno-merge-replaces-same-code ()
  ;; Entry has MILIB old and SFPL; applying MILIB new replaces MILIB in place.
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD A Book\n:PROPERTIES:\n:CALLNO: MILIB old 1; SFPL keep 2\n:END:\n")
    (goto-char (point-min))
    (let ((data (list :title "A Book"
                      :props '(("HOLDINGS" . "MILIB")
                               ("CALLNO" . "MILIB new 9")))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (org-reading-list-mi--apply-update data))
      (let ((callno (org-entry-get nil "CALLNO")))
        (should (string-match-p "MILIB new 9" callno))
        (should (string-match-p "SFPL keep 2" callno))
        (should-not (string-match-p "MILIB old 1" callno))))))

(ert-deftest org-reading-list-mi-test-callno-merge-no-existing ()
  ;; Entry has no CALLNO; result is exactly the incoming pair.
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD A Book\n:PROPERTIES:\n:TITLE: A Book\n:END:\n")
    (goto-char (point-min))
    (let ((data (list :title "A Book"
                      :props '(("HOLDINGS" . "MILIB")
                               ("CALLNO" . "MILIB 222 A1")))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (org-reading-list-mi--apply-update data))
      (should (equal (org-entry-get nil "CALLNO") "MILIB 222 A1")))))

(ert-deftest org-reading-list-mi-test-apply-update-overwrite-confirmed ()
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD A Book\n:PROPERTIES:\n:ABSTRACT: old short\n:END:\n")
    (goto-char (point-min))
    (let ((data (list :title "A Book"
                      :props '(("ABSTRACT" . "new much longer abstract text")
                               ("HOLDINGS" . "MILIB"))))
          (asked nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq asked t) t)))
        (org-reading-list-mi--apply-update data))
      (should asked)
      (should (equal (org-entry-get nil "ABSTRACT")
                     "new much longer abstract text")))))

(ert-deftest org-reading-list-mi-test-apply-update-overwrite-declined ()
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD A Book\n:PROPERTIES:\n:ABSTRACT: keep me\n:END:\n")
    (goto-char (point-min))
    (let ((data (list :title "A Book"
                      :props '(("ABSTRACT" . "do not use")))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (org-reading-list-mi--apply-update data))
      (should (equal (org-entry-get nil "ABSTRACT") "keep me")))))

(ert-deftest org-reading-list-mi-test-enrich-at-point ()
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD The New Gilded Age\n:PROPERTIES:\n"
            ":TITLE: The New Gilded Age\n:ISBN: 0375505415\n:END:\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'org-reading-list-mi--search-candidates)
               (lambda (&rest _)
                 (list (list :title "The new gilded age" :author "Remnick, David"
                             :year nil :bibid "b1146522"))))
              ((symbol-function 'org-reading-list-mi--bib-record)
               (lambda (_)
                 '(record nil
                          (datafield ((tag . "020") (ind1 . "") (ind2 . ""))
                                     (subfield ((code . "a")) "0375505415"))
                          (datafield ((tag . "092") (ind1 . "") (ind2 . ""))
                                     (subfield ((code . "a")) "973.92")
                                     (subfield ((code . "b")) "N53"))
                          (datafield ((tag . "520") (ind1 . "") (ind2 . ""))
                                     (subfield ((code . "a"))
                                               "Thirty-three essays explore the culture.")))))
              ;; Stub the OL/LoC base so the ISBN bridge stays offline.
              ((symbol-function 'org-reading-list--entry-data)
               (lambda (&rest _)
                 (list :title "The New Gilded Age" :tags nil
                       :isbns '("0375505415")
                       :props (list (cons "TITLE" "The New Gilded Age")
                                    (cons "ABSTRACT" "short")))))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (org-reading-list-mi-enrich)
      (should (string-match-p "MILIB" (org-entry-get nil "HOLDINGS")))
      (should (equal (org-entry-get nil "CALLNO") "MILIB 973.92 N53"))
      (should (string-match-p "essays"
                              (or (org-entry-get nil "ABSTRACT") ""))))))

(ert-deftest org-reading-list-mi-test-search-writes-subjects-and-projects ()
  ;; mi-search stores the full :SUBJECTS: and projects the heading tags.
  (let* ((tmp (make-temp-file "orl-mi-subj" nil ".org"))
         (org-reading-list-file tmp)
         (org-reading-list-headline "Books")
         (org-reading-list-tag-rewrites
          '(("gold_discoveries" . "gold_rush") ("history" . nil))))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "#+TAGS: gold_rush\n* Books\n"))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "q"))
                    ((symbol-function 'pop-to-buffer) #'set-buffer)
                    ((symbol-function 'org-reading-list-mi--search-candidates)
                     (lambda (&rest _) (list (list :title "Gold Book" :bibid "b1"))))
                    ((symbol-function 'org-reading-list-mi--choose)
                     (lambda (c) (plist-get (car c) :bibid)))
                    ((symbol-function 'org-reading-list-mi--bib-record)
                     (lambda (_) '(record nil)))
                    ((symbol-function 'org-reading-list-mi--entry-data)
                     (lambda (&rest _)
                       (list :title "Gold Book"
                             :subjects '("gold_discoveries" "history")
                             :tags '("gold_discoveries" "history")
                             :props '(("TITLE" . "Gold Book") ("BTYPE" . "book"))))))
            (org-reading-list-mi-search))
          (with-current-buffer (find-file-noselect tmp)
            (goto-char (point-min))
            (should (re-search-forward "^\\*\\* TOREAD Gold Book.*:gold_rush:$" nil t))
            (goto-char (point-min))
            (should (re-search-forward "^:SUBJECTS: gold_discoveries; history$" nil t))))
      (when (get-file-buffer tmp) (kill-buffer (get-file-buffer tmp)))
      (delete-file tmp))))

(ert-deftest org-reading-list-mi-test-subjects-source ()
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD X\n:PROPERTIES:\n:ISBN: 0375505415\n:END:\n")
    (goto-char (point-min))
    (re-search-forward "^\\* TOREAD X")
    (cl-letf (((symbol-function 'org-reading-list-mi--entry-bibid)
               (lambda () "b1"))
              ((symbol-function 'org-reading-list-mi--bib-record)
               (lambda (_)
                 '(record nil
                          (datafield ((tag . "650") (ind1 . " ") (ind2 . "0"))
                                     (subfield ((code . "a")) "Sailors"))))))
      (should (equal (org-reading-list-mi--subjects) '("sailors")))))
  ;; The source is registered.
  (should (memq #'org-reading-list-mi--subjects org-reading-list-subject-functions)))

(ert-deftest org-reading-list-mi-test-candidate-match-p ()
  ;; Surname must agree.
  (should-not (org-reading-list-mi--candidate-match-p
               '(:author "Justice, Donald, 1925-2004, author." :year "2004")
               "Colton, Walter" "1850"))
  ;; Surname agrees, years agree -> match.
  (should (org-reading-list-mi--candidate-match-p
           '(:author "Colton, Walter." :year "1850")
           "Colton, Walter" "1850"))
  ;; Surname agrees, years disagree -> no match.
  (should-not (org-reading-list-mi--candidate-match-p
               '(:author "Colton, Walter." :year "1999")
               "Colton, Walter" "1850"))
  ;; Year unknown on either side is permissive (surname still required).
  (should (org-reading-list-mi--candidate-match-p
           '(:author "Colton, Walter." :year nil)
           "Colton, Walter" "1850"))
  ;; No candidate author -> cannot confirm -> no match.
  (should-not (org-reading-list-mi--candidate-match-p
               '(:author nil :year "1850")
               "Colton, Walter" "1850")))

(ert-deftest org-reading-list-mi-test-entry-bibid-rejects-mismatch ()
  (cl-letf (((symbol-function 'org-reading-list-mi--search-candidates)
             (lambda (&rest _)
               (list '(:title "Collected poems" :author "Justice, Donald, 1925-2004, author."
                              :year "2004" :bibid "b1164904")
                     '(:title "Poets of World War II" :author nil
                              :year "2003" :bibid "b1158171")))))
    (with-temp-buffer
      (org-mode)
      (insert "* TOREAD Three Years in California\n:PROPERTIES:\n"
              ":TITLE: Three Years in California\n:AUTHOR: Colton, Walter\n"
              ":DATE: 1850\n:END:\n")
      (goto-char (point-min))
      (should-not (org-reading-list-mi--entry-bibid)))))

(ert-deftest org-reading-list-mi-test-entry-bibid-accepts-match ()
  (cl-letf (((symbol-function 'org-reading-list-mi--search-candidates)
             (lambda (&rest _)
               (list '(:title "Collected poems" :author "Justice, Donald" :year "2004" :bibid "b1")
                     '(:title "Three Years in California" :author "Colton, Walter"
                              :year "1850" :bibid "b999")))))
    (with-temp-buffer
      (org-mode)
      (insert "* TOREAD Three Years in California\n:PROPERTIES:\n"
              ":TITLE: Three Years in California\n:AUTHOR: Colton, Walter\n"
              ":DATE: 1850\n:END:\n")
      (goto-char (point-min))
      (should (equal (org-reading-list-mi--entry-bibid) "b999")))))

(ert-deftest org-reading-list-mi-test-apply-update-no-prompt-skips-differing ()
  ;; With NO-PROMPT, a differing non-empty field is left alone (never prompts),
  ;; while empty fields are still filled.
  (with-temp-buffer
    (org-mode)
    (insert "* TOREAD A Book\n:PROPERTIES:\n:ABSTRACT: keep me\n:END:\n")
    (goto-char (point-min))
    (let ((data (list :title "A Book"
                      :props '(("ABSTRACT" . "do not use") ("LCC" . "F999")))))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (error "must not prompt in no-prompt mode"))))
        (let ((changed (org-reading-list-mi--apply-update data t)))
          (should (equal (org-entry-get nil "ABSTRACT") "keep me"))
          (should (equal (org-entry-get nil "LCC") "F999"))
          (should (member "LCC" changed))
          (should-not (member "ABSTRACT" changed)))))))

(ert-deftest org-reading-list-mi-test-entry-record-data-failsafe ()
  ;; No confident bib id -> nil, so callers can fail safe.
  (cl-letf (((symbol-function 'org-reading-list-mi--entry-bibid) (lambda () nil)))
    (with-temp-buffer
      (org-mode)
      (insert "* TOREAD X\n:PROPERTIES:\n:TITLE: X\n:END:\n")
      (goto-char (point-min))
      (should-not (org-reading-list-mi--entry-record-data)))))

(ert-deftest org-reading-list-mi-test-entry-record-data-builds ()
  ;; A confident bib id + record yields entry-data.
  (cl-letf (((symbol-function 'org-reading-list-mi--entry-bibid) (lambda () "b1"))
            ((symbol-function 'org-reading-list-mi--bib-record)
             (lambda (_) '(record nil
                                  (datafield ((tag . "245") (ind1 . "1") (ind2 . "0"))
                                             (subfield ((code . "a")) "X /")))))
            ((symbol-function 'org-reading-list--entry-data) (lambda (&rest _) nil)))
    (with-temp-buffer
      (org-mode)
      (insert "* TOREAD X\n:PROPERTIES:\n:TITLE: X\n:END:\n")
      (goto-char (point-min))
      (let ((data (org-reading-list-mi--entry-record-data)))
        (should data)
        (should (equal (plist-get data :title) "X"))))))

(ert-deftest org-reading-list-mi-test-enrich-registered ()
  ;; The MI enrich source is registered on the orchestrator hook.
  (should (memq #'org-reading-list-mi--enrich org-reading-list-enrich-functions)))








(provide 'org-reading-list-mi-tests)
;;; org-reading-list-mi-tests.el ends here
