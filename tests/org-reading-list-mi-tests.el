;;; org-reading-list-mi-tests.el --- Tests for org-reading-list-mi -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Offline unit tests for org-reading-list-mi: no network access.
;; Run with: make test

;;; Code:

(require 'ert)
(require 'org-reading-list-mi)
(require 'cl-lib)

(ert-deftest org-reading-list-mi-test-defcustoms ()
  (should (stringp org-reading-list-mi-search-url))
  (should (string-match-p "%s.*%s" org-reading-list-mi-search-url))
  (should (string-match-p "search.milibrary.org" org-reading-list-mi-marc-url))
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

(defconst org-reading-list-mi-test--marc-text
  "LEADER 00000cam  2200000 a 4500
001    44669515
003    OCoLC
008    000718s2000    nyu           000 0 eng
010    |a   00059095
020    |a 0375505415|q (alk. paper)
020    |a 9780375505416|q (alk. paper)
035    |a (OCoLC)44669515
082 04 |a 973.92|2 21
092    |a 973.92|b N53
100 1  |a Remnick, David.
245 14 |a The new gilded age :|b the New Yorker looks at the culture of affluence /|c edited by David Remnick.
264  1 |a New York :|b Random House,|c 2000.
300    |a xiii, 432 pages ;|c 25 cm
520    |a Thirty-three essays explore the culture of affluence.
650  0 |a Popular culture|z United States.
700 1  |a Remnick, David."
  "Representative WebPAC labeled-MARC display text.")

(ert-deftest org-reading-list-mi-test-parse-marc-control-field ()
  (let ((rec (org-reading-list-mi--parse-marc
              org-reading-list-mi-test--marc-text)))
    ;; Control field 001 has no subfields; its value is the text.
    (should (equal (org-reading-list--marc-field rec "001") "44669515"))))

(ert-deftest org-reading-list-mi-test-parse-marc-subfields ()
  (let ((rec (org-reading-list-mi--parse-marc
              org-reading-list-mi-test--marc-text)))
    (should (equal (org-reading-list--marc-field rec "245" "a" "b")
                   "The new gilded age : the New Yorker looks at the culture of affluence /"))
    (should (member "9780375505416"
                    (org-reading-list--marc-isbns rec "a")))
    (should (equal (org-reading-list--marc-field rec "010") "00059095"))
    (should (equal (org-reading-list--marc-field rec "100")
                   "Remnick, David."))
    (should (equal (org-reading-list--marc-field rec "092" "a" "b")
                   "973.92 N53"))))

(ert-deftest org-reading-list-mi-test-parse-marc-indicators ()
  (let* ((rec (org-reading-list-mi--parse-marc
               org-reading-list-mi-test--marc-text))
         (df (seq-find (lambda (d) (equal (dom-attr d 'tag) "082"))
                       (dom-by-tag rec 'datafield))))
    (should (equal (dom-attr df 'ind1) "0"))
    (should (equal (dom-attr df 'ind2) "4"))))

(ert-deftest org-reading-list-mi-test-parse-marc-empty ()
  (should-not (org-reading-list-mi--parse-marc "no marc here\njust text")))

(defconst org-reading-list-mi-test--results-html
  "<table class=\"browseResult\">
     <tr class=\"browseEntry\">
       <td><a href=\"/record=b1146522~S1\">The new gilded age</a></td>
       <td>2000</td></tr>
     <tr class=\"browseEntry\">
       <td><a href=\"/search?/.b1234567/.b1234567/1,1,1,B/frameset\">Moby Dick in Manhattan</a></td>
       <td>1994</td></tr>
   </table>"
  "Trimmed WebPAC results markup with two entries.")

(ert-deftest org-reading-list-mi-test-results-candidates ()
  (let* ((dom (with-temp-buffer
                (insert org-reading-list-mi-test--results-html)
                (libxml-parse-html-region (point-min) (point-max))))
         (cands (org-reading-list-mi--results-candidates dom)))
    (should (= (length cands) 2))
    (should (equal (plist-get (car cands) :bibid) "b1146522"))
    (should (equal (plist-get (car cands) :title) "The new gilded age"))
    (should (equal (plist-get (cadr cands) :bibid) "b1234567"))))

(ert-deftest org-reading-list-mi-test-results-candidates-cap ()
  (let ((org-reading-list-mi-max-results 1)
        (dom (with-temp-buffer
               (insert org-reading-list-mi-test--results-html)
               (libxml-parse-html-region (point-min) (point-max)))))
    (should (= (length (org-reading-list-mi--results-candidates dom)) 1))))

(ert-deftest org-reading-list-mi-test-marc-text-from-pre ()
  (let ((dom (with-temp-buffer
               (insert "<html><body><pre>245 14 |a Hello /</pre></body></html>")
               (libxml-parse-html-region (point-min) (point-max)))))
    (should (string-match-p "245" (org-reading-list-mi--marc-text dom)))))

(ert-deftest org-reading-list-mi-test-bib-record-chains ()
  (cl-letf (((symbol-function 'org-reading-list-mi--fetch-html)
             (lambda (url)
               (should (string-match-p "marc~b1146522" url))
               (with-temp-buffer
                 (insert "<html><body><pre>"
                         org-reading-list-mi-test--marc-text
                         "</pre></body></html>")
                 (libxml-parse-html-region (point-min) (point-max))))))
    (let ((rec (org-reading-list-mi--bib-record "b1146522")))
      (should (equal (org-reading-list--marc-field rec "001") "44669515")))))

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
  (let ((rec (org-reading-list-mi--parse-marc
              org-reading-list-mi-test--marc-text)))
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
  ;; With an ISBN, the OL/LoC pipeline supplies the base; MI overlays
  ;; holdings/callno and the better abstract.
  (let* ((org-reading-list-file "/nonexistent/orl-test.org")
         (rec '(record nil
                       (datafield ((tag . "020") (ind1 . "") (ind2 . ""))
                                  (subfield ((code . "a")) "0375505415"))
                       (datafield ((tag . "092") (ind1 . "") (ind2 . ""))
                                  (subfield ((code . "a")) "973.92")
                                  (subfield ((code . "b")) "N53"))
                       (datafield ((tag . "520") (ind1 . "") (ind2 . ""))
                                  (subfield ((code . "a"))
                                            "A much longer and richer MI summary of the book."))))
         (called-with nil))
    (cl-letf (((symbol-function 'org-reading-list--entry-data)
               (lambda (id &optional source)
                 (setq called-with (list id source))
                 (list :title "The new gilded age" :tags '("x") :isbns '("0375505415")
                       :props (list (cons "TITLE" "The new gilded age")
                                    (cons "ISBN" "0375505415")
                                    (cons "ABSTRACT" "short OL note")
                                    (cons "FOUND" source))))))
      (let* ((data (org-reading-list-mi--entry-data rec "src"))
             (props (plist-get data :props)))
        ;; Bridged through the existing pipeline by ISBN.
        (should (equal (car called-with) "ISBN:0375505415"))
        (should (equal (cdr (assoc "HOLDINGS" props)) "MILIB"))
        (should (equal (cdr (assoc "CALLNO" props)) "MILIB 973.92 N53"))
        ;; MI's longer abstract wins.
        (should (string-match-p "richer MI summary"
                                (cdr (assoc "ABSTRACT" props))))))))

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
                 (list (list :title "The new gilded age" :author nil
                             :year nil :bibid "b1146522"))))
              ((symbol-function 'org-reading-list-mi--bib-record)
               (lambda (_)
                 (org-reading-list-mi--parse-marc
                  org-reading-list-mi-test--marc-text)))
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





(provide 'org-reading-list-mi-tests)
;;; org-reading-list-mi-tests.el ends here
