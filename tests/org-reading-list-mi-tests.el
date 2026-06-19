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

(provide 'org-reading-list-mi-tests)
;;; org-reading-list-mi-tests.el ends here
