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

(provide 'org-reading-list-mi-tests)
;;; org-reading-list-mi-tests.el ends here
