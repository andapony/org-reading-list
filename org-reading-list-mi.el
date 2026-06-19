;;; org-reading-list-mi.el --- MILibrary search for org-reading-list -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Rob Duncan

;; Author: Rob Duncan
;; URL: https://github.com/YOUR-USERNAME/org-reading-list
;; Version: 0.7.0
;; Package-Requires: ((emacs "27.1") (org "9.4") (org-reading-list "0.7.0"))
;; Keywords: bib, outlines
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Discovery search against the Mechanics' Institute Library catalog
;; (milibrary.org).  Search the classic WebPAC by title (or another
;; index), pick a result, and capture it into the reading list or enrich
;; an existing heading.  MI's plain-text MARC is parsed into the same
;; record shape the Library of Congress code in org-reading-list.el uses,
;; so the MARC field extractors and entry builders are shared.

;;; Code:

(require 'org-reading-list)
(require 'dom)
(require 'cl-lib)

(defcustom org-reading-list-mi-search-url
  "https://search.milibrary.org/search~S1/?searchtype=%s&searcharg=%s&searchscope=1&SORT=D"
  "Search template for the Mechanics' Institute WebPAC.
The first %s is the index code (see `org-reading-list-mi-indexes'),
the second the url-encoded query."
  :type 'string
  :group 'org-reading-list)

(defcustom org-reading-list-mi-record-url
  "https://search.milibrary.org/record=%s"
  "Stable record URL for a MILibrary bib id (e.g. \"b1146522\")."
  :type 'string
  :group 'org-reading-list)

(defcustom org-reading-list-mi-marc-url
  "https://search.milibrary.org/search~S1?/.%1$s/.%1$s/1,1,1,B/marc~%1$s"
  "MARC-display URL template for a MILibrary bib id.
The single bib id fills every %1$s; the page is plain-text MARC."
  :type 'string
  :group 'org-reading-list)

(defcustom org-reading-list-mi-holdings-code "MILIB"
  "Holdings/CALLNO code applied to entries captured from MILibrary.
Must also appear in `org-reading-list-holdings-codes' to be offered
for completion elsewhere."
  :type 'string
  :group 'org-reading-list)

(defcustom org-reading-list-mi-max-results 25
  "Maximum number of search candidates offered for selection."
  :type 'integer
  :group 'org-reading-list)

(defconst org-reading-list-mi-indexes
  '(("title" . "t") ("author" . "a") ("keyword" . "X") ("ISBN" . "i"))
  "Search-index labels mapped to WebPAC `searchtype' codes.")

(defun org-reading-list-mi--fetch-html (url)
  "GET URL and parse the response body as HTML; return a DOM, or nil."
  (let ((buf (org-reading-list--http-body-buffer url 20)))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (ignore-errors
              (libxml-parse-html-region (point-min) (point-max))))
        (kill-buffer buf)))))

(defun org-reading-list-mi--parse-field (tag indicators body)
  "Build a datafield DOM node from TAG, INDICATORS, and BODY.
TAG is a 3-character string; INDICATORS is the two-character
indicator string; BODY is the field text.  A BODY containing
subfield delimiters (\"\\|c value\") becomes coded subfields;
otherwise the whole BODY is one anonymous subfield (control fields)."
  (let* ((ind1 (if (> (length indicators) 0)
                   (string-trim (substring indicators 0 1)) ""))
         (ind2 (if (> (length indicators) 1)
                   (string-trim (substring indicators 1 2)) ""))
         (subs (if (string-match-p "|" body)
                   (mapcar
                    (lambda (chunk)
                      (let ((code (substring chunk 0 1))
                            (val (string-trim (substring chunk 1))))
                        `(subfield ((code . ,code)) ,val)))
                    ;; Split on the delimiter; leading | means first part is removed.
                    (split-string body "|" t))
                 (list `(subfield ((code . "a")) ,(string-trim body))))))
    `(datafield ((tag . ,tag) (ind1 . ,ind1) (ind2 . ,ind2)) ,@subs)))

(defun org-reading-list-mi--parse-marc (text)
  "Parse WebPAC labeled-MARC TEXT into a `record' DOM node, or nil.
Each line beginning with a 3-digit tag becomes a datafield in the
same shape the Library of Congress MARCXML helpers consume, so the
existing extractors in org-reading-list.el work on the result.
Return nil when TEXT contains no tagged fields."
  (let (fields)
    (dolist (line (split-string text "[\n\r]+" t))
      (when (string-match
             "\\`\\([0-9][0-9][0-9]\\) ?\\(..\\)? ?\\(.*\\)\\'" line)
        (let ((tag (match-string 1 line))
              (ind (or (match-string 2 line) ""))
              (body (or (match-string 3 line) "")))
          (push (org-reading-list-mi--parse-field tag ind body) fields))))
    (when fields
      `(record nil ,@(nreverse fields)))))

(defun org-reading-list-mi--href-bibid (href)
  "Return the bib id (\"bNNNNNNN\") embedded in HREF, or nil."
  (when (and href
             (string-match "\\.?\\(b[0-9]+\\)" href))
    (match-string 1 href)))

(defun org-reading-list-mi--results-candidates (dom)
  "Collect search candidates from results-page DOM.
Return a list of plists (:title :author :year :bibid), one per row
that links to a bib record, capped at `org-reading-list-mi-max-results'."
  (let (cands)
    (catch 'done
      (dolist (a (dom-by-tag dom 'a))
        (let ((bibid (org-reading-list-mi--href-bibid (dom-attr a 'href)))
              (title (string-trim (dom-text a))))
          (when (and bibid (not (string-empty-p title))
                     (not (seq-find
                           (lambda (c) (equal (plist-get c :bibid) bibid))
                           cands)))
            (push (list :title title :author nil :year nil :bibid bibid)
                  cands)
            (when (>= (length cands) org-reading-list-mi-max-results)
              (throw 'done nil))))))
    (nreverse cands)))

(defun org-reading-list-mi--marc-text (dom)
  "Return the labeled-MARC text from a MARC-display DOM, or nil.
Prefers the first <pre> block; falls back to the whole body text when
no <pre> is present."
  (let ((pre (car (dom-by-tag dom 'pre))))
    (let ((text (if pre (dom-texts pre) (dom-texts dom))))
      (and text (not (string-empty-p (string-trim text))) text))))

(defun org-reading-list-mi--bib-record (bibid)
  "Fetch and parse the MARC record for BIBID; return a record DOM or nil."
  (let* ((url (format org-reading-list-mi-marc-url bibid))
         (dom (org-reading-list-mi--fetch-html url))
         (text (and dom (org-reading-list-mi--marc-text dom))))
    (and text (org-reading-list-mi--parse-marc text))))

(defun org-reading-list-mi--better-abstract (a b)
  "Return the more informative of abstracts A and B, or nil.
A nil or blank value loses to a non-blank one; when both have
content, the longer string wins."
  (let ((a (and a (not (string-blank-p a)) a))
        (b (and b (not (string-blank-p b)) b)))
    (cond ((and a b) (if (>= (length a) (length b)) a b))
          (a a)
          (b b))))

(provide 'org-reading-list-mi)
;;; org-reading-list-mi.el ends here
