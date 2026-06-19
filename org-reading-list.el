;;; org-reading-list.el --- Bibliographic reading list in Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Rob Duncan

;; Author: Rob Duncan
;; URL: https://github.com/YOUR-USERNAME/org-reading-list
;; Version: 0.7.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: bib, outlines
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Maintain a reading list as an Org file, one heading per book, with
;; bibliographic data in property drawers.  The schema follows BibTeX
;; field names where equivalents exist, so the list exports to a .bib
;; with little translation; non-BibTeX identifiers (LCCN, OCLC, LC and
;; Dewey classification, local call numbers, holdings) ride along in
;; their own properties.  See doc/reading-list-schema.org for the full
;; schema, format conventions, and crosswalks to MARC 21, BibTeX, and
;; the Open Library API.
;;
;; Entry points:
;;
;;   `org-reading-list-insert'       File an entry in the reading list
;;                                   file, fetched from Open Library by
;;                                   ISBN, Open Library edition id, or
;;                                   OL URL, falling back to the LC
;;                                   Catalog for ISBNs and LCCNs Open
;;                                   Library lacks.  Books already in
;;                                   the list (same ISBN/OLID, or same
;;                                   title and author) prompt before
;;                                   adding.
;;   `org-reading-list-capture'      The same, shaped for org-capture
;;                                   templates via "%(...)".
;;   `org-reading-list-loc-enrich'   Fill missing LCCN/OCLC/LCC/DDC on
;;                                   the entry at point from the Library
;;                                   of Congress catalog (SRU/MARCXML).
;;   `org-reading-list-loc-tags'     Harvest LoC 650/651 subject
;;                                   headings as Org tags.
;;   `org-reading-list-set-holdings' Record where a book can be
;;                                   obtained (own collection,
;;                                   libraries), with completion.
;;   `org-reading-list-download-pdf' Download an entry's Internet
;;                                   Archive scan as a local PDF.
;;
;; A typical capture template, filing under the same heading as
;; `org-reading-list-insert' via `org-reading-list-goto-headline':
;;
;;   ("ri" "Book by ISBN" entry
;;    (file+function org-reading-list-file org-reading-list-goto-headline)
;;    "%(org-reading-list-capture)" :empty-lines 1)
;;
;; The Library of Congress asks automated clients of lx2.loc.gov to
;; stay under roughly 10 requests per minute; enrich entries one at a
;; time, and throttle any batch use.

;;; Code:

(require 'url)
(require 'url-util)
(require 'json)
(require 'subr-x)
(require 'seq)
(require 'dom)
(require 'org)
(require 'crm)

(defgroup org-reading-list nil
  "Bibliographic reading list in Org mode."
  :group 'org
  :prefix "org-reading-list-")

(defcustom org-reading-list-file (expand-file-name "~/org/reading-list.org")
  "Org file holding the reading list.
Used for cite-key collision checks and as the capture target."
  :type 'file)

(defcustom org-reading-list-headline "Books"
  "Heading in `org-reading-list-file' under which entries are filed.
`org-reading-list-insert' appends each entry as the last child of this
heading, mirroring the `file+headline' target of the capture template;
the heading is created if it is absent."
  :type 'string)

(defcustom org-reading-list-max-tags 6
  "Maximum number of subject tags attached from captured or fetched data."
  :type 'natnum)

(defcustom org-reading-list-pdf-directory
  (expand-file-name "~/org/reading-list-pdfs/")
  "Directory where `org-reading-list-download-pdf' stores local copies."
  :type 'directory)

(defcustom org-reading-list-holdings-codes '("OWN" "IA")
  "Known holdings codes for the :HOLDINGS: and :CALLNO: properties.
OWN is the personal collection and IA means readable online at the
Internet Archive.  Add short, uppercase, stable codes for the
libraries you use, e.g. (\"OWN\" \"IA\" \"MILIB\" \"SFPL\")."
  :type '(repeat string))

(defcustom org-reading-list-loc-sru-url
  (concat "http://lx2.loc.gov:210/lcdb"
          "?version=1.1&operation=searchRetrieve"
          "&query=%s&maximumRecords=5&recordSchema=marcxml")
  "SRU query template for the LC Catalog, %s replaced by a CQL query.
Queries used: `bath.isbn=…' and `bath.lccn=…'.  If an index stops
matching, try `dc.identifier=…' instead."
  :type 'string)

(defconst org-reading-list--name-suffixes
  '("Jr." "Jr" "Sr." "Sr" "II" "III" "IV")
  "Generational suffixes recognized when inverting author names.")

;;;; HTTP helpers

(defun org-reading-list--http-body-buffer (url timeout)
  "GET URL with TIMEOUT; return buffer with point after headers, or nil."
  (let ((buf (ignore-errors (url-retrieve-synchronously url t t timeout))))
    (when buf
      (with-current-buffer buf
        (goto-char (point-min))
        (if (re-search-forward "\n\n" nil t)
            buf
          (kill-buffer buf)
          nil)))))

(defun org-reading-list--fetch-json (url)
  "GET URL and parse the response body as JSON.
Objects become alists with symbol keys; arrays become lists.
Return nil on any failure."
  (let ((buf (org-reading-list--http-body-buffer url 15)))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (let ((json-object-type 'alist)
                  (json-array-type 'list)
                  (json-key-type 'symbol))
              (ignore-errors (json-read))))
        (kill-buffer buf)))))

(defun org-reading-list--fetch-xml (url)
  "GET URL and parse the response body as XML; return a DOM, or nil."
  (let ((buf (org-reading-list--http-body-buffer url 20)))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (ignore-errors
              (if (fboundp 'libxml-parse-xml-region)
                  (libxml-parse-xml-region (point) (point-max))
                (car (xml-parse-region (point) (point-max))))))
        (kill-buffer buf)))))

;;;; Open Library

(defun org-reading-list--lccn-normalize (s)
  "Normalize LCCN S to its canonical hyphenless form; nil for nil.
Strips spaces; a catalog-card form like \"61-10539\" has its hyphen
dropped and the serial zero-padded to six digits (\"61010539\"), the
form Open Library and LoC SRU indexes match on.  Values without a
hyphen are returned with spaces stripped, otherwise unchanged."
  (when s
    (let ((v (replace-regexp-in-string " " "" s)))
      (if (string-match "\\`\\([A-Za-z]*[0-9]*\\)-\\([0-9]+\\)\\'" v)
          (concat (match-string 1 v)
                  (format "%06d" (string-to-number (match-string 2 v))))
        v))))

(defun org-reading-list--bibkey (input)
  "Normalize INPUT into an Open Library bibkeys value.
INPUT may be an ISBN (10 or 13 digits, hyphens allowed), an Open
Library edition id (\"OL5851208M\"), an openlibrary.org URL containing
one (URL-encoded forms are decoded), or an explicit \"PREFIX:value\"
bibkey using ISBN:, OLID:, LCCN:, or OCLC:.  Work ids (\"OL…W\") are
rejected with an explanation, since bibliographic data is per edition.
LCCN values are normalized to canonical form, so catalog-card
numbers like \"LCCN:61-10539\" work as-is."
  (let* ((input (string-trim input))
         (decoded (url-unhex-string input)))
    (cond
     ((string-match "\\`LCCN:\\(.*\\)\\'" input)
      (concat "LCCN:" (org-reading-list--lccn-normalize
                       (match-string 1 input))))
     ((string-match-p "\\`\\(ISBN\\|OLID\\|OCLC\\):" input)
      input)
     ((string-match "\\bOL[0-9]+M\\b" decoded)
      (concat "OLID:" (match-string 0 decoded)))
     ((string-match-p "\\`[0-9][0-9Xx-]\\{8,16\\}\\'" input)
      (concat "ISBN:" (replace-regexp-in-string "-" "" input)))
     ((string-match "\\bOL[0-9]+W\\b" decoded)
      (user-error
       "%s is a work id; data is per edition — open the edition you mean and use its OL…M id or URL"
       (match-string 0 decoded)))
     (t (user-error "Can't interpret %S as an ISBN, OLID, or OL URL"
                    input)))))

(defun org-reading-list--openlibrary (bibkey)
  "Return the Open Library `data' record for BIBKEY, or nil."
  (let* ((url (format (concat "https://openlibrary.org/api/books"
                              "?bibkeys=%s&format=json&jscmd=data")
                      bibkey))
         (data (org-reading-list--fetch-json url)))
    (cdar data)))

;;;; Record access helpers

(defun org-reading-list--dig (rec &rest path)
  "Walk alist REC through PATH of symbol keys; return the value or nil."
  (dolist (k path rec)
    (setq rec (and (listp rec) (alist-get k rec)))))

(defun org-reading-list--first-name (rec key)
  "Return the `name' of the first element of list-valued KEY in REC."
  (org-reading-list--dig (car (org-reading-list--dig rec key)) 'name))

(defun org-reading-list--ia-id (rec)
  "Return the Internet Archive item identifier for REC, or nil.
Extracted from the first ebook's preview_url."
  (let ((url (org-reading-list--dig
              (car (org-reading-list--dig rec 'ebooks)) 'preview_url)))
    (when (and (stringp url)
               (string-match
                "archive\\.org/\\(?:details\\|stream\\)/\\([^/?#]+\\)" url))
      (match-string 1 url))))

;;;; Names

(defun org-reading-list--invert-name (name)
  "Convert direct-order NAME to inverted \"Surname, Given\" form.
Heuristic: the last token is the surname; a recognized generational
suffix yields BibTeX's \"Surname, Suffix, Given\" form.  Names already
containing a comma are returned unchanged.  Compound surnames
\(\"Vaughan Williams\", \"de la Torre\") will need manual correction."
  (if (or (not (stringp name)) (string-match-p "," name))
      name
    (let* ((toks (split-string name "[ \t]+" t))
           (suffix (car (member (car (last toks))
                                org-reading-list--name-suffixes)))
           (toks (if suffix (butlast toks) toks)))
      (cond
       ((null toks) name)
       ((= (length toks) 1) (car toks))
       (t (let ((surname (car (last toks)))
                (given (string-join (butlast toks) " ")))
            (if suffix
                (format "%s, %s, %s" surname suffix given)
              (format "%s, %s" surname given))))))))

(defun org-reading-list--authors (rec)
  "Return REC's authors, inverted and joined with \" and \", or nil."
  (let ((names (delq nil (mapcar (lambda (a) (alist-get 'name a))
                                 (org-reading-list--dig rec 'authors)))))
    (when names
      (mapconcat #'org-reading-list--invert-name names " and "))))

;;;; Dates

(defun org-reading-list--date (rec)
  "Extract a four-digit year from REC's publish_date, or nil.
Refine to YYYY-MM by hand when finer precision is known."
  (let ((d (org-reading-list--dig rec 'publish_date)))
    (when (and (stringp d) (string-match "[0-9]\\{4\\}" d))
      (match-string 0 d))))

;;;; Cite keys

(defun org-reading-list--slug (s)
  "Downcase S and strip everything but ASCII letters and digits."
  (replace-regexp-in-string "[^a-z0-9]" "" (downcase s)))

(defun org-reading-list--existing-citekeys ()
  "Collect :CUSTOM_ID: values already in `org-reading-list-file'."
  (when (file-readable-p org-reading-list-file)
    (with-temp-buffer
      (insert-file-contents org-reading-list-file)
      (goto-char (point-min))
      (let (keys)
        (while (re-search-forward
                "^[ \t]*:CUSTOM_ID:[ \t]+\\(\\S-+\\)" nil t)
          (push (match-string 1) keys))
        keys))))

(defun org-reading-list--citekey-base (author date)
  "Return the un-suffixed cite key for AUTHOR (inverted form) and DATE."
  (let* ((surname (if author (car (split-string author ",")) "anon"))
         (year (and date (substring date 0 (min 4 (length date)))))
         (base (concat (org-reading-list--slug surname) (or year ""))))
    (if (string-empty-p base) "anon" base)))

(defun org-reading-list--citekey-unique (base existing)
  "Return BASE, letter-suffixed as needed to avoid keys in EXISTING."
  (if (not (member base existing))
      base
    (let ((suffix ?a) key)
      (while (member (setq key (format "%s%c" base suffix)) existing)
        (setq suffix (1+ suffix)))
      key)))

(defun org-reading-list--citekey (author date)
  "Generate a cite key from AUTHOR (inverted form) and DATE.
Form: surname slug + year, e.g. \"lotchin1997\"; a letter suffix is
appended on collision with keys already in `org-reading-list-file'."
  (org-reading-list--citekey-unique
   (org-reading-list--citekey-base author date)
   (org-reading-list--existing-citekeys)))

;;;; Subjects as tags

(defun org-reading-list--tagify (s)
  "Normalize subject string S into an Org tag, or nil if unsuitable.
Downcases, converts runs of non-alphanumerics to underscores, and
rejects overlong headings."
  (when (and (stringp s) (< (length s) 40))
    (let ((tag (string-trim
                (replace-regexp-in-string "[^[:alnum:]]+" "_" (downcase s))
                "_+" "_+")))
      (unless (string-empty-p tag) tag))))

(defun org-reading-list--subject-tags (rec)
  "Return up to `org-reading-list-max-tags' tags from REC's subjects."
  (let* ((names (delq nil (mapcar (lambda (s) (alist-get 'name s))
                                  (org-reading-list--dig rec 'subjects))))
         (tags (delete-dups
                (delq nil (mapcar #'org-reading-list--tagify names)))))
    (seq-take tags org-reading-list-max-tags)))

;;;; Controlled-vocabulary tags

(defun org-reading-list--tag-rewrites-safe-p (value)
  "Return non-nil if VALUE is a valid `org-reading-list-tag-rewrites'.
That is an alist whose keys are strings and whose values are strings or nil."
  (and (listp value)
       (seq-every-p
        (lambda (e)
          (and (consp e)
               (stringp (car e))
               (or (null (cdr e)) (stringp (cdr e)))))
        value)))

(defcustom org-reading-list-tag-vocabulary nil
  "Global controlled tag vocabulary, merged with the buffer's #+TAGS:.
A list of allowed tag strings.  Normally nil: define the vocabulary
per file with a #+TAGS: line instead."
  :type '(repeat string))

(defcustom org-reading-list-tag-rewrites nil
  "Alist rewriting raw subject tags to the controlled vocabulary.
Each element maps a raw tag string to a vocabulary tag string, or to nil
to drop the raw tag.  Set per file via file-local variables or
`.dir-locals.el'.  See `org-reading-list-preen-tags'."
  :type '(alist :key-type string :value-type (choice string (const nil))))
(put 'org-reading-list-tag-rewrites 'safe-local-variable
     #'org-reading-list--tag-rewrites-safe-p)

(defcustom org-reading-list-tag-min 1
  "Minimum vocabulary tags an entry should carry after preening.
Entries below this are flagged by `org-reading-list-lint-tags' and, when
`org-reading-list-tag-infer-function' is set, passed to it."
  :type 'natnum)

(defcustom org-reading-list-tag-infer-function nil
  "Function to infer extra tags for thin entries, or nil.
Called by `org-reading-list-preen-tags' only when an entry has fewer
than `org-reading-list-tag-min' vocabulary tags after rewriting.  It
receives one plist argument with keys :heading, :title, :abstract,
:body, :tags, and :vocabulary, and returns a list of vocabulary tags to
add; returned tags outside the vocabulary are discarded."
  :type '(choice (const :tag "None" nil) function))

(defcustom org-reading-list-preen-on-capture nil
  "When non-nil, preen harvested tags against the target file at capture.
`org-reading-list-insert' and `org-reading-list-capture' rewrite the new
entry's tags via `org-reading-list-tag-rewrites' and the target file's
vocabulary before filing.  Off by default."
  :type 'boolean)

(defun org-reading-list--tag-vocabulary ()
  "Return the controlled tag vocabulary for the current buffer.
The tags defined by the buffer's #+TAGS: (via `org-current-tag-alist')
together with `org-reading-list-tag-vocabulary'."
  (delete-dups
   (append
    (delq nil (mapcar (lambda (e) (and (consp e) (stringp (car e)) (car e)))
                      org-current-tag-alist))
    org-reading-list-tag-vocabulary)))

(defun org-reading-list--rewrite-tags (tags vocab rewrites)
  "Rewrite TAGS to the controlled VOCAB using REWRITES.
VOCAB is a list of allowed tag strings.  REWRITES maps a raw tag to a
VOCAB tag, or to nil to drop it.  Return (NEW-TAGS . RECORD): NEW-TAGS is
deduplicated, sorted, and capped at `org-reading-list-max-tags'; RECORD
is a plist with :rewritten (alist of raw . target), :dropped-explicit,
and :dropped-unresolved."
  (let (kept rewritten dropped-explicit dropped-unresolved)
    (dolist (tag tags)
      (cond
       ((member tag vocab) (push tag kept))
       ((assoc tag rewrites)
        (let ((target (cdr (assoc tag rewrites))))
          (cond
           ((null target) (push tag dropped-explicit))
           ((member target vocab)
            (push target kept)
            (push (cons tag target) rewritten))
           (t (push tag dropped-unresolved)))))
       (t (push tag dropped-unresolved))))
    (cons (seq-take (sort (delete-dups (nreverse kept)) #'string<)
                    org-reading-list-max-tags)
          (list :rewritten (nreverse rewritten)
                :dropped-explicit (nreverse dropped-explicit)
                :dropped-unresolved (nreverse dropped-unresolved)))))

(defun org-reading-list--lint-collect ()
  "Collect tag-preen diagnostics for book entries in the current buffer.
Return a list of plists, one per heading with a non-empty :BTYPE:, with
keys :heading, :current, :new, :record (from
`org-reading-list--rewrite-tags'), and :thin (non-nil when the new tag
count is below `org-reading-list-tag-min')."
  (let ((vocab (org-reading-list--tag-vocabulary))
        (rewrites org-reading-list-tag-rewrites)
        results)
    (org-map-entries
     (lambda ()
       (let* ((cur (org-get-tags nil t))
              (res (org-reading-list--rewrite-tags cur vocab rewrites)))
         (push (list :heading (org-get-heading t t t t)
                     :current cur :new (car res) :record (cdr res)
                     :thin (< (length (car res)) org-reading-list-tag-min))
               results)))
     "BTYPE={.}")
    (nreverse results)))

(defun org-reading-list--infer-merge (new vocab ctx)
  "Add inferred vocabulary tags to NEW when it is below the minimum.
When `org-reading-list-tag-infer-function' is set and NEW has fewer than
`org-reading-list-tag-min' tags, call it with context plist CTX and add
the returned tags that are in VOCAB.  Return the merged, sorted, capped
list, or NEW unchanged."
  (if (and org-reading-list-tag-infer-function
           (< (length new) org-reading-list-tag-min))
      (let ((added (seq-filter (lambda (x) (member x vocab))
                               (funcall org-reading-list-tag-infer-function ctx))))
        (seq-take (sort (delete-dups (append new added)) #'string<)
                  org-reading-list-max-tags))
    new))


;;;###autoload
(defun org-reading-list-lint-tags ()
  "Report how `org-reading-list-preen-tags' would change tags in the buffer.
List, per book entry, the rewrites and drops that would apply and flag
entries left below `org-reading-list-tag-min'.  Modify nothing."
  (interactive)
  (let ((data (org-reading-list--lint-collect))
        (buf (get-buffer-create "*org-reading-list-tags*")))
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (e data)
          (let ((rec (plist-get e :record)))
            (insert (format "%s%s\n" (plist-get e :heading)
                            (if (plist-get e :thin) "   [THIN]" ""))
                    (format "  %s -> %s\n"
                            (or (plist-get e :current) "(none)")
                            (or (plist-get e :new) "(none)"))
                    (if (plist-get rec :dropped-unresolved)
                        (format "  unresolved (need a rewrite or vocab entry): %s\n"
                                (plist-get rec :dropped-unresolved))
                      ""))))
        (goto-char (point-min))))
    (display-buffer buf)))

(defun org-reading-list--preen-entry (vocab rewrites)
  "Preen the tags of the Org entry at point against VOCAB and REWRITES.
Rewrite the tags, then add inferred tags via `org-reading-list--infer-merge'
when the result is below `org-reading-list-tag-min'.  Set the entry's
tags and return them."
  (let* ((new (car (org-reading-list--rewrite-tags
                    (org-get-tags nil t) vocab rewrites)))
         (ctx (list :heading (org-get-heading t t t t)
                    :title (org-entry-get nil "TITLE")
                    :abstract (org-entry-get nil "ABSTRACT")
                    :body (org-get-entry)
                    :tags new
                    :vocabulary vocab)))
    (setq new (org-reading-list--infer-merge new vocab ctx))
    (org-set-tags new)
    new))

(defun org-reading-list--preen-data (data)
  "Return DATA with its :tags preened against the current buffer's vocabulary.
DATA is an `org-reading-list--entry-data' plist.  The current buffer
must be the target reading-list buffer (its #+TAGS: and file-local
`org-reading-list-tag-rewrites' supply the vocabulary and map).  When the
vocabulary is empty, return DATA unchanged.  When
`org-reading-list-tag-infer-function' is set and the rewritten set is
below `org-reading-list-tag-min', add the tags it returns."
  (let ((vocab (org-reading-list--tag-vocabulary)))
    (if (null vocab)
        data
      (let* ((new (car (org-reading-list--rewrite-tags
                        (plist-get data :tags) vocab
                        org-reading-list-tag-rewrites)))
             (props (plist-get data :props))
             (ctx (list :heading (plist-get data :title)
                        :title (plist-get data :title)
                        :abstract (cdr (assoc "ABSTRACT" props))
                        :body nil :tags new :vocabulary vocab)))
        (plist-put (copy-sequence data) :tags
                   (org-reading-list--infer-merge new vocab ctx))))))


;;;###autoload
(defun org-reading-list-preen-tags (&optional all)
  "Preen the tags of the Org entry at point to the controlled vocabulary.
Rewrite the tags via `org-reading-list-tag-rewrites' against the buffer's
vocabulary (its #+TAGS:).  With a prefix argument ALL, preen every book
entry (those with a non-empty :BTYPE:) in the buffer, after confirmation.
See `org-reading-list-lint-tags' for a non-destructive preview."
  (interactive "P")
  (let ((vocab (org-reading-list--tag-vocabulary))
        (rewrites org-reading-list-tag-rewrites))
    (cond
     ((null vocab)
      (message "No tag vocabulary: set #+TAGS: or org-reading-list-tag-vocabulary"))
     (all
      (let ((n (length (org-map-entries #'ignore "BTYPE={.}"))))
        (when (yes-or-no-p (format "Preen tags of %d entries? " n))
          (org-map-entries
           (lambda () (org-reading-list--preen-entry vocab rewrites))
           "BTYPE={.}")
          (message "Preened %d entries" n))))
     (t
      (org-reading-list--preen-entry vocab rewrites)
      (message "Preened: %s"
               (or (string-join (org-get-tags nil t) " ") "none"))))))



;;;; Entry construction

(defun org-reading-list--ol-entry-data (rec bibkey source)
  "Compute entry fields from Open Library record REC.
BIBKEY is the bibkey REC was fetched under; SOURCE is as in
`org-reading-list-entry'.  Return the plist described in
`org-reading-list--entry-data'."
  (let* ((title (org-reading-list--dig rec 'title))
         (subtitle (org-reading-list--dig rec 'subtitle))
         (full-title (if subtitle (format "%s: %s" title subtitle) title))
         (author (org-reading-list--authors rec))
         (date (org-reading-list--date rec))
         (tags (org-reading-list--subject-tags rec))
         (pages (org-reading-list--dig rec 'number_of_pages))
         (isbn13s (org-reading-list--dig rec 'identifiers 'isbn_13))
         (isbn10s (org-reading-list--dig rec 'identifiers 'isbn_10))
         (all-isbns
          (delete-dups
           (mapcar (lambda (i) (replace-regexp-in-string "-" "" i))
                   (delq nil
                         (append isbn13s isbn10s
                                 (and (string-prefix-p "ISBN:" bibkey)
                                      (list (substring bibkey 5))))))))
         (isbn (or (car isbn13s) (car isbn10s)
                   (and (string-prefix-p "ISBN:" bibkey)
                        (substring bibkey 5))))
         (olid (or (car (org-reading-list--dig
                         rec 'identifiers 'openlibrary))
                   (and (string-prefix-p "OLID:" bibkey)
                        (substring bibkey 5))))
         (props
          `(("CUSTOM_ID" . ,(org-reading-list--citekey author date))
            ("BTYPE"     . "book")
            ("AUTHOR"    . ,author)
            ("TITLE"     . ,full-title)
            ("ADDRESS"   . ,(org-reading-list--first-name
                             rec 'publish_places))
            ("PUBLISHER" . ,(org-reading-list--first-name
                             rec 'publishers))
            ("DATE"      . ,date)
            ("PAGES"     . ,(and pages (format "%s" pages)))
            ("ISBN"      . ,isbn)
            ("LCCN"      . ,(car (org-reading-list--dig
                                  rec 'identifiers 'lccn)))
            ("OCLC"      . ,(car (org-reading-list--dig
                                  rec 'identifiers 'oclc)))
            ("OLID"      . ,olid)
            ("IA"        . ,(org-reading-list--ia-id rec))
            ("LCC"       . ,(car (org-reading-list--dig
                                  rec 'classifications
                                  'lc_classifications)))
            ("DDC"       . ,(car (org-reading-list--dig
                                  rec 'classifications
                                  'dewey_decimal_class)))
            ("URL"       . ,(and (not isbn)
                                 (org-reading-list--dig rec 'url)))
            ;; Filled from a MARC 520 by `org-reading-list--loc-augment-data'
            ;; when the title/author bridge fires; Open Library's own data
            ;; view carries no summary.
            ("ABSTRACT"  . nil)
            ("ADDED"     . ,(format-time-string "[%Y-%m-%d %a]"))
            ("FOUND"     . ,source))))
    (list :title full-title :tags tags :isbns all-isbns :props props)))

(defun org-reading-list--entry-data (id &optional source)
  "Fetch bibliographic data for ID and compute entry fields.
Queries Open Library; for an ISBN or LCCN it lacks (common for
979-8 self-published and pre-ISBN books), falls back to the LC
Catalog over SRU.  When Open Library has the record but no ISBN or
LCCN (typical of OLID/pre-ISBN lookups), a title/author SRU search
fills in the LCCN/OCLC/LCC/DDC LoC holds (see
`org-reading-list--loc-augment-data').  ID and SOURCE are as in
`org-reading-list-entry'.  Return a plist:
:title is the full title, :tags the subject tags, :isbns every ISBN
on the record (hyphens stripped; used for duplicate checks), and
:props the property alist that `org-reading-list--entry-string'
renders.  Signal a `user-error' if no record is found."
  (let* ((bibkey (org-reading-list--bibkey id))
         (rec (org-reading-list--openlibrary bibkey)))
    (cond
     (rec (org-reading-list--loc-augment-data
           (org-reading-list--ol-entry-data rec bibkey source)))
     ((or (string-prefix-p "ISBN:" bibkey)
          (string-prefix-p "LCCN:" bibkey))
      (org-reading-list--loc-entry-data bibkey source))
     (t (user-error "No Open Library record for %s" bibkey)))))

(defun org-reading-list--entry-string (data)
  "Render DATA from `org-reading-list--entry-data' as an Org entry."
  (let ((tags (plist-get data :tags)))
    (concat
     (format "* TOREAD %s%s\n" (plist-get data :title)
             (if tags (format " :%s:" (string-join tags ":")) ""))
     ":PROPERTIES:\n"
     (mapconcat (lambda (kv)
                  (if (cdr kv)
                      (format ":%s: %s\n" (car kv) (cdr kv))
                    ""))
                (plist-get data :props) "")
     ":END:\n")))

(defun org-reading-list-entry (id &optional source)
  "Return an Org entry string for ID from Open Library or LoC data.
ID is an ISBN, an Open Library edition id, or an openlibrary.org URL
\(see `org-reading-list--bibkey').  SOURCE, if non-nil, is recorded in
the :FOUND: property (an article URL, an Org link, a person's name —
wherever you ran across the book).  ISBNs and LCCNs Open Library
lacks fall back to the LC Catalog (one SRU query); an OLID/pre-ISBN
record Open Library has but holds no ISBN or LCCN for is augmented
with LoC identifiers via a title/author search.  Entries without
an ISBN record :OLID: and the Open Library page in :URL: instead;
editions with a readable Internet Archive scan record the item id in
:IA:.
Signal a `user-error' if neither source has a record."
  (org-reading-list--entry-string (org-reading-list--entry-data id source)))

;;;; Duplicate detection

(defun org-reading-list--scan-entries ()
  "Collect duplicate-check data for every heading in the current buffer.
Return, in buffer order, one plist per heading with keys :pos (start
of the heading line), :heading (text sans TODO keyword, priority, tags,
and comment), :isbns (the :ISBN: property split on commas/spaces,
hyphens stripped), :olid, :title, and :author.  Headings without these
properties yield nil fields and never match anything."
  (let (entries)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (let* ((pos (match-beginning 0))
               (isbn (org-entry-get nil "ISBN")))
          (push (list :pos pos
                      :heading (org-get-heading t t t t)
                      :isbns (and isbn
                                  (mapcar
                                   (lambda (s)
                                     (replace-regexp-in-string "-" "" s))
                                   (split-string isbn "[, ]" t)))
                      :olid (org-entry-get nil "OLID")
                      :title (org-entry-get nil "TITLE")
                      :author (org-entry-get nil "AUTHOR"))
                entries))))
    (nreverse entries)))

(defun org-reading-list--dup-title-key (title)
  "Slug of TITLE's main part (before any subtitle colon), or nil."
  (when (stringp title)
    (let ((key (org-reading-list--slug (car (split-string title ":")))))
      (unless (string-empty-p key) key))))

(defun org-reading-list--dup-surname-key (author)
  "Slug of AUTHOR's surname (before the first comma), or nil.
AUTHOR is in inverted \"Surname, Given\" form."
  (when (stringp author)
    (let ((key (org-reading-list--slug (car (split-string author ",")))))
      (unless (string-empty-p key) key))))

(defun org-reading-list--find-duplicate (data entries)
  "Find an entry among ENTRIES that DATA likely duplicates.
DATA is a plist from `org-reading-list--entry-data'; ENTRIES is from
`org-reading-list--scan-entries'.  Return (exact . ENTRY) when any
fetched ISBN or the OLID matches, (similar . ENTRY) when both the
slugged main title and author surname match, nil otherwise.  Exact
wins over similar; similar requires an author on both sides, so
year, subtitle, and publisher differences alone do not defeat it."
  (let* ((isbns (plist-get data :isbns))
         (props (plist-get data :props))
         (olid (cdr (assoc "OLID" props)))
         (title-key (org-reading-list--dup-title-key
                     (plist-get data :title)))
         (surname-key (org-reading-list--dup-surname-key
                       (cdr (assoc "AUTHOR" props)))))
    (or (let ((hit (seq-find
                    (lambda (e)
                      (or (seq-intersection isbns (plist-get e :isbns))
                          (and olid (equal olid (plist-get e :olid)))))
                    entries)))
          (and hit (cons 'exact hit)))
        (and title-key surname-key
             (let ((hit (seq-find
                         (lambda (e)
                           (and (equal title-key
                                       (org-reading-list--dup-title-key
                                        (plist-get e :title)))
                                (equal surname-key
                                       (org-reading-list--dup-surname-key
                                        (plist-get e :author)))))
                         entries)))
               (and hit (cons 'similar hit)))))))

(defun org-reading-list--duplicate-in-file (data)
  "Scan `org-reading-list-file' for an entry that DATA duplicates.
DATA is a plist from `org-reading-list--entry-data'.  Return the
\(TYPE . ENTRY) cell from `org-reading-list--find-duplicate', or nil.
The file's buffer is created if it is not already visited."
  (with-current-buffer (find-file-noselect org-reading-list-file)
    (save-restriction
      (widen)
      (org-reading-list--find-duplicate
       data (org-reading-list--scan-entries)))))

(define-error 'org-reading-list-duplicate "Book already in reading list")

(defun org-reading-list--confirm-duplicate (dup)
  "Ask whether to insert despite DUP, a (TYPE . ENTRY) pair.
Return non-nil to insert anyway."
  (y-or-n-p (format (if (eq (car dup) 'exact)
                        "Already in list as %S (ISBN match) — add anyway? "
                      "Possibly already in list as %S (title/author match) — add anyway? ")
                    (plist-get (cdr dup) :heading))))

;;;; Holdings

;;;###autoload
(defun org-reading-list-set-holdings ()
  "Set the :HOLDINGS: property of the Org entry at point.
Prompts with completion over `org-reading-list-holdings-codes'; enter
several codes separated by commas (free-form entries are also
accepted).  The value is stored semicolon-separated per the schema.
The current value, if any, is offered as the initial input; emptying
the input removes the property."
  (interactive)
  (let* ((current (org-entry-get nil "HOLDINGS"))
         (initial (and current
                       (replace-regexp-in-string ";[ \t]*" "," current)))
         (codes (completing-read-multiple
                 "Holdings (comma-separated): "
                 org-reading-list-holdings-codes
                 nil nil initial)))
    (setq codes (delete "" (mapcar #'string-trim codes)))
    (if codes
        (org-entry-put nil "HOLDINGS" (string-join codes "; "))
      (org-entry-delete nil "HOLDINGS"))
    (message "HOLDINGS: %s"
             (or (org-entry-get nil "HOLDINGS") "(removed)"))))

(defun org-reading-list--holdings-add (code)
  "Add CODE to the entry's :HOLDINGS: unless an equivalent is present.
Codes are compared on their leading word, so \"OWN (pdf)\" counts as
already present when HOLDINGS contains any OWN entry."
  (let* ((h (org-entry-get nil "HOLDINGS"))
         (word (car (split-string code "[ (]" t))))
    (unless (and h (string-match-p
                    (concat "\\b" (regexp-quote word) "\\b") h))
      (org-entry-put nil "HOLDINGS"
                     (if h (concat h "; " code) code)))))

;;;; Internet Archive PDF download

;;;###autoload
(defun org-reading-list-download-pdf ()
  "Download the Internet Archive PDF for the Org entry at point.
Uses the :IA: identifier; saves to `org-reading-list-pdf-directory',
named after the entry's cite key (falling back to the IA id); records
an Org file link in :LOCALFILE: and adds \"OWN (pdf)\" to :HOLDINGS:.

Not every IA item offers a direct PDF; if the downloaded file isn't
one, it is deleted and the item page is suggested instead."
  (interactive)
  (let* ((ia (or (org-entry-get nil "IA")
                 (user-error "Entry at point has no :IA: property")))
         (key (or (org-entry-get nil "CUSTOM_ID") ia))
         (dir (file-name-as-directory
               (expand-file-name org-reading-list-pdf-directory)))
         (dest (concat dir key ".pdf"))
         (url (format "https://archive.org/download/%s/%s.pdf" ia ia)))
    (make-directory dir t)
    (when (and (file-exists-p dest)
               (not (y-or-n-p (format "%s exists; overwrite? " dest))))
      (user-error "Download aborted"))
    (message "Downloading %s …" url)
    (condition-case err
        (url-copy-file url dest t)
      (error (user-error "Download failed: %s"
                         (error-message-string err))))
    (let ((pdf-p (with-temp-buffer
                   (insert-file-contents-literally dest nil 0 5)
                   (goto-char (point-min))
                   (looking-at-p "%PDF"))))
      (unless pdf-p
        (delete-file dest)
        (user-error
         "Item %s has no direct PDF at that URL; check https://archive.org/details/%s for available formats"
         ia ia)))
    (org-entry-put nil "LOCALFILE"
                   (format "[[file:%s]]" (abbreviate-file-name dest)))
    (org-reading-list--holdings-add "OWN (pdf)")
    (message "Saved %s" (abbreviate-file-name dest))))

;;;; Library of Congress: fetching (SRU, MARCXML)

(defun org-reading-list--loc-marcxml (query)
  "Fetch the LoC MARCXML response for CQL QUERY; return a DOM, or nil."
  (org-reading-list--fetch-xml
   (format org-reading-list-loc-sru-url query)))

(defun org-reading-list--loc-entry-query ()
  "Build the SRU query for the Org entry at point.
Return (QUERY . ISBN); ISBN is nil unless querying by ISBN.  Prefers
:ISBN:, then :LCCN: (normalized, so catalog-card forms work), then a
title/author search built from :TITLE: and :AUTHOR: for entries with
neither identifier (see `org-reading-list--loc-title-author-cql').
Signal a `user-error' when none of those is available."
  (let* ((isbn (let ((v (org-entry-get nil "ISBN")))
                 (and v (car (split-string v "[, ]" t)))))
         (lccn (org-entry-get nil "LCCN"))
         (cql (org-reading-list--loc-title-author-cql
               (org-entry-get nil "TITLE") (org-entry-get nil "AUTHOR"))))
    (cond
     (isbn (cons (format "bath.isbn=%s" isbn) isbn))
     (lccn (cons (format "bath.lccn=%s"
                         (org-reading-list--lccn-normalize lccn))
                 nil))
     (cql (cons cql nil))
     (t (user-error
         "Entry at point has no :ISBN:, :LCCN:, or :TITLE:+:AUTHOR: to query by")))))

(defun org-reading-list--loc-entry-records ()
  "Fetch LoC MARC records for the Org entry at point, best match first.
One SRU request.  When the entry is matched only by title/author (it
has neither :ISBN: nor :LCCN:), records are filtered to those whose
author and year agree with the entry, guarding against unrelated
hits.  Signal a `user-error' on no response or no records."
  (let* ((isbn (let ((v (org-entry-get nil "ISBN")))
                 (and v (car (split-string v "[, ]" t)))))
         (lccn (org-entry-get nil "LCCN"))
         (qi (org-reading-list--loc-entry-query))
         (query (car qi))
         (dom (or (org-reading-list--loc-marcxml query)
                  (user-error "No response from LoC SRU for %s" query)))
         (recs (org-reading-list--loc-records dom (cdr qi))))
    (when (and (not isbn) (not lccn))
      (setq recs (seq-filter
                  (lambda (r)
                    (org-reading-list--loc-match-p
                     r (org-entry-get nil "AUTHOR") (org-entry-get nil "DATE")))
                  recs)))
    (unless recs
      (user-error "No LoC catalog record found for %s" query))
    recs))

;;;; Library of Congress: MARC field extraction

(defun org-reading-list--marc-field (rec tag &rest codes)
  "Return the first MARC datafield TAG in REC as a string, or nil.
Joins the subfields whose codes are in CODES (default: just \"a\")
with single spaces, in document order."
  (let ((codes (or codes '("a"))))
    (catch 'hit
      (dolist (df (dom-by-tag rec 'datafield))
        (when (equal (dom-attr df 'tag) tag)
          (let ((vals (delq nil
                            (mapcar
                             (lambda (sf)
                               (when (member (dom-attr sf 'code) codes)
                                 (let ((v (string-trim (dom-text sf))))
                                   (unless (string-empty-p v) v))))
                             (dom-by-tag df 'subfield)))))
            (when vals
              (throw 'hit (string-join vals " "))))))
      nil)))

(defun org-reading-list--marc-oclc (rec)
  "Return the OCLC number from the first (OCoLC)-prefixed 035 in REC."
  (catch 'hit
    (dolist (df (dom-by-tag rec 'datafield))
      (when (equal (dom-attr df 'tag) "035")
        (dolist (sf (dom-by-tag df 'subfield))
          (when (equal (dom-attr sf 'code) "a")
            (let ((v (string-trim (dom-text sf))))
              (when (string-prefix-p "(OCoLC)" v)
                (throw 'hit
                       (string-trim
                        (string-remove-prefix "(OCoLC)" v)))))))))
    nil))

(defun org-reading-list--marc-isbns (rec code)
  "All 020 subfield CODE values in MARC record REC, hyphens stripped.
A trailing qualifier embedded in the subfield (\"0689817479 (hc.)\")
is dropped; values not starting with a digit are skipped."
  (let (isbns)
    (dolist (df (dom-by-tag rec 'datafield))
      (when (equal (dom-attr df 'tag) "020")
        (dolist (sf (dom-by-tag df 'subfield))
          (when (equal (dom-attr sf 'code) code)
            (let ((v (replace-regexp-in-string
                      "-" "" (string-trim (dom-text sf)))))
              (when (string-match "\\`[0-9][0-9Xx]*" v)
                (push (match-string 0 v) isbns)))))))
    (nreverse isbns)))

(defun org-reading-list--marc-subject-tags (recs)
  "Collect normalized Org tags from 650/651 fields across RECS.
Each topical/geographic subfield ($a) and subdivision ($x $y $z)
becomes its own tag, per the schema's tagging convention.  Return
deduplicated tags in document order, uncapped — callers apply
`org-reading-list-max-tags'."
  (let (tags)
    (dolist (rec recs)
      (dolist (df (dom-by-tag rec 'datafield))
        (when (member (dom-attr df 'tag) '("650" "651"))
          (dolist (sf (dom-by-tag df 'subfield))
            (when (member (dom-attr sf 'code) '("a" "x" "y" "z"))
              (let ((tag (org-reading-list--tagify (dom-text sf))))
                (when tag (push tag tags))))))))
    (delete-dups (nreverse tags))))

(defun org-reading-list--marc-strip-punct (s)
  "Strip trailing ISBD separator punctuation and whitespace from S.
MARC transcribed fields end in prescribed punctuation (\"Title /\",
\"Place :\", \"Publisher,\") that is noise in Org properties.  Return
nil when S is nil or nothing remains."
  (when s
    (let ((v (string-trim-right s "[ /:;,.]+")))
      (unless (string-empty-p v) v))))

(defun org-reading-list--marc-pub-field (rec code)
  "Return publication subfield CODE from REC's 264, falling back to 260."
  (or (org-reading-list--marc-field rec "264" code)
      (org-reading-list--marc-field rec "260" code)))

(defun org-reading-list--marc-year (rec)
  "Return a four-digit year from REC's 264/260 $c, or nil."
  (let ((c (org-reading-list--marc-pub-field rec "c")))
    (when (and c (string-match "[0-9]\\{4\\}" c))
      (match-string 0 c))))

(defun org-reading-list--loc-records (dom isbn)
  "Return MARC record nodes from SRU response DOM, best match first.
When ISBN is non-nil, a record whose 020 $a equals it (the edition
actually queried, e.g. print rather than ebook) sorts to the front;
remaining records keep their response order and serve as fall-through
sources."
  (let ((recs (dom-by-tag dom 'record)))
    (if (not isbn)
        recs
      (let* ((norm (replace-regexp-in-string "-" "" isbn))
             (best (seq-find
                    (lambda (r)
                      (member norm (org-reading-list--marc-isbns r "a")))
                    recs)))
        (if best
            (cons best (remq best recs))
          recs)))))

(defun org-reading-list--loc-first (recs fn)
  "Apply FN to each record in RECS; return the first non-nil result."
  (seq-some fn recs))

;;;; Library of Congress: entry data (lookup fallback)

(defun org-reading-list--loc-id-fields (recs)
  "Identifier properties from MARC RECS as an alist, values possibly nil.
Covers LCCN, OCLC, LCC, and DDC (MARC 010, 035, 050, 082),
consulting RECS in order for each field; the LCCN's embedded spaces
are stripped.  The identifier subset of
`org-reading-list--loc-marc-fields'."
  `(("LCCN" . ,(org-reading-list--loc-first
                recs
                (lambda (r)
                  (let ((v (org-reading-list--marc-field r "010")))
                    (and v (replace-regexp-in-string " " "" v))))))
    ("OCLC" . ,(org-reading-list--loc-first
                recs #'org-reading-list--marc-oclc))
    ("LCC"  . ,(org-reading-list--loc-first
                recs
                (lambda (r)
                  (org-reading-list--marc-field r "050" "a" "b"))))
    ("DDC"  . ,(org-reading-list--loc-first
                recs
                (lambda (r)
                  (org-reading-list--marc-field r "082"))))))

(defun org-reading-list--marc-abstract (recs)
  "Return the first MARC 520 summary across RECS, or nil.
Joins the summary ($a) with its expansion ($b); sentence punctuation
is kept, unlike the transcribed bibliographic fields."
  (org-reading-list--loc-first
   recs
   (lambda (r) (org-reading-list--marc-field r "520" "a" "b"))))

(defun org-reading-list--loc-marc-fields (recs)
  "MARC-derived properties from RECS as an alist, values possibly nil.
The identifier fields of `org-reading-list--loc-id-fields' plus the
520 summary as ABSTRACT.  Shared by `org-reading-list--marc-entry-data',
`org-reading-list--loc-augment-data', and
`org-reading-list--loc-apply-fields', so all three fill the same set."
  (append (org-reading-list--loc-id-fields recs)
          (list (cons "ABSTRACT" (org-reading-list--marc-abstract recs)))))

(defun org-reading-list--marc-entry-data (recs bibkey source)
  "Compute entry fields from LoC MARC records RECS.
The MARC sibling of `org-reading-list--ol-entry-data': return the
same plist shape, described in `org-reading-list--entry-data'.  RECS
is best match first, as `org-reading-list--loc-records' returns;
each field falls through the records in order.  BIBKEY is the
\"ISBN:...\" or \"LCCN:...\" bibkey that was queried; SOURCE is as in
`org-reading-list-entry'.  For LCCN bibkeys the :ISBN: property
comes from the records\\=' 020 fields, when present.  Identifiers
with no LoC equivalent (:OLID:, :IA:, :URL:) are omitted."
  (let* ((queried (and (string-prefix-p "ISBN:" bibkey)
                       (replace-regexp-in-string
                        "-" "" (substring bibkey (length "ISBN:")))))
         (title (org-reading-list--loc-first
                 recs
                 (lambda (r)
                   (org-reading-list--marc-strip-punct
                    (org-reading-list--marc-field r "245" "a" "b")))))
         (author (org-reading-list--loc-first
                  recs
                  (lambda (r)
                    (org-reading-list--marc-strip-punct
                     (org-reading-list--marc-field r "100")))))
         (date (org-reading-list--loc-first
                recs #'org-reading-list--marc-year))
         (pages (org-reading-list--loc-first
                 recs
                 (lambda (r)
                   (let ((a (org-reading-list--marc-field r "300")))
                     (when (and a (string-match "[0-9]+" a))
                       (match-string 0 a))))))
         (isbns (delete-dups
                 (append (and queried (list queried))
                         (mapcan (lambda (r)
                                   (org-reading-list--marc-isbns r "a"))
                                 recs))))
         (tags (seq-take (org-reading-list--marc-subject-tags recs)
                         org-reading-list-max-tags))
         (props
          `(("CUSTOM_ID" . ,(org-reading-list--citekey author date))
            ("BTYPE"     . "book")
            ("AUTHOR"    . ,author)
            ("TITLE"     . ,title)
            ("ADDRESS"   . ,(org-reading-list--loc-first
                             recs
                             (lambda (r)
                               (org-reading-list--marc-strip-punct
                                (org-reading-list--marc-pub-field r "a")))))
            ("PUBLISHER" . ,(org-reading-list--loc-first
                             recs
                             (lambda (r)
                               (org-reading-list--marc-strip-punct
                                (org-reading-list--marc-pub-field r "b")))))
            ("DATE"      . ,date)
            ("PAGES"     . ,pages)
            ("ISBN"      . ,(car isbns))
            ,@(org-reading-list--loc-marc-fields recs)
            ("ADDED"     . ,(format-time-string "[%Y-%m-%d %a]"))
            ("FOUND"     . ,source))))
    (list :title title :tags tags :isbns isbns :props props)))

(defun org-reading-list--loc-entry-data (bibkey source)
  "Entry data from the LC Catalog for an ISBN or LCCN BIBKEY.
The fallback for identifiers Open Library lacks: one SRU query by
the ISBN or LCCN, mapped via `org-reading-list--marc-entry-data'.
SOURCE is as in `org-reading-list-entry'.  Signal a `user-error'
naming both sources when LoC has no record either."
  (let* ((isbn (and (string-prefix-p "ISBN:" bibkey)
                    (substring bibkey (length "ISBN:"))))
         (lccn (and (string-prefix-p "LCCN:" bibkey)
                    (substring bibkey (length "LCCN:"))))
         (dom (org-reading-list--loc-marcxml
               (if isbn
                   (format "bath.isbn=%s" isbn)
                 (format "bath.lccn=%s" lccn))))
         (recs (and dom (org-reading-list--loc-records dom isbn))))
    (unless recs
      (user-error "No Open Library or LoC record for %s" bibkey))
    (org-reading-list--marc-entry-data recs bibkey source)))

;;;; Library of Congress: title/author bridge

(defun org-reading-list--loc-title-author-cql (title author)
  "Build a url-safe SRU CQL query from TITLE and AUTHOR, or nil.
Searches the leading title phrase (before any subtitle colon or
comma, embedded quotes removed) and the author surname (the part
before the comma of an inverted name).  Returns nil unless both are
present — a title-only search is too broad to trust.  The bridge for
OLID/pre-ISBN books Open Library holds, but without an ISBN or LCCN
to query LoC by."
  (let* ((main (and (stringp title)
                    (string-trim
                     (replace-regexp-in-string
                      "\"" "" (car (split-string title "[:,]"))))))
         (surname (and (stringp author)
                       (string-trim
                        (replace-regexp-in-string
                         "\"" "" (car (split-string author ",")))))))
    (when (and main (not (string-empty-p main))
               surname (not (string-empty-p surname)))
      (url-hexify-string
       (format "bath.title=\"%s\" and bath.author=\"%s\"" main surname)))))

(defun org-reading-list--loc-match-p (rec author date)
  "Non-nil if MARC REC plausibly matches AUTHOR and DATE.
Guards the title/author bridge against unrelated hits: REC's 100
surname must equal AUTHOR's surname, and when both DATE and REC's
publication year are known they must agree.  AUTHOR is an inverted
name; DATE is a four-digit year or nil."
  (let* ((ours (org-reading-list--dup-surname-key author))
         (theirs (org-reading-list--dup-surname-key
                  (org-reading-list--marc-strip-punct
                   (org-reading-list--marc-field rec "100"))))
         (year (org-reading-list--marc-year rec)))
    (and ours theirs (equal ours theirs)
         (or (null date) (null year) (equal date year)))))

(defun org-reading-list--loc-augment-data (data)
  "Fill missing LoC identifiers on DATA via a title/author SRU search.
DATA is from `org-reading-list--ol-entry-data'.  When it already
carries an ISBN or LCCN, or lacks a title or author to search by, it
is returned unchanged.  Otherwise one SRU query by leading title and
author surname is run; the first record passing
`org-reading-list--loc-match-p' fills DATA's empty
LCCN/OCLC/LCC/DDC and ABSTRACT slots.  DATA's own descriptive fields
are kept."
  (let* ((props (plist-get data :props))
         (author (cdr (assoc "AUTHOR" props)))
         (date (cdr (assoc "DATE" props)))
         (cql (and (not (cdr (assoc "ISBN" props)))
                   (not (cdr (assoc "LCCN" props)))
                   (org-reading-list--loc-title-author-cql
                    (plist-get data :title) author))))
    (when cql
      (let* ((dom (org-reading-list--loc-marcxml cql))
             (match (and dom
                         (seq-find
                          (lambda (r)
                            (org-reading-list--loc-match-p r author date))
                          (dom-by-tag dom 'record)))))
        (when match
          (dolist (kv (org-reading-list--loc-marc-fields (list match)))
            (when (cdr kv)
              (let ((cell (assoc (car kv) props)))
                (if cell
                    (when (null (cdr cell)) (setcdr cell (cdr kv)))
                  (setq props (append props (list kv)))))))
          (setq data (plist-put data :props props)))))
    data))

;;;; Library of Congress: applying fetched data

(defun org-reading-list--loc-apply-fields (recs &optional force)
  "Set properties of the entry at point from MARC RECS.
Covers :LCCN:, :OCLC:, :LCC:, and :DDC: (MARC 010, 035, 050, 082)
and the :ABSTRACT: summary (MARC 520), consulting RECS in order for
each field.  Existing values are kept unless FORCE is non-nil.
Return the list of property names changed."
  (let ((fields (org-reading-list--loc-marc-fields recs))
        (changed '()))
    (dolist (kv fields)
      (let ((name (car kv))
            (val (cdr kv)))
        (when (and val
                   (or force
                       (not (org-entry-get nil name)))
                   (not (equal val (org-entry-get nil name))))
          (org-entry-put nil name val)
          (push name changed))))
    (nreverse changed)))

(defun org-reading-list--loc-apply-tags (recs &optional replace)
  "Add subject tags from MARC RECS to the entry at point.
Existing tags are kept and merged unless REPLACE is non-nil.  At most
`org-reading-list-max-tags' new tags are taken.  Return the new tags."
  (let* ((new (seq-take (org-reading-list--marc-subject-tags recs)
                        org-reading-list-max-tags))
         (tags (if replace
                   new
                 (delete-dups (append (org-get-tags nil t) new)))))
    (when tags
      (org-set-tags tags))
    new))

;;;; Library of Congress: commands

;;;###autoload
(defun org-reading-list-loc-enrich (&optional force)
  "Fill missing identifier properties of the Org entry at point from LoC.
Queries the LC Catalog over SRU by the entry's :ISBN:, or by :LCCN:
when there is no ISBN (pre-ISBN books), or by :TITLE: and :AUTHOR:
when the entry has neither identifier, and sets :LCCN:, :OCLC:,
:LCC:, and :DDC: from the MARC 010, 035, 050, and 082 fields.  When
several records match (print and ebook editions), the one whose 020 $a
matches the ISBN is preferred, with the others consulted for any field
it lacks.  Title/author results are kept only when their author and
year agree with the entry, so prune or correct an entry's :AUTHOR:
and :DATE: first if the search comes back empty.

By default existing property values are never overwritten; with a
prefix argument FORCE (\\[universal-argument]), fetched values replace
existing ones — useful for refreshing entries captured from
pre-publication CIP records once LoC completes them."
  (interactive "P")
  (let* ((recs (org-reading-list--loc-entry-records))
         (changed (org-reading-list--loc-apply-fields recs force)))
    (message "LoC enrich (%d record%s): %s"
             (length recs) (if (= (length recs) 1) "" "s")
             (if changed
                 (format "%s %s"
                         (if force "set" "added")
                         (string-join changed ", "))
               "nothing new to add"))))

;;;###autoload
(defun org-reading-list-loc-tags (&optional replace)
  "Add LoC subject headings to the Org entry at point as tags.
Queries like `org-reading-list-loc-enrich' and harvests 650/651
fields, each heading and subdivision becoming a normalized tag.
Existing tags are merged with the new ones; with a prefix argument
REPLACE, existing tags are replaced.  Prune to your tag vocabulary
afterward — LCSH is wordier than a working set of tags wants."
  (interactive "P")
  (let* ((recs (org-reading-list--loc-entry-records))
         (added (org-reading-list--loc-apply-tags recs replace)))
    (message "LoC tags: %s"
             (if added (string-join added " ") "none found"))))

;;;; Filing entries under a headline

(defun org-reading-list--demote (entry levels)
  "Add LEVELS leading stars to every heading line in ENTRY string."
  (if (<= levels 0)
      entry
    (replace-regexp-in-string
     "^\\*+ " (concat (make-string levels ?*) "\\&") entry)))

(defun org-reading-list--find-or-create-headline (headline)
  "Return the position of HEADLINE in the current buffer.
Match HEADLINE as `org-capture's `file+headline' target does, creating
it as a top-level heading at the end of the buffer when absent."
  (or (org-find-exact-headline-in-buffer headline (current-buffer) t)
      (progn
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* " headline "\n")
        (line-beginning-position 0))))

(defun org-reading-list--insert-under-headline (entry headline)
  "File ENTRY as the last child of HEADLINE in the current Org buffer.
HEADLINE is matched and created at the end of the buffer when absent,
like the capture template's `file+headline' target.  ENTRY, a
top-level Org subtree string, is demoted to sit one level below
HEADLINE.  Return the buffer position of the inserted entry."
  (goto-char (org-reading-list--find-or-create-headline headline))
  (let ((level (org-current-level)))
    (org-end-of-subtree t)
    (unless (bolp) (insert "\n"))
    (insert "\n")
    (let ((start (point)))
      (insert (org-reading-list--demote (string-trim-right entry) level)
              "\n")
      start)))

;;;###autoload
(defun org-reading-list-goto-headline ()
  "Move point to `org-reading-list-headline', creating it if absent.
Intended as the location function of a `file+function' capture target:

  (file+function org-reading-list-file org-reading-list-goto-headline)

so Org capture and `org-reading-list-insert' file under the same
heading.  Org capture visits and widens the buffer before calling this;
leaving point on the heading makes capture add the new entry as its
child, as `file+headline' would."
  (goto-char (org-reading-list--find-or-create-headline
              org-reading-list-headline)))

;;;; Entry points

;;;###autoload
(defun org-reading-list-insert (id)
  "File a reading-list entry for ID in `org-reading-list-file'.
ID is an ISBN, an Open Library edition id, or an openlibrary.org URL.
Prefixed identifiers (ISBN:, OLID:, LCCN:, OCLC:) are also accepted;
LCCNs may be in catalog-card form (\"LCCN:61-10539\").
The entry is appended as the last child of `org-reading-list-headline'
\(created if absent), mirroring the capture template's target, and the
file is displayed with point on the new entry.  When the book appears
to be in the list already — same ISBN or OLID, or same title and
author — you are asked first; declining jumps to the existing entry
instead of inserting."
  (interactive "sBook id (ISBN, OL edition id or URL, LCCN:…, OCLC:…): ")
  (let* ((data (org-reading-list--entry-data id))
         (dup (org-reading-list--duplicate-in-file data))
         (buf (find-file-noselect org-reading-list-file))
         (pos (when (or (null dup) (org-reading-list--confirm-duplicate dup))
                (with-current-buffer buf
                  (save-restriction
                    (widen)
                    (org-reading-list--insert-under-headline
                     (org-reading-list--entry-string
                      (if org-reading-list-preen-on-capture
                          (org-reading-list--preen-data data)
                        data))
                     org-reading-list-headline))))))
    (pop-to-buffer buf)
    (widen)
    (goto-char (or pos (plist-get (cdr dup) :pos)))
    (unless pos
      (message "Already in list: %s" (plist-get (cdr dup) :heading)))))

;;;###autoload
(defun org-reading-list-capture ()
  "Build a capture entry, prompting for an identifier and optional source.
The identifier is an ISBN, an Open Library edition id, or an
openlibrary.org URL.  Prefixed identifiers (ISBN:, OLID:, LCCN:,
OCLC:) are also accepted; LCCNs may be in catalog-card form
\(\"LCCN:61-10539\").  Intended for use in an Org capture template as
\"%(org-reading-list-capture)\".  Falls back to a bare manual entry if
the lookup fails.  When the book appears to be in the list already,
you are asked first; declining aborts the capture."
  (let ((id (string-trim
             (read-string
              "Book id (ISBN, OL edition id or URL, LCCN:…, OCLC:…): ")))
        (source (let ((s (string-trim
                          (read-string
                           "Found via (URL, blank to skip): "))))
                  (unless (string-empty-p s) s))))
    (condition-case err
        (let* ((data (org-reading-list--entry-data id source))
               (dup (org-reading-list--duplicate-in-file data)))
          (when (and dup (not (org-reading-list--confirm-duplicate dup)))
            (signal 'org-reading-list-duplicate
                    (list (plist-get (cdr dup) :heading))))
          (org-reading-list--entry-string
           (if org-reading-list-preen-on-capture
               (org-reading-list--preen-data data)
             data)))
      (org-reading-list-duplicate
       (user-error "Already in list: %s" (cadr err)))
      (error
       (format "* TOREAD %s\n:PROPERTIES:\n:BTYPE: book\n:ADDED: %s%s\n:END:\n"
               (read-string "Lookup failed — title: ")
               (format-time-string "[%Y-%m-%d %a]")
               (if source (format "\n:FOUND: %s" source) ""))))))

(provide 'org-reading-list)
;;; org-reading-list.el ends here
