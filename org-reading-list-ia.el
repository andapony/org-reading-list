;;; org-reading-list-ia.el --- Internet Archive edition discovery -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Rob Duncan

;; Author: Rob Duncan
;; URL: https://github.com/andapony/org-reading-list
;; Version: 0.7.0
;; Package-Requires: ((emacs "28.1") (org "9.4"))
;; Keywords: bib, outlines
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Discover earlier scanned editions of a reading-list book on the
;; Internet Archive and add a chosen edition as a new entry.  Load this
;; module to add the `find-editions' verb to `org-reading-list-dispatch'.

;;; Code:

(require 'org-reading-list)
(require 'tabulated-list)
(require 'ucs-normalize)
(require 'url-util)
(require 'seq)
(require 'subr-x)

(defgroup org-reading-list-ia nil
  "Internet Archive edition discovery for `org-reading-list'."
  :group 'org-reading-list)

(defcustom org-reading-list-ia-max-candidates 25
  "Maximum number of Internet Archive candidates fetched and displayed."
  :type 'integer
  :group 'org-reading-list-ia)

(defconst org-reading-list-ia--search-endpoint
  "https://archive.org/advancedsearch.php"
  "Internet Archive advanced-search endpoint.")

(defconst org-reading-list-ia--stopwords
  '("the" "a" "an" "and" "or" "of" "to" "in" "on" "for"
    "with" "from" "by" "at" "as")
  "Words ignored when tokenizing titles for matching.")

(defun org-reading-list-ia--title-tokens (title)
  "Return the significant lowercase tokens of TITLE.
Tokens are alphanumeric runs of two or more characters that are not
common stopwords; nil when TITLE is nil or blank."
  (when (stringp title)
    (seq-remove
     (lambda (tok)
       (or (< (length tok) 2)
           (member tok org-reading-list-ia--stopwords)))
     (split-string (downcase title) "[^a-z0-9]+" t))))

(defun org-reading-list-ia--surname (author)
  "Return the lowercase surname slug of AUTHOR, or nil.
AUTHOR is in inverted \"Surname, Given\" form.
Accented letters are normalized via Unicode NFD before slugging."
  (when (stringp author)
    (let* ((raw (car (split-string author ",")))
           (key (org-reading-list--slug
                 (ucs-normalize-NFD-string raw))))
      (unless (string-empty-p key) key))))

(defun org-reading-list-ia--search-url (author title rows)
  "Return the IA advancedsearch URL for AUTHOR and TITLE, ROWS results.
The query requires the author surname and the significant title tokens
and restricts to scanned texts."
  (let* ((surname (or (org-reading-list-ia--surname author) ""))
         (tokens (org-reading-list-ia--title-tokens title))
         (q (format "creator:(%s) AND title:(%s) AND mediatype:texts"
                    surname (string-join tokens " "))))
    (concat org-reading-list-ia--search-endpoint
            "?q=" (url-hexify-string q)
            "&fl[]=identifier&fl[]=title&fl[]=creator"
            "&fl[]=year&fl[]=publisher&fl[]=collection"
            "&rows=" (number-to-string rows)
            "&output=json")))

(defun org-reading-list-ia--as-string (v)
  "Coerce IA metadata value V to a string."
  (cond ((stringp v) v)
        ((numberp v) (number-to-string v))
        ((null v) "")
        ((listp v) (mapconcat #'org-reading-list-ia--as-string v "; "))
        (t (format "%s" v))))

(defun org-reading-list-ia--doc-field (doc key)
  "Return DOC's KEY as a single string, or nil when absent.
List-valued fields are joined with \"; \"."
  (let ((cell (assq key doc)))
    (when cell (org-reading-list-ia--as-string (cdr cell)))))

(defun org-reading-list-ia--search (author title rows)
  "Query IA for AUTHOR and TITLE; return up to ROWS candidate plists.
Each plist has :identifier :title :creator :year :publisher and
:collection.  Returns nil on any fetch failure."
  (let* ((url (org-reading-list-ia--search-url author title rows))
         (json (org-reading-list--fetch-json url))
         (docs (cdr (assq 'docs (cdr (assq 'response json))))))
    (mapcar
     (lambda (doc)
       (list :identifier (org-reading-list-ia--doc-field doc 'identifier)
             :title (org-reading-list-ia--doc-field doc 'title)
             :creator (org-reading-list-ia--doc-field doc 'creator)
             :year (org-reading-list-ia--doc-field doc 'year)
             :publisher (org-reading-list-ia--doc-field doc 'publisher)
             :collection (org-reading-list-ia--doc-field doc 'collection)))
     docs)))

(defun org-reading-list-ia--candidate-match-p (cand author title)
  "Non-nil if CAND plausibly matches AUTHOR and TITLE.
The author surname slug must appear in CAND's :creator, and at least
half of TITLE's significant tokens must appear in CAND's :title.  This
guards against same-title, wrong-author hits."
  (let* ((surname (org-reading-list-ia--surname author))
         (creator (org-reading-list--slug
                   (ucs-normalize-NFD-string
                    (downcase (or (plist-get cand :creator) "")))))
         (want (org-reading-list-ia--title-tokens title))
         (have (org-reading-list-ia--title-tokens (plist-get cand :title))))
    (and surname
         (string-match-p (regexp-quote surname) creator)
         want
         (>= (length (seq-intersection want have #'equal))
             (max 1 (/ (1+ (length want)) 2)))
         t)))

(defun org-reading-list-ia--metadata (identifier)
  "Fetch IA metadata for IDENTIFIER; return a quality plist, or nil.
The plist has :ppi (integer) :imagecount (integer) :open (boolean)
:google (boolean) :olid (string or nil) and :scan t.  Returns nil when
the item has no readable scan (a catalog stub)."
  (let* ((url (concat "https://archive.org/metadata/" identifier))
         (json (org-reading-list--fetch-json url))
         (md (cdr (assq 'metadata json))))
    (when md
      (let* ((field (lambda (k)
                      (downcase (org-reading-list-ia--as-string
                                 (cdr (assq k md))))))
             (imagecount (string-to-number (funcall field 'imagecount)))
             (ppi (string-to-number (funcall field 'ppi)))
             (has-scandate (and (assq 'scandate md) t))
             (olid (let ((v (org-reading-list-ia--as-string
                             (cdr (assq 'openlibrary_edition md)))))
                     (unless (string-empty-p v) v))))
        (when (or (> imagecount 0) has-scandate)
          (list :ppi ppi
                :imagecount imagecount
                :open (not (equal (funcall field 'access-restricted-item)
                                  "true"))
                :google (and (or (string-match-p "google" (funcall field 'contributor))
                                 (string-match-p "google" (funcall field 'sponsor))
                                 (string-match-p "googlebooks" (funcall field 'collection))
                                 (string-suffix-p "goog" (downcase identifier)))
                             t)
                :olid olid
                :scan t))))))

(defun org-reading-list-ia--year-int (year)
  "Return the first four-digit year in YEAR as an integer, or nil."
  (when (and (stringp year) (string-match "[0-9]\\{4\\}" year))
    (string-to-number (match-string 0 year))))

(defun org-reading-list-ia--rank (rows)
  "Sort enriched ROWS earliest-year first, quality breaking ties.
Tie-break order: non-Google before Google, open before lending, higher
ppi first.  Rows with no :year-int sort last."
  (sort (copy-sequence rows)
        (lambda (a b)
          (let ((ya (or (plist-get a :year-int) most-positive-fixnum))
                (yb (or (plist-get b :year-int) most-positive-fixnum)))
            (cond
             ((/= ya yb) (< ya yb))
             ((not (eq (plist-get a :google) (plist-get b :google)))
              (not (plist-get a :google)))
             ((not (eq (plist-get a :open) (plist-get b :open)))
              (and (plist-get a :open) t))
             (t (> (or (plist-get a :ppi) 0)
                   (or (plist-get b :ppi) 0))))))))

(defun org-reading-list-ia--editions (author title)
  "Discover ranked scanned editions of TITLE by AUTHOR on IA.
Return a plist (:rows ROWS :truncated BOOL).  ROWS are ranked candidate
plists enriched with metadata; candidates failing the match guard or
lacking a readable scan are dropped.  At most
`org-reading-list-ia-max-candidates' matches are processed."
  (let* ((cap org-reading-list-ia-max-candidates)
         (all (org-reading-list-ia--search author title (1+ cap)))
         (matches (seq-filter
                   (lambda (c)
                     (org-reading-list-ia--candidate-match-p c author title))
                   all))
         (truncated (> (length matches) cap))
         (use (seq-take matches cap))
         (rows nil))
    (dolist (c use)
      (let ((md (org-reading-list-ia--metadata (plist-get c :identifier))))
        (when md
          (push (append c md
                        (list :year-int
                              (org-reading-list-ia--year-int
                               (plist-get c :year))))
                rows))))
    (list :rows (org-reading-list-ia--rank (nreverse rows))
          :truncated truncated)))

(defun org-reading-list-ia--new-entry (cand source today existing-keys)
  "Return an Org entry string for edition CAND of the work in SOURCE.
SOURCE is a plist (:author :title :subjects :tags :citekey) read from
the existing entry; work-level fields are copied from it and
edition-level fields come from CAND.  TODAY is an inactive Org
timestamp; EXISTING-KEYS is the list of cite keys already in the file,
for collision suffixing."
  (let* ((author (plist-get source :author))
         (year (org-reading-list-ia--year-int (plist-get cand :year)))
         (year-str (and year (number-to-string year)))
         (key (org-reading-list--citekey-unique
               (org-reading-list--citekey-base author year-str)
               existing-keys))
         (tags (plist-get source :tags))
         (pages (let ((n (plist-get cand :imagecount)))
                  (and n (> n 0) (number-to-string n))))
         (props
          (seq-filter
           #'cdr
           (list
            (cons "CUSTOM_ID" key)
            (cons "BTYPE" "book")
            (cons "AUTHOR" author)
            (cons "TITLE" (plist-get source :title))
            (cons "PUBLISHER" (plist-get cand :publisher))
            (cons "DATE" year-str)
            (cons "PAGES" pages)
            (cons "IA" (plist-get cand :identifier))
            (cons "OLID" (plist-get cand :olid))
            (cons "SUBJECTS" (plist-get source :subjects))
            (cons "ADDED" today)
            (cons "FOUND"
                  (format "IA edition discovery; earlier edition of [[#%s]]"
                          (plist-get source :citekey)))))))
    (concat
     (format "* TOREAD %s%s\n"
             (plist-get source :title)
             (if (and tags (not (string-empty-p tags)))
                 (format " :%s:" tags) ""))
     ":PROPERTIES:\n"
     (mapconcat (lambda (kv) (format ":%s: %s\n" (car kv) (cdr kv))) props "")
     ":END:\n")))

(defun org-reading-list-ia--add-backlink (origpos newkey)
  "Insert an \"Earlier edition\" link to NEWKEY under the entry at ORIGPOS."
  (save-excursion
    (goto-char origpos)
    (org-back-to-heading t)
    (org-end-of-meta-data t)
    (unless (bolp) (insert "\n"))
    (insert (format "Earlier edition: [[#%s]]\n" newkey))))

(defun org-reading-list-ia--add-edition (cand)
  "Add edition CAND as a new entry, back-linked from the entry at point.
Work-level fields are read from the entry at point; the new entry is
filed under `org-reading-list-headline'; an \"Earlier edition\" link is
added to the original.  Return the new entry's buffer position."
  (let* ((origpos (save-excursion (org-back-to-heading t) (point)))
         (src-citekey (org-entry-get nil "CUSTOM_ID")))
    (when (or (null src-citekey) (string-empty-p src-citekey))
      (user-error
       "Entry needs a :CUSTOM_ID: before adding an edition %s"
       "(run org-reading-list-ensure-citekey)"))
    (let* ((source (list :author (org-entry-get nil "AUTHOR")
                         :title (org-entry-get nil "TITLE")
                         :subjects (org-entry-get nil "SUBJECTS")
                         :tags (string-join (org-get-tags nil t) ":")
                         :citekey src-citekey))
           (today (format-time-string "[%Y-%m-%d %a]"))
           (entry (org-reading-list-ia--new-entry
                   cand source today (org-reading-list--buffer-citekeys)))
           (newkey (and (string-match ":CUSTOM_ID: \\(\\S-+\\)" entry)
                        (match-string 1 entry))))
      ;; Back-link first (does not shift ORIGPOS), then file the new entry.
      (org-reading-list-ia--add-backlink origpos newkey)
      (save-restriction
        (widen)
        (org-reading-list--insert-under-headline
         entry org-reading-list-headline)))))

(defun org-reading-list-ia--format-row (row current-year)
  "Return a tabulated-list cell vector for edition ROW.
CURRENT-YEAR is the listed entry's year; the matching row is marked
\"← current\"."
  (let* ((yi (plist-get row :year-int))
         (year (cond ((null yi) (or (plist-get row :year) "?"))
                     ((and current-year (= yi current-year))
                      (format "%d ← current" yi))
                     (t (number-to-string yi))))
         (pages (let ((n (plist-get row :imagecount)))
                  (if (and n (> n 0)) (number-to-string n) "?")))
         (ppi (let ((p (plist-get row :ppi)))
                (if (and p (> p 0)) (number-to-string p) "?"))))
    (vector year
            (or (plist-get row :title) "")
            (or (plist-get row :identifier) "")
            (if (plist-get row :google) "Google" "Library")
            (if (plist-get row :open) "Open" "Lending")
            ppi
            pages
            (or (plist-get row :olid) "—"))))

(defun org-reading-list-ia--confirm (row)
  "Show a one-line preview of ROW and ask for confirmation."
  (yes-or-no-p
   (format "Add %s (%s) as a new entry [IA:%s]? "
           (or (plist-get row :title) "edition")
           (or (plist-get row :year) "?")
           (plist-get row :identifier))))

(defun org-reading-list-ia--act-on (row)
  "On confirmation, add ROW as a new edition entry; else return nil."
  (when (org-reading-list-ia--confirm row)
    (org-reading-list-ia--add-edition row)))

(defvar-local org-reading-list-ia--rows nil
  "Editions backing the current candidate buffer.")

(defvar-local org-reading-list-ia--origin nil
  "Marker at the entry the candidate buffer was launched from.")

(defun org-reading-list-ia--pick ()
  "Add the edition on the current candidate-table line."
  (interactive)
  (let ((row (nth (1- (line-number-at-pos)) org-reading-list-ia--rows))
        (origin org-reading-list-ia--origin))
    (when (and row origin)
      (with-current-buffer (marker-buffer origin)
        (save-excursion
          (goto-char origin)
          (org-reading-list-ia--act-on row)))
      (message "Added edition %s" (plist-get row :identifier)))))

(define-derived-mode org-reading-list-ia-candidates-mode tabulated-list-mode
  "IA-Editions"
  "Major mode for the Internet Archive candidate table."
  (setq tabulated-list-format
        [("Year" 14 nil) ("Title" 30 nil) ("IA id" 22 nil)
         ("Source" 8 nil) ("Open?" 8 nil) ("ppi" 5 nil)
         ("Leaves" 7 nil) ("OLID" 12 nil)])
  (tabulated-list-init-header))

(defun org-reading-list-ia--show-candidates (rows current-year origin truncated)
  "Display ROWS in a candidate buffer launched from ORIGIN.
CURRENT-YEAR marks the listed edition; TRUNCATED notes a capped search."
  (let ((buf (get-buffer-create "*IA editions*")))
    (with-current-buffer buf
      (org-reading-list-ia-candidates-mode)
      (setq org-reading-list-ia--rows rows
            org-reading-list-ia--origin origin
            tabulated-list-entries
            (let ((i 0))
              (mapcar (lambda (r)
                        (prog1 (list (number-to-string i)
                                     (org-reading-list-ia--format-row
                                      r current-year))
                          (setq i (1+ i))))
                      rows)))
      (tabulated-list-print)
      (when truncated
        (message "Results truncated at %d candidates"
                 org-reading-list-ia-max-candidates))
      (local-set-key (kbd "RET") #'org-reading-list-ia--pick))
    (pop-to-buffer buf)))

(defun org-reading-list-ia--find-at-point ()
  "Find and offer scanned editions for the reading-list entry at point."
  (let* ((author (org-entry-get nil "AUTHOR"))
         (title (org-entry-get nil "TITLE"))
         (current (org-reading-list-ia--year-int (org-entry-get nil "DATE")))
         (origin (save-excursion (org-back-to-heading t) (point-marker))))
    (unless (and author title)
      (user-error "Entry needs :AUTHOR: and :TITLE: to search IA"))
    (let* ((res (org-reading-list-ia--editions author title))
           (rows (plist-get res :rows)))
      (if (null rows)
          (message "No scanned editions found on the Internet Archive")
        (org-reading-list-ia--show-candidates
         rows current origin (plist-get res :truncated))))))

(declare-function org-reading-list-ia--report "org-reading-list-ia")

;;;###autoload
(defun org-reading-list-ia-find-editions (&optional all)
  "Find scanned earlier editions of a reading-list book on the Internet Archive.
Without a prefix, operate on the entry at point: show a ranked
candidate table and add the chosen edition as a new entry.  With a
prefix argument ALL, produce a read-only discovery report across the
whole buffer, with drill-in to the per-entry table."
  (interactive "P")
  (if all
      (org-reading-list-ia--report)
    (org-reading-list-ia--find-at-point)))

(provide 'org-reading-list-ia)
;;; org-reading-list-ia.el ends here
