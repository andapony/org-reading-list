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






(provide 'org-reading-list-ia)
;;; org-reading-list-ia.el ends here
