;;; org-reading-list-ia.el --- Internet Archive edition discovery -*- lexical-binding: t; -*-

;; Author: Rob Duncan
;; Keywords: bib, org

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

(provide 'org-reading-list-ia)
;;; org-reading-list-ia.el ends here
