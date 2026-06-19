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
;; (milibrary.org).  Search the catalog by keyword (title-scoped by
;; default), pick a result, and capture it into the reading list or
;; enrich an existing heading.  Results come from the keyword results
;; page, where each row carries a stable bib id; the bibliographic
;; record is then fetched as structured XML from the /xrecord= endpoint
;; and mapped into the same record shape the Library of Congress code in
;; org-reading-list.el uses, so the MARC field extractors and entry
;; builders are shared.

;;; Code:

(require 'org-reading-list)
(require 'dom)
(require 'cl-lib)

(defcustom org-reading-list-mi-search-url
  "https://search.milibrary.org/search~S1/?searchtype=X&searcharg=%s&searchscope=1&SORT=D"
  "Keyword-search template for the Mechanics' Institute WebPAC.
The single %s is the url-encoded search argument.  Index scoping is
applied by `org-reading-list-mi--search-candidates' using the catalog's
keyword field syntax, e.g. \"t:(...)\" for a title-scoped search."
  :type 'string
  :group 'org-reading-list)

(defcustom org-reading-list-mi-record-url
  "https://search.milibrary.org/record=%s"
  "Stable record URL for a MILibrary bib id (e.g. \"b1146522\")."
  :type 'string
  :group 'org-reading-list)

(defcustom org-reading-list-mi-xrecord-url
  "https://search.milibrary.org/xrecord=%s"
  "XML-record endpoint template for a MILibrary bib id.
Returns the bib record as Innovative Interfaces XML, which
`org-reading-list-mi--xrecord-to-record' maps to a MARC record node."
  :type 'string
  :group 'org-reading-list)

(defcustom org-reading-list-mi-holdings-code "MILIB"
  "Holdings/CALLNO code applied to entries captured from MILibrary.
You may add it to `org-reading-list-holdings-codes' to have it offered
for completion by `org-reading-list-set-holdings'."
  :type 'string
  :group 'org-reading-list)

(defcustom org-reading-list-mi-max-results 25
  "Maximum number of search candidates offered for selection."
  :type 'integer
  :group 'org-reading-list)

(defconst org-reading-list-mi-indexes
  '(("title" . "t") ("author" . "a") ("keyword" . nil) ("ISBN" . "i"))
  "Search-index labels mapped to WebPAC keyword field codes.
A nil code means an unscoped keyword search.")

(defun org-reading-list-mi--fetch-html (url)
  "GET URL and parse the response body as HTML; return a DOM, or nil."
  (let ((buf (org-reading-list--http-body-buffer url 20)))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (ignore-errors
              (libxml-parse-html-region (point-min) (point-max))))
        (kill-buffer buf)))))

(defun org-reading-list-mi--xrecord-subfields (varfld)
  "Return the MARC subfields of Innovative Interfaces VARFLD.
Each MARCSUBFLD becomes a (subfield ((code . C)) DATA) node; subfields
without a code are dropped."
  (delq nil
        (mapcar
         (lambda (sf)
           (let ((code (string-trim
                        (dom-text (car (dom-by-tag sf 'SUBFIELDINDICATOR)))))
                 (val (string-trim
                       (dom-text (car (dom-by-tag sf 'SUBFIELDDATA))))))
             (unless (string-empty-p code)
               `(subfield ((code . ,code)) ,val))))
         (dom-by-tag varfld 'MARCSUBFLD))))

(defun org-reading-list-mi--xrecord-to-record (dom)
  "Map an Innovative Interfaces xrecord DOM to a MARC `record' node.
Each VARFLD with subfields becomes a datafield carrying the same tag,
indicators, and subfield codes the Library of Congress MARCXML helpers
read, so the existing extractors in org-reading-list.el work unchanged.
Fixed and control fields (no subfields) are skipped.  Return nil when
no variable fields are found."
  (let (fields)
    (dolist (vf (dom-by-tag dom 'VARFLD))
      (let* ((info (car (dom-by-tag vf 'MARCINFO)))
             (tag (and info
                       (string-trim
                        (dom-text (car (dom-by-tag info 'MARCTAG))))))
             (ind1 (and info
                        (string-trim
                         (dom-text (car (dom-by-tag info 'INDICATOR1))))))
             (ind2 (and info
                        (string-trim
                         (dom-text (car (dom-by-tag info 'INDICATOR2))))))
             (subs (org-reading-list-mi--xrecord-subfields vf)))
        (when (and tag (not (string-empty-p tag)) subs)
          (push `(datafield ((tag . ,tag)
                             (ind1 . ,(or ind1 ""))
                             (ind2 . ,(or ind2 "")))
                            ,@subs)
                fields))))
    (when fields
      `(record nil ,@(nreverse fields)))))

(defun org-reading-list-mi--bib-record (bibid)
  "Fetch the III xrecord for BIBID and map it to a MARC record, or nil."
  (let ((dom (org-reading-list--fetch-xml
              (format org-reading-list-mi-xrecord-url bibid))))
    (and dom (org-reading-list-mi--xrecord-to-record dom))))

(defun org-reading-list-mi--node-class-text (node class)
  "Return the text of NODE's first descendant with CSS CLASS, or nil.
Text nested in inline elements (e.g. a wrapping anchor) is included;
internal runs of whitespace are collapsed to single spaces."
  (let ((d (car (dom-by-class node class))))
    (and d (let ((s (string-trim
                     (replace-regexp-in-string "[ \t\n\r]+" " " (dom-texts d)))))
             (unless (string-empty-p s) s)))))

(defun org-reading-list-mi--row-bibid (row)
  "Return the bib id from ROW's save checkbox, or nil.
ROW is a brief-citation result node; its `save' input value is a bib
id of the form \"bNNNNNNN\"."
  (seq-some
   (lambda (input)
     (and (equal (dom-attr input 'name) "save")
          (let ((v (dom-attr input 'value)))
            (and v (string-match-p "\\`b[0-9]+\\'" v) v))))
   (dom-by-tag row 'input)))

(defun org-reading-list-mi--row-format (row)
  "Return ROW's media/format label (e.g. \"Books\", \"DVD\"), or nil.
Read from the brief-citation media icon's alt text."
  (let* ((media (car (dom-by-class row "briefcitMedia")))
         (img (and media (car (dom-by-tag media 'img))))
         (alt (and img (dom-attr img 'alt))))
    (and alt (not (string-empty-p alt)) alt)))

(defun org-reading-list-mi--row-year (row)
  "Return a four-digit year from ROW's publisher line, or nil."
  (let ((pub (org-reading-list-mi--node-class-text row "briefcitPublisher")))
    (and pub (string-match "[0-9]\\{4\\}" pub) (match-string 0 pub))))



(defun org-reading-list-mi--single-record-bibid (dom)
  "Return the bib id of a single-record page DOM, or nil.
A unique search lands on the record page; its permalink href carries
\"record=bNNNNNNN\"."
  (seq-some
   (lambda (a)
     (let ((h (dom-attr a 'href)))
       (and h (string-match "record=\\(b[0-9]+\\)" h) (match-string 1 h))))
   (dom-by-tag dom 'a)))

(defun org-reading-list-mi--results-candidates (dom)
  "Parse a MILibrary keyword-results DOM into candidate plists.
Return a list of plists (:title :author :year :format :bibid), one per
brief-citation row, capped at `org-reading-list-mi-max-results'.  When
the search matched a single record (the catalog shows the record page
directly) return one candidate built from that page."
  (let ((rows (dom-by-class dom "briefcitCell"))
        cands)
    (if rows
        (catch 'done
          (dolist (row rows)
            (let ((bibid (org-reading-list-mi--row-bibid row))
                  (title (org-reading-list-mi--node-class-text
                          row "briefcitTitle")))
              (when (and bibid title)
                (push (list :title title
                            :author (org-reading-list-mi--node-class-text
                                     row "briefcitAuthor")
                            :year (org-reading-list-mi--row-year row)
                            :format (org-reading-list-mi--row-format row)
                            :bibid bibid)
                      cands)
                (when (>= (length cands) org-reading-list-mi-max-results)
                  (throw 'done nil)))))
          nil)
      (let ((bibid (org-reading-list-mi--single-record-bibid dom)))
        (when bibid
          (push (list :title (or (org-reading-list-mi--node-class-text
                                  dom "bibInfoData")
                                 bibid)
                      :author nil :year nil :format nil :bibid bibid)
                cands))))
    (nreverse cands)))

(defun org-reading-list-mi--better-abstract (a b)
  "Return the more informative of abstracts A and B, or nil.
A nil or blank value loses to a non-blank one; when both have
content, the longer string wins."
  (let ((a (and a (not (string-blank-p a)) a))
        (b (and b (not (string-blank-p b)) b)))
    (cond ((and a b) (if (>= (length a) (length b)) a b))
          (a a)
          (b b))))

(defun org-reading-list-mi--callno (rec)
  "Return the MILibrary local call number from REC, or nil.
Reads MARC 092 (Dewey-based local class), falling back to 099 or 090,
joining the call-number subfields."
  (or (org-reading-list--marc-field rec "092" "a" "b")
      (org-reading-list--marc-field rec "099" "a" "f")
      (org-reading-list--marc-field rec "090" "a" "b")))

(defun org-reading-list-mi--set-prop (props key value)
  "Return PROPS with KEY set to VALUE (added if absent, replaced if present)."
  (let ((cell (assoc key props)))
    (if cell
        (progn (setcdr cell value) props)
      (append props (list (cons key value))))))

(defun org-reading-list-mi--overlay (data rec)
  "Attach MI holdings, call number, and the better abstract to DATA.
DATA is an entry-data plist; REC is the MILibrary record DOM.  The
MILIB holdings code is added, :CALLNO: gets a \"CODE callno\" pair, and
:ABSTRACT: is the more informative of DATA's and REC's 520 summary."
  (let* ((props (plist-get data :props))
         (code org-reading-list-mi-holdings-code)
         (callno (org-reading-list-mi--callno rec))
         (mi-abstract (org-reading-list--marc-field rec "520" "a" "b"))
         (best (org-reading-list-mi--better-abstract
                (cdr (assoc "ABSTRACT" props)) mi-abstract)))
    (setq props (org-reading-list-mi--set-prop props "HOLDINGS" code))
    (when callno
      (setq props (org-reading-list-mi--set-prop
                   props "CALLNO" (format "%s %s" code callno))))
    (when best
      (setq props (org-reading-list-mi--set-prop props "ABSTRACT" best)))
    (plist-put data :props props)))

(defun org-reading-list-mi--enrich-empty (base extra)
  "Fill empty fields of entry-data BASE from EXTRA; return BASE.
BASE's own non-empty values always win.  Absent or empty props are
filled from EXTRA, the richer abstract is chosen, a missing title is
taken from EXTRA, subjects are unioned, and EXTRA's ISBNs are merged
in for duplicate detection."
  (let ((bprops (plist-get base :props))
        (eprops (plist-get extra :props)))
    (dolist (kv eprops)
      (when (cdr kv)
        (let ((cell (assoc (car kv) bprops)))
          (cond ((null cell)
                 (setq bprops (append bprops (list (cons (car kv) (cdr kv))))))
                ((null (cdr cell))
                 (setcdr cell (cdr kv)))))))
    (let ((best (org-reading-list-mi--better-abstract
                 (cdr (assoc "ABSTRACT" bprops))
                 (cdr (assoc "ABSTRACT" eprops)))))
      (when best
        (let ((cell (assoc "ABSTRACT" bprops)))
          (if cell
              (setcdr cell best)
            (setq bprops (append bprops (list (cons "ABSTRACT" best))))))))
    (setq base (plist-put base :props bprops))
    (unless (plist-get base :title)
      (setq base (plist-put base :title (plist-get extra :title))))
    (setq base (plist-put base :subjects
                          (delete-dups (append (plist-get base :subjects)
                                               (plist-get extra :subjects)))))
    (plist-put base :isbns
               (delete-dups (append (plist-get base :isbns)
                                    (plist-get extra :isbns))))))


(defun org-reading-list-mi--entry-data (rec &optional source)
  "Build reading-list entry data from MILibrary record REC.
REC's own MARC is the authoritative base: for an MI-specific search the
held item, edition, publisher, and call number come from MI.  When REC
carries an ISBN (020) or LCCN (010), Open Library and the LC Catalog are
consulted to fill fields MI's record leaves empty (e.g. :OLID:, :IA:,
:DDC:, or a richer abstract); MI's own values always win, and a failed
or empty lookup is ignored.  MI holdings and the local call number are
attached.  SOURCE, if non-nil, is recorded in :FOUND:."
  (let* ((isbn (car (org-reading-list--marc-isbns rec "a")))
         (lccn (let ((v (org-reading-list--marc-field rec "010")))
                 (and v (replace-regexp-in-string " " "" v))))
         (base (org-reading-list--marc-entry-data (list rec) "MI" source))
         (extra (and (or isbn lccn)
                     (ignore-errors
                       (org-reading-list--entry-data
                        (concat (if isbn "ISBN:" "LCCN:") (or isbn lccn))
                        source)))))
    (org-reading-list-mi--overlay
     (if extra (org-reading-list-mi--enrich-empty base extra) base)
     rec)))

(defconst org-reading-list-mi--skip-props
  '("ADDED" "FOUND" "CUSTOM_ID" "HOLDINGS" "CALLNO")
  "Properties the update applier never enriches or overwrites directly.
HOLDINGS and CALLNO are handled by the holdings logic; the rest are
entry-local bookkeeping.")

(defun org-reading-list-mi--callno-merge (existing pair)
  "Return a merged :CALLNO: value combining EXISTING pairs with PAIR.
EXISTING is the current property value (semicolon-separated library
code/call-number pairs) or nil.  PAIR is the incoming pair, e.g.
\"MILIB 973.92 N53\".  If an existing pair shares PAIR's leading
token (library code), it is replaced in place; otherwise PAIR is
appended.  Returns the merged string."
  (if (or (null existing) (string-empty-p (string-trim existing)))
      pair
    (let* ((code (car (split-string pair)))
           (parts (mapcar #'string-trim (split-string existing ";")))
           (replaced nil)
           (merged
            (mapcar (lambda (p)
                      (if (equal (car (split-string p)) code)
                          (progn (setq replaced t) pair)
                        p))
                    parts)))
      (string-join (if replaced merged (append merged (list pair))) "; "))))

(defun org-reading-list-mi--apply-update (data)
  "Update the Org entry at point from entry-data DATA.
Adds the MILIB holdings code and call number, fills empty properties,
and overwrites a differing non-empty property only after `y-or-n-p'
confirmation.  Return the list of property names changed."
  (let ((props (plist-get data :props))
        (changed '()))
    ;; Holdings + call number.
    (org-reading-list--holdings-add org-reading-list-mi-holdings-code)
    (let ((pair (cdr (assoc "CALLNO" props))))
      (when pair
        (let* ((prior (org-entry-get nil "CALLNO"))
               (merged (org-reading-list-mi--callno-merge prior pair)))
          (unless (equal merged prior)
            (org-entry-put nil "CALLNO" merged)
            (push "CALLNO" changed)))))
    ;; Enrich / refresh the rest.
    (dolist (kv props)
      (let ((name (car kv))
            (val (cdr kv)))
        (when (and val (not (member name org-reading-list-mi--skip-props)))
          (let ((current (org-entry-get nil name)))
            (cond
             ((null current)
              (org-entry-put nil name val)
              (push name changed))
             ((and (not (equal current val))
                   (y-or-n-p (format "Replace :%s: %S with %S? "
                                     name current val)))
              (org-entry-put nil name val)
              (push name changed)))))))
    (nreverse changed)))

(defun org-reading-list-mi--search-candidates (field query)
  "Return MILibrary candidates for QUERY in keyword FIELD.
FIELD is a keyword field code (e.g. \"t\", \"a\", \"i\") that scopes the
search, or nil for an unscoped keyword search.  Signal a `user-error'
on no response or no results."
  (let* ((arg (if field (format "%s:(%s)" field query) query))
         (url (format org-reading-list-mi-search-url (url-hexify-string arg)))
         (dom (or (org-reading-list-mi--fetch-html url)
                  (user-error "No response from MILibrary for %s" query))))
    (or (org-reading-list-mi--results-candidates dom)
        (user-error "No MILibrary results for %s" query))))

(defun org-reading-list-mi--read-index ()
  "Prompt for a search index; return its keyword field code, or nil."
  (cdr (assoc (completing-read "Search by: "
                               (mapcar #'car org-reading-list-mi-indexes)
                               nil t nil nil "title")
              org-reading-list-mi-indexes)))

(defun org-reading-list-mi--choose (candidates)
  "Prompt to choose one of CANDIDATES; return its bib id."
  (let* ((labels (mapcar
                  (lambda (c)
                    (cons (org-reading-list-mi--candidate-label c)
                          (plist-get c :bibid)))
                  candidates))
         (pick (completing-read "MILibrary result: " labels nil t)))
    (cdr (assoc pick labels))))

(defun org-reading-list-mi--entry-bibid ()
  "Find the MILibrary bib id for the Org entry at point, or nil.
Searches by :ISBN: first, then by :TITLE:, taking the first candidate.
Signal a `user-error' when the entry has neither to search by."
  (let* ((isbn (let ((v (org-entry-get nil "ISBN")))
                 (and v (car (split-string v "[, ]" t)))))
         (title (org-entry-get nil "TITLE")))
    (cond
     (isbn (plist-get
            (car (org-reading-list-mi--search-candidates "i" isbn)) :bibid))
     ((and title (not (string-empty-p title)))
      (plist-get
       (car (org-reading-list-mi--search-candidates "t" title)) :bibid))
     (t (user-error "Entry has no :ISBN: or :TITLE: to search MILibrary by")))))

(defun org-reading-list-mi--candidate-label (c)
  "Return a display label for candidate plist C.
Shows the title, author, and a bracketed format/year tag when known so
editions (print, eBook, DVD, audiobook) can be told apart."
  (let ((author (plist-get c :author))
        (tags (delq nil (list (plist-get c :format) (plist-get c :year)))))
    (concat (plist-get c :title)
            (and author (format " - %s" author))
            (and tags (format "  [%s]" (string-join tags ", "))))))


;;;###autoload
(defun org-reading-list-mi-enrich ()
  "Enrich the Org entry at point from its MILibrary record.
Looks the entry up in MILibrary by :ISBN: (or :TITLE:), fetches the
matching record as XML, and applies the update: add the MILIB holdings
code and call number, fill empty properties, and refresh differing
properties on confirmation.  Signal a `user-error' when no MILibrary
record is found."
  (interactive)
  (let* ((bibid (or (org-reading-list-mi--entry-bibid)
                    (user-error "No MILibrary match for this entry")))
         (rec (or (org-reading-list-mi--bib-record bibid)
                  (user-error "No MARC record for %s" bibid)))
         (data (org-reading-list-mi--entry-data rec))
         (changed (org-reading-list-mi--apply-update data)))
    (message "Enriched from MILibrary%s"
             (if changed (format ": %s" (string-join changed ", "))
               " (no change)"))))

;;;###autoload
(defun org-reading-list-mi-search (&optional choose-index)
  "Search MILibrary, pick a result, and capture or update it.
Prompts for a title query (with a prefix arg CHOOSE-INDEX, first pick
the search index: title, author, keyword, or ISBN).  The chosen
record is fetched as XML and turned into a reading-list entry: a new
heading under `org-reading-list-headline', or, when the book is already
in `org-reading-list-file', an in-place update (MI holdings and call
number added, empty fields filled, differing fields refreshed on
confirmation).  When `org-reading-list-preen-on-capture' is enabled, a
new entry's tags are preened against the file's controlled vocabulary,
as in `org-reading-list-insert'.  Point is left on the entry."
  (interactive "P")
  (let* ((field (if choose-index (org-reading-list-mi--read-index) "t"))
         (query (read-string "MILibrary search: "))
         (candidates (org-reading-list-mi--search-candidates field query))
         (bibid (org-reading-list-mi--choose candidates))
         (rec (or (org-reading-list-mi--bib-record bibid)
                  (user-error "No MARC record for %s" bibid)))
         (data (org-reading-list-mi--entry-data rec))
         (dup (org-reading-list--duplicate-in-file data))
         (buf (find-file-noselect org-reading-list-file)))
    (pop-to-buffer buf)
    (widen)
    (if dup
        (progn
          (goto-char (plist-get (cdr dup) :pos))
          (let ((changed (org-reading-list-mi--apply-update data)))
            (message "Updated %s%s" (plist-get (cdr dup) :heading)
                     (if changed (format " (%s)" (string-join changed ", "))
                       " (no change)"))))
      (let ((pos (with-current-buffer buf
                   (save-restriction
                     (widen)
                     (org-reading-list--insert-under-headline
                      (org-reading-list--entry-string
                       (if org-reading-list-preen-on-capture
                           (org-reading-list--preen-data data)
                         data))
                      org-reading-list-headline)))))
        (goto-char pos)
        (message "Added %s" (plist-get data :title))))))

(provide 'org-reading-list-mi)
;;; org-reading-list-mi.el ends here
