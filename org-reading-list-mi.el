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

(defun org-reading-list-mi--entry-data (rec &optional source)
  "Build reading-list entry data from MILibrary record REC.
When REC carries an ISBN (020) or LCCN (010), the rich base comes from
the existing Open Library + LoC pipeline (`org-reading-list--entry-data');
otherwise the entry is built directly from REC's MARC.  MI holdings, the
local call number, and the better abstract are overlaid either way.
SOURCE, if non-nil, is recorded in :FOUND:."
  (let* ((isbn (car (org-reading-list--marc-isbns rec "a")))
         (lccn (let ((v (org-reading-list--marc-field rec "010")))
                 (and v (replace-regexp-in-string " " "" v))))
         (base (cond
                (isbn (org-reading-list--entry-data
                       (concat "ISBN:" isbn) source))
                (lccn (org-reading-list--entry-data
                       (concat "LCCN:" lccn) source))
                (t (org-reading-list--marc-entry-data
                    (list rec) "MI" source)))))
    (org-reading-list-mi--overlay base rec)))

(defconst org-reading-list-mi--skip-props
  '("ADDED" "FOUND" "CUSTOM_ID" "HOLDINGS" "CALLNO")
  "Properties the update applier never enriches or overwrites directly.
HOLDINGS and CALLNO are handled by the holdings logic; the rest are
entry-local bookkeeping.")

(defun org-reading-list-mi--apply-update (data)
  "Update the Org entry at point from entry-data DATA.
Adds the MILIB holdings code and call number, fills empty properties,
and overwrites a differing non-empty property only after `y-or-n-p'
confirmation.  Return the list of property names changed."
  (let ((props (plist-get data :props))
        (changed '()))
    ;; Holdings + call number.
    (org-reading-list--holdings-add org-reading-list-mi-holdings-code)
    (let ((callno (cdr (assoc "CALLNO" props))))
      (when (and callno (not (equal callno (org-entry-get nil "CALLNO"))))
        (org-entry-put nil "CALLNO" callno)
        (push "CALLNO" changed)))
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

(defun org-reading-list-mi--search-candidates (index query)
  "Return MILibrary candidates for QUERY under search INDEX code.
INDEX is a WebPAC `searchtype' code; QUERY is the raw search string."
  (let* ((url (format org-reading-list-mi-search-url
                      index (url-hexify-string query)))
         (dom (or (org-reading-list-mi--fetch-html url)
                  (user-error "No response from MILibrary for %s" query))))
    (or (org-reading-list-mi--results-candidates dom)
        (user-error "No MILibrary results for %s" query))))

(defun org-reading-list-mi--read-index ()
  "Prompt for a search index; return its WebPAC `searchtype' code."
  (cdr (assoc (completing-read "Search by: "
                               (mapcar #'car org-reading-list-mi-indexes)
                               nil t nil nil "title")
              org-reading-list-mi-indexes)))

(defun org-reading-list-mi--choose (candidates)
  "Prompt to choose one of CANDIDATES; return its bib id."
  (let* ((labels (mapcar
                  (lambda (c)
                    (cons (concat (plist-get c :title)
                                  (let ((y (plist-get c :year)))
                                    (if y (format " (%s)" y) "")))
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

;;;###autoload
(defun org-reading-list-mi-enrich ()
  "Enrich the Org entry at point from its MILibrary record.
Looks the entry up in MILibrary by :ISBN: (or :TITLE:), fetches the
matching record's MARC, and applies the update: add the MILIB holdings
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
record's MARC is fetched and turned into a reading-list entry: a new
heading under `org-reading-list-headline', or, when the book is already
in `org-reading-list-file', an in-place update (MI holdings and call
number added, empty fields filled, differing fields refreshed on
confirmation).  Point is left on the entry."
  (interactive "P")
  (let* ((index (if choose-index (org-reading-list-mi--read-index) "t"))
         (query (read-string "MILibrary search: "))
         (candidates (org-reading-list-mi--search-candidates index query))
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
                      (org-reading-list--entry-string data)
                      org-reading-list-headline)))))
        (goto-char pos)
        (message "Added %s" (plist-get data :title))))))



(provide 'org-reading-list-mi)
;;; org-reading-list-mi.el ends here
