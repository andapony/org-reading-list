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

(provide 'org-reading-list-mi)
;;; org-reading-list-mi.el ends here
