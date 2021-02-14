;;; org-volume-publishers.el --- Scraping support for individual publishers -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/org-volume

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides support for retrieving details of books from
;; individual publishers' websites.

;;; Code:

(require 'org-volume-utils)

(require 'cl-lib)
(require 'subr-x)
(require 'dash)

(declare-function xml-parse-tag "xml")

(define-widget 'org-volume-publisher-site 'lazy
  "Specification of a publisher web site that provides details of books."
  :tag "Website"
  :type '(plist :options
                (((const :tag "Regular expression for the publisher name" :regexp)
                  regexp)
                 ((const :tag "URL prefix for individual volume pages" :details-inurl)
                  string)
                 ((const :tag "Function parsing the web page" :details-parser)
                  function)
                 ((const :tag "Does the web page contains a table of contents?" :toc)
                  boolean))))

(defcustom org-volume-publisher-site-list
  `((:regexp ,(rx bol "Pragmatic Bookshelf")
             :details-inurl "pragprog.com/titles/"
             :details-parser org-volume--pragprog-parse-details-html
             :toc t))
  "List of publishers used to retrieve details of a title.

Each item should be a plist."
  :type '(repeat org-volume-publisher-site)
  :group 'org-volume)

;;;; Entry point

(defun org-volume--fetch-details-from-publisher (publisher title callback)
  "Get details on a title from a publisher.

PUBLISHER is the name of a publisher and it should be matched by
a regular expression specified as :regexp field in the site
definition in `org-volume-publisher-site-list'.

TITLE should be part of the title. It is used as the search
query. The search process proceeds as follows:

1. A web page containing the details of the title is searched in
:details-inurl parameter of the site.

2. The web page is parsed by a function using :details-parser of
the site. It is a function that extracts data from the current
buffer.

3. CALLBACK is run on the extracted data and performs a side effect.
For example, it may insert text into a dynamic block."
  (-if-let ((&plist :details-inurl :details-parser :toc)
            (cl-some (lambda (plist)
                       (when (string-match-p (plist-get plist :regexp) publisher)
                         plist))
                     org-volume-publisher-site-list))
      (cond
       ((and details-inurl details-parser)
        (if toc
            (org-volume--duckduckgo-redirect title
              :inurl details-inurl
              :parser details-parser
              :callback callback)
          (error "Publisher %s doesn't support TOC" publisher)))
       (t
        (error "You have to specify :details-inurl and :details-parser: %s"
               publisher)))
    (error "Cannot find a publisher matching %s" publisher)))

;;;; Pragmatic Bookshelf

(defun org-volume--pragprog-parse-details-html ()
  "Extract details from a web page in Pragmatic Programmers."
  (goto-char (point-min))
  (search-forward "<a id=\"extracts\">")
  (re-search-forward (rx "</a>" (* space)))
  (list :toc (org-volume--pragprog-analyse-toc
              (xml-parse-tag))))

(defun org-volume--pragprog-analyse-toc (tree)
  "Returns a clean list structure from a TREE of html tags."
  (cl-labels
      ((handle-block
        (x)
        (pcase x
          (`nil
           nil)
          ("\n"
           nil)
          (`(ul nil "\n" . ,items)
           (-non-nil (-map #'handle-block items)))
          (`(li nil ,headline . ,items)
           (cons (handle-headline headline)
                 (-non-nil (-map #'handle-flaw items))))
          (_ (error "Mismatch in handle-block: %s" x))))
       (handle-flaw
        (x)
        (pcase x
          (`nil
           nil)
          ("\n"
           nil)
          (`(a ,_ . ,_)
           nil)
          (`(ul ,_ . ,items)
           (-flatten-n 1 (-non-nil (-map #'handle-block items))))
          ((pred stringp)
           x)
          (_ (error "Mismatch in handle-flaw: %s" x))))
       (handle-headline
        (x)
        (pcase x
          (`nil
           nil)
          (`(strong ,_ ,literal)
           (string-trim literal))
          ("\n"
           nil)
          ((pred stringp)
           (string-trim x))
          (_
           (error "Mismatch in handle-headline: %s" x)))))
    (-flatten-n 1 (-non-nil (handle-block tree)))))

(provide 'org-volume-publishers)
;;; org-volume-publishers.el ends here
