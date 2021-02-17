;;; org-volume-book.el --- Book search API -*- lexical-binding: t -*-

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

;; This library provides support for book search for org-volume.

;;; Code:

(require 'request)

(require 'org-volume-utils)

(declare-function xml-parse-tag "xml")

;;;; Google Books for searching books

(defconst org-volume-google-books-v1-api-endpoint
  "https://www.googleapis.com/books/v1/volumes")

(defun org-volume-google-books (params)
  "Google Books provider for the \"volume\" dynamic block.

This function retrieves information on the title from Google
Books API v1 and insert the metadata into the buffer in an Org
format.

PARAMS is a plist. It supports the following parameters:

* `:q': An unescaped search query. This is usually retrieved from
  the current Org heading.

* `:lang': Language of the volumes, e.g. \"en\".

* `:filter': Explicit text filter. Google Books can return a lot
  of unintended items, so you can use this text to filter the
  result based the actual Org output."
  (let ((query (plist-get params :q))
        (lang (plist-get params :lang))
        (limit (plist-get params :limit))
        (post-filter (plist-get params :filter))
        (endpoint org-volume-google-books-v1-api-endpoint))
    (request endpoint
      :params `((q . ,query)
                (langRestrict . ,lang))
      :sync t
      :error (lambda () (message "Error querying %s at Google Books v1" query))
      :parser #'org-volume--parse-json-1
      :complete
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let ((items (alist-get 'items data)))
           (dolist (item (pcase limit
                           (1 (list (org-volume--completing-read-with-format
                                     (format "Select a book for \"%s\": " query)
                                     items #'org-volume--google-book-format)))
                           ((pred numberp) (-take limit items))
                           (_ items)))
             (let ((start (point)))
               (org-volume--google-books-insert-item item)
               (when (and post-filter
                          (not (string-match-p (regexp-quote post-filter)
                                               (buffer-substring-no-properties start (point)))))
                 (delete-region start (point))))))
         (backward-delete-char 2))))))

(defun org-volume--google-book-format (item)
  "Format ITEM for display in `completing-read'."
  (let-alist (alist-get 'volumeInfo item)
    (format "%s from %s by %s"
            \.title
            (or \.publisher "nil")
            (string-join \.authors ", "))))

(defun org-volume--google-books-insert-item (item)
  "Insert information on an ITEM."
  (let* ((v (alist-get 'volumeInfo item))
         (imageLinks (alist-get 'imageLinks v))
         (identifiers (alist-get 'industryIdentifiers v))
         (isbn13 (-some->> identifiers
                   (-find (lambda (al) (string= "ISBN_13" (alist-get 'type al))))
                   (alist-get 'identifier)))
         (isbn10 (-some->> identifiers
                   (-find (lambda (al) (string= "ISBN_10" (alist-get 'type al))))
                   (alist-get 'identifier))))
    (cl-labels
        ((dl (key value) (when value (insert (format "- %s :: %s\n"
                                                     (->> (format "%s" key)
                                                          (replace-regexp-in-string "-" " ")
                                                          (capitalize))
                                                     value)))))
      (let-alist v
        (dl 'title .title)
        (dl 'subtitle .subtitle)
        (dl 'authors (string-join \.authors " / "))
        (dl 'publisher \.publisher)
        (dl 'published-date \.publishedDate)
        (dl 'pages \.pageCount)
        (dl 'language \.language)
        (dl 'categories (string-join \.categories ", "))
        (dl 'print-type \.printType)
        (dl 'isbn13 isbn13)
        (dl 'isbn10 isbn10)
        (dl 'thumbnail (or (alist-get 'smallThumbnail imageLinks)
                           (alist-get 'thumbnail imageLinks)))
        (insert (if \.description
                    (concat "\n" \.description "\n")
                  ""))))
    (insert "\n")))

;;;; OttoBib for ISBN search

(defun org-volume--ottobib (isbn callback)
  "Retrieve information on a volume specified by an isbn code.

This function query information on ISBN from OttoBib, and then
run CALLBACK on the response data.

The response data is converted from BibTeX to an alist."
  (declare (indent 1))
  (let ((endpoint (format "https://www.ottobib.com/isbn/%s/bibtex" isbn)))
    (request endpoint
      :sync t
      :error (lambda () (message "Error fetching %s" endpoint))
      :parser #'org-volume--parse-ottobib
      :complete callback)))

(defun org-volume--parse-ottobib ()
  "Parse the web page of OttoBib."
  (goto-char (point-min))
  (search-forward "<textarea")
  (search-backward "<")
  (pcase (xml-parse-tag)
    (`(textarea ,_ ,content)
     (with-temp-buffer
       (insert content)
       (goto-char (point-min))
       (save-excursion
         (while (search-forward "\\n" nil t)
           (replace-match "\n")))
       (org-volume--parse-bibtex)))
    (x (error "Parse failure: %s" x))))

(provide 'org-volume-book)
;;; org-volume-book.el ends here
