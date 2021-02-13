;;; org-volume-book.el --- Book search API -*- lexical-binding: t -*-

(require 'org-volume-utils)
(require 'org)
(require 'ol)

;;;; Google Books for searching books

(defun org-volume-google-books (params)
  "Insert the response of Google Books into an Org dynamic block."
  (let ((query (org-link-display-format (org-get-heading t t t t)))
        (lang (or (plist-get params :lang) "en"))
        (post-filter (plist-get params :filter))
        (endpoint "https://www.googleapis.com/books/v1/volumes"))
    (request endpoint
      :params `((q . ,query)
                (langRestrict . ,lang))
      :sync t
      :error (lambda () (message "Error querying %s" query))
      :parser #'org-volume--parse-json-1
      :complete
      (cl-function
       (lambda (&key data &allow-other-keys)
         (dolist (item (alist-get 'items data))
           (let ((start (point)))
             (org-volume--google-books-insert-item item)
             (when (and post-filter
                        (not (string-match-p (regexp-quote post-filter)
                                             (buffer-substring-no-properties start (point)))))
               (delete-region start (point)))))
         (backward-delete-char 2))))))

(defun org-volume--google-books-insert-item (item)
  "Insert information on an item."
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
       ;; TODO: Use something else to prevent the message "Replace N occurrences"" "
       (replace-string "\\n" "\n")
       (org-volume--parse-bibtex)))
    (x (error "Parse failure: " x))))

(provide 'org-volume-book)
;;; org-volume-book.el ends here
