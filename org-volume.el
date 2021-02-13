;;; org-librarian-block.el --- Dynamic block -*- lexical-binding: t -*-

(require 'dash)
(require 'org)
(require 'ol)
(require 'org-download)
(require 'request)

;;;; Custom variables

(defcustom org-volume-site-alist
  '((book . org-volume-google-books))
  "FIXME")

(defcustom org-volume-default-site 'book
  "Default site.

This must be a symbol that is the key of one of the items in
`org-volume-site-alist'.

By setting this variable locally, for example, you can change the
default site by context.")

;;;; Dynamic blocks

(defun org-dblock-write:volume (params)
  (let ((site-id (or (plist-get params :site) org-volume-default-site)))
    (funcall (or (alist-get site-id org-volume-site-alist)
                 (error "Cannot find a site %s in org-volume-site-alist" site-id))
             params)))

;;;; Convenient commands for the user

(defun org-volume-update-entry-from-dblock ()
  (interactive)
  (unless (looking-at org-dblock-start-re)
    (user-error "You have to start on the beginning of a dynamic block"))
  (org-save-outline-visibility t
    (org-show-entry)
    (let ((end (save-excursion
                 (re-search-forward org-dblock-end-re nil t))))
      (cl-labels
          ((getprop (key)
                    (save-excursion
                      (when (re-search-forward (concat "^" (regexp-quote (format "- %s :: " key)))
                                               nil end)
                        (buffer-substring-no-properties (point) (line-end-position))))))
        (unless (f-directory-p org-download-image-dir)
          (make-directory org-download-image-dir))
        (-some->> (getprop "Title")
          (org-edit-headline))
        (-some->> (getprop "Isbn13")
          (org-entry-put nil "ISBN_13"))
        (when-let (type (getprop "Print Type"))
          (save-excursion
            (org-back-to-heading)
            (let ((tags (org-get-tags nil t)))
              (org-set-tags (-uniq (cons (downcase type) tags))))))
        (when-let (thumbnail (getprop "Thumbnail"))
          (let ((org-download-image-dir (f-join (f-dirname (buffer-file-name)) "image/"))
                (org-download-heading-lvl nil)
                (pos (point)))
            (save-excursion
              (org-back-to-heading)
              (if (re-search-forward (rx bol "#+DOWNLOADED:") pos t)
                  (delete-region (line-beginning-position) pos)
                (goto-char pos))
              (org-download-image thumbnail))))))))

;;;; Internal utility functions

(defun org-volume--json-parse-1 ()
  (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil))

;;;; Backends

;;;;; Google Books for searching books

(defun org-volume-google-books (params)
  (let ((query (org-link-display-format (org-get-heading t t t t)))
        (lang (or (plist-get params :lang) "en"))
        (post-filter (plist-get params :filter))
        (endpoint "https://www.googleapis.com/books/v1/volumes"))
    (request endpoint
      :params `((q . ,query)
                (langRestrict . ,lang))
      :sync t
      :error (lambda () (message "Error querying %s" query))
      :parser #'org-volume--json-parse-1
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
        (dl 'title (concat \.title (if \.subtitle
                                       (concat " - " \.subtitle)
                                     "")))
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

(defun org-volume-ottobib (isbn)
  (format "https://www.ottobib.com/isbn/%s/bibtex" isbn))

(provide 'org-librarian-block)
;;; org-librarian-block.el ends here
