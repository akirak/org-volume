;;; org-volume.el --- Retrieve metadata of volumes from inside Org -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (dash "2.13") (request "0.3.2") (org-download "0.1") (f "0.20"))
;; Keywords: outlines hypermedia
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

;; This package provides Org dynamic blocks for metadata of volumes of
;; medium such as books.
;;
;; It retrieves information from web sites and APIs and insert it into
;; a block in Org. It builds a search query from the current Org
;; entry.
;;
;; It also provides a command for updating the current Org entry from
;; the retrieved information. For example, it can update the heading
;; to match the accurate title of the volume, add a tag to denote the
;; format, and set entry properties which will be convenient for
;; scripting.
;;
;; Out of the box it supports retrieving information of books using
;; Google Books API v1.
;;
;; It is designed to be extensible, so you can add custom media types
;; and metadata providers. For example, it would be possible to add
;; providers for video, music, etc.

;;; Code:

(require 'org-volume-publishers)
(require 'org-volume-book)
(require 'org-volume-utils)

(require 'dash)
(require 'org)
(require 'ol)
(require 'request)
(require 'f)

(defvar org-download-image-dir)
(defvar org-download-heading-lvl)
(declare-function org-download-image "ext:org-download")

(defgroup org-volume nil
  "Metadata management for Org."
  :group 'org)

;;;; Custom variables

(defcustom org-volume-site-alist
  '((book . org-volume-google-books))
  "Alist of metadata provider sites.

This is a list of (SYMBOL . FUNCTION) where SYMBOL a unique
identifier for the site and FUNCTION is a function that takes a
plist and generates a dynamic block content.

In a volume dynamic block, you can choose a site by specifying
:site parameter. By default, a site specified as
`org-volume-default-site' is chosen."
  :type '(alist :key-type symbol
                :value-type function))

(defcustom org-volume-default-site 'book
  "Default site in volume dynamic blocks.

This must be a symbol that is the key of one of the items in
`org-volume-site-alist'.

By setting this variable locally, for example, you can change the
default site by context."
  :type 'symbol)

(defcustom org-volume-dblock-defaults
  (list :lang "en")
  "Default parameters for the \"volume\" dynamic block."
  :type 'plist)

(defcustom org-volume-toc-dblock-defaults nil
  "Default parameters for the \"volume-toc\" dynamic block."
  :type 'plist)

(defcustom org-volume-image-download-method
  (when (require 'org-download nil t)
    'org-download)
  "Method used to download thumbnail images from URLs."
  :type '(choice (const :tag "Use org-download package" org-download)
                 (const :tag "Don't download images" nil)))

;;;; Dynamic blocks

;;;###autoload
(defun org-dblock-write:volume (params)
  "Generate content for the \"volume\" dynamic block.

This calls one of the functions in `org-volume-site-alist' based
on :site parameter of the block or `org-volume-default-site'.

The callee function should generate a description list of the
title of the entry.

PARAMS is a plist, as in other dynamic block definitions."
  (let ((params (org-combine-plists org-volume-dblock-defaults params))
        (site-id (or (plist-get params :site) org-volume-default-site)))
    (funcall (or (alist-get site-id org-volume-site-alist)
                 (error "Cannot find a site %s in org-volume-site-alist" site-id))
             (append params
                     ;; Add default parameters from the current context.
                     ;; They are overridden by explicit parameters of the block.
                     (list :q (org-link-display-format
                               (org-get-heading t t t t)))))))

;;;###autoload
(defun org-dblock-write:volume-toc (params)
  "Generate content for the \"volume-toc\" dynamic block.

PARAMS is a plist, as in other dynamic block definitions."
  (let* ((params (org-combine-plists org-volume-toc-dblock-defaults params))
         (title (or (plist-get params :title)
                    (org-entry-get nil "VOLUME_TITLE")))
         (publisher (or (plist-get params :publisher)
                        (org-entry-get nil "VOLUME_PUBLISHER")))
         (isbn (or (plist-get params :isbn)
                   (org-entry-get nil "ISBN_13"))))
    (unless isbn
      (user-error "The entry needs to be set a ISBN_13 property"))
    (if publisher
        (org-volume--insert-org-toc
         :publisher publisher
         :title title)
      (org-volume--ottobib isbn
        (cl-function
         (lambda (&key data &allow-other-keys)
           (if-let (publisher (cdr (assoc "publisher" data)))
               (org-volume--insert-org-toc
                :publisher publisher
                :title title)
             (error "No publisher data is available for %s" isbn))))))))

;;;; Convenient commands for the user

;;;###autoload
(defun org-volume-update-entry-from-dblock ()
  "Update the current Org entry from the \"volume\" block at point."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
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
          (when org-volume-use-org-download
            (unless (f-directory-p org-download-image-dir)
              (make-directory org-download-image-dir)))
          (when-let (title (getprop "Title"))
            (org-entry-put nil "VOLUME_TITLE" title)
            (if-let (subtitle (getprop "Subtitle"))
                (org-edit-headline (concat title " - " subtitle))
              (org-edit-headline title)))
          (-some->> (getprop "Publisher")
            (org-entry-put nil "VOLUME_PUBLISHER"))
          (-some->> (getprop "Isbn13")
            (org-entry-put nil "ISBN_13"))
          (when-let (type (getprop "Print Type"))
            (save-excursion
              (org-back-to-heading)
              (let ((tags (org-get-tags nil t)))
                (org-set-tags (-uniq (cons (downcase type) tags))))))
          (when-let (thumbnail (getprop "Thumbnail"))
            (cl-case org-volume-image-download-method
              (org-download
               (let ((org-download-image-dir (f-join (f-dirname (buffer-file-name)) "image/"))
                     (org-download-heading-lvl nil)
                     (pos (point)))
                 (save-excursion
                   (org-back-to-heading)
                   (if (re-search-forward (rx bol "#+DOWNLOADED:") pos t)
                       (delete-region (line-beginning-position) pos)
                     (goto-char pos))
                   (org-download-image thumbnail)))))))))))

;;;; Internal functions

(cl-defun org-volume--insert-org-toc (&key publisher title)
  "Insert the table of contents in Org.

The data is retrieved from a remote site.

PUBLISHER is the name of a publisher, and TITLE is the
title. They are passed to
`org-volume--fetch-details-from-publisher', which see."
  (org-volume--fetch-details-from-publisher
   publisher title
   (cl-function
    (lambda (&key data response &allow-other-keys)
      (insert "For details, visit " (request-response-url response) "\n\n")
      (if-let (toc (plist-get data :toc))
          (insert (org-volume--toc-to-org-list toc))
        (error "TOC was not returned"))))))

(defun org-volume--toc-to-org-list (list)
  "Transform a table of contents to an Org string.

LIST represents the table of contents. The output will be an
ordered list of Org."
  (cl-labels
      ((go
        (level x)
        (cl-etypecase x
          (string (replace-regexp-in-string
                   (char-to-string #o240) " "
                   (concat (apply #'concat (-repeat level " ")) "+ " x "\n")))
          (list (mapconcat (-partial #'go (1+ level)) x "")))))
    (go -1 list)))

(provide 'org-volume)
;;; org-volume.el ends here
