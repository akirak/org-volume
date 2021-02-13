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

(defun org-dblock-write:volume-toc (params)
  (let ((title (org-entry-get nil "VOLUME_TITLE"))
        (publisher (org-entry-get nil "VOLUME_PUBLISHER"))
        (isbn (org-entry-get nil "ISBN_13")))
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
          (let ((org-download-image-dir (f-join (f-dirname (buffer-file-name)) "image/"))
                (org-download-heading-lvl nil)
                (pos (point)))
            (save-excursion
              (org-back-to-heading)
              (if (re-search-forward (rx bol "#+DOWNLOADED:") pos t)
                  (delete-region (line-beginning-position) pos)
                (goto-char pos))
              (org-download-image thumbnail))))))))

;;;; Internal functions

(cl-defun org-volume--insert-org-toc (&key publisher title)
  (org-volume--fetch-details-from-publisher
   publisher title
   (cl-function
    (lambda (&key data response &allow-other-keys)
      (insert "For details, visit " (request-response-url response) "\n\n")
      (if-let (toc (plist-get data :toc))
          (insert (org-volume--toc-to-org-list toc))
        (error "TOC was not returned"))))))

(defun org-volume--toc-to-org-list (list)
  (cl-labels
      ((go
        (level x)
        (cl-etypecase x
          (string (replace-regexp-in-string
                   (char-to-string #o240) " "
                   (concat (apply #'concat (-repeat level " ")) "+ " x "\n")))
          (list (mapconcat (-partial #'go (1+ level)) x "")))))
    (go -1 list)))

(provide 'org-librarian-block)
;;; org-librarian-block.el ends here
