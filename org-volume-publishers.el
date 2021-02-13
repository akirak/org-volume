;;; org-volume-publishers.el --- Scraping support for individual publishers -*- lexical-binding: t -*-

(defcustom org-volume-publisher-site-list
  `((:regexp ,(rx bol "Pragmatic Bookshelf")
             :details-inurl "pragprog.com/titles/"
             :details-parser org-volume--pragprog-parse-details-html
             :toc t))
  "FIXME")

;;;; Entry point

(defun org-volume--fetch-details-from-publisher (publisher title callback)
  "Get details on a title from a publisher."
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
        (error error "You have to specify :details-inurl and :details-parser: %s"
               publisher)))
    (error "Cannot find a publisher matching %s" publisher)))

;;;; Pragmatic Bookshelf

(defun org-volume--pragprog-parse-details-html ()
  (goto-char (point-min))
  (search-forward "<a id=\"extracts\">")
  (re-search-forward (rx "</a>" (* space)))
  (list :toc (org-volume--pragprog-destructure-toc
              (xml-parse-tag))))

(defun org-volume--pragprog-destructure-toc (inp)
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
          (_ (error "Mismatch in handle-block: %s" x))
          ))
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
    (-flatten-n 1 (-non-nil (handle-block inp)))))

(provide 'org-volume-publishers)
;;; org-volume-publishers.el ends here
