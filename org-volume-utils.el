;;; org-volume-utils.el --- Utilities for org-volume.el -*- lexical-binding: t -*-

(require 'dash)
(require 'request)
(require 'json)

(declare-function eww-decode-url-file-name "eww")
(declare-function bibtex-parse-entry "bibtex")

;;;; General parsing utilities

(defun org-volume--parse-json-1 ()
  "Parse JSON in the buffer with sensible output defaults."
  (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil))

(defun org-volume--parse-bibtex ()
  "Parse the BibTeX entry in the buffer."
  (goto-char (point-min))
  (cl-labels ((parse-tag
               (raw)
               (save-match-data
                 (cond
                  ((string-match (rx bol "{" (group (+ anything)) "}" eol) raw)
                   (pcase (->> (split-string (match-string 1 raw) ",")
                               (-map #'string-trim))
                     (`(,single) single)
                     (xs xs)))
                  (t
                   raw)))))
    (-map (pcase-lambda (`(,key . ,value))
            (cons key (parse-tag value)))
          (bibtex-parse-entry))))

;;;; Request helpers

(cl-defun org-volume--duckduckgo-redirect (title &key inurl parser callback)
  "Find a web page using DuckDuckGo and run a function on its content.

FIXME: Docstring"
  (declare (indent 1))
  (request "https://duckduckgo.com"
    :params `((q . ,(concat "\\" title " inurl:" inurl)))
    :sync t
    :error (lambda () (error "Error querying at duckduckgo.com"))
    :parser
    (lambda ()
      (goto-char (point-min))
      (search-forward "uddg=")
      (let ((start (point)))
        (re-search-forward (rx (+ (not (any "'")))))
        (eww-decode-url-file-name (buffer-substring start (point)))))
    :complete
    (cl-function
     (lambda (&key data response &allow-other-keys)
       (if data
           (request data
             :error (lambda () (error "Error retrieving the web page of a navigated site: %s"
                                      data))
             :sync t
             :parser parser
             :complete callback)
         (error "DuckDuckGo failed: %s" (request-response-error-thrown response)))))))

;;;; Data conversion

(provide 'org-volume-utils)
;;; org-volume-utils.el ends here
