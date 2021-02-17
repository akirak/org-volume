;;; org-volume-utils.el --- Utilities for org-volume.el -*- lexical-binding: t -*-

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

;; This library is a collection of miscelleneous provider-agnostic
;; utility functions for the package.

;;; Code:

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

;;;; Completing-read helper

(defun org-volume--completing-read-with-format (prompt items formatter)
  "Let the user select an item from data with a formatter.

This is a wrapper around `completing-read'.

PROMPT is a string, and ITEMS can be a list of any data.
FORMATTER is used to format each item in the candidates."
  (let ((choice (completing-read prompt
                                 (-map (lambda (x)
                                         (propertize (funcall formatter x)
                                                     'item x))
                                       items)
                                 nil t)))
    (get-char-property 0 'item choice)))

;;;; Request helpers

(defconst org-volume-duckduckgo-endpoint
  "https://duckduckgo.com")

(cl-defun org-volume--duckduckgo-redirect (title &key inurl parser callback)
  "Find a web page using DuckDuckGo and run a function on its response.

This function performs an \"I'm feeling lucky\" search for TITLE
using DuckDuckGo. The search domain is limited to INURL which
should be a URL prefix of the result web page excluding the
scheme.

PARSER is used to transform the response body, and CALLBACK is
called on the data."
  (declare (indent 1))
  (request org-volume-duckduckgo-endpoint
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
             :error (lambda ()
                      (error "Error retrieving the web page of a navigated site: %s"
                             data))
             :sync t
             :parser parser
             :complete callback)
         (error "DuckDuckGo failed: %s"
                (request-response-error-thrown response)))))))

;;;; Data conversion

(provide 'org-volume-utils)
;;; org-volume-utils.el ends here
