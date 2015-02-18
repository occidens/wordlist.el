;;; wordlist.el --- Fetch word lists from the web  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  William West

;; Author: William West <occidens@gmail.com>
;; Maintainer: William West <occidens@gmail.com>
;; Created: 5 Feb 2015
;; Keywords: tools

;; This program is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tools to fetch word lists from the web. Currently, SCOWL
;; (http://wordlist.aspell.net/) is supported out of the box.

;;; Code:
(require 'url)
(require 'url-http)

(defvar wordlist-url
  "http://app.aspell.net/create?max_size=70&spelling=US&max_variant=3&diacritic=both&special=hacker&special=roman-numerals&download=wordlist&encoding=utf-8&format=inline"
  "Url from which to fetch wordlist")

(defvar wordlist-use-cached t
  "When non-nil, unconditionally use the cached version of the
word list, if it exists. Otherwise, use cached version if the
`url' package would.")

;http://app.aspell.net/create?max_size=60&spelling=US&max_variant=0&diacritic=strip&special=hacker&special=roman-numerals&download=wordlist&encoding=utf-8&format=inline
(defconst wordlist-sources
  `((scowl
     :BASEURL "http://app.aspell.net/create"
     :max_size    (10
		   20 35	   ;small
		   40 50	   ;medium
		   55 60 70 ;large
		   80	   ;huge
		   95)	   ;insane
     :spelling    (US	   ;en_US
		   GBs	   ;en_GB-ise
		   GBz	   ;en_GB-ize
		   CA)	   ;en_CA
     :max_variant (0	   ;none
		   1	   ;common
		   2	   ;acceptable
		   3)	   ;seldom-used
     :diacritic   (strip	   ;cafe
		   keep	   ;cafee
		   both)	   ;cafe and cafee
     :special     (hacker roman-numeral)
     :download    (wordlist)
     :encoding    (utf-8 iso-8859-1)
     :format      (inline)))
  "Definition of sources for word lists")

(defun wordlist-definition (id)
  (cdr (assq id wordlist-sources)))

(defvar wordlist-defs
  `((scowl1
     :SOURCE      scowl
     :max_size    70
     :spelling    US
     :max_variant 3
     :diacritic   both
     :special     hacker
     :download    wordlist
     :encoding    utf-8
     :format      inline)
    (scowl2
     :SOURCE      scowl
     :max_size    20
     :spelling    CA
     :max_variant 2
     :diacritic   strip
     :special     hacker
     :download    wordlist
     :encoding    utf-8
     :format      inline))
  "List of word lists")

(defun wordlist-url (wordlist)
  "Return the URL for WORDLIST based on its specification"
  (let* ((query (cdr (assq wordlist wordlist-defs)))
	 (source (plist-get query :SOURCE))
	 (source-spec (cdr (assq source wordlist-sources)))
	 (baseurl (plist-get source-spec :BASEURL)))
    (concat baseurl "?"
	    (wordlist--build-query
	     (wordlist--normalize-query query source-spec)))))

(defun wordlist--build-query (params)
  (mapconcat
   (let ((first t))
     (lambda (elem)
       (prog1
	   (cond ((keywordp elem)
		  (concat (unless first "&")
			  (substring (symbol-name elem) 1) "="))
		 (t
		  (concat (url-hexify-string (format "%s" elem)))))
	 (setq first nil))))
   params ""))

(defun wordlist--normalize-query (query spec)
  "Check QUERY against and order terms according to SPEC."
  (let ((spec-tail spec)
	key valid-values value norm-query)
    (while
	;; Loop through all key-value pairs in SPEC
	(progn
	  (setq key (car spec-tail)
		valid-values (cadr spec-tail)
		spec-tail (nthcdr 2 spec-tail))
	  ;; Re-initialize the spec-tail
	  (let ((query-tail query))
	    ;; Find multiple instances of KEY in QUERY
	    (while (setq query-tail (plist-member query-tail key))
	      (setq value              ;N.B. `plist-member' returns
		    (cadr query-tail)  ;spec-tail of plist from first
				       ;matching key. Then...
		    query-tail	       ;...once we are done, get
		    (cddr query-tail)) ;remaining plist spec after
				       ;value
	      ;; If value is valid...
	      (when (cond ((consp valid-values)
			   (memq value valid-values))
			  (t (eq value valid-values)))
		;; Append it to normalized query
		(setq norm-query (append norm-query (list key value))))))
	    spec-tail))
    norm-query))

(defun wordlist--url-retrieve (url)
  "Return cached version of the wordlist at URL if it exists; otherwise retrieve"
  (let ((url-automatic-caching t)
	(cachefile (funcall url-cache-creation-function url)))
    (if (and wordlist-use-cached (file-exists-p cachefile))
	(url-fetch-from-cache url)
      (url-retrieve-synchronously wordlist-url))))

(defun wordlist-retrieve (wordlist)
  (wordlist--url-retrieve (wordlist-url wordlist)))

(defun wordlist-get (wordlist)
  (let ((buf nil)
	(lines nil)
	(ln 0))
    (with-current-buffer
	(setq buf (wordlist-retrieve wordlist))
      (set-buffer-multibyte t)
      (beginning-of-buffer)
      (search-forward "---\n")
      (while (not (eobp))
	(let* ((beg (point))
	       (end (1- (line-end-position))))
	  (push
	   (buffer-substring-no-properties beg end) lines))
	(forward-line)
	(setq ln (1+ ln))))
    (kill-buffer buf)
    lines))

(provide 'wordlist)
;;; wordlist.el ends here
