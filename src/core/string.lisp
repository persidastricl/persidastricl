;;; -----
;;;
;;;  Copyright (c) 2019-2023 Michael D Pendergrass, pupcus.org
;;;
;;;  This program and the accompanying materials are made
;;;  available under the terms of the Eclipse Public License 2.0
;;;  which is available at https://www.eclipse.org/legal/epl-2.0/
;;;
;;;  SPDX-License-Identifier: EPL-2.0
;;;
;;; -----

;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; string.lisp
;;;
;;; -----

(in-package :string)

(named-readtables:in-readtable persidastricl:syntax)

(defun blank? (s)
  (= (length (trim s)) 0))

(defun capitalize (s)
  (string-capitalize s))

(defun join (delimeter strings)
  "join strings by interposing delimeter between them"
  (let ((join-format (format nil "~~{~~a~~^~a~~}" delimeter)))
    (format nil join-format strings)))

(defun condense (s)
  "remove redundant whitespace within a string s"
  (join " " (cl-ppcre:all-matches-as-strings "\\S+" s)))

(defun ends-with? (s substr)
  (true? (cl-ppcre:scan (str substr "$") s)))

(defun escape (original cmap)
  (reduce
   (lambda (s ch)
     (str s (get cmap ch ch)))
   original
   :initial-value ""))

(defun includes? (s subs-or-regex)
  (true? (cl-ppcre:scan-to-strings subs-or-regex s)))

(defun index-of (s value &optional (from 0))
  (cl-ppcre:scan (str value) s :start from))

(defun last-index-of (s value &optional (from (length s)))
  (search (str value) s :from-end t :end2 from))

(defun lower-case (s)
  (string-downcase s))

(defun re-quote-replacement (s)
  (cl-ppcre:quote-meta-chars s))

(defun replace-first (s pattern replacement)
  (cl-ppcre:regex-replace pattern s replacement :preserve-case t :simple-calls t))

(defun replace (s pattern replacement)
  (cl-ppcre:regex-replace-all pattern s replacement :preserve-case t :simple-calls t))

(defun reverse (s)
  (cl:reverse s))

(defun split (s re &optional limit)
  (if limit
      (cl-ppcre:split re s :limit limit)
      (cl-ppcre:split re s)))

(defun split-lines (s)
  (cl-ppcre:split "\\r?\\n" (trim s)))

(defun starts-with? (s substr)
  (true? (cl-ppcre:scan (str "^" substr) s)))


(let ((WHITESPACE '(#\TAB #\SPACE #\LINEFEED #\RETURN #\NEWLINE #\PAGE)))
  (defun trim (s)
    "remave all whitespace from the ends of the string s"
    (when s (string-trim WHITESPACE s)))

  (defun triml (s)
    "remave all whitespace from the left end of the string s"
    (when s (string-left-trim WHITESPACE s)))

  (defun trimr (s)
    "remave all whitespace from the right end of the string s"
    (when s (string-right-trim WHITESPACE s))))

(defun trim-newline (s)
  (replace s "[\\n\\r]+$" ""))

(defun upper-case (s)
  (string-upcase s))
