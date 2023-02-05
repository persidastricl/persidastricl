;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; string.lisp
;;;
;;; -----

(in-package :string)

(defgeneric to-string (obj)
  (:method (obj) (format nil "~s" (or obj "")))
  (:method ((obj (eql nil))) "")
  (:method ((s string)) s))

(defun str (&rest things)
  "take a list and concatenate the elements into a string"
  (reduce
   #'(lambda (r &optional s)
       (concatenate 'string r (to-string s)))
   things
   :initial-value ""))

(defun join (delimeter strings)
  "join strings by interposing delimeter between them"
  (let ((join-format (format nil "~~{~~a~~^~a~~}" delimeter)))
    (format nil join-format strings)))

(defun condense (s)
  "remove redundant whitespace within a string s"
  (join " " (cl-ppcre:all-matches-as-strings "\\S+" s)))

(let ((WHITESPACE '(#\TAB #\SPACE #\LINEFEED #\RETURN #\NEWLINE #\PAGE)))
  (defun trim (s)
    "remave all whitespace from the ends of the string s"
    (when s (string-trim WHITESPACE s))))

(defun blank? (s)
  (= (length (trim s)) 0))

(defun replace (s pattern replacement)
  (cl-ppcre:regex-replace pattern s replacement :preserve-case t :simple-calls t))

(defun replace-all (s pattern replacement)
  (cl-ppcre:regex-replace-all pattern s replacement :preserve-case t :simple-calls t))

(defun ->keyword (s)
  (intern (string-upcase s) "KEYWORD"))
