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

(def WHITESPACE '(#\TAB
                  #\Vt
                  #\SPACE
                  #\LINEFEED
                  #\RETURN
                  #\NEWLINE
                  #\PAGE
                  #\Next-Line
                  #\NO-BREAK_SPACE
                  #\EN_SPACE
                  #\EN_QUAD
                  #\FORMFEED
                  #\EM_SPACE
                  #\EM_QUAD
                  #\OGHAM_SPACE_MARK
                  #\THREE-PER-EM_SPACE
                  #\FOUR-PER-EM_SPACE
                  #\SIX-PER-EM_SPACE
                  #\FIGURE_SPACE
                  #\PUNCTUATION_SPACE
                  #\THIN_SPACE
                  #\HAIR_SPACE
                  #\LINE_SEPARATOR
                  #\PARAGRAPH_SEPARATOR
                  #\NARROW_NO-BREAK_SPACE
                  #\MEDIUM_MATHEMATICAL_SPACE
                  #\IDEOGRAPHIC_SPACE))

(let ((wss (into #{} WHITESPACE)))
  (labels ((is-whitespace? (ch)
             (contains? wss ch)))
    (defun blank? (s)
      (or (nil? s)
          (empty? s)
          (every? #'is-whitespace? s)))))

(defun capitalize (s)
  (assert (string? s))
  (string-capitalize s))

(defun join (delimeter-or-strings &optional (strings nil strings-p))
  "join strings by interposing delimeter between them"
  (if (not strings-p)
      (apply #'str (->list delimeter-or-strings))
      (let ((join-format (format nil "~~{~~a~~^~a~~}" delimeter-or-strings)))
        (format nil join-format (->list strings)))))

(defun condense (s)
  "remove redundant whitespace within a string s"
  (assert (string? s))
  (join " " (cl-ppcre:all-matches-as-strings "\\S+" s)))

(defun ends-with? (s substr)
  (assert (string? s))
  (true? (cl-ppcre:scan (str substr "$") s)))

(defun escape (ss cmap)
  (reduce
   (lambda (s ch)
     (str s (get cmap ch ch)))
   ss
   :initial-value ""))

(defun includes? (s pattern)
  (assert (string? s))
  (true? (cl-ppcre:scan-to-strings pattern s)))

(defun index-of (s value &optional (from 0))
  (assert (string? s))
  (cl-ppcre:scan (str value) s :start (max 0 from)))

(defun last-index-of (s value &optional (from (length s)))
  (assert (string? s))
  (let ((from (if (neg? from) (max 0 from) (min from (length s)))))
    (search (str value) s :from-end t :end2 from)))

(defun lower-case (s)
  (assert (string? s))
  (string-downcase s))

(defun re-quote-replacement (s)
  (assert (string? s))
  (cl-ppcre:quote-meta-chars s))

(labels ((re-groups* (match gs ge s)
           (let* ((n (length gs))
                  (groups (loop :for i :below n
                                :collect (subs s (elt gs i) (elt ge i)))))
             (if (zero? (count groups))
                 match
                 (into [match] groups)))))

  (defun replace-first (s pattern replacement)
    (assert (string? s))
    (labels ((replace-first-by (s re f)
               (let ((scanner (cl-ppcre:create-scanner re)))
                 (let* ((lst (multiple-value-list (cl-ppcre:scan scanner s :start 0)))
                        (pos (first lst)))
                   (if pos
                       (let* ((end (second lst))
                              (gs (third lst))
                              (ge (fourth lst))
                              (match (subs s pos end))
                              (rep (funcall f (re-groups* match gs ge s))))
                         (str (subs s 0 pos) rep (subs s end)))
                       s)))))
      (cond
        ((instance? 'character pattern) (replace-first s (str pattern) replacement))
        ((instance? 'character replacement) (replace-first s pattern (str replacement)))
        ((instance? 'string replacement) (cl-ppcre:regex-replace pattern s replacement))
        ((functionp replacement) (replace-first-by s pattern replacement))
        (t s))))

  (defun replace (s pattern replacement)
    (assert (string? s))
    (labels ((replace-by (s re f)
               (let ((scanner (cl-ppcre:create-scanner re)))
                 (labels ((replace* (start result)
                            (let* ((lst (multiple-value-list (cl-ppcre:scan scanner s :start start)))
                                   (pos (first lst)))
                              (if pos
                                  (let* ((next (second lst))
                                         (gs (third lst))
                                         (ge (fourth lst))
                                         (match (subs s pos next)))
                                    (replace* next (str result (subs s start pos) (funcall f (re-groups* match gs ge s)))))
                                  (str result (subs s start))))))
                   (replace* 0 "")))))
      (cond
        ((instance? 'character pattern) (replace s (str pattern) replacement))
        ((instance? 'character replacement) (replace s pattern (str replacement)))
        ((instance? 'string replacement) (cl-ppcre:regex-replace-all pattern s replacement))
        ((functionp replacement) (replace-by s pattern replacement))
        (t s)))))

(defun reverse (s)
  (assert (string? s))
  (cl:reverse s))

(defun split (s re &optional limit)
  (assert (string? s))
  (if limit
      (cl-ppcre:split re s :limit limit)
      (cl-ppcre:split re s)))

(defun split-lines (s)
  (assert (string? s))
  (cl-ppcre:split "\\r?\\n" (trim s)))

(defun starts-with? (s substr)
  (assert (string? s))
  (true? (cl-ppcre:scan (str "^" substr) s)))

(defun trim (s)
  "remave all whitespace from the ends of the string s"
  (assert (string? s))
  (string-trim WHITESPACE s))

(defun triml (s)
  "remave all whitespace from the left end of the string s"
  (assert (string? s))
  (string-left-trim WHITESPACE s))

(defun trimr (s)
  "remave all whitespace from the right end of the string s"
  (assert (string? s))
  (string-right-trim WHITESPACE s))

(defun trim-newline (s)
  (assert (string? s))
  (replace s "[\\n\\r]+$" ""))

(defun upper-case (s)
  (assert (string? s))
  (string-upcase s))
