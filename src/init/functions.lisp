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

;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   functions.lisp
;;;
;;; -----

(in-package #:persidastricl)

(proclaim '(inline nil? pos? neg? zero? even? odd? int? pos-int? neg-int? nat-int? inc dec fnil))

(defun nil? (x)
  (null x))

(defun false? (x)
  (null x))

(defun true? (x)
  (not (false? x)))

(defun pos? (n)
  (> n 0))

(defun neg? (n)
  (< n 0))

(defun zero? (n)
  (zerop n))

(defun even? (n)
  (evenp n))

(defun odd? (n)
  (oddp n))

(defun int? (n)
  (integerp n))

(defun pos-int? (n)
  (and (int? n)
       (pos? n)))

(defun neg-int? (n)
  (and (int? n)
       (neg? n)))

(defun nat-int? (n)
  (and (int? n)
       (not (neg? n))))

(defun string? (s)
  (stringp s))

(defun identical? (x y)
  (eq x y))

(defun dotted-pair? (x)
  (and (consp x)
       (not (listp (cdr x)))))

(defun inc (n)
  (1+ n))

(defun dec (n)
  (1- n))

(defun quot (x y)
  (truncate x y))

(defun fnil (f default)
  (lambda (x &rest args)
    (apply f (or x default) args)))

(defun ->fillable (array)
  "non-destructively retuan a copy of a non-fillable array as a fillable array "
  (let ((n (cl:length array)))
    (make-array n :fill-pointer n :initial-contents array)))

(defun flatten (x)
  "return list of all leaf nodes in x"
  (labels ((flatten* (x acc)
             (cond ((null x) acc)
                   ((cl:atom x) (cons x acc))
                   (t (flatten* (first x) (flatten* (rest x) acc))))))
    (flatten* x nil)))

(defun instance? (type object)
  (typep object type))

(defun some? (x)
  (not (nil? x)))

(defmacro alias (name fn)
  `(setf (fdefinition ',name) #',fn))

(defgeneric to-string (obj)
  (:method (obj) (format nil "~s" (or obj "")))
  (:method ((obj (eql nil))) "")
  (:method ((obj character)) (format nil "~a" (or obj "")))
  (:method ((s string)) s))

(defun str (&rest things)
  "take a list and concatenate the elements into a string"
  (cl:reduce
   #'(lambda (r &optional s)
       (concatenate 'string r (to-string s)))
   things
   :initial-value ""))

(defun subs (s start &optional (end (cl:length s)))
  (subseq s start end))

(defun comp (&rest functions)
  (let ((fns (reverse functions)))
    (lambda (&rest args)
      (reduce
       (lambda (current f)
         (funcall f current))
       (rest fns)
       :initial-value (apply (first fns) args)))))

(defun partial (f &rest initial-args)
  (lambda (&rest more-args)
    (apply f (append initial-args more-args))))

(defun keyword (s)
  (if (keywordp s) s
      (let* ((s (str s))
             (s (cond
                  ((every (complement #'upper-case-p) s) (string-upcase s))
                  ((every (complement #'lower-case-p) s) (string-downcase s))
                  (t s))))
        (intern s "KEYWORD"))))

(defun name (sym)
  (let ((s (symbol-name sym)))
    (cond ((every (complement #'upper-case-p) s) (string-upcase s))
          ((every (complement #'lower-case-p) s) (string-downcase s))
          (t s))))
