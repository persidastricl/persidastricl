;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   functions.lisp
;;;
;;; -----

(in-package #:persidastricl)

(proclaim '(inline nil? fnil))

(defun nil? (x)
  (null x))

(defun fnil (f default)
  (lambda (x &rest args)
    (apply f (or x default) args)))

(defun ->fillable (array)
  "non-destructively retuan a fillable array copy of a non-fillable array"
  (let ((n (length array)))
    (make-array n :fill-pointer n :initial-contents array)))

(defun flatten (x)
  "return list of all leaf nodes in x"
  (labels ((flatten* (x acc)
             (cond ((null x) acc)
                   ((cl:atom x) (cons x acc))
                   (t (flatten* (first x) (flatten* (rest x) acc))))))
    (flatten* x nil)))
