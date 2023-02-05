;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   util.lisp
;;;
;;; -----

(in-package #:persidastricl)

(proclaim '(inline last-one single-p append-one conc-one ensure-list))

(defun emptyp (sequence)
  (typecase sequence
    (null t)
    (array (zerop (length sequence)))
    (otherwise nil)))

(defmacro while (condition &rest body)
  `(loop while ,condition
         do (progn ,@body)))

(defmacro if-not (pred truthy &optional falsey)
  `(if (not ,pred) ,truthy ,falsey ))

(defmacro when-not (pred &body body)
  `(when (not ,pred)
     ,@body))

(defmacro if-let ((var test-form) then-form &optional else-form)
  `(let ((,var ,test-form))
     (if ,var ,then-form ,else-form)))

(defmacro when-let ((var test-form) &body body)
  `(let ((,var ,test-form))
     (when ,var
       ,@body)))

(defun partition (source n)
  "return list with source divided into lists of n items plus final list (< n) of remainders"
  (when (zerop n) (error "partition is zero length"))
  (labels ((rec (source acc)
             (let ((more (nthcdr n source)))
               (if (consp more)
                   (rec more (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (when source (rec source nil))))

(defun flatten (x)
  "return list of all leaf nodes in x"
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (first x) (rec (rest x) acc))))))
    (rec x nil)))

(defun find-first (pred lst)
  "return the values of first occurence of element in list that satisfies pred, and its result"
  (when lst
    (let ((result (funcall pred (first lst))))
      (if result
          (values (first lst) result)
          (find-first pred (rest lst))))))
