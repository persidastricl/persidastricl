;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   counted.lisp
;;;
;;; CLOS class for tracking a count of items
;;;
;;; -----

(in-package #:persidastricl)

(defclass counted ()
  ((count :type integer :initarg :count :reader :count))
  (:default-initargs :count 0))

(defgeneric count (thing)
  (:method (thing) (cl:length thing))
  (:method ((thing counted)) (:count thing)))

(defgeneric length (thing)
  (:method (thing) (cl:length thing))
  (:method ((thing counted)) (:count thing)))

;; -----
;;  redefine/override util version to include 'counted' and other 'count'ed objects based on the count method
;;
;; -----

(defun empty? (x)
  (typecase x
    (null t)
    (array (zerop (length x)))
    (counted (= (count x) 0))
    (hash-set (= (count x) 0))
    (hash-map (= (count x) 0))
    (otherwise nil)))

;; TODO: for hash-maps and hash-sets, add some defmethod that checks if we have any data by looking at the size of the root nodes data vectors rather than counting the entire DS.
