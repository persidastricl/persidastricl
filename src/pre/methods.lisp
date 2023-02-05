;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   methods.lisp
;;;
;;; generic functions
;;;
;;; -----

(in-package #:persidastricl)

(defgeneric cons (se1 se2)
  (:method (se1 se2) (cl:cons se1 se2)))

(defgeneric first (thing)
  (:method (thing) (cl:first thing)))

(defgeneric rest (thing)
  (:method (thing) (cl:rest thing)))

(defgeneric last (thing)
  (:method (thing) (cl:last thing)))

(defgeneric butlast (thing &optional n)
  (:method (thing &optional (n 1)) (cl:butlast thing n)))

(defgeneric empty (object)
  (:documentation "return an empty data-object of the same type as the original object argument")
  (:method ((object t)) (make-instance (type-of object))))

(defgeneric ->array (object))
(defgeneric ->vector (object))
(defgeneric ->vec (object))

(defgeneric ->list (object)
  (:method ((obj (eql nil))) nil))

(defgeneric ->pist (object))
(defgeneric ->alist (object))
