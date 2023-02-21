;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   methods.lisp
;;;
;;; generic functions
;;;
;;; -----

(in-package #:persidastricl)

(defgeneric persistent! (obj)
  (:method  (obj) (let ((new-object-type (transient->persistent-name obj)))
                    (change-class obj new-object-type))))

(defmethod empty? ((obj hamt))
  (empty? (:root obj)))

(defmethod empty? ((obj bpvt))
  (zerop (count obj)))

(defmethod take (n (vector vector))
  (take* n vector))
