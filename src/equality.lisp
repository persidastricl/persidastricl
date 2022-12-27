;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; equality.lisp
;;;
;;; -----

(in-package :persidastricl)

;; -----
;;  there is no defined notion of equality for CLOS objects in LISP
;;  so here, we define what we need ourselves
;;

(defun slot-values (obj)
  (let ((_class (class-of obj)))
    (map
     'list
     (lambda (slot-def)
       (sb-mop:slot-value-using-class _class obj slot-def))
     (sb-mop:class-slots _class))))

(defgeneric == (obj1 obj2))

(defmethod == (obj1 obj2)
  (equalp obj1 obj2))

;; -----
;; equality of CLOS objects is defined here as:
;;
;;  objects are 'equal'
;;  IF
;;     they are the same type of object
;;  AND
;;    they are EITHER
;;      indeed the same object (at the same address)
;;      OR
;;      every slot defined for this class has `==` values
;;

(defmethod == ((obj1 standard-object) (obj2 standard-object))
  (and (equalp (class-of obj1) (class-of obj2))
       (or (eq obj1 obj2)
           (every
            (lambda (v1 v2)
              (== v1 v2))
            (slot-values obj1)
            (slot-values obj2)))))

(defmethod == ((v1 simple-vector) (v2 simple-vector))
  (or (eq v1 v2)
      (and (== (length v1) (length v2))
           (every
            (lambda (e1 e2)
              (== e1 e2))
            v1
            v2))))
