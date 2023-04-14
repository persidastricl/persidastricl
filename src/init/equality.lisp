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
;;; equality.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  there is no defined notion of equality for CLOS objects in LISP
;;  so here, we define what we think we need ourselves (subject to change, of course :-))
;;

(defun slot-values (obj)
  (let ((_class (class-of obj)))
    (cl:map
     'list
     (lambda (slot-def)
       (c2mop:slot-value-using-class _class obj slot-def))
     (c2mop:class-slots _class))))

(defgeneric == (obj1 obj2)
  (:method ((obj1 (eql nil)) obj2) (eql obj2 nil))
  (:method (obj1 (obj2 (eql nil))) (eql obj1 nil))
  (:method (obj1 obj2) (equalp obj1 obj2)))

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

(defmethod == ((s1 string) (s2 string))
  (string= s1 s2))

(defmethod == ((s1 character) (s2 character))
  (char= s1 s2))

(defmethod == ((v1 sequence) (v2 sequence))
  (cond
    ((and (dotted-pair? v1)
          (dotted-pair? v2)) (and (== (car v1) (car v2))
                                  (== (cdr v1) (cdr v2))))
    ((dotted-pair? v2) nil)
    ((dotted-pair? v1) nil)

    ((and (nil? v1) (nil? v2)) t)
    ((or (nil? v1) (nil? v2)) nil)

    (t (or (eq v1 v2)
           (and (== (cl:length v1) (cl:length v2))
                (every
                 (lambda (e1 e2)
                   (== e1 e2))
                 v1
                 v2))))))
