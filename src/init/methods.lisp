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
;;;   methods.lisp
;;;
;;; generic functions
;;;
;;; -----

(in-package #:persidastricl)

(defgeneric empty (object)
  (:documentation "return an empty data-object of the same type as the original object argument")
  (:method ((lst list)) '())
  (:method ((object t)) (make-instance (type-of object))))

(defgeneric empty? (thing)
  (:method ((thing (eql nil))) t)
  (:method ((a array)) (zerop (length a)))
  (:method ((l list)) (zerop (length l))))



(labels
    ((count* (thing &optional (n 0))
       (cond
         ((null thing) n)
         ((dotted-pair? thing) (+ n 2))
         ((null (seq thing)) n)
         (:otherwise (count* (rest thing) (1+ n))))))
  (defgeneric count (thing)
    (:method (thing) (count* thing))
    (:method ((ht hash-table)) (hash-table-count ht))))

(defgeneric length (thing)
  (:method (thing) (cl:length thing))
  (:method ((ht hash-table)) (hash-table-count ht)))

(defgeneric bounded-count (n thing))

(defgeneric cons (se1 se2)
  (:method (se1 se2) (cl:cons se1 se2)))

(defgeneric first (thing)
  (:method (thing) (cl:first thing))
  (:method ((lst list)) (cl:first lst))
  (:method ((seq sequence)) (first (coerce seq 'list))))

(defgeneric rest (thing)
  (:method (thing) (cl:rest thing))
  (:method ((lst list)) (cl:rest lst))
  (:method ((seq sequence)) (rest (coerce seq 'list))))

(defgeneric second (thing)
  (:method (thing) (first (rest thing))))

(defgeneric third (thing)
  (:method (thing) (first (rest (rest thing)))))

(defgeneric nth (thing n &optional default)
  (:method (thing n &optional (default nil)) (or (cl:nth n thing) default))
  (:method ((l list) n &optional (default nil)) (or (cl:nth n l) default))
  (:method ((s sequence) n &optional (default nil)) (nth (coerce s 'list) n default)))

(defgeneric next (thing)
  (:method (thing) (cl:rest thing))
  (:method ((lst list)) (cl:rest lst))
  (:method ((seq sequence)) (rest (coerce seq 'list))))

(defgeneric last (thing)
  (:method (thing) (cl:last thing))
  (:method ((lst list)) (cl:last lst))
  (:method ((seq sequence)) (cl:last (coerce seq 'list))))

(defgeneric butlast (thing &optional n)
  (:method (thing &optional (n 1)) (cl:butlast thing n))
  (:method ((lst list) &optional (n 1)) (cl:butlast lst n))
  (:method ((seq sequence) &optional (n 1)) (cl:butlast (coerce seq 'list) n)))

(defgeneric head (obj)
  (:method (obj) (first obj))
  (:method ((seq sequence)) (first (coerce seq 'list))))

(defgeneric tail (obj)
  (:method (obj) (rest obj))
  (:method ((seq sequence)) (rest (coerce seq 'list))))

(defgeneric ->list (object)
  (:method ((lst list)) lst)
  (:method ((seq sequence)) (coerce seq 'list)))

(defmethod ->list ((ht hash-table))
  (loop for v being each hash-values of ht using (hash-key k)
        collect (list k (if (typep v 'hash-table) (->list v) v))))

(defgeneric ->array (object)
  (:method (object) (make-array (length object) :initial-contents object))
  (:method ((ht hash-table)) (make-array (length ht) :initial-contents (cl:map 'list #'cl:vector (->list ht)))))

(defgeneric ->vector (object)
  (:method (object) (->array object)))

(defgeneric ->plist (object))

(defgeneric ->alist (object))

(defgeneric pop (target)
  (:method ((lst list)) (cl:pop lst) lst)
  (:method ((v array)) (if (array-has-fill-pointer-p v)
                           (progn (vector-pop v) v)
                           (pop (->fillable v)))))

(defgeneric peek (coll)
  (:method ((lst list)) (first lst))
  (:method ((v array)) (elt v (1- (length v)))))
