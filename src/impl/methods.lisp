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
  (:method ((object t)) (make-instance (type-of object))))

(defgeneric ->array (object))
(defgeneric ->vector (object))
(defgeneric ->vec (object))

(defgeneric ->list (object)
  (:method ((obj (eql nil))) nil))
(defgeneric ->pist (object))
(defgeneric ->alist (object))


(defgeneric ->seq (s)
  (:method (s) (coerce (->list s) 'list)))

(defmethod ->seq ((m hash-map))
  (flatten (map 'list e:->list (->list m))))


(defgeneric into (obj sequence))

(defmethod into ((obj collection) (sequence sequence))
  (apply #'conj obj (->seq sequence)))

(defmethod into ((obj collection) (hs hash-set))
  (apply #'conj obj (->seq hs)))

(defmethod into ((obj collection) (lazy-seq lazy-sequence))
  (apply #'conj obj (->seq lazy-seq)))

(defmethod into ((obj hash-map) (sequence sequence))
  (apply #'assoc obj (->seq sequence)))

(defmethod into ((obj hash-map) (lazy-seq lazy-sequence))
  (apply #'assoc obj (->seq lazy-seq)))

(defmethod into ((obj hash-map) (hm hash-map))
  (apply #'assoc obj (->seq hm)))
