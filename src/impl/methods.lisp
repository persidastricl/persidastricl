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

(defgeneric ->list (object))
(defgeneric ->pist (object))
(defgeneric ->alist (object))

(defgeneric ->seq (s)
  (:method (s) (coerce (seq s) 'list)))

(defmethod ->seq ((s hash-map))
  (flatten (map 'list e:->list (seq s))))

(defgeneric into (obj sequence))

(defmethod into ((obj hash-set) (sequence sequence))
  (apply #'conj obj (->seq sequence)))

(defmethod into ((obj hash-set) (hs hash-set))
  (apply #'conj obj (seq hs)))

(defmethod into ((obj hash-map) (sequence sequence))
  (apply #'assoc obj (->seq sequence)))

(defmethod into ((obj hash-map) (hm hash-map))
  (apply #'assoc obj (->seq (seq hm))))
