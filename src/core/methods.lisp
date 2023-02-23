;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   core/generics.lisp
;;;
;;;   core (generic) functions
;;;
;;; -----

(in-package #:persidastricl)

(defgeneric into (obj sequence))

(defmethod into ((obj list) seq)
  (->list seq))

(defmethod into ((obj array) seq)
  (let* ((lst (->list seq))
         (size (length lst)))
    (make-array size :fill-pointer size :initial-contents lst)))

(defmethod into ((obj collection) (sequence sequence))
  (apply #'conj obj (->list sequence)))

(defmethod into ((obj collection) (hs hash-set))
  (apply #'conj obj (->list hs)))

(defmethod into ((obj collection) (lazy-seq lazy-sequence))
  (lreduce #'conj lazy-seq :initial-value obj))

(defmethod into ((obj hash-map) (sequence sequence))
  (apply #'assoc obj (->list sequence)))

(defmethod into ((obj hash-map) (lazy-seq lazy-sequence))
  (apply #'assoc obj (->list lazy-seq)))

(defmethod into ((obj hash-map) (hm hash-map))
  (apply #'assoc obj (->list hm)))
