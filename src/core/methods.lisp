;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   core/generics.lisp
;;;
;;;   core (generic) functions
;;;
;;; -----

(in-package #:persidastricl)

(defmethod conj ((l list) &rest items)
  (->list (concat (seq l) items)))

(defgeneric into (obj sequence))

(defmethod into ((l list) sequence)
  (apply #'conj l (->list sequence)))

(defmethod into ((a array) sequence)
  (let ( (size (+ (count (->list a)) (count (->list sequence)))))
    (make-array size :initial-contents (->list (concat (->list a) sequence)))))

(defmethod into ((s1 string) (s2 string))
  (concatenate 'string s1 s2))

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
