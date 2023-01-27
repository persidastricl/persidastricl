;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hash-set.lisp
;;;
;;; mixin class for persistent/transient -hash-set classes
;;;
;;; -----

(in-package #:persidastricl)

(defclass hash-set (hamt collection) ())

(defmethod contains? ((hs hash-set) item)
  (with-slots (root bit-size) hs
    (not (== :not-found (get root item (list (h:hash item) 0 bit-size :not-found))))))

(defmethod ->vector ((hs hash-set))
  (map 'vector #'identity (seq hs)))

(defmethod ->array ((hs hash-set))
  (->vector hs))

(defmethod ->vec ((hs hash-set))
  (->vector hs))

(defmethod ->list ((hs hash-set))
  (seq hs))
