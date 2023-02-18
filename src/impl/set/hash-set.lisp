;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hash-set.lisp
;;;
;;; mixin class for persistent/transient -hash-set classes
;;;
;;; -----

(in-package #:persidastricl)

(defclass hash-set (hamt collection)
  ((root :initarg :root :reader :root)))

(defun set? (x)
  (typep x 'hash-set))

(defmethod contains? ((hs hash-set) item)
  (with-slots (root) hs
    (not (== :not-found (n:get root item (list (h:hash item) 0 :not-found))))))

(defmethod ->vector ((hs hash-set))
  (map 'cl:vector #'identity (seq hs)))

(defmethod ->array ((hs hash-set))
  (->vector hs))

(defmethod ->vec ((hs hash-set))
  (->vector hs))

(defmethod ->list ((hs hash-set))
  (seq hs))

(defmethod count ((obj hash-set))
  (count (->list obj)))
