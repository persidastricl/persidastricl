;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hash-set.lisp
;;;
;;; mixin class for persistent/transient -hash-set classes
;;;
;;; -----

(in-package #:persidastricl)

(defclass hash-set ()
  ((root :type 'hash-set-node :initarg :root :reader :root)))

(defmethod contains? ((hs hash-set) item)
  (with-slots (root) hs
    (not (== :not-found (get root item (list (h:hash item) 0 :not-found))))))
