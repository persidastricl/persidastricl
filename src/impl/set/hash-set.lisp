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
  ((root :initarg :root :reader root)))

(defun set? (x)
  (typep x 'hash-set))

(defmethod contains? ((hs hash-set) item)
  (with-slots (root) hs
    (let ((r (loc root item :hash (h:hash item) :depth 0 :default :not-found)))
      (unless (== r :not-found)
        r))))

(defmethod ->vector ((hs hash-set))
  (coerce (seq hs) 'cl:vector))

(defmethod ->array ((hs hash-set))
  (->vector hs))

(defmethod ->vec ((hs hash-set))
  (->vector hs))

(defmethod ->list ((hs hash-set))
  (into '() (seq hs)))

(defmacro with-funcallable-set ((symbol definition) &body body)
  `(let ((,symbol ,definition))
     (labels ((,symbol (k &optional (default nil))
                (contains? ,symbol k)))
       ,@body)))
