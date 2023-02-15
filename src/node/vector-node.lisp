;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; vector-node.lisp
;;;
;;; -----

(in-package :node)

;; -----
;;  vector-node
;;
;;  base class for transient/persistent dynamic vector nodes

(defclass vector-node (bpvt-node)
  ((level :initarg :level :reader :level)
   (data :initarg :data :reader :data))
  (:default-initargs :data (make-array 0) :level 1))

(defmethod put ((node vector-node) item index)
  (let ((i (b:bits index (:level node))))
    (put (elt (:data node) i) item index)))

;; get walks nodes based on level
;; recursive to level of indexed node node (returns value)

(defmethod get ((node vector-node) index context)
  (let ((i (b:bits index (:level node))))
    (get (elt (:data node) i) index context)))

(defmethod count ((node vector-node))
  (cl:length (:data node)))
