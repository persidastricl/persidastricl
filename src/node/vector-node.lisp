;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; vector-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  vector-node
;;
;;  base class for transient/persistent dynamic vector nodes

(defclass vector-node (bpvt-node)
  ((level :initarg :level :reader level)
   (data :initarg :data :reader data))
  (:default-initargs :data (make-array 0) :level 1))

(defmethod add ((node vector-node) item &key index)
  (let ((i (b:bits index (level node))))
    (add (elt (data node) i) item :index index)))

;; locate walks nodes based on level
;; recursive to level of leaf-node node (which then returns value)

(defmethod loc ((node vector-node) index &key default)
  (let ((i (b:bits index (level node))))
    (loc (elt (data node) i) index :default default)))

(defmethod count ((node vector-node))
  (cl:length (data node)))
