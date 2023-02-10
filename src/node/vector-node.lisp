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

;; get walks nodes based on level
;; recursive to level of indexed node node (returns value)

(defmethod get ((node vector-node) index context)
  (let ((i (b:bits index (:level node))))
    (get (elt (:data node) i) index context)))

(defun add-leaf-node (node leaf first-index-of-values)
  (with-slots (level data) node
    (let* ((i (b:bits first-index-of-values (:level node))))
      (labels ((sub-node (i)
                 (when (< i (length data))
                   (elt data i))))
        (if (> level 1)
            ;; node of sub-nodes
            (if-let (sub-node (sub-node i))
              ;; have subnode for this level
              (make-instance (type-of node) :level level :data (v:update data i (add-leaf-node sub-node leaf first-index-of-values)))
              ;; no sub-node for this level (need to add one)
              (let ((new-sub-node (make-instance (type-of node) :level (1- level))))
                (assert (= i (length data)))
                (make-instance (type-of node) :level level :data (v:append data (add-leaf-node new-sub-node leaf first-index-of-values)))))
            ;; level 1 node
            (progn
              (assert (== i (length data)))
              (make-instance (type-of node) :level level :data (v:append data leaf))))))))

(defmethod count ((node vector-node))
  (cl:length (:data node)))
