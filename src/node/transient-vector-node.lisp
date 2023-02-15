;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; transient-vector-node.lisp
;;;
;;; -----

(in-package :node)

(defclass transient-vector-node (vector-node) ()
  (:default-initargs :data (make-array 32 :adjustable t :fill-pointer 0 :initial-element nil)))

(defmethod add-leaf-node ((node transient-vector-node) leaf first-index-of-values)
  (with-slots (level data) node
    (let ((i (b:bits first-index-of-values level)))
      (labels ((sub-node (idx)
                 (when (< idx (length data))
                   (elt data idx))))
        (if (> level 1)
            ;; node of sub-nodes
            (let ((sub-node (sub-node i)))
              (if sub-node
                  ;; have subnode for this level
                  (add-leaf-node sub-node leaf first-index-of-values)
                  ;; no sub-node for this level (need to add one)
                  (let ((new-sub-node (make-instance (type-of node) :level (1- level))))
                    (assert (= i (length data)))
                    (vector-push (add-leaf-node new-sub-node leaf first-index-of-values) data))))
            ;; level 1 node
            (progn
              (assert (= i (length data)))
              (vector-push leaf data))))))
  node)
