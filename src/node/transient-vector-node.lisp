;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; transient-vector-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defclass transient-vector-node (vector-node) ()
  (:default-initargs :data (make-array 32 :adjustable t :fill-pointer 0 :initial-element nil)))

(defmethod add-leaf-node ((node transient-vector-node) leaf first-index-of-values)
  (with-slots (level data) node
    (let ((i (b:bits first-index-of-values level))
          (items-in-data (length data)))
      (labels ((sub-node (idx)
                 (when (< idx items-in-data)
                   (elt data idx))))
        (if (> level 1)
            ;; node of sub-nodes
            (let ((sub-node (sub-node i)))
              (if sub-node
                  ;; have subnode for this level
                  (add-leaf-node sub-node leaf first-index-of-values)
                  ;; no sub-node for this level (need to add one)
                  (let ((new-sub-node (make-instance (type-of node) :level (1- level))))
                    (assert (= i items-in-data))
                    (vector-push (add-leaf-node new-sub-node leaf first-index-of-values) data))))
            ;; level 1 node
            (progn
              (assert (= i items-in-data))
              (vector-push leaf data))))))
  node)

(defmethod get-leaf-node ((node transient-vector-node) first-index-of-values)
  (with-slots (level data) node
    (let ((i (b:bits first-index-of-values level))
          (items-in-data (length data)))
      (labels ((sub-node (idx)
                 (when (< idx items-in-data)
                   (elt data idx))))
        (if (> level 1)
            ;; node of sub-nodes
            (let ((sub-node (sub-node i)))
              (if sub-node
                  ;; have subnode for this level
                  (get-leaf-node sub-node first-index-of-values)
                  ;; no sub-node for this level (should have one!)
                  (error "no subnode found when getting previous leaf node")))
            ;; level 1 node
            (progn
              (assert (= i (1- items-in-data)))
              (elt data i)))))))

(defmethod remove-leaf-node ((node transient-vector-node) first-index-of-values)
  (with-slots (level data) node
    (let ((i (b:bits first-index-of-values level))
          (items-in-data (length data)))
      (labels ((sub-node (idx)
                 (when (< idx items-in-data)
                   (elt data idx))))
        (if (> level 1)
            ;; node of sub-nodes
            (let ((sub-node (sub-node i)))
              (if sub-node
                  ;; have subnode for this level
                  (let ((new-node  (remove-leaf-node sub-node first-index-of-values)))
                    (when (empty? (data new-node))
                      (vector-pop data)))
                  ;; no sub-node for this level (should have one!)
                  (error "no subnode found when removing previous leaf node")))
            ;; level 1 node
            (progn
              (assert (= i (1- items-in-data)))
              (vector-pop data))))))
  node)
