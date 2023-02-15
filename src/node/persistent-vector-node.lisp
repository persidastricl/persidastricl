;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; persistent-vector-node.lisp
;;;
;;; -----

(in-package :node)

(define-immutable-class persistent-vector-node (vector-node) ())

(defmethod add-leaf-node ((node persistent-vector-node) leaf first-index-of-values)
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
                  (make-instance (type-of node) :level level :data (v:update data i (add-leaf-node sub-node leaf first-index-of-values)))
                  ;; no sub-node for this level (need to add one)
                  (let ((new-sub-node (make-instance (type-of node) :level (1- level))))
                    (assert (= i (length data)))
                    (make-instance (type-of node) :level level :data (v:append data (add-leaf-node new-sub-node leaf first-index-of-values))))))
            ;; level 1 node
            (progn
              (assert (== i (length data)))
              (make-instance (type-of node) :level level :data (v:append data leaf))))))))


(defmethod put ((node persistent-vector-node) item index)
  (with-slots (level data) node
    (let ((i (b:bits index level)))
      (make-instance (type-of node) :level level :data (put (elt data i) item index)))))
