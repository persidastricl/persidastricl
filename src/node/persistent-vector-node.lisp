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
                  (make-instance (type-of node) :level level :data (v:update data i (add-leaf-node sub-node leaf first-index-of-values)))
                  ;; no sub-node for this level (need to add one)
                  (let ((new-sub-node (make-instance (type-of node) :level (1- level))))
                    (assert (= i items-in-data))
                    (make-instance (type-of node) :level level :data (v:append data (add-leaf-node new-sub-node leaf first-index-of-values))))))
            ;; level 1 node
            (progn
              (assert (= i items-in-data))
              (make-instance (type-of node) :level level :data (v:append data leaf))))))))


(defmethod get-leaf-node ((node persistent-vector-node) first-index-of-values)
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

(defmethod remove-leaf-node ((node persistent-vector-node) first-index-of-values)
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
                  (let ((new-node (remove-leaf-node sub-node first-index-of-values)))
                    (make-instance (type-of node)
                                   :level level
                                   :data (if (empty? (:data new-node))
                                             (v:delete data i)
                                             (v:update data i))))
                  ;; no sub-node for this level (should have one!)
                  (error "no subnode found when getting previous leaf node")))
            ;; level 1 node
            (progn
              (assert (= i (1- items-in-data)))
              (make-instance (type-of node) :level level :data (v:delete data i))))))))


(defmethod put ((node persistent-vector-node) item index)
  (with-slots (level data) node
    (let ((i (b:bits index level)))
      (make-instance (type-of node) :level level :data (put (elt data i) item index)))))
