;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; persistent-vector-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

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
                                   :data (if (empty? (data new-node))
                                             (v:delete data i)
                                             (v:update data i))))
                  ;; no sub-node for this level (should have one!)
                  (error "no subnode found when getting previous leaf node")))
            ;; level 1 node
            (progn
              (assert (= i (1- items-in-data)))
              (make-instance (type-of node) :level level :data (v:delete data i))))))))


(defmethod add ((node persistent-vector-node) item &key index)
  (with-slots (level data) node
    (let ((i (b:bits index level)))
      (make-instance (type-of node) :level level :data (add (elt data i) item :index index)))))

(defmethod update-instance-for-different-class :before ((old transient-vector-node)
                                                        (new persistent-vector-node)
                                                        &key)

  (slot-makunbound new 'data)
  (slot-makunbound new 'level)

  (with-slots (data level) old
    (setf (slot-value new 'data) (cl:map
                                  'vector
                                  (lambda (node)
                                    (change-class node (transient->persistent-name node)))
                                  (slot-value old 'data)))
    (setf (slot-value new 'level) level)))
