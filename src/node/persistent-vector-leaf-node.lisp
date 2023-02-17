;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; persistent-vector-leaf-node.lisp
;;;
;;; -----

(in-package :node)

(define-immutable-class persistent-vector-leaf-node (vector-leaf-node) ()
  (:default-initargs :level 0))

(defmethod cons (item (node persistent-vector-leaf-node))
  (make-instance (type-of node) :level 0 :data (v:append (:data node) item)))

(defmethod put ((node persistent-vector-leaf-node) item index)
  (with-slots (data) node
    (let ((i (b:bits index 0)))
      (make-instance (type-of node) :level 0 :data (v:update (:data node) i item)))))

(defmethod pop ((node persistent-vector-leaf-node))
  (with-slots (data) node
    (make-instance (type-of node) :level 0 :data (v:delete data (1- (length data))))))

(defmethod update-instance-for-different-class :before ((old transient-vector-leaf-node)
                                                        (new persistent-vector-leaf-node)
                                                        &key)
  (slot-makunbound new 'data)
  (slot-makunbound new 'level)

  (with-slots (data level) old
    (let ((size (length data)))
      (setf (slot-value new 'data) (make-array size :initial-contents data))
      (setf (slot-value new 'level) level))))
