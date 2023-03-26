;;; -----
;;;
;;;  Copyright (c) 2019-2023 Michael D Pendergrass, pupcus.org
;;;
;;;  This program and the accompanying materials are made
;;;  available under the terms of the Eclipse Public License 2.0
;;;  which is available at https://www.eclipse.org/legal/epl-2.0/
;;;
;;;  SPDX-License-Identifier: EPL-2.0
;;;
;;; -----

;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; persistent-vector-leaf-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

(define-immutable-class persistent-vector-leaf-node (vector-leaf-node) ()
  (:default-initargs :level 0))

(defmethod cons (item (node persistent-vector-leaf-node))
  (make-instance (type-of node) :level 0 :data (v:append (data node) item)))

(defmethod add ((node persistent-vector-leaf-node) item &key index)
  (with-slots (data) node
    (let ((i (b:bits index 0)))
      (make-instance (type-of node) :level 0 :data (v:update (data node) i item)))))

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
