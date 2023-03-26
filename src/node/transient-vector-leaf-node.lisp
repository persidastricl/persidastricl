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
;;; transient-vector-leaf-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defclass transient-vector-leaf-node (vector-leaf-node) ()
  (:default-initargs :data (make-array 32 :adjustable t :fill-pointer 0 :initial-element nil)))

(defmethod cons (item (node transient-vector-leaf-node))
  (with-slots (data) node
    (vector-push item data))
  node)

(defmethod add ((node transient-vector-leaf-node) item &key index)
  (with-slots (data) node
    (let ((i (b:bits index 0)))
      (setf (elt data i) item)))
  node)

(defmethod pop ((node transient-vector-leaf-node))
  (with-slots (data) node
    (vector-pop data))
  node)
