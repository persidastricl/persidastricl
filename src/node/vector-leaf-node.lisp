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
;;; vector-leaf-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defparameter *items-per-node* 32)

(defclass vector-leaf-node (vector-node) ()
  (:default-initargs :level 0))

(defmethod cons :before (item (node vector-leaf-node))
  ;; cannot add more items than we have bits in the hash
  (assert (< (length (data node)) *items-per-node*)))

;; locate value at level 0 index

(defmethod loc ((node vector-leaf-node) index &key (default nil))
  (let* ((i (b:bits index 0))
         (v (elt (data node) i)))
    (or v default)))
