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

;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   overflow-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  overflow-node
;;
;;  base class for overflow nodes
;; -----

(defclass overflow-node (node)
  ((hash :reader hash :initarg :hash)
   (data :reader data :initarg :data))
  (:default-initargs :hash nil :data '()))

(defmethod sub-nodes ((node overflow-node))
  (declare (ignore node))
  '())

(defmethod single-value-node? ((node overflow-node))
  (= (length (data node)) 1))

(defmethod nth-value ((node overflow-node) index)
  (elt (data node) index))

(defmethod count ((node overflow-node))
  (length (data node)))
