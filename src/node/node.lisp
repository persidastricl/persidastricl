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
;;;   node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  base class for all node objects
;;
;; -----

(defclass node () ())

(defun empty-overflow-node (node)
  "create an overflow node for the type of node we are currently using xxxx-node --> xxx-overflow-node"
  (labels ((replace* (s pattern replacement)
             (cl-ppcre:regex-replace pattern s replacement :preserve-case t :simple-calls t)))
    (-> (type-of node)
      str
      (replace* "(?i)node$" "overflow-node")
      read-from-string
      make-instance)))

(defun empty-node (node &key hash depth)
  "given the current node and the context of the caller, determine if we
 need a new node or a new overflow node (ie. the max depth has been
 reached) "
  (declare (ignore hash))
  (let ((max-depth (floor (/ (h:size) b::*default-hash-slice-bit-size*))))
    (if (< depth max-depth)
        (empty node)
        (empty-overflow-node node))))
