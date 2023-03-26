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
;;;   transient-hash-set-overflow-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  transient-hash-set-overflow-node
;;
;; -----

(defclass transient-hash-set-overflow-node (transient-overflow-node hash-set-overflow-node) ())

(defmethod add ((node transient-hash-set-overflow-node) item &key hash &allow-other-keys)
  (when (hash node) (assert (eq (hash node) hash)))
  (unless (hash node) (setf (hash node) hash))
  (setf (data node) (adjoin item (data node) :test #'==))
  node)

(defmethod remove ((node transient-hash-set-overflow-node) item &key hash &allow-other-keys)
  (when (hash node) (assert (eq (hash node) hash)))
  (unless (hash node) (setf (hash node) hash))
  (setf (data node) (remove-if (lambda (e) (== item e)) (data node)))
  node)
