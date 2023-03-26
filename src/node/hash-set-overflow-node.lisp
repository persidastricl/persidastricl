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
;;;   hash-set-overflow-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  hash-set-overflow-node
;;
;; -----

(defclass hash-set-overflow-node (overflow-node) ())

(defmethod single-remaining-data ((node hash-set-overflow-node))
  (first (data node)))

(defmethod loc ((node hash-set-overflow-node) item &key hash (default nil) &allow-other-keys)
  (when (hash node) (assert (eq (hash node) hash)))
  (if-let ((target (member item data :test #'==)))
    (first target)
    default))
