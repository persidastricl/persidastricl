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
;;;   persistent-hash-set-overflow-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  persistent-hash-set-overflow-node
;;
;; -----

(define-immutable-class persistent-hash-set-overflow-node (hash-set-overflow-node) ())

(defmethod add ((node persistent-hash-set-overflow-node) item &key hash &allow-other-keys)
  (when (hash node) (assert (eq (hash node) hash)))
  (make-instance (type-of node) :hash (or (hash node) hash) :data (adjoin item (data node) :test #'==)))

(defmethod remove ((node persistent-hash-set-overflow-node) item &key hash &allow-other-keys)
  (when (hash node) (assert (eq (hash node) hash)))
  (make-instance (type-of node) (or (hash node) hash) :data (remove-if (lambda (e) (== item e)) (data node))))
