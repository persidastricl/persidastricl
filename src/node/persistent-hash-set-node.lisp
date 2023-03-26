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
;;;   persistent-hash-set-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; persistent-hash-set-node
;;
;; -----

(define-immutable-class persistent-hash-set-node (persistent-node hash-set-node) ()
  (:default-initargs :dmap (empty-persistent-bitmap-vector) :nmap (empty-persistent-node-bitmap-vector)))
