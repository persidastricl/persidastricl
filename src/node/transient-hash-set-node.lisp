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
;;;   transient-hash-set-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; transient-hash-set-node
;;
;; -----

(defclass transient-hash-set-node (transient-node hash-set-node) ()
  (:default-initargs :dmap (empty-transient-bitmap-vector) :nmap (empty-transient-node-bitmap-vector)))
