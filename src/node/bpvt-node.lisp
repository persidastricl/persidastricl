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
;;;   bpvt-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defgeneric add-leaf-node (node leaf first-index-of-values))
(defgeneric get-leaf-node (node first-index-of-values))
(defgeneric remove-leaf-node (node first-index-of-values))

;; -----
;;  bpvt-node
;;
;;  base class for transient/persistent dynamic vector nodes

(defclass bpvt-node (node) ())
