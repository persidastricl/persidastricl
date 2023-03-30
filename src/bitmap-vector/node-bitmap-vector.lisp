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
;;;   node-bitmap-vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  mixin/marker class for bitmap-vectors that track sub-nodes
;;
;; -----

(defclass node-bitmap-vector (bitmap-vector) ())
