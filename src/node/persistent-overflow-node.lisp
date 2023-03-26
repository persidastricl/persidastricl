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
;;;   persistent-overflow-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  persistent-overflow-node
;;
;; -----

(define-immutable-class persistent-overflow-node (overflow-node) ())
