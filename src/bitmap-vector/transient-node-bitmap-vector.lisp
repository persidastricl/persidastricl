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
;;;   transient-node-bitmap-vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; transient-node-bitmap-vector object
;;
;; -----

(defclass transient-node-bitmap-vector (transient-bitmap-vector) ())

(defun empty-transient-node-bitmap-vector ()
  (make-instance 'transient-node-bitmap-vector))
