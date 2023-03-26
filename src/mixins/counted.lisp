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
;;;   counted.lisp
;;;
;;; CLOS class for tracking a count of items
;;;
;;; -----

(in-package #:persidastricl)

(defclass counted ()
  ((count :type integer :initarg :count))
  (:default-initargs :count 0))

(defmethod count ((thing counted))
  (slot-value thing 'count))

(defmethod length ((thing counted))
  (count thing))

(defmethod bounded-count (n (thing counted))
  (:method (n (thing counted)) (count thing)))

(defmethod empty? ((thing counted))
  (zerop (count thing)))
