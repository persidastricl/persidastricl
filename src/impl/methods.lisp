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
;;;   methods.lisp
;;;
;;; generic functions
;;;
;;; -----

(in-package #:persidastricl)

(defgeneric persistent! (obj)
  (:method  (obj) (let ((new-object-type (transient->persistent-name obj)))
                    (change-class obj new-object-type))))

(defmethod empty? ((obj hamt))
  (empty? (root obj)))

(defmethod empty? ((obj bpvt))
  (zerop (count obj)))
