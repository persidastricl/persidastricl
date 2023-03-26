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
;;;   collection.lisp
;;;
;;; marker class for a collection of things
;;;
;;; -----

(in-package #:persidastricl)

(defclass collection () ())

(defgeneric contains? (collection item))

(defgeneric conj (collection &rest items))

(defgeneric disj (collection &rest items))

(defun collection? (x)
  (or (typep x 'collection)
      (typep x 'sequence)))
