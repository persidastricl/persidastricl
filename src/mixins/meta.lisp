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
;;;   meta.lisp
;;;
;;; CLOS class for metadata
;;;
;;; -----

(in-package #:persidastricl)

(defclass metadata ()
  ((meta :initarg :meta :reader :meta :documentation "map of metadata"))
  (:default-initargs :meta nil))

(defgeneric with-meta (object meta))

(defgeneric meta (object)
  (:method ((object metadata)) (with-slots (meta) object meta)))

(defun vary-meta (obj f & args)
  (with-meta obj (apply f (meta obj) args)))
