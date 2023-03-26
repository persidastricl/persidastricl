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
;;;   thunk.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; thunk
;;
;; -----

(defclass thunk ()
  ((fn :initarg :fn)
   (r  :initarg :r))
  (:default-initargs :r nil))

(defmacro delay (&rest body)
  `(make-instance 'thunk :fn (lambda () ,@body)))

(defgeneric force (obj)
  (:method (obj) obj))

(defmethod force ((thunk thunk))
  (when (slot-value thunk 'fn)
    (setf (slot-value thunk 'r) (funcall (slot-value thunk 'fn)))
    (setf (slot-value thunk 'fn) nil))
  (slot-value thunk 'r))
