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
;;;   macros.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))

(defmacro if-not (pred truthy &optional falsey)
  `(if (not ,pred) ,truthy ,falsey ))

(defmacro when-not (pred &body body)
  `(when (not ,pred)
     ,@body))

(defmacro if-let (bindings then &optional else)
  (let ((binding (first bindings)))
    (assert (= (length binding) 2))
    (let ((var (elt binding 0))
          (form (elt binding 1))
          (temp (gensym)))
      `(let ((,temp ,form))
         (if ,temp
             (let ((,var ,temp))
               ,then)
             ,else)))))

(defmacro when-let (bindings &body body)
  (let ((binding (first bindings)))
    (assert (= (length binding) 2))
    (let ((var (elt binding 0))
          (form (elt binding 1))
          (temp (gensym)))
      `(let ((,temp ,form))
         (when ,temp
           (let ((,var ,temp))
             ,@body))))))

(defmacro fn (name-or-args &body args-and-or-body)
  (if (not (listp name-or-args))
      `(labels ((,name-or-args ,(first args-and-or-body) ,@(rest args-and-or-body)))
         #',name-or-args)
      `(lambda ,name-or-args
         ,@args-and-or-body)))

(defmacro comment (&body body)
  (declare (ignore body)))

(defmacro def (name value &optional doc-string)
  `(defparameter ,name ,value ,doc-string))
