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
;;;   hash-set.lisp
;;;
;;; mixin class for persistent/transient -hash-set classes
;;;
;;; -----

(in-package #:persidastricl)

(defclass hash-set (hamt collection)
  ((root :initarg :root :reader root)))

(defun set? (x)
  (typep x 'hash-set))

(defmethod contains? ((hs hash-set) item)
  (with-slots (root) hs
    (let ((r (loc root item :hash (h:hash item) :depth 0 :default :not-found)))
      (not (== r :not-found)))))

(defmethod ->vector ((hs hash-set))
  (coerce (seq hs) 'cl:vector))

(defmethod ->array ((hs hash-set))
  (->vector hs))

(defmethod ->vec ((hs hash-set))
  (->vector hs))

(defmethod ->list ((hs hash-set))
  (into '() (seq hs)))

(defmacro with-funcallable-set ((symbol definition) &body body)
  `(let ((,symbol ,definition))
     (labels ((,symbol (k)
                (contains? ,symbol k)))
       ,@body)))

(defmethod compare ((s1 hash-set) (s2 hash-set))
  (compare (seq s1) (seq s2)))
