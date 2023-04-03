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
;;;   atom.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; atom
;;
;; -----

(stmx:transactional
    (defclass atom ()
      ((value :initarg :value :reader :value))
      (:default-initargs :value nil)))

(defun atom (&optional value)
  (make-instance 'atom :value value))

(defmethod cl-murmurhash:murmurhash ((object atom) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (list "atom" (slot-value object 'value)) :seed seed :mix-only mix-only))

(defun swap! (atom fn &rest args)
  (stmx:atomic
   (setf (slot-value atom 'value) (apply fn (slot-value atom 'value) args))))

(defun reset! (atom new-value)
  (stmx:atomic
   (setf (slot-value atom 'value) new-value)))

(defgeneric deref (thing)
  (:method ((atom atom)) (stmx:atomic (slot-value atom 'value))))
