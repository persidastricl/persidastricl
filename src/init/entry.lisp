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
;;;   entry.lisp
;;;
;;; -----

(in-package #:persidastricl)

(proclaim '(inline map-entry key value ->vec ->list ->cons))

(defclass entry (collection)
  ((key :initarg :key :reader key)
   (value :initarg :value :reader value :reader val)))

(defun map-entry (k v)
  (make-instance 'entry :key k :value v))

(defmethod ->vec ((entry entry))
  (persistent-vector (key entry) (value entry)))

(defmethod ->array ((entry entry))
  (cl:vector (key entry) (value entry)))

(defmethod ->vector ((entry entry))
  (->array entry))

(defmethod ->list ((entry entry))
  (list (key entry) (value entry)))

(defmethod count ((entry entry))
  (length (->list entry)))

(defmethod length ((entry entry))
  (length (->list entry)))

(defmethod first ((entry entry))
  (key entry))

(defmethod rest ((entry entry))
  (list (value entry)))

(defmethod last ((entry entry))
  (value entry))

(defmethod head ((entry entry))
  (key entry))

(defmethod tail ((entry entry))
  (list (value entry)))

(defmethod next ((entry entry))
  (list (value entry)))

(defmethod seq ((entry entry))
  (->list entry))

(defmethod ->cons ((entry entry))
  (cons (key entry) (value entry)))

(defun pprint-map-entry (stream entry &rest other-args)
  (declare (ignore other-args))
  (pprint-logical-block (stream (->list entry) :prefix "[" :suffix "]")
    (write (pprint-pop) :stream stream)
    (write-char #\space stream)
    (write (pprint-pop) :stream stream)))

(defmethod print-object ((obj entry) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "~/persidastricl::pprint-map-entry/" obj)
      (format stream "(persidastricl::map-entry ~s ~s)" (key obj) (value obj))))

(set-pprint-dispatch 'entry 'pprint-map-entry)

(defmethod make-load-form ((obj entry) &optional env)
  (declare (ignore env))
  `(persidastricl::map-entry ,(key obj) ,(value obj)))

(defmethod cl-murmurhash:murmurhash ((object entry) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (->list object) :seed seed :mix-only mix-only))

(defmethod compare ((e1 entry) (e2 entry))
  (compare (->list e1) (->list e2)))
