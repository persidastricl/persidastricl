;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   entry.lisp
;;;
;;; -----

(in-package #:persidastricl)

(proclaim '(inline map-entry key value ->vec ->list ->cons))

(defclass entry (collection)
  ((key :initarg :key :reader :key)
   (value :initarg :value :reader :value)))

(defun map-entry (k &optional (v nil))
  (make-instance 'entry :key k :value v))

(defgeneric key (target)
  (:method ((entry entry)) (:key entry)))

(defgeneric value (target)
  (:method ((entry entry)) (:value entry)))

(defmethod ->vec ((entry entry))
  (persistent-vector (key entry) (value entry)))

(defmethod ->array ((entry entry))
  (cl:vector (key entry) (value entry)))

(defmethod ->vector ((entry entry))
  (->array entry))

(defmethod ->list ((entry entry))
  (list (key entry) (value entry)))

(defmethod first ((entry entry))
  (key entry))

(defmethod rest ((entry entry))
  (list (value entry)))

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

(defmethod print-object ((obj Entry) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "[~s ~s]" (key obj) (value obj))
      (format stream "(persidastricl:map-entry ~s ~s)" (key obj) (value obj))))

(defmethod make-load-form ((obj Entry) &optional env)
  (declare (ignore env))
  `(persidastricl:map-entry ,(key Entry) ,(value Entry)))

(defmethod cl-murmurhash:murmurhash ((object entry) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (->list object) :seed seed :mix-only mix-only))
