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

(named-readtables:in-readtable persidastricl:syntax)

;; -----
;; atom
;;
;; -----

(defclass atom (metadata)
  ((value :initarg :value :reader :value)
   (watches :initarg :watches :reader :watches))
  (:default-initargs :value nil :watches {}))

(defun atom (&optional value)
  (make-instance 'atom :value value))

(defmethod cl-murmurhash:murmurhash ((object atom) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (list "atom" (slot-value object 'value)) :seed seed :mix-only mix-only))

(defgeneric deref (thing)
  (:method ((atom atom)) (slot-value atom 'value)))

(defun add-watch (atom k f)
  (with-slots (watches) atom
    (setf watches (assoc watches k f)))
  atom)

(defun remove-watch (atom k)
  (with-slots (watches) atom
    (setf watches (dissoc watches k)))
  atom)

(defun notify-watches (atom old-val new-val)
  (with-slots (watches) atom
    (let ((ks (->list (keys watches))))
      (loop for k in ks
            do (let ((f (get watches k)))
                 (when f (funcall f k atom old-val new-val)))))))

(defun reset! (atom new-value)
  (let ((v (deref atom)))
    (atomics:atomic-update
     (slot-value atom 'value)
     (lambda (v)
       (declare (ignore v))
       new-value))
    (notify-watches atom v new-value))
  new-value)

(defun swap! (atom fn &rest args)
  (loop do (let* ((v (deref atom))
                  (nv (apply fn v args)))
             (when (atomics:cas (slot-value atom 'value) v nv)
               (notify-watches atom v nv)
               (return nv)))))

(defmethod with-meta ((a atom) (meta hash-map))
  (setf (slot-value a 'meta) meta)
  a)
