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

;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; walk.lisp
;;;
;;; -----

(in-package :walk)

(named-readtables:in-readtable persidastricl:syntax)

(defgeneric walk (inner outer form))

(defmethod walk (inner outer form)
  (funcall outer form))

(defmethod walk (inner outer (form (eql nil)))
  (funcall outer form))

(defmethod walk (inner outer (form list))
  (funcall outer (->list (map inner form))))

(defmethod walk (inner outer (entry p::entry))
  (funcall outer (p::map-entry (funcall inner (key entry)) (funcall inner (val entry)))))

(defmethod walk (inner outer (seq p::lazy-sequence))
  (funcall outer (doall (map inner seq))))

(defmethod walk (inner outer (m p::hash-map))
  (funcall outer (into (empty m) (map inner m))))

(defmethod walk (inner outer (collection p::collection))
  (funcall outer (into (empty collection) (map inner collection))))

(defun postwalk (f form)
  (walk
   (lambda (frm) (postwalk f frm))
   f
   form))

(defun prewalk (f form)
  (walk
   (lambda (frm) (prewalk f frm))
   #'identity
   (funcall f form)))

(defun keywordize-keys (m)
  (labels ((keywordize-entry (e)
             (let ((k (key e))
                   (v (val e)))
               (if (string? k)
                   (p::map-entry (keyword k) v)
                   e))))
    (postwalk (lambda (x) (if (map? x) (into {} (map #'keywordize-entry x)) x)) m)))

(defun stringify-keys (m)
  (labels ((stringify-entry (e)
             (let ((k (key e))
                   (v (val e)))
               (if (keywordp k)
                   (p::map-entry (name k) v)
                   e))))
    (postwalk (lambda (x) (if (map? x) (into {} (map #'stringify-entry x)) x)) m)))

(defun prewalk-demo (form)
  (prewalk (lambda (x) (princ "Walked: ") (princ (format nil "~s~%" x)) x) form))

(defun postwalk-demo (form)
  (postwalk (lambda (x) (princ "Walked: ") (princ (format nil "~s~%" x)) x) form))

(defun prewalk-replace (smap form)
  (prewalk (lambda (x) (let ((v (get smap x))) (or v x))) form))

(defun postwalk-replace (smap form)
  (postwalk (lambda (x) (let ((v (get smap x))) (or v x))) form))

;; TODO: I don't think this is working quite right yet
;; (defun macroexpand-all (form)
;;   (prewalk (lambda (x) (if (listp x) (macroexpand x) x)) form))
