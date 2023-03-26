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
;;;   transient-hash-set.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; transient-hash-set
;;
;;  an implementation of a transient/imperative hashed-array-mapped-trie (hamt)
;; -----

(defclass transient-hash-set (hash-set)
  ((root :type transient-hash-set-node :initarg :root :reader root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'transient-hash-set-node)))

(defmethod conj ((ths transient-hash-set) &rest items)
  (with-slots (root) ths
    (setf root (cl:reduce
                (lambda (node item)
                  (add node item :hash (h:hash item) :depth 0))
                items
                :initial-value root)))
  ths)

(defmethod disj ((ths transient-hash-set) &rest items)
  (with-slots (root count) ths
    (setf root (cl:reduce
                (lambda (node item)
                  (remove node item :hash (h:hash item) :depth 0))
                items
                :initial-value root)))
  ths)

(defun transient-hash-set (&rest items)
  (let ((s (make-instance 'transient-hash-set)))
    (unless (empty? items)
      (apply #'conj s items))
    s))

(defun pprint-transient-hash-set (stream ths &rest other-args)
  (declare (ignore other-args))
  (let ((*print-length* (min *print-hamt-items* (or *print-lines* *print-hamt-items*))))
    (pprint-logical-block (stream (->list (take (inc *print-length*) (seq ths))) :prefix "@#{" :suffix "}")
      (pprint-exit-if-list-exhausted)
      (loop
        (write (pprint-pop) :stream stream)
        (pprint-exit-if-list-exhausted)
        (write-char #\space stream)
        (pprint-newline :fill stream)))))

(defmethod print-object ((object transient-hash-set) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "~/persidastricl::pprint-transient-hash-set/" object)
      (format stream "(persidastricl:transient-hash-set ~{~s~^ ~})" (->list object))))

(set-pprint-dispatch 'transient-hash-set 'pprint-transient-hash-set)

(defmethod make-load-form ((object transient-hash-set) &optional env)
  (declare (ignore env))
  (let ((items (->list object)))
    `(persidastricl:transient-hash-set ,@items)))

(defun t-set (object)
  (into (transient-hash-set) (seq object)))
