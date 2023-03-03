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

(defmethod print-object ((object transient-hash-set) stream)
  (let ((items (->list object)))
    (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
        (format stream "@#{~{~s~^ ~}}" items)
        (format stream "(persidastricl:transient-hash-set ~{~s~^ ~})" items))))

(defmethod make-load-form ((object transient-hash-set) &optional env)
  (declare (ignore env))
  (let ((items (->list object)))
    `(persidastricl:transient-hash-set ,@items)))

(defun t-set (object)
  (into (transient-hash-set) (seq object)))
