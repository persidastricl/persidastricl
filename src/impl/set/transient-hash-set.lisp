;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-hash-set.lisp
;;;
;;; -----

(in-package :persidastricl)

;; -----
;; transient-hash-set
;;
;;  an implementation of a transient/imperative hashed-array-mapped-trie (hamt)
;; -----

(defclass transient-hash-set (hash-set)
  ((root :type n:transient-hash-set-node :initarg :root :reader :root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'n:transient-hash-set-node)))

(defmethod conj ((ths transient-hash-set) &rest items)
  (with-slots (root) ths
    (setf root (reduce
                (lambda (node item)
                  (n:put node item (list (h:hash item) 0)))
                items
                :initial-value root)))
  ths)

(defmethod disj ((ths transient-hash-set) &rest items)
  (with-slots (root count) ths
    (setf root (reduce
                (lambda (node item)
                  (n:delete node item (list (h:hash item) 0)))
                items
                :initial-value root)))
  ths)

(defun transient-hash-set (&rest items)
  (let ((s (make-instance 'transient-hash-set)))
    (when-not (emptyp s)
      (apply #'conj s items))
    s))

(defmethod print-object ((object transient-hash-set) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "@#{~{~s~^ ~}}" (seq object))
      (format stream "(persidastricl:transient-hash-set (list ~{~s~^ ~}))" (seq object))))

(defmethod make-load-form ((obj transient-hash-set) &optional env)
  (declare (ignore env))
  (let ((items (flatten (seq obj))))
    `(persidastricl:transient-hash-set ,@items)))

(defun t-set (object)
  (into (transient-hash-set) (seq object)))
