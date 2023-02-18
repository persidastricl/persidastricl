;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-set.lisp
;;;
;;; -----

(in-package :persidastricl)

;; -----
;; persistent-hash-set
;;
;;  an implementation of a persistent/immutable hashed-array-mapped-trie (hamt)
;; -----

(define-immutable-class persistent-hash-set (hash-set)
  ((root :type n:persistent-hash-set-node :initarg :root :reader :root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'n:persistent-hash-set-node) :meta nil))

(defmethod conj ((phs persistent-hash-set) &rest items)
  (with-slots (root meta) phs
    (let ((new-root (reduce
                     (lambda (node item)
                       (n:put node item (list (h:hash item) 0)))
                     items
                     :initial-value root)))
      (if (== new-root root)
          phs
          (make-instance 'persistent-hash-set :root new-root :meta meta)))))

(defmethod disj ((phs persistent-hash-set) &rest items)
  (with-slots (root meta) phs
    (let ((new-root  (reduce
                      (lambda (node item)
                        (n:delete node item (list (h:hash item) 0)))
                      items
                      :initial-value root)))
      (if (== new-root root)
          phs
          (make-instance 'persistent-hash-set :root new-root :meta meta)))))

(defun persistent-hash-set (&rest items)
  (let ((s (make-instance 'persistent-hash-set)))
    (if-not (empty? items)
            (apply #'conj s items)
            s)))

(defmethod print-object ((object persistent-hash-set) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "#{~{~s~^ ~}}" (seq object))
      (format stream "(persidastricl:persistent-hash-set (list ~{~s~^ ~}))" (seq object))))

(defmethod make-load-form ((obj persistent-hash-set) &optional env)
  (declare (ignore env))
  (let ((items (flatten (seq obj))))
    `(persidastricl:persistent-hash-set ,@items)))

(defun set (object)
  (into (persistent-hash-set) (seq object)))
