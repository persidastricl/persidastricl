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
  ((root :type transient-hash-set-node :initarg :root :reader :root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'transient-hash-set-node) :bit-size 5))

(defmethod conj ((ths transient-hash-set) &rest items)
  (when-not (emptyp items)
    (with-slots (root count bit-size) ths
      (labels ((conj* (node item)
                 (if (contains? ths item)
                     (values node 0)
                     (values (put node item (list (h:hash item) 0 bit-size)) 1))))
        (destructuring-bind (new-root new-count) (reduce
                                                  (lambda (twople item)
                                                    (destructuring-bind (node count) twople
                                                      (multiple-value-bind (node added) (conj* node item)
                                                        (list node (+ count added)))))
                                                  items
                                                  :initial-value (list root count))
          (setf root new-root)
          (setf count new-count)))))
  ths)

(defmethod disj ((ths transient-hash-set) &rest items)
  (when-not (emptyp items)
    (with-slots (root count bit-size) ths
      (labels ((disj* (node item)
                 (if (contains? ths item)
                     (values (del node item (list (h:hash item) 0 bit-size)) 1)
                     (values node 0))))
        (destructuring-bind (new-root new-count) (reduce
                                                  (lambda (twople item)
                                                    (destructuring-bind (node count) twople
                                                      (multiple-value-bind (node removed) (disj* node item)
                                                        (list node (- count removed)))))
                                                  items
                                                  :initial-value (list root count))
          (setf root new-root)
          (setf count new-count)))))
  ths)

(defun transient-hash-set (&rest items)
  (let ((s (make-instance 'transient-hash-set)))
    (when-not (emptyp s)
      (apply #'conj s items))
    s))

(defmethod print-object ((object transient-hash-set) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "@#{~{~s~^ ~}}" (seq object))
      (format stream "(persidastricl:list->transient-hash-set (list ~{~s~^ ~}))" (seq object))))

(defmethod make-load-form ((obj transient-hash-set) &optional env)
  (declare (ignore env))
  (let ((items (flatten (seq obj))))
    `(persidastricl:transient-hash-set ,@items)))
