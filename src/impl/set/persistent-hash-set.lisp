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
  ((root :type persistent-hash-set-node :initarg :root :reader :root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'persistent-hash-set-node) :meta nil :count 0 :bit-size 5))

(defmethod conj ((phs persistent-hash-set) &rest items)
  (if (null items)
      phs
      (labels ((conj* (node item)
                 (if (contains? phs item)
                     (values node 0)
                     (values (put node item (list (h:hash item) 0 (:bit-size phs))) 1))))
        (destructuring-bind (root count) (reduce
                                          (lambda (twople item)
                                            (destructuring-bind (node count) twople
                                              (multiple-value-bind (node added) (conj* node item)
                                                (list node (+ count added)))))
                                          items
                                          :initial-value (list (:root phs) (:count phs)))
          (make-instance 'persistent-hash-set
                         :root root
                         :meta (:meta phs)
                         :count count
                         :bit-size (:bit-size phs))))))


(defmethod disj ((phs persistent-hash-set) &rest items)
  (if (null items)
      phs
      (labels ((disj* (node item)
                 (if (contains? phs item)
                     (values (del node item (list (h:hash item) 0 (:bit-size phs))) 1)
                     (values node 0))))
        (destructuring-bind (root count) (reduce
                                          (lambda (twople item)
                                            (destructuring-bind (node count) twople
                                              (multiple-value-bind (node removed) (disj* node item)
                                                (list node (- count removed)))))
                                          items
                                          :initial-value (list (:root phs) (:count phs)))
          (make-instance 'persistent-hash-set
                         :root root
                         :meta (:meta phs)
                         :count count
                         :bit-size (:bit-size phs))))))


(defun persistent-hash-set (&rest items)
  (let ((s (make-instance 'persistent-hash-set)))
    (if-not (emptyp s)
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
