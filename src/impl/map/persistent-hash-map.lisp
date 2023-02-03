;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-map.lisp
;;;
;;; -----

(in-package :persidastricl)

;; -----
;; persistent-hash-map
;;
;;  an implementation of a persistent/immutable hashed-array-mapped-trie (hamt)
;; -----

(define-immutable-class persistent-hash-map (hash-map)
  ((root :type persistent-hash-map-node :initarg :root :reader :root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'persistent-hash-map-node) :meta nil :count 0 :bit-size 5))

(defmethod assoc ((phm persistent-hash-map) k v &rest kv-pairs)
  (let ((kv-pairs (if (and k v)
                      (list* k v kv-pairs)
                      kv-pairs)))
    (if (emptyp kv-pairs)
        phm
        (labels ((assoc* (node kv-pair)
                   (destructuring-bind (k v) kv-pair
                     (let* ((hash (h:hash k))
                            (current (lookup phm k :not-found)))
                       (if (== current v)
                           (values node 0)
                           (values (put node (e:map-entry k v) (list hash 0 (:bit-size phm)))
                                   (if (== current :not-found) 1 0)))))))
          (destructuring-bind (root count) (reduce
                                            (lambda (twople kv-pair)
                                              (destructuring-bind (node count) twople
                                                (multiple-value-bind (node added) (assoc* node kv-pair)
                                                  (list node (+ count added)))))
                                            (partition kv-pairs 2)
                                            :initial-value (list (:root phm) (:count phm)))
            (make-instance 'persistent-hash-map
                           :root root
                           :meta (:meta phm)
                           :count count
                           :bit-size (:bit-size phm)))))))

(defmethod dissoc ((phm persistent-hash-map) &rest keys)
  (if (emptyp keys)
      phm
      (labels ((dissoc* (node k)
                 (let* ((hash (h:hash k))
                        (current (lookup phm k :not-found)))
                   (if (== current :not-found)
                       (values node 0)
                       (values (del node k (list hash 0 (:bit-size phm))) 1)))))
        (destructuring-bind (root count) (reduce
                                          (lambda (twople k)
                                            (destructuring-bind (node count) twople
                                              (multiple-value-bind (node removed) (dissoc* node k)
                                                (list node (- count removed)))))
                                          keys
                                          :initial-value (list (:root phm) (:count phm)))
          (make-instance 'persistent-hash-map
                         :root root
                         :meta (:meta phm)
                         :count count
                         :bit-size (:bit-size phm))))))

(defun persistent-hash-map (&rest kvs)
  (let ((m (make-instance 'persistent-hash-map)))
    (if-not (emptyp kvs)
            (apply #'assoc m kvs)
            m)))

(defmethod print-object ((object persistent-hash-map) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "{~{~s~^ ~}}" (flatten (map 'list #'e:->list (seq object))))
      (format stream "(persidastricl:persistent-hash-map ~{~s~^ ~})" (flatten (map 'list #'e:->list (seq object))))))

(defmethod make-load-form ((obj persistent-hash-map) &optional env)
  (declare (ignore env))
  (let ((items (flatten (map 'list #'e:->list (seq obj)))))
    `(persidastricl:persistent-hash-map ,@items)))
