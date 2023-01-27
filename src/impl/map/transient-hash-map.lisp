;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-hash-map.lisp
;;;
;;; -----

(in-package :persidastricl)

;; -----
;; transient-hash-map
;;
;;  an implementation of a transient/imperative hashed-array-mapped-trie (hamt)
;; -----

(defclass transient-hash-map (hash-map)
  ((root :type transient-hash-map-node :initarg :root :reader :root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'transient-hash-map-node) :meta nil :count 0 :bit-size 5))

(defmethod assoc ((thm transient-hash-map) &rest kv-pairs)
  (when-not (emptyp kv-pairs)
    (with-slots (root count bit-size) thm
      (labels ((assoc* (node kv-pair)
                 (destructuring-bind (k v) kv-pair
                   (let ((current (lookup thm k :not-found)))
                     (if (== current v)
                         (values node 0)
                         (values (put node (e:map-entry k v) (list (h:hash k) 0 bit-size))
                                 (if (== current :not-found) 1 0)))))))
        (destructuring-bind (new-root new-count) (reduce
                                                  (lambda (twople kv-pair)
                                                    (destructuring-bind (node count) twople
                                                      (multiple-value-bind (node added) (assoc* node kv-pair)
                                                        (list node (+ count added)))))
                                                  (partition kv-pairs 2)
                                                  :initial-value (list root count))
          (setf root new-root)
          (setf count new-count)))))
  thm)

(defmethod dissoc ((thm transient-hash-map) &rest keys)
  (when-not (emptyp keys)
    (with-slots (root count bit-size) thm
      (labels ((dissoc* (node k)
                 (let ((current (lookup thm k :not-found)))
                   (if (== current :not-found)
                       (values node 0)
                       (values (del node k (list (h:hash k) 0 bit-size)) 1)))))
        (destructuring-bind (new-root new-count) (reduce
                                                  (lambda (twople key)
                                                    (destructuring-bind (node c) twople
                                                      (multiple-value-bind (node removed) (dissoc* node key)
                                                        (list node (- c removed)))))
                                                  keys
                                                  :initial-value (list root count))
          (setf root new-root)
          (setf count new-count)))))
  thm)

(defun transient-hash-map (&rest kvs)
  (let ((m (make-instance 'transient-hash-map)))
    (when-not (emptyp kvs)
      (apply #'assoc m kvs))
    m))

(defmethod print-object ((object transient-hash-map) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "@{~{~s~^ ~}}" (flatten (map 'list #'e:->list (seq object))))
      (format stream "(persidastricl:transient-hash-map ~{~s~^ ~})" (flatten (map 'list #'e:->list (seq object))))))

(defmethod make-load-form ((obj transient-hash-map) &optional env)
  (declare (ignore env))
  (let ((items (flatten (map 'list #'e:->list (seq obj)))))
    `(persidastricl:transient-hash-map ,@items)))
