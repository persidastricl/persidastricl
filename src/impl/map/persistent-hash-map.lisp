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
  ((root :type n:persistent-hash-map-node :initarg :root :reader :root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'n:persistent-hash-map-node) :meta nil))

(defmethod assoc ((phm persistent-hash-map) k v &rest kv-pairs)
  (with-slots (root meta) phm
    (let ((new-root (reduce
                     (lambda (node kv-pair)
                       (let ((entry (apply #'e:map-entry kv-pair)))
                         (n:put node entry (list (h:hash (e:key entry)) 0))))
                     (partition (list* k v kv-pairs) 2)
                     :initial-value root)))
      (if (== new-root root)
          phm
          (make-instance 'persistent-hash-map :root new-root :meta meta)))))

(defmethod dissoc ((phm persistent-hash-map) &rest keys)
  (with-slots (root meta) phm
    (let ((new-root (reduce
                     (lambda (node k)
                       (n:delete node k (list (h:hash k) 0)))
                     keys
                     :initial-value root)))
      (if (== new-root root)
          phm
          (make-instance 'persistent-hash-map :root new-root :meta meta)))))

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
