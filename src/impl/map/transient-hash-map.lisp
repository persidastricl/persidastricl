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
  ((root :type n:transient-hash-map-node :initarg :root :reader :root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'n:transient-hash-map-node) :meta nil))

(defmethod assoc ((thm transient-hash-map) k v &rest kv-pairs)
  (with-slots (root) thm
    (setf root (reduce
                (lambda (node kv-pair)
                  (let ((entry (apply #'e:map-entry kv-pair)))
                    (n:put node entry (list (h:hash (e:key entry)) 0))))
                (->list (partition-all (list* k v  kv-pairs) 2))
                :initial-value root)))
  thm)

(defmethod dissoc ((thm transient-hash-map) &rest keys)
  (with-slots (root) thm
    (setf root (reduce
                (lambda (node k)
                  (n:delete node k (list (h:hash k) 0)))
                keys
                :initial-value root)))
  thm)

(defun transient-hash-map (&rest kvs)
  (let ((m (make-instance 'transient-hash-map)))
    (when-not (empty? kvs)
      (apply #'assoc m kvs))
    m))

(defmethod print-object ((object transient-hash-map) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "@{~{~s~^ ~}}" (->list object))
      (format stream "(persidastricl:transient-hash-map ~{~s~^ ~})" (->list object))))

(defmethod make-load-form ((obj transient-hash-map) &optional env)
  (declare (ignore env))
  (let ((items (flatten (map 'list #'e:->list (seq obj)))))
    `(persidastricl:transient-hash-map ,@items)))
