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

(defmethod assoc ((thm transient-hash-map) k v &rest kv-pairs)
  (with-slots (root count bit-size) thm
    (let ((added 0))
      (labels ((assoc* (node kv-pair)
                 (destructuring-bind (k v) kv-pair
                   (let ((current (lookup thm k :not-found)))
                     (when (== current :not-found) (incf added))
                     (if (== current v)
                         node
                         (put-it node (e:map-entry k v) (list (h:hash k) 0 bit-size)))))))
        (setf root (reduce
                    #'assoc*
                    (partition kv-pairs 2)
                    :initial-value (assoc* root (list k v))))
        (setf count (+ count added)))))
  thm)

(defmethod dissoc ((thm transient-hash-map) &rest keys)
  (when-not (emptyp keys)
    (with-slots (root count bit-size) thm
      (let ((removed 0))
        (labels ((dissoc* (node k)
                   (let ((current (lookup thm k :not-found)))
                     (when-not (== current :not-found) (incf removed))
                     (if (== current :not-found)
                         node
                         (del-it node k (list (h:hash k) 0 bit-size))))))
          (setf root (reduce
                      #'dissoc*
                      keys
                      :initial-value root))
          (setf count (- count removed))))))
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
