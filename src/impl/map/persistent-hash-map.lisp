;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-map.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; persistent-hash-map
;;
;;  an implementation of a persistent/immutable hashed-array-mapped-trie (hamt)
;; -----

(define-immutable-class persistent-hash-map (hash-map)
  ((root :type persistent-hash-map-node :initarg :root :reader root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'persistent-hash-map-node) :meta nil))

(defmethod assoc ((phm persistent-hash-map) k v &rest kv-pairs)
  (with-slots (root meta) phm
    (let ((new-root (cl:reduce
                     (lambda (node kv-pair)
                       (let* ((entry (apply #'map-entry kv-pair))
                              (kk (key entry)))
                         (when (keywordp kk) (make-funcallable-keyword kk))
                         (add node entry :hash (h:hash kk) :depth 0)))
                     (->list (partition-all (list* k v kv-pairs) 2))
                     :initial-value root)))
      (if (eq new-root root)
          phm
          (make-instance 'persistent-hash-map :root new-root :meta meta)))))

(defmethod dissoc ((phm persistent-hash-map) &rest keys)
  (with-slots (root meta) phm
    (let ((new-root (cl:reduce
                     (lambda (node k)
                       (remove node k :hash (h:hash k) :depth 0))
                     keys
                     :initial-value root)))
      (if (== new-root root)
          phm
          (make-instance 'persistent-hash-map :root new-root :meta meta)))))

(defun persistent-hash-map (&rest kvs)
  (let ((m (make-instance 'persistent-hash-map)))
    (if-not (empty? kvs)
            (apply #'assoc m kvs)
            m)))
;;
;; for a re-readable representation use pr (todo)
;;
(defmethod print-object ((object persistent-hash-map) stream)
  (if-let ((seq (seq object)))
    (let ((more? (first (drop *print-hamt-items* seq)))
          (items (into '() (take (* 2 *print-hamt-items*) (->plist object)))))
      (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
          (format stream "{~{~s~^ ~}~@[ ...~]}" items more?)
          (format stream "(persidastricl:persistent-hash-map ~{~s~^ ~}~@[ ...~])" items more?)))
    (format stream "{}")))

(defmethod make-load-form ((object persistent-hash-map) &optional env)
  (declare (ignore env))
  (let ((items (into '() (->plist object))))
    `(persidastricl:persistent-hash-map ,@items)))
