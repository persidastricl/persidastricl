;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-hash-map.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; transient-hash-map
;;
;;  an implementation of a transient/imperative hashed-array-mapped-trie (hamt)
;; -----

(defclass transient-hash-map (hash-map)
  ((root :type transient-hash-map-node :initarg :root :reader root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'transient-hash-map-node) :meta nil))

(defmethod assoc ((thm transient-hash-map) k v &rest kv-pairs)
  (with-slots (root) thm
    (setf root (cl:reduce
                (lambda (node kv-pair)
                  (let* ((entry (apply #'map-entry kv-pair))
                         (kk (key entry)))
                    (when (keywordp kk) (make-funcallable-keyword kk))
                    (add node entry :hash (h:hash kk) :depth 0)))
                (->list (partition-all (list* k v  kv-pairs) 2))
                :initial-value root)))
  thm)

(defmethod dissoc ((thm transient-hash-map) &rest keys)
  (with-slots (root) thm
    (setf root (cl:reduce
                (lambda (node k)
                  (remove node k :hash (h:hash k) :depth 0))
                keys
                :initial-value root)))
  thm)

(defun transient-hash-map (&rest kvs)
  (let ((m (make-instance 'transient-hash-map)))
    (unless (empty? kvs)
      (apply #'assoc m kvs))
    m))

(defun pprint-transient-hash-map (stream thm &rest other-args)
  (declare (ignore other-args))
  (let ((*print-length* (min *print-hamt-items* (or *print-lines* *print-hamt-items*))))
    (pprint-logical-block (stream (->list (take (inc *print-length*) (seq thm))) :prefix "@{" :suffix "}")
      (pprint-exit-if-list-exhausted)
      (loop
        (write (pprint-pop) :stream stream)
        (pprint-exit-if-list-exhausted)
        (write-char #\space stream)
        (pprint-newline :fill stream)))))

(defmethod print-object ((thm transient-hash-map) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "~/persidastricl::pprint-transient-hash-map/" thm)
      (format stream "(persidastricl:transient-hash-map ~{~s~^ ~})" (into '() (->plist thm)))))

(set-pprint-dispatch 'transient-hash-map 'pprint-transient-hash-map)

(defmethod make-load-form ((object transient-hash-map) &optional env)
  (declare (ignore env))
  (let ((items (into '() (->plist object))))
    `(persidastricl:transient-hash-map ,@items)))
