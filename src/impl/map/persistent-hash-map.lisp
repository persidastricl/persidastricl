;;; -----
;;;
;;;  Copyright (c) 2019-2023 Michael D Pendergrass, pupcus.org
;;;
;;;  This program and the accompanying materials are made
;;;  available under the terms of the Eclipse Public License 2.0
;;;  which is available at https://www.eclipse.org/legal/epl-2.0/
;;;
;;;  SPDX-License-Identifier: EPL-2.0
;;;
;;; -----

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

(defun pprint-persistent-hash-map (stream phm &rest other-args)
  (declare (ignore other-args))
  (let ((*print-length* (min *print-hamt-items* (or *print-lines* *print-hamt-items*))))
    (pprint-logical-block (stream (->list (take (inc *print-length*) (seq phm))) :prefix "{" :suffix "}")
      (pprint-exit-if-list-exhausted)
      (loop
        (pprint-logical-block (stream (->list (pprint-pop)))
          (write (pprint-pop) :stream stream)
          (write-char #\space stream)
          (write (pprint-pop) :stream stream))
        (pprint-exit-if-list-exhausted)
        (write-char #\space stream)
        (pprint-newline :fill stream)))))

(defmethod print-object ((phm persistent-hash-map) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "~/persidastricl::pprint-persistent-hash-map/" phm)
      (format stream "(persidastricl:persistent-hash-map ~{~s~^ ~})" (->list (->plist phm)))))

(set-pprint-dispatch 'persistent-hash-map 'pprint-persistent-hash-map)

(defmethod make-load-form ((object persistent-hash-map) &optional env)
  (declare (ignore env))
  (let ((items (into '() (->plist object))))
    `(persidastricl:persistent-hash-map ,@items)))

(defmethod with-meta ((object persistent-hash-map) (meta hash-map))
  (with-slots (root) object
    (make-instance (type-of object) :root root :meta meta)))
