;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-set.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; persistent-hash-set
;;
;;  an implementation of a persistent/immutable hashed-array-mapped-trie (hamt)
;; -----

(define-immutable-class persistent-hash-set (hash-set)
  ((root :type persistent-hash-set-node :initarg :root :reader root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'persistent-hash-set-node) :meta nil))

(defmethod conj ((phs persistent-hash-set) &rest items)
  (with-slots (root meta) phs
    (let ((new-root (cl:reduce
                     (lambda (node item)
                       (add node item :hash (h:hash item) :depth 0))
                     items
                     :initial-value root)))
      (if (== new-root root)
          phs
          (make-instance 'persistent-hash-set :root new-root :meta meta)))))

(defmethod disj ((phs persistent-hash-set) &rest items)
  (with-slots (root meta) phs
    (let ((new-root  (cl:reduce
                      (lambda (node item)
                        (remove node item :hash (h:hash item) :depth 0))
                      items
                      :initial-value root)))
      (if (== new-root root)
          phs
          (make-instance 'persistent-hash-set :root new-root :meta meta)))))

(defun persistent-hash-set (&rest items)
  (let ((s (make-instance 'persistent-hash-set)))
    (if-not (empty? items)
            (apply #'conj s items)
            s)))

(defun pprint-persistent-hash-set (stream phs &rest other-args)
  (declare (ignore other-args))
  (pprint-logical-block (stream (->list phs) :prefix "#{" :suffix "}")
    (pprint-exit-if-list-exhausted)
    (loop
      (write (pprint-pop) :stream stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (pprint-newline :fill stream))))

(defmethod print-object ((object persistent-hash-set) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "~/persidastricl::pprint-persistent-hash-set/" object)
      (format stream "(persidastricl:persistent-hash-set ~{~s~^ ~})" (->list object))))

(set-pprint-dispatch 'persistent-hash-set 'pprint-persistent-hash-set)

(defmethod make-load-form ((object persistent-hash-set) &optional env)
  (declare (ignore env))
  (let ((items (->list object)))
    `(persidastricl:persistent-hash-set ,@items)))

(defun set (object)
  (into (persistent-hash-set) (seq object)))
