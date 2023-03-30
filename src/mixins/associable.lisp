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
;;;   associable.lisp
;;;
;;; marker class for an associable data structure
;;;
;;; -----

(in-package #:persidastricl)

(defclass associable () ())

(defgeneric assoc (associable k v &rest kv-pairs))
(defgeneric dissoc (associable &rest keys))
(defgeneric lookup (associable k &optional default))
(defgeneric get (associable k &optional default))

(defmethod assoc ((lst list) k1 v1 &rest kv-pairs)
  (labels ((assoc* (l k v)
             (check-type k integer)
             (let ((start (->list (take k l))))
               (concatenate 'list start (list* v (->list (drop (1+ k) l)))))))
    (reduce
     (lambda (l kv-pair)
       (apply #'assoc* l kv-pair))
     (->list (partition-all (list* k1 v1 kv-pairs) 2))
     :initial-value lst)))

(defmethod dissoc ((lst list) &rest keys)
  (labels ((dissoc* (l k)
             (check-type k integer)
             (let ((start (->list (take k l))))
               (concatenate 'list start (->list (drop (1+ k) l))))))
    (reduce
     (lambda (l k)
       (dissoc* l k))
     keys
     :initial-value lst)))

(defmethod lookup ((lst list) position &optional default)
  (or (first (drop position lst)) default))

(defmethod get ((lst list) position &optional default)
  (lookup lst position default))

(defmethod empty ((ht hash-table))
  (make-hash-table :test #'equalp))

(defmethod assoc ((ht hash-table) k1 v1 &rest kv-pairs)
  (labels ((assoc* (m k v)
             (setf (gethash k m) v)
             m))
    (reduce-kv
     #'assoc*
     (->list (partition-all (list* k1 v1 kv-pairs) 2))
     :initial-value ht)))

(defmethod dissoc ((ht hash-table) &rest keys)
  (labels ((dissoc* (m k)
             (remhash k m)
             m))
    (cl:reduce
     #'dissoc*
     keys
     :initial-value ht)))

(defmethod lookup ((ht hash-table) k &optional default)
  (gethash k ht default))

(defmethod get ((ht hash-table) k &optional default)
  (gethash k ht default))

(defmethod keys ((ht hash-table))
  (loop for key being the hash-keys of ht collect key))

(defmethod vals ((ht hash-table))
  (loop for v being the hash-values of ht collect v))

(defmethod assoc ((vec array) index value &rest iv-pairs)
  (labels ((assoc* (v i val)
             (check-type i integer)
             (if (= i (length v))
                 (v:append v val)
                 (v:update v i val))))
    (reduce
     (lambda (v iv-pair)
       (apply #'assoc* v iv-pair))
     (->list (partition-all (list* index value iv-pairs) 2))
     :initial-value vec)))

(defmethod dissoc ((vec array) &rest indexes)
  (labels ((dissoc* (v i)
             (check-type i integer)
             (v:delete v i)))
    (reduce
     (lambda (v i)
       (apply #'dissoc* v i))
     indexes
     :initial-value vec)))

(defmethod lookup ((vec array) k &optional default)
  (or (elt vec k) default))

(defmethod get ((vec array) k &optional default)
  (or (elt vec k) default))

(defun funcallable-keyword? (k)
  (handler-case
      (symbol-function k)
    (undefined-function ()
      nil)))

(defun make-funcallable-keyword (k)
  (assert (keywordp k))
  (unless (funcallable-keyword? k)
    (eval `(defun ,k (hm &optional (default nil)) (lookup hm ,k default)))))

(defun make-funcallable-keywords (&rest kws)
  (dolist (k kws) (make-funcallable-keyword k)))

(defun pprint-hash-table (stream ht &rest other-args)
  (declare (ignore other-args))
  (pprint-logical-block (stream (->list ht) :prefix "%{" :suffix "}")
    (pprint-exit-if-list-exhausted)
    (loop
      (pprint-logical-block (stream (pprint-pop))
        (write (pprint-pop) :stream stream)
        (write-char #\space stream)
        (write (pprint-pop) :stream stream))
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (pprint-newline :fill stream))))

(defmethod print-object ((ht hash-table) stream)
  (format stream "~/persidastricl::pprint-hash-table/" ht))

(set-pprint-dispatch 'hash-table 'pprint-hash-table)
