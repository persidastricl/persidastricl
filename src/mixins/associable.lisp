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

;; -----
;;  do we really need these for lists and vectors
;;  i.e. will we ever call them (I am thinking yes with update-in and assoc-in ???)

(defmethod assoc ((lst list) k1 v1 &rest kv-pairs)
  (labels ((assoc* (l k v)
             (check-type k integer)
             (let ((start (->list (take k l))))
               (concatenate 'list start (list* v (->list (drop (1+  k) l)))))))
    (reduce
     (lambda (l kv-pair)
       (apply #'assoc* l kv-pair))
     (->list (partition-all (list* k1 v1 kv-pairs) 2))
     :initial-value lst)))

(defmethod lookup ((lst list) k1 &optional default)
  (or (first (drop k1 lst)) default))

(defmethod get ((lst list) position &optional default)
  (lookup lst position default))

(defmethod assoc ((vec array) index value &rest iv-pairs)
  (labels ((assoc* (v i val)
             (check-type i integer)
             (v:update v i val)))
    (reduce
     (lambda (v iv-pair)
       (apply #'assoc* v iv-pair))
     (->list (partition-all (list* index value iv-pairs) 2))
     :initial-value vec)))

(defun funcallable-keyword? (k)
  (handler-case
      (symbol-function k)
    (undefined-function ()
      nil)))

(defun make-funcallable-keyword (k)
  (assert (keywordp k))
  (when-not (funcallable-keyword? k)
    (eval `(defun ,k (hm &optional (default nil)) (lookup hm ,k default)))))


(defun make-funcallable-keywords (&rest kws)
  (dolist (k kws) (make-funcallable-keyword k)))
