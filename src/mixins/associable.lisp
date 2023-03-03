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
  (gethash k m default))

(defmethod get ((ht hash-table) k &optional default)
  (gethash k m default))

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

(defmethod print-object ((object hash-table) stream)
  (let ((more? (first (drop *print-hamt-items* (seq object))))
        (items (into '() (take (* 2 *print-hamt-items*) (->plist object)))))
    (format stream "%{~{~s~^ ~}~@[ ...~]}" items more?)))
