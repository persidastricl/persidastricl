;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   methods.lisp
;;;
;;; generic functions
;;;
;;; -----

(in-package #:persidastricl)

(defun fnil (f default)
  (lambda (x &rest args)
    (apply f (or x default) args)))

(defun empty? (sequence)
  (typecase sequence
    (null t)
    (array (zerop (length sequence)))
    (otherwise nil)))

(defgeneric cons (se1 se2)
  (:method (se1 se2) (cl:cons se1 se2)))

(defgeneric first (thing)
  (:method (thing) (cl:first thing))
  (:method ((lst list)) (cl:first lst))
  (:method ((seq sequence)) (first (coerce seq 'list))))

(defgeneric rest (thing)
  (:method (thing) (cl:rest thing))
  (:method ((lst list)) (cl:rest lst))
  (:method ((seq sequence)) (rest (coerce seq 'list))))

(defgeneric last (thing)
  (:method (thing) (cl:last thing)))

(defgeneric butlast (thing &optional n)
  (:method (thing &optional (n 1)) (cl:butlast thing n)))

(defgeneric empty (object)
  (:documentation "return an empty data-object of the same type as the original object argument")
  (:method ((object t)) (make-instance (type-of object))))

(defgeneric ->list (object)
  (:method (obj) (when obj (cons (head obj) (->list (tail obj)))))
  (:method ((lst list)) lst)
  (:method ((seq sequence)) (coerce seq 'list))
  (:method ((obj lazy-sequence)) (when (head obj) (cons (head obj) (->list (tail obj))))))

(defgeneric ->array (object)
  (:method (object) (make-array (length object) :initial-contents object))
  (:method ((seq lazy-sequence)) (when seq
                                   (let ((lst (->list seq)))
                                     (make-array (length lst) :initial-contents lst)))))

(defgeneric ->vector (object)
  (:method (object) (->array object))
  (:method ((s lazy-sequence)) (->array s)))

(defgeneric ->vec (object)
  (:method (object) (->array object))
  (:method ((s lazy-sequence)) (->array s)))

(defgeneric ->plist (object))
(defgeneric ->alist (object))

(defmethod first ((seq lazy-sequence))
  (:head seq))

(defmethod rest ((seq lazy-sequence))
  (tail seq))

(defmacro fn (args &body body)
  `(lambda ,args
     ,@body))

(defmacro named-fn (name args &body body)
  `(labels ((,name ,args ,@body))
     #',name))

(defun flatten (x)
  "return list of all leaf nodes in x"
  (labels ((flatten* (x acc)
             (cond ((null x) acc)
                   ((cl:atom x) (cons x acc))
                   (t (flatten* (first x) (flatten* (rest x) acc))))))
    (flatten* x nil)))
