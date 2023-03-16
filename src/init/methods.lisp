;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   methods.lisp
;;;
;;; generic functions
;;;
;;; -----

(in-package #:persidastricl)

(defgeneric empty (object)
  (:documentation "return an empty data-object of the same type as the original object argument")
  (:method ((lst list)) '())
  (:method ((object t)) (make-instance (type-of object))))

(defgeneric empty? (thing)
  (:method ((thing (eql nil))) t)
  (:method ((a array)) (zerop (length a)))
  (:method ((l list)) (zerop (length l))))

(labels
    ((count* (thing &optional (n 0))
       (cond
         ((null thing) n)
         ((dotted-pair? thing) (+ n 2))
         (:otherwise (count* (rest thing) (1+ n))))))
  (defgeneric count (thing)
    (:method (thing) (count* thing))
    (:method ((ht hash-table)) (hash-table-count ht))))

(defgeneric length (thing)
  (:method (thing) (cl:length thing))
  (:method ((ht hash-table)) (hash-table-count ht)))

(defgeneric bounded-count (n thing))

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

(defgeneric second (thing)
  (:method (thing) (first (rest thing))))

(defgeneric third (thing)
  (:method (thing) (first (rest (rest thing)))))

(defgeneric nth (thing n &optional default)
  (:method (thing n &optional (default nil)) (or (cl:nth n thing) default))
  (:method ((l list) n &optional (default nil)) (or (cl:nth n l) default))
  (:method ((s sequence) n &optional (default nil)) (nth (coerce s 'list) n default)))

(defgeneric next (thing)
  (:method (thing) (cl:rest thing))
  (:method ((lst list)) (cl:rest lst))
  (:method ((seq sequence)) (rest (coerce seq 'list))))

(defgeneric last (thing)
  (:method (thing) (cl:last thing)))

(defgeneric butlast (thing &optional n)
  (:method (thing &optional (n 1)) (cl:butlast thing n)))

(defgeneric head (obj)
  (:method (obj) (first obj))
  (:method ((seq sequence)) (first (coerce seq 'list))))

(defgeneric tail (obj)
  (:method (obj) (rest obj))
  (:method ((seq sequence)) (rest (coerce seq 'list))))

(defgeneric ->list (object)
  (:method ((lst list)) lst)
  (:method ((seq sequence)) (coerce seq 'list)))

(defmethod ->list ((ht hash-table))
  (loop for v being each hash-values of ht using (hash-key k)
        collect (list  k v)))

(defgeneric ->array (object)
  (:method (object) (make-array (length object) :initial-contents object))
  (:method ((ht hash-table)) (make-array (length ht) :initial-contents (cl:map 'list #'cl:vector (->list ht)))))

(defgeneric ->vector (object)
  (:method (object) (->array object)))

(defgeneric ->plist (object)
  (:method ((ht hash-table)) (apply #'concatenate 'list (->list ht))))

(defgeneric ->alist (object)
  (:method ((ht hash-table)) (loop for v being each hash-values of ht using (hash-key k)
                                   collect (cons k v))))

(defgeneric ins (target position item))
(defgeneric upd (target position item))
(defgeneric del (target position))

(defgeneric at-index (target index))
(defgeneric at-position (target position))

(defgeneric add (target item &rest args))
(defgeneric loc (target item &rest args))
(defgeneric remove (target item &rest args))

(defgeneric pop (target)
  (:method ((lst list)) (cl:pop lst) lst)
  (:method ((v array)) (if (array-has-fill-pointer-p v)
                           (progn (vector-pop v) v)
                           (pop (->fillable v)))))

(defgeneric peek (coll)
  (:method ((lst list)) (first lst))
  (:method ((v array)) (elt v (1- (length v)))))
