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

(defgeneric count (thing)
  (:method (thing) (cl:length thing)))

(defgeneric length (thing)
  (:method (thing) (cl:length thing)))

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

(defgeneric ->array (object)
  (:method (object) (make-array (length object) :initial-contents object)))

(defgeneric ->vector (object)
  (:method (object) (->array object)))

(defgeneric ->vec (object)
  (:method (object) (->array object)))

(defgeneric ->plist (object))
(defgeneric ->alist (object))

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
