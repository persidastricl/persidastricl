;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; bpvt.lisp
;;;
;;; -----

(in-package :persidastricl)

(defun ->fillable (array)
  (let ((n (length array)))
    (make-array n :fill-pointer n :initial-contents array)))

(defgeneric pop (coll)
  (:method ((lst list)) (cl:pop lst) lst)
  (:method ((v array)) (if (array-has-fill-pointer-p v)
                           (progn (vector-pop v) v)
                           (pop (->fillable v)))))

(defgeneric peek (coll)
  (:method ((lst list)) (first lst))
  (:method ((v array)) (elt v (1- (length v)))))

(defclass bpvt (counted collection seqable)
  ((root :initarg :root :reader :root)
   (tail :initarg :tail :reader :tail)
   (tail-offset :initarg :tail-offset :reader :tail-offset)))

(defmethod cl-murmurhash:murmurhash ((object bpvt) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (->list object) :seed seed :mix-only mix-only))
