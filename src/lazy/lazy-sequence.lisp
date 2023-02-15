;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   thunk.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defvar *print-lazy-items* 5)

(defgeneric head (obj)
  (:method (obj) (first obj))
  (:method ((seq sequence)) (first (coerce seq 'list))))

(defgeneric tail (obj)
  (:method (obj) (rest obj))
  (:method ((seq sequence)) (rest (coerce seq 'list))))

;; -----
;; lazy-sequence
;;
;; -----

(defclass lazy-sequence ()
  ((head :initarg :head :reader :head)
   (tail :initarg :tail :reader :tail)))

(defmethod print-object ((object lazy-sequence) stream)
  (let ((items (->list (take *print-lazy-items* object)))
        (more (first (drop *print-lazy-items* object))))
    (format stream "(~{~s~^ ~}~@[ ...~])" items more)))

(defmacro lseq (head tail)
  `(make-instance 'lazy-sequence :head ,head :tail (delay ,tail)))

(defgeneric lazy-seq (obj)
  (:method ((obj function)) (let ((head (funcall obj))) (lseq head (lazy-seq obj))))
  (:method ((obj list)) (lseq (head obj) (lazy-seq (rest obj))))
  (:method ((obj lazy-sequence)) obj))

(defmethod head ((seq lazy-sequence))
  (:head seq))

(defmethod tail ((seq lazy-sequence))
  (when-let ((tail (:tail seq))) (force tail)))

(defgeneric seq (object)
  (:method ((object list)) (lazy-seq object))
  (:method ((object sequence)) (seq (coerce object 'list)))
  (:method ((object lazy-sequence)) object))
