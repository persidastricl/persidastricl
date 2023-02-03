;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   thunk.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defgeneric head (obj)
  (:method (obj) (car obj)))

(defgeneric tail (obj)
  (:method (obj) (cdr obj)))

;; -----
;; thunk
;;
;; -----

(defclass thunk ()
  ((fn :initarg :fn :accessor :fn)
   (r  :initarg :r  :accessor :r))
  (:default-initargs :r nil))

(defmacro delay (&rest body)
  `(make-instance 'thunk :fn (lambda () ,@body)))

(defgeneric force (obj)
  (:method (obj) obj))

(defmethod force ((thunk thunk))
  (if (and (slot-boundp thunk 'fn) (functionp (:fn thunk)))
      (let ((values (multiple-value-list (funcall (:fn thunk)))))
        (setf (:r thunk) values)
        (slot-makunbound thunk 'fn)))
  (values-list (:r thunk)))

(defclass lazy-sequence ()
  ((head :initarg :head :reader :head)
   (tail :initarg :tail :reader :tail)))

(defmethod print-object ((object lazy-sequence) stream)
  (format stream "(~a ...)" (:head lazy-sequence)))

(defmacro lazy-seq (head tail)
  `(make-instance 'lazy-sequence :head ,head :tail (delay ,tail)))

(defmethod seq ((object lazy-sequence)) object)

(defmethod ->seq ((s lazy-sequence))
  (cl:cons (head s) (->seq (tail s))))

(defmethod head ((seq lazy-sequence))
  (:head seq))

(defmethod tail ((seq lazy-sequence))
  (force (:tail seq)))

(defun ensure-seq (s)
  (if (consp s) s (seq s)))

