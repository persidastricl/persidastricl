;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   thunk.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; thunk
;;
;; -----

(defclass thunk ()
  ((fn :initarg :fn)
   (r  :initarg :r))
  (:default-initargs :r nil))

(defmacro delay (&rest body)
  `(make-instance 'thunk :fn (lambda () ,@body)))

(defgeneric force (obj)
  (:method (obj) obj))

;; (defmethod force ((thunk thunk))
;;   (if (and (slot-boundp thunk 'fn) (functionp (:fn thunk)))
;;       (let ((values (multiple-value-list (funcall (:fn thunk)))))
;;         (setf (:r thunk) values)
;;         (slot-makunbound thunk 'fn)))
;;   (values-list (:r thunk)))

;; (defmethod force ((thunk thunk))
;;   (if (functionp (slot-value thunk 'fn))
;;       (let ((values (multiple-value-list (funcall (slot-value thunk 'fn)))))
;;         (setf (slot-value thunk 'r) values)
;;         (setf (slot-value thunk 'fn) nil)))
;;   (values-list (slot-value thunk 'r)))

;; (defmethod force ((thunk thunk))
;;   (if (functionp (:fn thunk))
;;       (let ((values (multiple-value-list (funcall (:fn thunk)))))
;;         (setf (:r thunk) values)
;;         (setf (:fn thunk) nil)))
;;   (values-list (:r thunk)))


;; (defmethod force ((thunk thunk))
;;   (if (functionp (:fn thunk))
;;       (let ((value (funcall (:fn thunk))))
;;         (setf (:r thunk) value)
;;         (setf (:fn thunk) nil)))
;;   (:r thunk))

(defmethod force ((thunk thunk))
  (if (slot-value thunk 'fn)
      (let ((value (funcall (slot-value thunk 'fn))))
        (setf (slot-value thunk 'r) value)
        (setf (slot-value thunk 'fn) nil)))
  (slot-value thunk 'r))
