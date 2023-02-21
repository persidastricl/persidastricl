;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   atom.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; atom
;;
;; -----

(stmx:transactional
    (defclass atom ()
      ((value :initarg :value :reader :value))
      (:default-initargs :value nil)))

(defun atom (&optional value)
  (make-instance 'atom :value value))

(defun swap! (atom fn &rest args)
  (stmx:atomic
   (setf (slot-value atom 'value) (apply fn (slot-value atom 'value) args))))

(defun reset! (atom new-value)
  (stmx:atomic
   (setf (slot-value atom 'value) new-value)))

(defgeneric deref (thing)
  (:method ((atom atom)) (stmx:atomic (slot-value atom 'value))))
