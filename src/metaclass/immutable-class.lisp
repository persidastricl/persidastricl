;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   immutable-class.lisp
;;;
;;; metaobject class to enforce immutable CLOS classes
;;;
;;; -----

(in-package #:persidastricl)

(defclass immutable-class (standard-class) ()
  (:documentation "the class of classes that may not be modified after initialization"))

(defmethod c2mop:validate-superclass ((class immutable-class) (super standard-class)) t)

(define-condition invalid-access-to-immutable-object (error)
  ((slot-name :initarg :slot-name :reader slot-name)
   (instance  :initarg :instance :reader instance))
  (:report (lambda (condition stream)
             (format
              stream
              "attempt to modify slot with name `~A`~%in instance ~S~%an object of type `~S`~%which is a class defined with the metaclass `~S`~%and cannot be changed once bound!"
              (slot-name condition)
              (instance condition)
              (type-of (instance condition))
              (class-name (find-class 'immutable-class))))))

(defmethod (setf c2mop:slot-value-using-class) :before (value (class immutable-class) instance slot-definition)
  (let ((slot-name (c2mop:slot-definition-name slot-definition)))
    (when (slot-boundp instance slot-name)
      (error (make-condition 'invalid-access-to-immutable-object :slot-name slot-name :instance instance) ))))

(defmacro define-immutable-class (class supers slots &rest options)
  (when (cl:assoc ':metaclass options)
    (error "Defining an immutable class with a metaclass?"))
  `(defclass ,class ,supers ,slots
     ,@options
     (:metaclass immutable-class)))
