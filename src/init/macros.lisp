;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   macros.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defmacro while (condition &rest body)
  `(loop while ,condition
         do (progn ,@body)))

(defmacro if-not (pred truthy &optional falsey)
  `(if (not ,pred) ,truthy ,falsey ))

(defmacro when-not (pred &body body)
  `(when (not ,pred)
     ,@body))

(defmacro if-let (bindings then &optional else)
  (let ((binding (first bindings)))
    (assert (= (length binding) 2))
    (let ((var (elt binding 0))
          (form (elt binding 1))
          (temp (gensym)))
      `(let ((,temp ,form))
         (if ,temp
             (let ((,var ,temp))
               ,then)
             ,else)))))

(defmacro when-let (bindings &body body)
  (let ((binding (first bindings)))
    (assert (= (length binding) 2))
    (let ((var (elt binding 0))
          (form (elt binding 1))
          (temp (gensym)))
      `(let ((,temp ,form))
         (when ,temp
           (let ((,var ,temp))
             ,@body))))))

;; (defmacro fn (args &body body)
;;   `(lambda ,args
;;      ,@body))

;; (defmacro named-fn (name args &body body)
;;   `(labels ((,name ,args ,@body))
;;      #',name))

(defmacro fn (name-or-args &rest args-and-or-body)
  (if (not (listp name-or-args))
      `(labels ((,name-or-args ,(first args-and-or-body) ,@(rest args-and-or-body)))
         #',name-or-args)
      `(lambda ,name-or-args
         ,@args-and-or-body)))

(defmacro comment (&body body)
  (declare (ignore body)))

(defmacro def (name value &optional doc-string)
  `(defparameter ,name ,value ,doc-string))
