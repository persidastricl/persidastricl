;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   util.lisp
;;;
;;; -----

(in-package #:util)

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

(defun empty? (sequence)
  (typecase sequence
    (null t)
    (array (zerop (length sequence)))
    (otherwise nil)))

(defun persistent->transient-name (persistent-object)
  (let ((object-name (s:str (type-of persistent-object))))
    (when-not (s:includes? object-name "(?i)persistent")
      (error "object ~a is not a persistent object!" object-name))
    (-> object-name
      (s:replace "(?i)persistent" "transient")
      read-from-string)))

(defun transient->persistent-name (transient-object)
  (let ((object-name (s:str (type-of transient-object))))
    (when-not (s:includes? object-name "(?i)transient")
      (error "object ~a is not a transient object!" object-name))
    (-> object-name
      (s:replace "(?i)transient" "persistent")
      read-from-string)))
