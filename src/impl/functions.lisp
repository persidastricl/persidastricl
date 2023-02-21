;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   impl/functions.lisp
;;;
;;; -----

(in-package #:persidastricl)

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

(defun transient! (obj)
  "create a new transient object copy of the type of persistent object given without modifying the persistent object"
  (let ((lst (->list obj)))
    (-> (persistent->transient-name obj)
      (apply lst))))
