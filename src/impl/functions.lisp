;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   impl/functions.lisp
;;;
;;; -----

(in-package #:persidastricl)

(labels ((replace* (s pattern replacement)
           (cl-ppcre:regex-replace pattern s replacement :preserve-case t :simple-calls t))
         (includes*? (s subs-or-regex)
           (cl-ppcre:scan-to-strings subs-or-regex s)))

  (defun persistent->transient-name (persistent-object)
    (let ((object-name (str (type-of persistent-object))))
      (unless (includes*? object-name "(?i)persistent")
        (error "object ~a is not a persistent object!" object-name))
      (-> object-name
        (replace* "(?i)persistent" "transient")
        read-from-string)))

  (defun transient->persistent-name (transient-object)
    (let ((object-name (str (type-of transient-object))))
      (unless (includes*? object-name "(?i)transient")
        (error "object ~a is not a transient object!" object-name))
      (-> object-name
        (replace* "(?i)transient" "persistent")
        read-from-string))))

(defun transient! (obj)
  "create a new transient object copy of the type of persistent object given without modifying the persistent object"
  (let ((lst (->list obj)))
    (-> (persistent->transient-name obj)
      (apply lst))))
