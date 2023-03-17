;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; data.lisp
;;;
;;; -----

(in-package :data)

(named-readtables:in-readtable persidastricl:syntax)

(defgeneric diff (d1 d2)
  (:method (d1 d2) (if (== d1 d2) [nil nil d2] [d1 d2 nil])))

(defmethod diff ((s1 sequence) (s1 sequence))
  (into [] ( )))
