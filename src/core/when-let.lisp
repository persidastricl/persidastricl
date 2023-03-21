;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; when-let.lisp
;;;
;;; -----

(in-package :persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

;; when-let had to be defined after the dlet macro is defined in destructure.lisp and NOT in the same file as dlet

(defmacro when-first (bindings &rest body)
  (assert (and (= 1 (count bindings))
               (= 2 (count (first bindings)))))
  (dlet (([x xs] (first  bindings)))
        (let ((ss (gensym)))
          `(let ((,ss (seq ,xs)))
             (when ,ss
               (let ((,x (first ,ss)))
                 ,@body))))))
