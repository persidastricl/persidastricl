;;; -----
;;;
;;;  Copyright (c) 2019-2023 Michael D Pendergrass, pupcus.org
;;;
;;;  This program and the accompanying materials are made
;;;  available under the terms of the Eclipse Public License 2.0
;;;  which is available at https://www.eclipse.org/legal/epl-2.0/
;;;
;;;  SPDX-License-Identifier: EPL-2.0
;;;
;;; -----

;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; when-first.lisp
;;;
;;; -----

(in-package :persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(defmacro when-first (bindings &rest body)
  (assert (and (= 1 (count bindings))
               (= 2 (count (first bindings)))))
  (dlet (([x xs] (first  bindings)))
    (let ((ss (gensym)))
      `(let ((,ss (seq ,xs)))
         (when ,ss
           (let ((,x (first ,ss)))
             ,@body))))))
