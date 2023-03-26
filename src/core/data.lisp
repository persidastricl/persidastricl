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
;;; data.lisp
;;;
;;; -----

(in-package :data)

(named-readtables:in-readtable persidastricl:syntax)

(defgeneric diff (d1 d2)
  (:method (d1 d2) (if (== d1 d2) [nil nil d2] [d1 d2 nil])))

(defmethod diff ((s1 sequence) (s1 sequence))
  (into [] ( )))
