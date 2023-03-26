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

;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   seqable.lisp
;;;
;;; CLOS marker class for collections that are capable of becoming sequences of items
;;;
;;; -----

(in-package #:persidastricl)

(defmethod seq ((it iterator))
  (labels ((seq* (it)
             (when (has-next? it)
               (lseq (next it) (seq* it)))))
    (seq* it)))

(defmethod seq ((object seqable))
  (seq (iterator object)))
