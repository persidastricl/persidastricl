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
;;;   key-value-bitmap-vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  mixin class for pulling key-values out of the bitmap vectors
;;
;; -----

(defclass key-value-bitmap-vector (bitmap-vector) ())

(defmethod at-index ((bv key-value-bitmap-vector) index)
  (with-slots (data) bv
    (when (> (length data) index)
      (let ((idx (* 2 index)))
        (map-entry (elt data idx) (elt data (1+ idx)))))))

(defmethod at-position ((bv key-value-bitmap-vector) position)
  (when (is-set bv position)
    (let ((index (b:index position (bitmap bv))))
      (at-index bv index))))
