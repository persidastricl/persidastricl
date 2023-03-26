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
;;;   transient-bitmap-vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; transient-bitmap-vector object
;;
;; -----

(defclass transient-bitmap-vector (bitmap-vector) ())

(defmethod ins ((bv transient-bitmap-vector) bit-position item)
  "inserts the `item` in the data vector at the calculated index
position determinded by the bit-count of bit-position with respect to
all bits currently set below it in the bitmap."
  (with-slots (bitmap data) bv
    (setf bitmap (b:set bit-position bitmap))
    (setf data (v:insert data (b:index bit-position bitmap) item)))
  bv)

(defmethod upd ((bv transient-bitmap-vector) bit-position item)
  "updates the `item in the data vector at the calculated index position
determinded by the bit-count of bit-position with respect to all bits
currently set below it in the bitmap."
  (with-slots (bitmap data) bv
    (v:modify data (b:index bit-position bitmap) item))
  bv)

(defmethod del ((bv transient-bitmap-vector) bit-position)
  "removes the `item` from the data vector at the calculated index
position determinded by the bit-count of bit-position with respect to
all bits currently set below it in the bitmap."
  (with-slots (bitmap data) bv
    (setf bitmap (b:clear bit-position bitmap))
    (setf data (v:delete data (b:index bit-position bitmap))))
  bv)

(defun empty-transient-bitmap-vector ()
  (make-instance 'transient-bitmap-vector))
