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
;;;   persistent-bitmap-vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; persistent-bitmap-vector object
;;
;; -----

(define-immutable-class persistent-bitmap-vector (bitmap-vector) ())

(defmethod ins ((bv persistent-bitmap-vector) bit-position item)
  "inserts the `item` in the data vector at the calculated index
position determinded by the bit-count of bit-position with respect to
all bits currently set below it in the bitmap. "
  (let ((bitmap (b:set bit-position (bitmap bv))))
    (make-instance (type-of bv)
                   :bitmap bitmap
                   :data (v:insert (data bv) (b:index bit-position bitmap) item))))

(defmethod upd ((bv persistent-bitmap-vector) bit-position item)
  "updates the `item in the data vector at the calculated index position
determinded by the bit-count of bit-position with respect to all bits
currently set below it in the bitmap"
  (let ((bitmap (bitmap bv)))
    (make-instance (type-of bv)
                   :bitmap bitmap
                   :data (v:update (data bv) (b:index bit-position bitmap) item))))

(defmethod del ((bv persistent-bitmap-vector) bit-position)
  "removes the `item` from the data vector at the calculated index
position determinded by the bit-count of bit-position with respect to
all bits currently set below it in the bitmap."
  (let ((bitmap (b:clear bit-position (bitmap bv))))
    (make-instance (type-of bv)
                   :bitmap bitmap
                   :data (v:delete (data bv) (b:index bit-position bitmap)))))

(defun empty-persistent-bitmap-vector ()
  (make-instance 'persistent-bitmap-vector))
