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

;; -----
;; methods
;;
;; -----

(defmethod insert ((bv transient-bitmap-vector) bit-position item)
  "inserts the `item` in the data vector at the calculated index
position determinded by the bit-count of bit-position with respect to
all bits currently set below it in the bitmap."
  (with-slots (bitmap data) bv
    (setf bitmap (b:set bit-position bitmap))
    (setf data (v:insert data (b:index bit-position bitmap) item)))
  bv)

(defmethod update ((bv transient-bitmap-vector) bit-position item)
  "updates the `item in the data vector at the calculated index position
determinded by the bit-count of bit-position with respect to all bits
currently set below it in the bitmap."
  (with-slots (bitmap data) bv
    (setf data (v:update data (b:index bit-position bitmap) item)))
  bv)

(defmethod remove ((bv transient-bitmap-vector) bit-position)
  "removes the `item` from the data vector at the calculated index
position determinded by the bit-count of bit-position with respect to
all bits currently set below it in the bitmap."
  (with-slots (bitmap data) bv
    (setf bitmap (b:clear bit-position bitmap))
    (setf data (v:delete data (b:index bit-position bitmap))))
  bv)

(defun EMPTY-TRANSIENT-BITMAP-VECTOR ()
  (make-instance 'transient-bitmap-vector))
