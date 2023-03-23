;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-key-value-bitmap-vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; transient-key-value-bitmap-vector object
;;
;; -----

(defclass transient-key-value-bitmap-vector (transient-bitmap-vector key-value-bitmap-vector) ())

;; -----
;; methods
;;
;; -----

(defmethod ins ((bv transient-key-value-bitmap-vector) bit-position entry)
  "inserts the key/value from `entry` in the data vector at the 2
calculated index positions determinded by the bit-count of
bit-position with respect to all bits currently set below it in the
bitmap. The indexes in the data vector are: (* 2 determined-bit-count)
for key, and (1+ (* 2 determined-bit-count)) for value"
  (with-slots (bitmap data) bv
    (setf bitmap (b:set bit-position bitmap))
    (setf data (v:insert data (* (b:index bit-position bitmap) 2) (key entry) (value entry))))
  bv)

(defmethod upd ((bv transient-key-value-bitmap-vector) bit-position entry)
  "updates the value from `entry` in the data vector at the calculated
index position determinded by the bit-count of bit-position with
respect to all bits currently set below it in the bitmap. The indexes
in the data vector is: (1+ (* 2 determined-bit-count)) for v"
  (with-slots (bitmap data) bv
    (v:modify data (1+ (* (b:index bit-position bitmap) 2)) (value entry)))
  bv)

(defmethod del ((bv transient-key-value-bitmap-vector) bit-position)
  "removes the key/value from `entry` in the data vector at the 2
calculated index positions determinded by the bit-count of
bit-position with respect to all bits currently set below it in the
bitmap. The indexes in the data vector are: (* 2 determined-bit-count)
for key, and (1+ (* 2 determined-bit-count)) for value"
  (with-slots (bitmap data) bv
    (setf bitmap (b:clear bit-position bitmap))
    (setf data (v:delete data (* (b:index bit-position bitmap) 2) 2)))
  bv)

(defun empty-transient-key-value-bitmap-vector ()
  (make-instance 'transient-key-value-bitmap-vector))
