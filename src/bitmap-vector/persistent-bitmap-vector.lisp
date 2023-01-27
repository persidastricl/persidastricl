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

;; -----
;;  methods
;;
;; -----

(defmethod insert ((bv persistent-bitmap-vector) bit-position item)
  "inserts the `item` in the data vector at the calculated index
position determinded by the bit-count of bit-position with respect to
all bits currently set below it in the bitmap. "
  (let ((bitmap (b:set bit-position (:bitmap bv))))
    (make-instance (type-of bv)
                   :bitmap bitmap
                   :data (v:insert (:data bv) (b:index bit-position bitmap) item))))

(defmethod update ((bv persistent-bitmap-vector) bit-position item)
  "updates the `item in the data vector at the calculated index position
determinded by the bit-count of bit-position with respect to all bits
currently set below it in the bitmap"
  (let ((bitmap (:bitmap bv)))
    (make-instance (type-of bv)
                   :bitmap bitmap
                   :data (v:update (:data bv) (b:index bit-position bitmap) item))))

(defmethod remove ((bv persistent-bitmap-vector) bit-position)
  "removes the `item` from the data vector at the calculated index
position determinded by the bit-count of bit-position with respect to
all bits currently set below it in the bitmap."
  (let ((bitmap (b:clear bit-position (:bitmap bv))))
    (make-instance (type-of bv)
                   :bitmap bitmap
                   :data (v:delete (:data bv) (b:index bit-position bitmap)))))

(defun EMPTY-PERSISTENT-BITMAP-VECTOR ()
  (make-instance 'persistent-bitmap-vector))
