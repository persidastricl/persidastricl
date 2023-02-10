;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-key-value-bitmap-vector.lisp
;;;
;;; -----

(in-package :bitmap-vector)

;; -----
;; persistent-key-value-bitmap-vector object
;;
;; -----

(define-immutable-class persistent-key-value-bitmap-vector (persistent-bitmap-vector key-value-bitmap-vector) ())

;; -----
;; methods
;;
;; -----

(defmethod insert ((bv persistent-key-value-bitmap-vector) bit-position entry)
  "inserts the key/value from `entry` in the data vector at the 2
calculated index positions determinded by the bit-count of
bit-position with respect to all bits currently set below it in the
bitmap. The indexes in the data vector are: (* 2 determined-bit-count)
for key, and (1+ (* 2 determined-bit-count)) for value"
  (let ((bitmap (b:set bit-position (:bitmap bv))))
    (make-instance 'persistent-key-value-bitmap-vector
                   :bitmap bitmap
                   :data (v:insert (:data bv) (* (b:index bit-position bitmap) 2) (e:key entry) (e:value entry)))))

(defmethod update ((bv persistent-key-value-bitmap-vector) bit-position entry)
  "updates the value from `entry` in the data vector at the calculated
index position determinded by the bit-count of bit-position with
respect to all bits currently set below it in the bitmap. The indexes
in the data vector is: (1+ (* 2 determined-bit-count)) for v"
  (let ((bitmap (:bitmap bv)))
    (make-instance 'persistent-key-value-bitmap-vector
                   :bitmap bitmap
                   :data (v:update (:data bv) (1+ (* (b:index bit-position bitmap) 2)) (e:value entry)))))

(defmethod remove ((bv persistent-key-value-bitmap-vector) bit-position)
  "removes the key/value from `entry` in the data vector at the 2
calculated index positions determinded by the bit-count of
bit-position with respect to all bits currently set below it in the
bitmap. The indexes in the data vector are: (* 2 determined-bit-count)
for key, and (1+ (* 2 determined-bit-count)) for value"
  (let ((bitmap (b:clear bit-position (:bitmap bv))))
    (make-instance 'persistent-key-value-bitmap-vector
                   :bitmap bitmap
                   :data (v:delete (:data bv) (* (b:index bit-position bitmap) 2) 2))))

(defun EMPTY-PERSISTENT-KEY-VALUE-BITMAP-VECTOR ()
  (make-instance 'persistent-key-value-bitmap-vector))
