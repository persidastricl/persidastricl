;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   key-value-bitmap-vector.lisp
;;;
;;; -----

(in-package #:bitmap-vector)

;; -----
;;  mixin class for pulling key-values out of the bitmap vectors
;;
;; -----

(defclass key-value-bitmap-vector (bitmap-vector) ())

(defmethod at-index ((bv key-value-bitmap-vector) index)
  (with-slots (data) bv
    (when (> (length data) index)
      (let ((idx (* 2 index)))
        (e:map-entry (elt data idx) (elt data (1+ idx)))))))

(defmethod at-position ((bv key-value-bitmap-vector) position)
  (when (is-set bv position)
    (let ((index (b:index position (:bitmap bv))))
      (at-index bv index))))
