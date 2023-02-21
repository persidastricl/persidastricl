;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   bitmap-vector.lisp
;;;
;;; base class for tracking a bitmap and an associated vector of data
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; bitmap-vector object
;;
;; models the HAMT idea of a n-bit bitmap (default 32 bits)
;; representing items in a data vector (where an 'on' or 'set' bit in
;; the bitmap means data is present in the vector) and the data vector
;; containing those items ordered and indexed the same as the order of
;; bits in the bitmap
;;
;; -----

(defclass bitmap-vector ()
  ((bitmap :initarg :bitmap :reader :bitmap :type unsigned-integer)
   (data :initarg :data :reader :data))
  (:default-initargs :bitmap 0 :data (make-array 0)))

(defun is-set (bv position)
  "is the bit set in the `bitmap` at `position`"
  (b:set? position (:bitmap bv)))

;; -----
;;  methods
;;
;; -----

(defmethod at-index ((bv bitmap-vector) index)
  "return the item at the `index` in `data` vector"
  (elt (:data bv) index))

(defmethod at-position ((bv bitmap-vector) position)
  "if the `bitmap` is set at `position` return the `data` element at the
relative index offset determined by the bit `position` in relation to
all bits currently set in the `bitmap`"
  (when (is-set bv position)
    (elt (:data bv) (b:index position (:bitmap bv)))))

(defmethod count ((bv bitmap-vector))
  (logcount (:bitmap bv)))
