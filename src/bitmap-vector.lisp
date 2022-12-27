;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   bitmap-vector.lisp
;;;
;;; base class for tracking a bitmap and an associated vector of data
;;;
;;; -----

(in-package #:persidastricl)

(defgeneric insert (bv position x))
(defgeneric update (bv position x))
(defgeneric remove (bv position))

(defgeneric at-index (bv index))
(defgeneric at-position (bv position))

;; -----
;; bitmap-vector object
;;
;; models the HAMT idea of a combination of a 32 bit bitmap
;; representing items in a data vector (where an 'on' bit in the
;; bitmap means data is present in the vector) and the data vector
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


;; -----
;; transient-node-bitmap-vector object
;;
;; -----

(defclass transient-node-bitmap-vector (transient-bitmap-vector) ())

;; -----
;;  methods
;;
;; -----

(defmethod insert ((bv transient-node-bitmap-vector) bit-position item)
  "inserts `item` in the `data` vector at the relative index offset
determined by the `bit-position` in relation to all bits currently set
in the `bitmap`"
  (with-slots (bitmap data) bv
    (setf bitmap (b:set bit-position bitmap))
    (setf data (v:insert data (b:index bit-position bitmap) item))
    bv))

(defmethod update ((bv transient-node-bitmap-vector) bit-position item)
  "update `item` in the `data` vector at the relative index offset
determined by the 'bit-position' in relation to all bits currently set
in the `bitmap`"
  (with-slots (bitmap data) bv
    (setf data (v:update data (b:index bit-position bitmap) item))
    bv))

(defmethod remove ((bv transient-node-bitmap-vector) bit-position)
  "remove `item` in the `data` vector at the relative index offset
determined by the `bit-position` in relation to all bits currently set
in the `bitmap` and clear the bit in `bit-position` in the `bitmap`"
  (with-slots (bitmap data) bv
    (setf bitmap (b:clear bit-position bitmap))
    (setf data (v:delete data (b:index bit-position bitmap)))
    bv))

(defun EMPTY-TRANSIENT-NODE-BITMAP-VECTOR ()
  (make-instance 'transient-node-bitmap-vector))


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
    (make-instance 'persistent-bitmap-vector
                   :bitmap bitmap
                   :data (v:insert (:data bv) (b:index bit-position bitmap) item))))

(defmethod update ((bv persistent-bitmap-vector) bit-position item)
  "updates the `item in the data vector at the calculated index position
determinded by the bit-count of bit-position with respect to all bits
currently set below it in the bitmap"
  (let ((bitmap (:bitmap bv)))
    (make-instance 'persistent-bitmap-vector
                   :bitmap bitmap
                   :data (v:update (:data bv) (b:index bit-position bitmap) item))))

(defmethod remove ((bv persistent-bitmap-vector) bit-position)
  "removes the `item` from the data vector at the calculated index
position determinded by the bit-count of bit-position with respect to
all bits currently set below it in the bitmap."
  (let ((bitmap (b:clear bit-position (:bitmap bv))))
    (make-instance 'persistent-bitmap-vector
                   :bitmap bitmap
                   :data (v:delete (:data bv) (b:index bit-position bitmap)))))

(defun EMPTY-PERSISTENT-BITMAP-VECTOR ()
  (make-instance 'persistent-bitmap-vector))

;; -----
;; persistent-node-bitmap-vector object
;;
;; -----

(define-immutable-class persistent-node-bitmap-vector (persistent-bitmap-vector) ())

;; -----
;;  methods
;;
;; -----

(defmethod insert ((bv persistent-node-bitmap-vector) bit-position item)
  "inserts `item` in the `data` vector at the relative index offset
determined by the `bit-position` in relation to all bits currently set
in the `bitmap`"
  (let ((new-bitmap (b:set bit-position (:bitmap bv))))
    (make-instance 'persistent-node-bitmap-vector
                   :bitmap new-bitmap
                   :data (v:insert (:data bv) (b:index bit-position new-bitmap) item))))

(defmethod update ((bv persistent-node-bitmap-vector) bit-position item)
  "update `item` in the `data` vector at the relative index offset
determined by the 'bit-position' in relation to all bits currently set
in the `bitmap`"
  (let ((new-bitmap (b:set bit-position (:bitmap bv))))
    (make-instance 'persistent-node-bitmap-vector
                   :bitmap new-bitmap
                   :data (v:update (:data bv) (b:index bit-position new-bitmap) item))))

(defmethod remove ((bv persistent-node-bitmap-vector) bit-position)
  "remove `item` in the `data` vector at the relative index offset
determined by the `bit-position` in relation to all bits currently set
in the `bitmap` and clear the bit in `bit-position` in the `bitmap`"
  (let ((new-bitmap (b:clear bit-position (:bitmap bv))))
    (make-instance 'persistent-node-bitmap-vector
                   :bitmap new-bitmap
                   :data (v:delete (:data bv) (b:index bit-position new-bitmap)))))

(defun EMPTY-PERSISTENT-NODE-BITMAP-VECTOR ()
  (make-instance 'persistent-node-bitmap-vector))

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


;; TODO:  change-class from persistent->transient
;; TODO:  change-class from transient->persistent
