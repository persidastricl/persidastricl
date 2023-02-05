;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   vector.lisp
;;;
;;; vector operations for data vectors in the hamt bitmap-data classes
;;;
;;; -----

(in-package #:vector)

(defun append (v &rest items)
  "non-destructively append `items` onto the end of vector `v`"
  (let* ((n (length items))
         (len (length v))
         (new (make-array (+ len n))))
    (loop for i from 0 below len
          do
             (setf (elt new i) (elt v i)))
    (loop for i from len below (+ len n)
          do
             (setf (elt new i) (elt items (- i len))))
    new))

(defun insert (v index &rest items)
  "non-destructively insert `items` into a vector `v` at the given
`index`; elements after the inserted `items` at `index` are now shifted
by (length items)"
  (let* ((n (length items))
         (len (length v))
         (new (make-array (+ len n))))
    (loop for i from 0 below index
          do
             (setf (elt new i) (elt v i)))
    (loop for i from index below (+ index n)
          do
             (setf (elt new i) (elt items (- i index))))
    (loop for i from index below len
          do
             (setf (elt new (+ n i)) (elt v i)))
    new))

(defun update (v index &rest items)
  "non-destructively update a vector `v` at `index` with the new `items`"
  (let ((len (length v)))
    (assert (<= index (1- len)))
    (let* ((n (length items))
           (len (max len (+ index n)))
           (new (make-array len)))
      (loop for i from 0 below index
            do
               (setf (elt new i) (elt v i)))
      (loop for i from index below (+ index n)
            do
               (setf (elt new i) (elt items (- i index))))
      (loop for i from (+ index n) below len
            do
               (setf (elt new i) (elt v i)))
      new)))

(defun delete (v index &optional (n 1))
  "non-destructively delete from vector `v` at `index`, shifting all
elements after `index` by -1"
  (let ((len (length v)))
    (assert (<= index (1- len)))
    (let* ((n (min n (- len index)))
           (new (make-array (- len n))))
      (loop for i from 0 below index
            do
               (setf (elt new i) (elt v i)))
      (loop for i from (+ index n) below len
            do
               (setf (elt new (- i n)) (elt v i)))
      new)))

;;
;; destructive update
;;

(defun modify (v index &rest items)
  "destructively update a vector `v` at `index` with the new `items`"
  (let ((n-items (length items)))
    (assert (<= index (- (length v) n-items)))
    (loop for i from 0 below n-items
          do
             (setf (elt v (+ index i)) (elt items i)))
    v))
