;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   bitop.lisp
;;;
;;; bit operations needed for persidastricl
;;;
;;; -----

(in-package #:bits)

(proclaim '(inline set? set clear bits below index))

(defun set? (bit-position bitmap)
  "Is the `bit-position` bit on?"
  (> (logand (ash 1 bit-position) bitmap) 0))

(defun set (bit-position bitmap)
  "Set the `bit-position` bit `on`"
  (logior (ash 1 bit-position) bitmap))

(defun clear (bit-position bitmap)
  "Set the `bit-position` bit off"
  (if (set? bit-position bitmap)
      (logxor (ash 1 bit-position) bitmap)
      bitmap))

(defparameter *default-hash-slice-bit-size* 5)

(defun bits (hash depth &optional (size *default-hash-slice-bit-size*))
  "Given a `hash`, break the `hash` into `size` bit slices and return
the `size` bits at the `depth` slice"
  (ldb (byte size (* size depth)) hash))

(defun below (bit-position bitmap)
  "Return bitmap of all bits below `bit-position` (exclusive)"
  (ldb (byte bit-position 0) bitmap))

(defun index (bit-position bitmap)
  "Determine the number of `1` or `on` bits below
bit-position (exclusive) (which will be the index into the data array
of a bitmap-indexed-vector (biv) used in HAMT)"
  (logcount (below bit-position bitmap)))