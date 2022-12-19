;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hash.lisp
;;;
;;; hashing function
;;;
;;;  currently it is murmur hashing but this could change (ie. xxhash)
;;;
;;; -----

(in-package #:hash)

(defun hash (item)
  (cl-murmurhash:murmurhash item))
