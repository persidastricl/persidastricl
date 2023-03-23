;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; bpvt.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defvar *print-bpvt-items* 100)

(defclass bpvt (counted collection seqable)
  ((root :initarg :root :reader root)
   (tail-end :initarg :tail-end :reader tail-end)
   (tail-offset :initarg :tail-offset :reader tail-offset)))

(defmethod cl-murmurhash:murmurhash ((object bpvt) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (->list object) :seed seed :mix-only mix-only))
