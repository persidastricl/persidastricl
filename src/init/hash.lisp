;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hash.lisp
;;;
;;; -----

(in-package #:hash)

(defclass hasher ()
  ((size :initarg :size)
   (fn :initarg :fn)))

(defgeneric do-hash (fn object &rest args)
  (:method (fn (object t) &rest args) (apply fn object args)))

(defvar murmur32 (make-instance 'hasher
                                :size 32
                                :fn (lambda (obj &rest args)
                                      (let ((cl-murmurhash:*hash-size* 32))
                                        (apply #'do-hash #'cl-murmurhash:murmurhash obj args)))))

(defvar murmur128 (make-instance 'hasher
                                 :size 128
                                 :fn (lambda (obj &rest args)
                                       (let ((cl-murmurhash:*hash-size* 128))
                                         (apply #'do-hash #'cl-murmurhash:murmurhash obj args)))))

(defvar sip-hash64 (make-instance 'hasher
                                  :size 64
                                  :fn (lambda (obj &rest args)
                                        (apply #'do-hash #'sip-hash:hash-64-4-8 (babel:string-to-octets (s:str obj)) 0 0 args
                                               ))))


(defvar sip-hash128 (make-instance 'hasher
                                   :size 128
                                   :fn (lambda (obj &rest args)
                                         (multiple-value-bind (lo hi) (apply
                                                                       #'do-hash
                                                                       #'sip-hash:hash-128-4-8
                                                                       (babel:string-to-octets (s:str obj)) 0 0 args)
                                           (+ (ash hi 64) lo)))))

(defparameter *default-hasher* murmur32)

(defun size ()
  (slot-value *default-hasher* 'size))

(defgeneric hash (obj &optional hasher &rest args)
  (:method ((obj t) &optional (hasher *default-hasher*) &rest args) (apply (slot-value hasher 'fn) obj args)))

;;
;; for testing overflow nodes
;;
(defvar constant5bit (make-instance 'hasher
                                    :size 5
                                    :fn (lambda (obj &rest args)
                                          #b00001)))
