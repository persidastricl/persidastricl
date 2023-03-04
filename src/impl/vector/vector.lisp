;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defclass vector (bpvt) ())

(defun vector? (x)
  (typep x 'vector))

(defmethod get ((vector vector) index &optional (default nil))
  (if (< index (count vector))
      (if (>= index (tail-offset vector))
          (loc (tail vector) index :default default)
          (loc (root vector) index :default default))
      default))

(defmethod first ((vector vector))
  (get vector 0))

(defmethod peek ((vector vector))
  (get vector (1- (count vector))))

(defmethod seq ((vector vector))
  (when (pos? (count vector))
    (labels ((next (i)
               (when (< i (count vector))
                 (let ((value (get vector i)))
                   (lseq value (next (1+ i)))))))
      (lseq (first vector) (next 1)))))

(defmethod rest ((vector vector))
  (drop 1 (seq vector)))

(defmethod ->list ((v vector))
  (cl:map 'list (lambda (i) (get v i)) (loop for i from 0 below (count v) collect i)))

(defmethod conj ((vector vector) &rest values)
  (cl:reduce
   #'(lambda (v value)
       (cons value v))
   values
   :initial-value vector))
