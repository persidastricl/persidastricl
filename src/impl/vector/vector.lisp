;;; -----
;;;
;;;  Copyright (c) 2019-2023 Michael D Pendergrass, pupcus.org
;;;
;;;  This program and the accompanying materials are made
;;;  available under the terms of the Eclipse Public License 2.0
;;;  which is available at https://www.eclipse.org/legal/epl-2.0/
;;;
;;;  SPDX-License-Identifier: EPL-2.0
;;;
;;; -----

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
          (loc (tail-end vector) index :default default)
          (loc (root vector) index :default default))
      default))

(defmethod first ((vector vector))
  (get vector 0))

(defmethod peek ((vector vector))
  (get vector (1- (count vector))))

(defmethod seq ((vector vector))
  (when (pos? (count vector))
    (labels ((next* (i)
               (when (< i (count vector))
                 (let ((value (get vector i)))
                   (lseq value (next* (1+ i)))))))
      (lseq (first vector) (next* 1)))))

(defmethod rest ((vector vector))
  (drop 1 (seq vector)))

(defmethod last ((vector vector))
  (peek vector))

;; TODO: check if using a subvec be better here
(defmethod butlast ((vector vector) &optional (n 1))
  (into [] (take (max 0 (- (count vector) n)) vector)))

(defmethod nth ((vector vector) n &optional (default nil))
  (nth (seq vector) n default))

(defmethod next ((vector vector))
  (next (seq vector)))

(defmethod head ((vector vector))
  (head (seq vector)))

(defmethod tail ((vector vector))
  (tail (seq vector)))

(defmethod ->list ((v vector))
  (cl:map 'list (lambda (i) (get v i)) (loop for i from 0 below (count v) collect i)))

(defmethod conj ((vector vector) &rest values)
  (cl:reduce
   #'(lambda (v value)
       (cons value v))
   values
   :initial-value vector))

(defmethod compare ((v1 vector) (v2 vector))
  (compare (seq v1) (seq v2)))

(defmethod into ((v vector) sequence)
  (reduce #'conj (seq sequence) :initial-value v))
