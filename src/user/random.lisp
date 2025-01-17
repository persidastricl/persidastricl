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

;; -----
;;  random.lisp
;;
;; -----

(in-package #:persidastricl)

(named-readtables:in-readtable syntax)

(def multiplier #x5DEECE66D)
(def addend #xB)
(def mask (- (ash 1 48) 1))

(defclass random-generator ()
  ((seed :initarg :seed)))

(def seed-uniquifier 8682522807148012)

(defun seed-uniquifier ()
  (let* ((current seed-uniquifier)
         (next (* current 181783497276652981)))
    (if (= current (atomics:cas seed-uniquifier current next))
        next
        (seed-uniquifier))))

(defun initial-scramble (seed)
  (logand (logxor seed multiplier) mask))

(defun random-generator (&optional (seed (logxor seed-uniquifier (get-universal-time))))
  (make-instance 'random-generator :seed (initial-scramble seed)))

(defun next-bits (rnd bits)
  (let* ((seed (slot-value rnd 'seed))
         (next (logand (+ (* seed multiplier) addend) mask)))
    (cond
      ((atomics:cas (slot-value rnd 'seed) seed next) (ash next (- 0 (- 48 bits))))
      (t (next-bits rnd bits)))))

(defun next-int (rnd n)
  (assert (> n 0))
  (cond
    ((= n (logand n (- n))) (ash (* n (next-bits rnd 31)) -31))
    (t (let* ((bits (next-bits rnd 31))
              (val (mod bits n)))
         (if (< (+ (- bits val) (1- n)) 0)
             (next-int rnd n)
             val)))))

(defun rand-seq (n &optional (rg (random-generator)))
  (lseq (next-int rg n) (rand-seq n rg)))
