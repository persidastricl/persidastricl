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
    (if (= current (sb-ext:compare-and-swap seed-uniquifier current next))
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
      ((= seed (sb-ext:compare-and-swap (slot-value rnd 'seed) seed next)) (ash next (- 0 (- 48 bits))))
      (t (next-bits rnd bits)))))

(defun next-int (rnd n)
  (assert (> n 0))
  (cond
    ((= n (logand n (- n))) (ash (* n (next-bits rnd 31)) -31))
    (t (let* ((bits (next-bits rnd 31))
              (val (mod bits n)))
         (if (< (+ (- bits val) (1- n)) 0)
             (next-n rnd n)
             val)))))

(expt 2 31)

(logand 16 -16)

(next-int r1 (expt 2 32))

(defun rand-seq (rnd n)
  (lseq (next-n rnd n) (rand-seq rnd n)))


;; (frequencies (take 10000 (rand-seq r1 10)))

;; (take 100 (repeatedly (lambda () (next-n r1 23))))

;; (def r1 (rnd 8484848484848484))

;; (next-n r1 10)

(frequencies (take 10000 (repeatedly (lambda () (next-bits r1 2)))))
