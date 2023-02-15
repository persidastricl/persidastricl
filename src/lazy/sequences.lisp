;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   lazy-seq/sequences.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defun fact (i)
  (cond
    ((= i 0) 1)
    ((< i 0) (- (fact (- i))))
    (:otherwise (lreduce #'* (range i :start i :step -1)))))

(defvar fib (list* 0 (lseq 1 (lmap #'+ fib (tail fib)))))

(defvar trib
  (list* 0 1 1 (lseq 2 (lmap #'+
                             (tail trib)
                             (tail (tail trib))
                             (tail (tail (tail trib)))))))

(defvar squares (lmap (lambda (i) (* i i)) (drop 1 (integers))))

(defun n-choose-k (n k)
  (/ (fact n) (fact (- n k)) (fact k)))

(defvar triangulars (lmap (lambda (n) (n-choose-k (1+ n) 2)) (drop 1 (integers))))

(defvar hexagonals
  (lmap
   (lambda (n)
     (- (* 2 (* n n)) n))
   (drop 1 (integers))))

(defun catalan (n)
  (/ (fact (* 2 n)) (* (fact (1+ n)) (fact n))))

(defun catalan-seq (&optional (n 0))
  (lmap #'catalan (drop n (integers))))

;; -----
;;  primes' sieve of erasthenos (sp)
;;
;; -----

(defvar primes-seq
  (let ((sieve (transient-hash-map)))
    (labels ((find-next-empty-multiple (factor multiple)
               (let ((target (* multiple factor)))
                 (if (and
                      (oddp target)
                      (null (lookup sieve target)))
                     multiple
                     (find-next-empty-multiple factor (+ multiple 2)))))
             (is-prime-p (n)
               (if-let ((value (lookup sieve n)))
                 (destructuring-bind (factor multiple) value
                   (let ((next (find-next-empty-multiple factor multiple)))
                     (-> (assoc sieve (* factor next) (list factor (+ next 2)))
                       (dissoc n))
                     nil))
                 (let ((next (find-next-empty-multiple n 3)))
                   (assoc sieve (* n next ) (list n (+ next 2)))
                   t)))
             (next-prime (p)
               (if (is-prime-p p)
                   p
                   (next-prime (+ p 2))))
             (lazy-primes (p)
               (let ((next (next-prime p)))
                 (lseq next (lazy-primes (+ next 2))))))
      (cons 2 (lazy-primes 3)))))
