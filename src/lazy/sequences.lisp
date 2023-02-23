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

(defvar fib
  (lseq 0 (lseq 1 (lmap #'+ fib (tail fib)))))

(defvar trib
  (concat (list 0 1 1) (lseq 2
                             (lmap #'+
                                   (drop 1 trib)
                                   (drop 2 trib)
                                   (drop 3 trib)))))
(defun squares ()
  (lmap (lambda (i) (* i i)) (drop 1 (integers))))

(defun n-choose-k (n k)
  (/ (fact n) (fact (- n k)) (fact k)))

(defun triangulars ()
  (lmap (lambda (n) (n-choose-k (1+ n) 2)) (drop 1 (integers))))

(defun binomial-coefficients (&optional (n 0))
  (lseq (mapv
         (lambda (k)
           (n-choose-k n k))
         (range (inc n)))
        (binomial-coefficients (inc n))))

(defun hexagonals ()
  (lmap
   (lambda (n)
     (- (* 2 (* n n)) n))
   (drop 1 (integers))))

(defun catalan (n)
  (/ (fact (* 2 n)) (* (fact (1+ n)) (fact n))))

(defun catalan-seq (&optional (n 0))
  (lmap #'catalan (drop n (integers))))

;; -----
;;  primes' sieve of erasthenos (sp) with persistent hash map
;;
;; -----

(defun find-next-empty-multiple (factor multiple sieve)
  (let ((target (* multiple factor)))
    (if (and
         (oddp target)
         (not (get sieve target)))
        multiple
        (find-next-empty-multiple factor (+ multiple 2) sieve))))

(defun is-prime-p (n sieve)
  (if-let ((value (get (deref sieve) n)))
    (let ((factor (first value))
          (multiple (second value)))
      (let ((next (find-next-empty-multiple factor multiple (deref sieve))))
        (swap! sieve (lambda (sieve)
                       (-> sieve
                         (assoc (* factor next) (list factor (+ next 2)))
                         (dissoc n))))
        nil))
    (let ((next (find-next-empty-multiple n 3 (deref sieve))))
      (swap! sieve #'assoc (* n next ) (list n (+ next 2)))
      t)))

(defun next-prime (p sieve)
  (if (is-prime-p p sieve)
      p
      (next-prime (+ p 2) sieve)))

(defun lazy-primes (p sieve)
  (let ((next (next-prime p sieve)))
    (lseq next (lazy-primes (+ next 2) sieve))))

(defun primes-seq ()
  (lseq 2 (lazy-primes 3 (atom (persistent-hash-map)))))


;; -----
;;  primes' sieve of erasthenos (sp) with transient hash map
;;
;; -----

(defun find-next-empty-multiple (factor multiple sieve)
  (let ((target (* multiple factor)))
    (if (and
         (oddp target)
         (not (get sieve target)))
        multiple
        (find-next-empty-multiple factor (+ multiple 2) sieve))))

(defun is-prime-p (n sieve)
  (if-let ((value (get sieve n)))
    (let ((factor (first value))
          (multiple (second value)))
      (let ((next (find-next-empty-multiple factor multiple sieve)))
        (-> sieve
          (assoc (* factor next) (list factor (+ next 2)))
          (dissoc n))
        nil))
    (let ((next (find-next-empty-multiple n 3 sieve)))
      (assoc sieve (* n next ) (list n (+ next 2)))
      t)))

(defun next-prime (p sieve)
  (if (is-prime-p p sieve)
      p
      (next-prime (+ p 2) sieve)))

(defun lazy-primes (p sieve)
  (let ((next (next-prime p sieve)))
    (lseq next (lazy-primes (+ next 2) sieve))))

(defun primes-seq ()
  (lseq 2 (lazy-primes 3 (transient-hash-map))))
