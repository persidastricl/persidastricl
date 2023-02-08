;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   lazy-seq/methods.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defvar fib (cons 0 (lazy-seq 1 (lmap #'+ fib (tail fib)))))

(defvar trib
  (cons 0 (cons 1 (cons 1 (lazy-seq 2 (lmap #'+
                                            (tail trib)
                                            (tail (tail trib))
                                            (tail (tail (tail trib)))))))))

(defvar lucas
  (cons 2 (cons 1 (lazy-seq 3 (lmap #'+ (tail lucas) (tail (tail lucas)))))))

(defun catalan-seq (&optional (n 0))
  (labels ((fact (i)
             (apply #'* (->list (range i :start i :step -1))))
           (catalan* (i)
             (let ((v (/ (fact (* 2 i)) (* (fact (1+ i)) (fact i)))))
               (lazy-seq v (catalan* (1+ i))))))
    (catalan* n)))


;; -----
;;  primes' sieve of erasthenos (sp)
;;
;; -----

(defun primes-seq ()
  (let ((sieve (transient-hash-map)))
    (labels ((find-next-empty-multiple (factor multiple)
               (let ((target (* multiple factor)))
                 (if (and
                      (oddp target)
                      (null (lookup sieve target)))
                     multiple
                     (find-next-empty-multiple factor (+ multiple 2)))))
             (is-prime-p (n)
               (if-let (value (lookup sieve n))
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
                 (lazy-seq next (lazy-primes (+ next 2))))))
      (cons 2 (lazy-primes 3)))))


(time (drop 100000 (primes-seq)) )

(time
 (into [] (take 10 (drop 100000 (primes-seq)))))
