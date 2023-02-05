;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   lazy-seq/methods.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defun lreduce (f s &key (initial-value nil initial-value-p))
  (declare (ignore initial-value-p))
  (labels ((reduce* (seq current-value)
             (if seq
                 (let ((new-value (head seq)))
                   (reduce* (tail seq) (funcall f current-value new-value)))
                 current-value)))
    (reduce* (ensure-seq s) initial-value)))

(defun get-in (obj path &optional (default nil))
  (or (lreduce
       (lambda (obj k)
         (get obj k))
       path
       :initial-value obj)
      default))

(defun take (n seq)
  (when seq
    (let ((seq (ensure-seq seq)))
      (when (> n 0)
        (lazy-seq (head seq) (take (1- n) (tail seq)))))))


(defun drop (n seq)
  (when seq
    (let ((seq (ensure-seq seq)))
      (if(> n 0)
         (drop (1- n) (tail seq))
         seq))))

(defun drop (n seq)
  (labels ((drop* (n seq)
             (if (and seq (> n 0))
                 (drop* (1- n) (tail seq))
                 seq)))
    (drop* n (ensure-seq seq))))

(defun filter (pred seq)
  (labels ((filter* (seq)
             (when seq
               (let ((v (head seq)))
                 (if (funcall pred v)
                     (lazy-seq v (filter* (tail seq) ))
                     (filter* (tail seq)))))))
    (filter* (ensure-seq seq))))

(defun lmap (f &rest seqs)
  (if (emptyp seqs)
      nil
      (let ((n (count seqs))
            (seqs (map 'list #'ensure-seq seqs)))
        (labels ((apply* (args)
                   (apply f args))
                 (map* (s)
                   (let ((args (remove-if #'null (map 'list #'head s))))
                     (when (= n (count args))
                       (let ((r (apply* args)))
                         (lazy-seq r (map* (map 'list #'tail s))))))))
          (map* seqs)))))

(defun map-indexed (f &rest seqs)
  (apply #'lmap f (integers) seqs))

(defun range (n &key (start 0) (step 1))
  (when (> n 0)
    (lazy-seq start (range (1- n) :start (+ start step) :step step))))

(defun integers (&key (from 0))
  (lazy-seq from (integers :from (1+ from))))

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

(defun find-next-empty-multiple (factor multiple sieve)
  (let ((target (* multiple factor)))
    (if (and
         (oddp target)
         (null (lookup sieve target)))
        multiple
        (find-next-empty-multiple factor (+ multiple 2) sieve))))

(defun is-prime-p (n sieve)
  (if-let (value (lookup sieve n))
    (destructuring-bind (factor multiple) value
      (let ((next (find-next-empty-multiple factor multiple sieve)))
        (values nil (-> (assoc sieve (* factor next) (list factor (+ next 2)))
                      (dissoc n)))))
    (let ((next (find-next-empty-multiple n 3 sieve)))
      (values t (assoc sieve (* n next ) (list n (+ next 2)))))))

(defun next-prime (p sieve)
  (multiple-value-bind (prime? nsieve) (is-prime-p p sieve)
    (if prime?
        (values p nsieve)
        (next-prime (+ p 2) nsieve))))

(defun lazy-primes-seq (p sieve)
  (multiple-value-bind (next new-sieve) (next-prime p sieve)
    (lazy-seq next (lazy-primes-seq (+ next 2) new-sieve))))

(defun primes-seq ()
  (lazy-seq 2 (lazy-primes-seq 3 (persistent-hash-map))))
