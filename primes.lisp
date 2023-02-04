
(in-package #:persidastricl)

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


(setf lseq  )

(time
 (into [] (take 200 (drop 50000 (primes-seq)))))

(take 3000 FIB)

(into #{} )

(take 10 fib)

(->seq (take 2 (primes-seq)))

(time
 (into [] (take 20 (primes-seq))))

(primes-seq)

(print-object (tail  (primes-seq)) nil)
