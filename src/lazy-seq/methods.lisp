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

(defun integers (&key (from 0))
  (lazy-seq from (integers :from (1+ from))))

(defun range (n &key (start 0) (step 1))
  (when (> n 0)
    (lazy-seq start (range (1- n) :start (+ start step) :step step))))

(defvar fib (cons 0 (lazy-seq 1 (lmap #'+ fib (tail fib)))))


;; (take 10 (drop 1000 (integers)))

;; (into #{} (filter (lambda (i) (= 0 (rem i 3))) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)))

;; (take 2 (drop 10 (filter (lambda (i) (= 0 (rem i 3))) fib)))
