;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   lazy-seq/methods.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defun lreduce (f s &key (initial-value nil initial-value-p))
  (when s
    (let ((s (seq s)))
      (labels ((reduce* (seq current-value)
                 (if seq
                     (let ((new-value (head seq)))
                       (reduce* (tail seq) (funcall f current-value new-value)))
                     current-value)))
        (reduce*
         (if initial-value-p s (tail s))
         (if initial-value-p initial-value (head s)))))))

;; (defun reduce-kv (f s &key (initial-value nil initial-value-p))
;;   (when s
;;     (let ((s (seq s)))
;;       (labels ((reduce* (seq current-value)
;;                  (if seq
;;                      (let ((new-value (head seq)))
;;                        (reduce* (tail seq) (funcall f current-value new-value)))
;;                      current-value)))
;;         (reduce*
;;          (if initial-value-p s (tail s))
;;          (if initial-value-p initial-value (head s)))))))

(defun get-in (obj path &optional (default nil))
  (or (lreduce
       (lambda (obj k)
         (get obj k))
       path
       :initial-value obj)
      default))


(labels ((take* (n target)
           (when (and (head target) (> n 0))
             (lseq (head target) (take* (1- n) (tail target))))))
  (defgeneric take (n seq)
    (:method (n (seq list)) (take* n seq))
    (:method (n (seq sequence)) (take* n (coerce seq 'list)))
    (:method (n (seq lazy-sequence)) (take* n seq))))

(defun drop (n seq)
  (labels ((drop* (n s)
             (if (and s (> n 0))
                 (drop* (1- n) (tail s))
                 s)))
    (drop* n (seq seq))))

(defun filter (pred seq)
  (labels ((filter* (s)
             (when s
               (let ((v (head s)))
                 (if (funcall pred v)
                     (lseq v (filter* (tail s) ))
                     (filter* (tail s)))))))
    (filter* (seq seq))))

(defun lmap (f &rest seqs)
  (if (empty? seqs)
      nil
      (let ((n (count seqs))
            (seqs (map 'list #'seq seqs)))
        (labels ((apply* (args)
                   (apply f args))
                 (map* (s)
                   (let ((args (remove-if #'null (map 'list #'head s))))
                     (when (= n (count args))
                       (let ((r (apply* args)))
                         (lseq r (map* (map 'list #'tail s))))))))
          (map* seqs)))))

(defun integers (&key (from 0))
  (lseq from (integers :from (1+ from))))

(defun map-indexed (f &rest seqs)
  (apply #'lmap f (integers) seqs))

(defun range (n &key (start 0) (step 1))
  (when (> n 0)
    (lseq start (range (1- n) :start (+ start step) :step step))))

(defgeneric partition (source n)
  (:method ((source list) n) (partition (lazy-seq source) n))
  (:method ((source lazy-sequence) n) (let ((v (->list (take n source)))
                                            (rest (drop n source)))
                                        (if (head rest)
                                            (lseq v (partition rest n))
                                            (list v)))))
