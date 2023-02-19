;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   core/functions.lisp
;;;
;;;   core (regular) functions
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; lazy-ness
;;
;; -----

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

(defun reduce-kv (f s &key (initial-value nil initial-value-p))
  (when s
    (let ((s (seq s)))
      (labels ((reduce* (seq current-value)
                 (if seq
                     (let ((new-value (head seq)))
                       (reduce* (tail seq) (apply f current-value (e:->list new-value))))
                     current-value)))
        (reduce*
         (if initial-value-p s (tail s))
         (if initial-value-p initial-value (head s)))))))

(defun get-in (obj path &optional (default nil))
  (or (lreduce
       (lambda (obj k)
         (get obj k))
       path
       :initial-value obj)
      default))

(defun assoc-in (obj path v)
  (let* ((k (first path))
         (more (rest path))
         (next (get obj k (empty obj))))
    (if more
        (assoc obj k (assoc-in next more v))
        (assoc obj k v))))

(defun update (m k f &rest args)
  (let ((current (get m k)))
    (assoc m k (apply f current args))))

(defun update-in (obj path f &rest args)
  (let* ((k (first path))
         (more (rest path)))
    (if (first more)
        (assoc obj k (apply #'update-in (get obj k (empty obj)) more f args))
        (assoc obj k (apply f (get obj k) args)))))

(labels ((take* (n target)
           (when (and (head target) (> n 0))
             (lseq (head target) (take* (1- n) (tail target))))))
  (defgeneric take (n seq)
    (:method (n (seq list)) (take* n seq))
    (:method (n (seq sequence)) (take* n (coerce seq 'list)))
    (:method (n (seq lazy-sequence)) (take* n seq))
    (:method (n (vector vector)) (take* n vector))))

(defun drop (n seq)
  (labels ((drop* (n s)
             (if (and s (> n 0))
                 (drop* (1- n) (tail s))
                 s)))
    (drop* n (seq seq))))

(defun filter (pred seq)
  (when seq
    (labels ((filter* (s)
               (when s
                 (let ((v (head s)))
                   (if (funcall pred v)
                       (lseq v (filter* (tail s) ))
                       (filter* (tail s)))))))
      (filter* (seq seq)))))

(defun lmap (f &rest seqs)
  (when-not (empty? seqs)
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

(defun mapv (f &rest seqs)
  (into (persistent-vector) (apply #'lmap f seqs)))

(defun filterv (pred seq)
  (into (persistent-vector) (filter pred seq)))

(defun keep (f &rest seqs)
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
                         (if r
                             (lseq r (map* (map 'list #'tail s)))
                             (map* (map 'list #'tail s))))))))
          (map* seqs)))))

(defun integers (&key (from 0))
  (lseq from (integers :from (1+ from))))

(defun map-indexed (f &rest seqs)
  (apply #'lmap f (integers) seqs))

(defun keep-indexed (f &rest seqs)
  (apply #'keep f (integers) seqs))

(defun range (n &key (start 0) (step 1))
  (when (> n 0)
    (lseq start (range (1- n) :start (+ start step) :step step))))

(defgeneric partition (source n)
  (:method ((source list) n) (partition (lazy-seq source) n))
  (:method ((source lazy-sequence) n) (let ((v (->list (take n source)))
                                            (rest (drop n source)))
                                        (if (head rest)
                                            (lseq v (partition rest n))
                                            (if (= n (length v))
                                                (list v)
                                                '())))))


(defgeneric partition-all (source n)
  (:method ((source list) n) (partition-all (lazy-seq source) n))
  (:method ((source lazy-sequence) n) (let ((v (->list (take n source)))
                                            (rest (drop n source)))
                                        (if (head rest)
                                            (lseq v (partition-all rest n))
                                            (list v)))))

(defun cycle (coll)
  (labels ((more (c)
             (let ((v (head c))
                   (tail (or (tail c) coll)))
               (lseq v (more tail)))))
    (more coll)))

(defun repeat (x)
  (repeatedly (constantly x)))

(defun repeatedly (f)
  (lazy-seq f))

(defun zipmap (seq1 seq2)
  (labels ((zipmap* (m s1 s2)
             (let ((k (head s1))
                   (v (head s2)))
               (if (and k v)
                   (zipmap* (assoc m k v) (tail s1) (tail s2))
                   m))))
    (zipmap* (persistent-hash-map) seq1 seq2)))

(defun interleave (seq1 seq2)
  (let ((e1 (first seq1))
        (e2 (first seq2)))
    (when (and e1 e2)
      (lseq e1 (lseq e2 (interleave (rest seq1) (rest seq2)))))))

(defun line-seq (stream)
  (when stream
    (let ((line (read-line stream nil nil)))
      (when line
        (lseq line (line-seq stream))))))

(defun string? (s)
  (stringp s))

(defun some (pred &rest seqs)
  (head (apply #'keep pred seqs)))

(defun some-fn (&rest fns)
  (lambda (&rest s)
    (some
     (lambda (f)
       (some f s))
     fns)))

(defvar emptyable? (some-fn #'string? #'sequential? #'collection? #'map?))

(defun mremove (pred m)
  (labels ((check (m k v)
             (let ((result (unwind-protect (funcall pred v))))
               (if-not result
                       (assoc m k v)
                       m)))
           (collection? (x) (typep x 'persidastricl::collection))
           (collection (v)
             (into (empty v)
                   (lmap
                    #'scrub
                    v)))
           (scrub (item)
             (cond
               ((map? item) (mremove pred item))
               ((collection? item) (collection item))
               (t item))))
    (when m
      (reduce-kv
       (lambda (m k v)
         (check m k (scrub v)))
       m
       :initial-value (persistent-hash-map)))))

(defun take-while (pred s)
  (let ((v (head s)))
    (when (and v (funcall pred v))
      (lseq v (take-while pred (tail s))))))

(defun drop-while (pred s)
  (let ((v (head s)))
    (if (funcall pred v)
        (drop-while pred (tail s))
        (lseq v (tail s)))))

(defun has-no-value? (x)
  (or (null x)
      (and (funcall emptyable? x)
           (empty? x))))

(defun only-valid-values (x)
  (mremove #'has-no-value? x))
