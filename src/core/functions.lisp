;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   core/functions.lisp
;;;
;;;   core (regular) functions
;;;
;;; -----

(in-package #:persidastricl)

(defun memoize (f)
  (let ((mem (atom (persistent-hash-map))))
    (lambda (&rest args)
      (let ((r (get (deref mem) args)))
        (or r
            (let ((ret (apply f args)))
              (swap! mem #'assoc args ret)
              ret))))))

(defun lreduce (f s &key (initial-value nil initial-value-p))
  (when s
    (let ((s (seq s)))
      (labels ((reduce* (current-value seq)
                 (if seq
                     (let ((new-value (head seq)))
                       (reduce* (funcall f current-value new-value) (tail seq)))
                     current-value)))
        (reduce*
         (if initial-value-p initial-value (head s))
         (if initial-value-p s (tail s)))))))

(defun run! (f coll)
  (lreduce
   (lambda (ign item)
     (declare (ignore ign))
     (funcall f item))
   coll
   :initial-value nil)
  nil)

(defun reduce-kv (f s &key (initial-value nil initial-value-p))
  (when s
    (let ((s (seq s)))
      (labels ((reduce* (seq current-value)
                 (if seq
                     (let ((new-value (head seq)))
                       (reduce* (tail seq) (apply f current-value (->list new-value))))
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

(defun every? (pred coll)
  (cond
    ((nil? (seq coll)) t)
    ((funcall pred (head coll)) (every? pred (tail coll)))
    (:otherwise nil)))

(defun update (m k f &rest args)
  (let ((current (get m k)))
    (assoc m k (apply f current args))))

(defun update-in (obj path f &rest args)
  (let* ((k (first path))
         (more (rest path)))
    (if (first more)
        (assoc obj k (apply #'update-in (get obj k (empty obj)) more f args))
        (assoc obj k (apply f (get obj k) args)))))

(defun filter (pred seq)
  (when (keywordp pred) (make-funcallable-keyword pred))
  (when seq
    (labels ((filter* (s)
               (when s
                 (let ((v (head s)))
                   (if (funcall pred v)
                       (lseq v (filter* (tail s) ))
                       (filter* (tail s)))))))
      (filter* (seq seq)))))

(defun lmap (f &rest seqs)
  (when (keywordp f) (make-funcallable-keyword f))
  (when seqs
    (let ((n (count seqs))
          (seqs (map 'list #'seq seqs)))
      (labels ((apply* (args)
                 (apply f args))
               (map* (ss)
                 (let ((args (map 'list #'head ss)))
                   (when (= n (count args))
                     (let ((r (apply* args)))
                       (lseq r (map* (remove-if #'nil? (map 'list #'tail ss)))))))))
        (map* seqs)))))

(defun mapv (f &rest seqs)
  (into (persistent-vector) (apply #'lmap f seqs)))

(defun concat (&rest seqs)
  (labels ((concat* (s &rest xs)
             (lseq (head s)
                   (if (tail s)
                       (apply #'concat* (tail s) xs)
                       (when-let ((next-s (head xs)))
                         (apply #'concat* (seq next-s) (tail xs)))))))
    (when seqs
      (let ((seqs (remove-if #'empty? seqs)))
        (apply #'concat* (seq (head seqs)) (tail seqs))))))

(defun mapcat (f s &rest seqs)
  (let ((xs (apply #'lmap f s seqs)))
    (apply #'concat (->list xs))))

(defun filterv (pred seq)
  (into (persistent-vector) (filter pred seq)))

(defun keep (f &rest seqs)
  (when (keywordp f) (make-funcallable-keyword f))
  (when seqs
    (let ((n (count seqs))
          (seqs (map 'list #'seq seqs)))
      (labels ((apply* (args)
                 (apply f args))
               (map* (ss)
                 (let ((args (map 'list #'head ss)))
                   (when (= n (count args))
                     (let ((r (apply* args)))
                       (if r
                           (lseq r (map* (remove-if #'nil? (map 'list #'tail ss))))
                           (map* (remove-if #'nil? (map 'list #'tail ss)))))))))
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

(defun take-while (pred s)
  (when-let ((s (seq s)))
    (let ((v (head s)))
      (when (funcall pred v)
        (lseq v (take-while pred (tail s)))))))

(defun take-nth (n s)
  (when-let ((s (seq s)))
    (lseq (head s) (take-nth n (drop n s)))))

(defun drop-while (pred s)
  (when-let ((s (seq s)))
    (let ((v (head s)))
      (if (funcall pred v)
          (drop-while pred (tail s))
          (lseq v (tail s))))))

(defun drop-last (n seq)
  (lmap
   (lambda (x y) (declare (ignore y))
     x)
   (seq seq)
   (drop n seq)))

(defun take-last (n seq)
  (labels ((take* (s lead)
             (if lead
                 (take* (tail s) (tail lead))
                 s)))
    (take* (seq seq) (drop n seq))))

(defun split-at (n seq)
  (list (take n seq) (drop n seq)))

(defun split-with (pred seq)
  (list (take-while pred seq) (drop-while pred seq)))

(defun iterate (f x)
  (let ((v (funcall f x)))
    (lseq v (iterate f v))))

(defun partition (source n)
  (when-let ((s (seq source)))
    (let ((v (->list (take n s)))
          (rest (drop n s)))
      (if (tail rest)
          (lseq v (partition rest n))
          (if (= n (length v))
              (list v)
              '())))))

(defun partition-all (source n)
  (when-let ((s (seq source)))
    (let ((v (->list (take n s)))
          (rest (drop n s)))
      (if (tail rest)
          (lseq v (partition-all rest n))
          (list v)))))

(defun partition-by (f seq)
  (when-let ((s (seq seq)))
    (let* ((v (head s))
           (fv (funcall f v))
           (run (into (list v) (take-while (lambda (v) (== fv (funcall f v))) (tail s)))))
      (cons run (partition-by f (drop (cl:length run) s))))))

(defun group-by (f coll)
  (when (keywordp f) (make-funcallable-keyword f))
  (lreduce
   (lambda (ret x)
     (let ((k (funcall f x)))
       (assoc ret k (conj (get ret k (persistent-vector)) x))))
   coll
   :initial-value (persistent-hash-map)))

;; TODO: nth & rand-nth ??

(defun cycle (coll)
  (labels ((more (c)
             (let ((v (head c))
                   (tail (or (tail c) coll)))
               (lseq v (more tail)))))
    (more coll)))

(defun repeatedly (f)
  (lazy-seq f))

(defun repeat (x)
  (repeatedly (constantly x)))

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

(defun interpose (separator seq)
  (drop 1 (interleave (repeat separator) seq)))

(defun line-seq (stream)
  (when stream
    (let ((line (read-line stream nil nil)))
      (when line
        (lseq line (line-seq stream))))))


(defun tree-seq (branch? children root)
  "Returns a lazy sequence of the nodes in a tree, via a depth-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
  tree."
  (labels ((walk (node)
             (lseq node
                   (when (funcall branch? node)
                     (mapcat #'walk (funcall children node))))))
    (walk root)))

(defun flatten (x)
  (filter (complement #'sequential?)
          (tail (tree-seq #'collection? #'seq x))))

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

(defun has-no-value? (x)
  (or (null x)
      (and (funcall emptyable? x)
           (empty? x))))

(defun only-valid-values (x)
  (mremove #'has-no-value? x))

(defun juxt (&rest fns)
  (run! (lambda (f) (when (keywordp f) (make-funcallable-keyword f))) fns)
  (lambda (&rest args)
    (lreduce
     (lambda (v f)
       (conj v (apply f args)))
     fns
     :initial-value (persistent-vector))))
