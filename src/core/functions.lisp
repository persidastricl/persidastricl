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

;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   core/functions.lisp
;;;
;;;   core (regular) functions
;;;
;;; -----

(in-package #:persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(defun memoize (f)
  (let ((mem (atom (persistent-hash-map))))
    (lambda (&rest args)
      (let ((r (get (deref mem) args)))
        (or r
            (let ((ret (apply f args)))
              (swap! mem #'assoc args ret)
              ret))))))

(defmacro defmemoized (sym f)
  `(setf (fdefinition ',sym) ,(memoize f)))

(defmacro fdef (sym f)
  `(setf (fdefinition ',sym) ,f))

(defun reduce (f s &key (initial-value nil initial-value-p))
  (if s
      (let ((s (seq s)))
        (labels ((reduce* (current-value seq)
                   (if seq
                       (let* ((new-value (head seq))
                              (result (funcall f current-value new-value)))
                         (reduce* result (tail seq)))
                       current-value)))
          (reduce*
           (if initial-value-p initial-value (head s))
           (if initial-value-p s (tail s)))))
      initial-value))

(defun reductions (f s &key (initial-value nil initial-value-p))
  (if s
      (let ((s (seq s)))
        (labels ((reduce* (current-value seq accum)
                   (if seq
                       (let* ((new-value (head seq))
                              (result (funcall f current-value new-value)))
                         (reduce* result (tail seq) (conj accum result)))
                       accum)))
          (let ((init (if initial-value-p initial-value (head s)))
                (seq (if initial-value-p s (tail s))))
            (reduce* init seq []))))
      [initial-value]))

(defun frequencies (coll)
  (reduce
   (lambda (m target)
     (assoc m x (inc (get m x 0))))
   coll
   :initial-value {}))

(defun run! (f coll)
  (reduce
   (lambda (ign item)
     (declare (ignore ign))
     (funcall f item))
   coll
   :initial-value nil)
  nil)

(defun reduce-kv (f s &key (initial-value nil initial-value-p))
  (if s
      (let ((s (seq s)))
        (labels ((reduce* (seq current-value)
                   (if seq
                       (let ((new-value (head seq)))
                         (reduce* (tail seq) (apply f current-value (->list new-value))))
                       current-value)))
          (reduce*
           (if initial-value-p s (tail s))
           (if initial-value-p initial-value (head s)))))
      initial-value))

(defun get-in (obj path &optional (default nil))
  (or (reduce
       (lambda (obj k)
         (get obj k))
       path
       :initial-value obj)
      default))

(defun assoc-in (obj path v)
  (let* ((k (first path))
         (more (rest path))
         (next (or (get obj k) (empty obj))))
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
        (assoc obj k (apply #'update-in (or (get obj k) (empty obj)) more f args))
        (assoc obj k (apply f (get obj k) args)))))

(defun filter (pred sequence)
  (when (keywordp pred) (make-funcallable-keyword pred))
  (when-let ((seq (seq sequence)))
    (labels ((filter* (s)
               (when s
                 (let ((v (head s)))
                   (if (funcall pred v)
                       (lseq v (filter* (tail s) ))
                       (filter* (tail s)))))))
      (filter* seq))))

(defun map (f &rest seqs)
  (when (keywordp f) (make-funcallable-keyword f))
  (when seqs
    (let ((n (count seqs))
          (seqs (cl:map 'list #'seq seqs)))
      (labels ((apply* (args)
                 (apply f args))
               (map* (ss)
                 (let ((args (cl:map 'list #'head ss)))
                   (when (= n (count args))
                     (let ((r (apply* args)))
                       (lseq r (map* (remove-if #'nil? (cl:map 'list #'tail ss)))))))))
        (map* seqs)))))


(defun every? (f &rest seqs)
  (when (keywordp f) (make-funcallable-keyword f))
  (when seqs
    (let ((n (count seqs))
          (seqs (cl:map 'list #'seq seqs)))
      (labels ((apply* (args)
                 (apply f args))
               (every?* (ss)
                 (let ((args (cl:map 'list #'head ss))
                       (rest-seqs (remove-if #'nil? (cl:map 'list #'tail ss))))
                   (when (= n (count args))
                     (let ((r (apply* args)))
                       (when (true? r)
                         (if (empty? rest-seqs)
                             t
                             (every?* rest-seqs))))))))
        (every?* seqs)))))

(fdef not-every? (comp #'not #'every?))

(defun every-pred (&rest preds)
  (lambda (&rest args)
    (every?
     (lambda (pred)
       (every? pred args))
     preds)))

(defun mapv (f &rest seqs)
  (into (persistent-vector) (apply #'map f seqs)))

(defun concat (&rest colls)
  (labels ((cat (s more)
             (if-let ((s (seq s)))
               (lseq (first s) (cat (rest s) more))
               (when more
                 (cat (first more) (rest more))))))
    (cat (first colls) (rest colls))))

(defun mapcat (f s &rest seqs)
  (let ((xs (apply #'map f s seqs)))
    (apply #'concat (->list xs))))
;;
;; TODO: can we figure out a way to do this without the 'eval'?? or does it even matter??
;;
(defmacro lazy-cat (&rest colls)
  `(let ((xs (map (lambda (x) (delay (eval x))) ',colls)))
     (labels ((cat (s more)
                (if-let ((s (seq s)))
                  (lseq (first s) (cat (rest s) more))
                  (when more
                    (cat (force (first more)) (rest more))))))
       (cat (force (first xs)) (rest xs)))))

(defun filterv (pred seq)
  (into (persistent-vector) (filter pred seq)))

(defun keep (f &rest seqs)
  (when (keywordp f) (make-funcallable-keyword f))
  (when seqs
    (let ((n (count seqs))
          (seqs (cl:map 'list #'seq seqs)))
      (labels ((apply* (args)
                 (apply f args))
               (map* (ss)
                 (let ((args (cl:map 'list #'head ss)))
                   (when (= n (count args))
                     (let ((r (apply* args)))
                       (if r
                           (lseq r (map* (remove-if #'nil? (cl:map 'list #'tail ss))))
                           (map* (remove-if #'nil? (cl:map 'list #'tail ss)))))))))
        (map* seqs)))))

(defun integers (&key (from 0) (step 1))
  (lseq from (integers :from (+ from step) :step step)))

(defun map-indexed (f &rest seqs)
  (apply #'map f (integers) seqs))

(defun keep-indexed (f &rest seqs)
  (apply #'keep f (integers) seqs))

(defun range (&optional n1 n2 n3)
  "returns a lazy sequence of numbers based on the following:

   (range) infinite sequence (0 1 2 ...)
   (range end) finite sequence (0 1 ... end)
   (range start end) and (< start end) finite sequence (start start+1 start+2 ... end)
   (range start end) and (> start end) finite sequence (start start-1 start-2 ... end)
   (range start end step) and (< start end) (pos? step) finite sequence (start start+step ... end)
   (range start end step) and (> start end) (neg? step) finite sequence (start start-step ... end)
   (range start nil step) infinite sequence (start start+step ...)

   returns nil    when  1. (zero? step)
                    or  2. (= start end)
                    or  3. (< start stop) but (neg? step)
                    or  4. (> start stop) but (pos? step)"

  (let* ((args (cond
                 ((and n1 n2 n3) (list n1 n2 n3))
                 ((and n1 n2) (list n1 n2 (if (<= n1 n2) 1 -1)))
                 ((and n1 n3) (list n1 nil n3))
                 ((and n1) (list 0 n1 (if (neg? n1) -1 1)))
                 (t (list 0 nil 1))))
         (start (nth args 0))
         (end   (nth args 1))
         (step  (nth args 2)))

    (if (or (zerop step) (and end start (= start end))) nil

        (labels ((finite-up* (end &key (start 0) (step 1))
                   (when (< start end)
                     (lseq start (finite-up* end :start (+ start step) :step step))))

                 (finite-down* (end &key (start 0) (step 1))
                   (when (> start end)
                     (lseq start (finite-down* end :start (+ start step) :step step))))

                 (infinite* (&key (start 0) (step 1))
                   (lseq start (infinite* :start (+ start step) :step step))))
          (if end
              (cond
                ((and (> start end) (neg? step)) (finite-down* end :start start :step step))
                ((and (< start end) (pos? step)) (finite-up* end :start start :step step)))
              (infinite* :start start :step step))))))

(defun nrange (n &key (start 0) (step 1))
  (when (> n 0)
    (lseq start (nrange (1- n) :start (+ start step) :step step))))

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
  (map
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
      (if rest
          (lseq v (partition rest n))
          (if (= n (length v))
              (list v)
              '())))))

(defun partition-all (source n)
  (when-let ((s (seq source)))
    (let ((v (->list (take n s)))
          (rest (drop n s)))
      (if rest
          (lseq v (partition-all rest n))
          (list v)))))

(defun partition-by (f seq)
  (when-let ((s (seq seq)))
    (let* ((v (head s))
           (fv (funcall f v))
           (run (into (list v) (take-while (lambda (v) (== fv (funcall f v))) (tail s)))))
      (lseq run (partition-by f (drop (cl:length run) s))))))

(defun group-by (f coll)
  (when (keywordp f) (make-funcallable-keyword f))
  (reduce
   (lambda (ret x)
     (let ((k (funcall f x)))
       (assoc ret k (conj (get ret k (persistent-vector)) x))))
   coll
   :initial-value (persistent-hash-map)))

(defun rand-nth (coll)
  (assert (collection? coll))
  (nth (seq coll) (random (count coll))))

(defun cycle (coll)
  (labels ((more (c)
             (let ((v (head c))
                   (tail (or (tail c) coll)))
               (lseq v (more tail)))))
    (more coll)))

(defun repeatedly (f)
  (lseq (funcall f) (repeatedly f)))

(defun repeat (x)
  (repeatedly (constantly x)))

(defun distinct (coll)
  (labels ((step* (xs seen)
             (when (seq xs)
               (let ((f (first xs)))
                 (if (contains? seen f)
                     (step* (rest xs) seen)
                     (lseq f (step* (rest xs) (conj seen f))))))))
    (when (seq coll)
      (step* (seq coll) #{}))))

(defun dedup (seq)
  (labels ((next* (s prev)
             (when (seq s)
               (let ((v (first s)))
                 (if (== v prev)
                     (next* (rest s) prev)
                     (lseq v (next* (rest s) v)))))))
    (when (seq seq)
      (let ((v (first seq)))
        (lseq v (next* (rest seq) v))))))

(defun distinct? (x &rest  more)
  (labels ((distinct* (s target others)
             (if target
                 (if (contains? s target)
                     nil
                     (distinct* (conj s target) (first others) (rest others)))
                 t)))
    (distinct* #{} x more)))

(defun shuffle (coll)
  (assert (collection? coll))
  (let* ((l (->list coll))
         (n (count l)))
    (do ((tail (nthcdr 0 l) (cdr tail)))
        ((zerop n))
      (rotatef (car tail) (car (nthcdr (random n) tail)))
      (decf n))
    (vec l)))

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
  (labels ((walk (node)
             (lseq node
                   (when (funcall branch? node)
                     (mapcat #'walk (funcall children node))))))
    (walk root)))

(defun re-seq (re s)
  (let ((scanner (cl-ppcre:create-scanner re)))
    (labels ((scan* (pos)
               (let* ((match (multiple-value-list (cl-ppcre:scan scanner s :start pos)))
                      (start (get match 0))
                      (end   (get match 1)))
                 (when start
                   (let ((v (subs s start end)))
                     (lseq v (scan* end)))))))
      (scan* 0))))

(labels ((best (pred k c1 c2 &rest contestants)
           (labels ((best* (the-one the-value challengers)
                      (let ((challenger (first challengers)))
                        (if challenger
                            (let ((challenger-value (get challenger k)))
                              (if (funcall pred the-value challenger-value)
                                  (best* the-one the-value (rest challengers))
                                  (best* challenger challenger-value (rest challengers))))
                            the-one))))
             (let* ((v1 (get c1 k))
                    (v2 (get c2 k))
                    (c1? (funcall pred v1 v2))
                    (w (if c1? c1 c2))
                    (v (if c1? v1 v2)))
               (best* w v contestants)))))

  (defun min-key (k &rest challengers)
    (apply #'best #'< k challengers))

  (defun max-key (k &rest challengers)
    (apply #'best #'> k challengers)))

(defun replace (smap coll)
  (if (vector? coll)
      (reduce
       (lambda (v i)
         (let ((rep (get smap (nth v i))))
           (if rep
               (assoc v i rep)
               v)))
       (range (count coll))
       :initial-value coll)
      (map
       (lambda (x)
         (get smap x x))
       coll)))

(defun flatten (x)
  (filter (complement #'sequential?)
          (tail (tree-seq #'collection? #'seq x))))

(defun some (pred &rest seqs)
  (head (apply #'keep pred seqs)))

(fdef not-any? (comp #'not #'some))

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
                   (map
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
    (reduce
     (lambda (v f)
       (conj v (apply f args)))
     fns
     :initial-value (persistent-vector))))

(defun merge (&rest ms)
  (if (some #'identity ms)
      (reduce
       (lambda (m1 m2)
         (if m2
             (into m1 (->plist m2))
             m1))
       (filter #'some? ms))))

(defun merge-with (f &rest ms)
  (labels ((merge-kv (m k v2)
             (let ((v1 (get m k :not-found)))
               (if (== v1 :not-found)
                   (assoc m k v2)
                   (assoc m k (funcall f v1 v2)))))
           (merge* (m1 m2)
             (reduce-kv
              #'merge-kv
              m2
              :initial-value m1)))
    (reduce #'merge* (filter #'some? ms))))

(defun slurp (f)
  (with-open-file (is f :if-does-not-exist nil)
    (when is
      (str:join #\NEWLINE (->list (line-seq is))))))

(defun spit (f contents &key (if-exists :overwrite) (if-does-not-exist :create))
  (with-open-file (os f :if-exists if-exists :if-does-not-exist if-does-not-exist :direction :output)
    (write-string contents os))
  f)

(defun trampoline  (f &rest args)
  (if args
      (trampoline (lambda () (apply f args)))
      (let ((ret (funcall f)))
        (if (functionp ret)
            (trampoline ret)
            ret))))

(defun dorun (coll)
  (let ((s (seq coll)))
    (when s
      (dorun (next s)))))

(defun dorun-n (n coll)
  (when (and (seq coll) (pos? n))
    (dorun-n (dec n) (next coll))))

(defun doall (coll)
  (dorun coll)
  coll)

(defun do-n (n coll)
  (dorun-n n coll)
  coll)
