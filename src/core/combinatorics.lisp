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

;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; combinatorics.lisp
;;;
;;; -----

(in-package :combinatorics)

(named-readtables:in-readtable persidastricl:syntax)

(defun all-different? (items)
  (apply #'distinct? (->list items)))

(defun less-than (a b)
  (when (= -1 (compare a b)) t))

(defun index-combinations (n cnt)
  (labels ((iter-comb (c j)
             (labels ((iter* (c* j*)
                        (if (= j* 1)
                            [c* j*]
                            (iter* (assoc c* (dec j*) (dec (get c* j*))) (dec j*)))))
               (if (> j n) nil
                   (let ((c (assoc c j (dec (get c j)))))
                     (if (< (get c j) j)
                         [c (inc j)]
                         (iter* c j))))))
           (step* (c j)
             (lseq (rseq (subvec c 1 (inc n)))
                   (let ((next-step (iter-comb c j)))
                     (when next-step (step* (get next-step 0) (get next-step 1)))))))
    (let ((c (vec (cons nil (loop for j from 1 upto n collect (+ j cnt (- (inc n))))))))
      (step* c 1))))

(defun distribute (m index total distribution already-distributed)
  (labels ((distribute* (distribution index already-distributed)
             (if (>= index (count m)) nil
                 (let* ((quantity-to-distribute (- total already-distributed))
                        (mi (get m index)))
                   (if (<= quantity-to-distribute mi)
                       (conj distribution [index quantity-to-distribute total])
                       (distribute* (conj distribution [index mi (+ already-distributed mi)])
                                    (inc index)
                                    (+ already-distributed mi)))))))
    (distribute* distribution index already-distributed)))

(defun next-distribution (m total distribution)
  (labels ((f (distribution)
             (dlet (([index this-bucket this-and-to-the-left] (peek distribution))
                    (distribution (if (= this-bucket 1)
                                      (pop distribution)
                                      (conj (pop distribution)
                                            [index (dec this-bucket) (dec this-and-to-the-left)]))))
                   (cond
                     ((<= (- total (dec this-and-to-the-left)) (apply #'+ (->list (subvec m (inc index)))))
                      (distribute m (inc index) total distribution (dec this-and-to-the-left)))

                     ((seq distribution) (f distribution))))))

    (dlet (([index this-bucket this-and-to-the-left] (peek distribution)))
          (cond
            ((< index (dec (count m)))
             (if (= this-bucket 1)
                 (conj (pop distribution) [(inc index) 1 this-and-to-the-left])
                 (conj (pop distribution)
                       [index (dec this-bucket) (dec this-and-to-the-left)]
                       [(inc index) 1 this-and-to-the-left])))
            ((= this-bucket total) nil)
            (t (f (pop distribution)))))))

(defun bounded-distributions (m n)
  (labels ((step* (distribution)
             (lseq distribution
                   (let ((next-step (next-distribution m n distribution)))
                     (when next-step
                       (step* next-step))))))
    (step* (distribute m 0 n [] 0))))

(defun multi-comb (lst n)
  (let* ((f (frequencies lst))
         (v (vec (distinct lst)))
         (m (mapv (lambda (i) (get f (get v i))) (range (count v))))
         (qs (bounded-distributions m n)))
    (map
     (lambda (q)
       (mapcat
        (lambda (d)
          (let ((index (get d 0))
                (this-bucket (get d 1)))
            (take this-bucket (repeat (get v index)))))
        q))
     qs)))

(defun combinations (items n)
  (let ((v-items (vec (reverse (->list items)))))
    (if (zero? n) (list ())
        (let ((cnt (count items)))
          (cond ((> n cnt) nil)
                ((= n 1) (map (lambda (item) (list item)) (distinct items)) )
                ((all-different? items) (if (= n cnt)
                                            (list (->list items))
                                            (map (lambda (c) (map (lambda (d) (get v-items d)) c)) (index-combinations n cnt))))
                (t (multi-comb items n)))))))

(defun subsets (items)
  (mapcat
   (lambda (n)
     (combinations items n))
   (range (inc (count items)))))

(defun cartesian-product (&rest seqs)
  (let ((v-original-seqs (vec seqs)))
    (labels ((step* (v-seqs)
               (labels ((increment (v-seqs)
                          (labels ((p (i vs)
                                     (if (= i -1) nil
                                         (let ((rst (next (get vs i))))
                                           (if rst
                                               (assoc vs i rst)
                                               (p (dec i) (assoc vs i (get v-original-seqs i))))))))
                            (p (dec (count v-seqs)) v-seqs))))
                 (when v-seqs
                   (lseq (map #'first v-seqs) (step* (increment v-seqs)))))))
      (when (every? #'seq seqs)
        (step* v-original-seqs)))))

(defun selections (items n)
  (apply #'cartesian-product (->list (take n (repeat items)))))

(defun iter-perm (v)
  (let* ((len (count v))
         (j (labels ((jl (i)
                       (cond
                         ((= i -1) nil)
                         ((< (get v i) (get v (inc i))) i)
                         (t (jl (dec i))))))
              (jl (- len 2)))))
    (when j
      (let* ((vj (get v j))
             (l (labels ((ll (i)
                           (if (< vj (get v i)) i (ll (dec i)))))
                  (ll (dec len)))))
        (labels ((vl (v k l)
                   (if (< k l)
                       (vl (assoc v k (get v l) l (get v k)) (inc k) (dec l))
                       v)))
          (vl (assoc v j (get v l) l vj) (inc j) (dec len)))))))

(defun vec-lex-permutations (v)
  (when v (lseq v (vec-lex-permutations (iter-perm v)))))

(defun lex-permutations (c)
  (let ((vec-sorted (vec (sort (->list c) #'less-than))))
    (if (zero? (count vec-sorted))
        (list [])
        (vec-lex-permutations vec-sorted))))

(defun sorted-numbers? (s)
  (and (every? #'numberp s)
       (or (empty? s) (apply #'<= (->list s)))))

(defun multi-perm (l)
  (let* ((f (frequencies l))
         (v (vec (distinct l)))
         (indices (mapcat
                   (lambda (i)
                     (take (get f (get v i)) (repeat i)))
                   (range (count v)))))
    (map
     (lambda (idxs)
       (map
        (lambda (idx)
          (get v idx))
        idxs))
     (lex-permutations indices))))

(defun permutations (items)
  (cond
    ((sorted-numbers? items) (lex-permutations items))

    ((all-different? items)
     (let ((v (vec items)))
       (map
        (lambda (idxs)
          (map
           (lambda (idx)
             (get v idx))
           idxs))
        (lex-permutations (range (count v))))))

    (t (multi-perm items))))

(defun permuted-combinations (items n)
  (->> (combinations items n)
    (mapcat #'permutations)
    concat))

(defun factorial (n)
  (assert (and (integerp n) (not (neg? n))))
  (labels ((fact* (n acc) (if (<= n 1) acc (fact* (dec n) (* acc n)))))
    (fact* n 1)))

;;
;; see http://en.wikipedia.org/wiki/Factorial_number_system)
;;
(defun factorial-numbers (n)
  (assert (and  (integerp n) (not (neg? n))))
  (labels ((digits (n d divisor)
             (if (zero? n) d
                 (let ((q (quot n divisor))
                       (r (rem n divisor)))
                   (digits q (cons r d) (inc divisor))))))
    (digits n '() 1)))

(defun remove-nth (l n)
  (labels ((ll (l n acc)
             (if (zero? n) (into acc (rest l))
                 (ll (rest l) (dec n) (conj acc (first l))))))
    (ll l n [])))

(defun nth-permutation-distinct (l n)
  (when (>= n (factorial (count l)))
    (error "~a is too large. Input has only ~a permutations." n (factorial (count l))))
  (let ((length (count l))
        (fact-nums (factorial-numbers n)))
    (labels ((perm* (indices l perm)
               (if (empty? indices) perm
                   (let* ((i (first indices))
                          (item (nth l i)))
                     (perm* (rest indices) (remove-nth l i) (conj perm item))))))
      (perm* (concat (take (- length (count fact-nums)) (repeat 0)) fact-nums) l []))))

(defun count-permutations-from-frequencies (freqs)
  (let ((counts (vals freqs)))
    (reduce
     #'/
     (map #'factorial counts)
     :initial-value (factorial (apply #'+ (->list counts))))))

(defun count-permutations (l)
  (if (all-different? l)
      (factorial (count l))
      (count-permutations-from-frequencies (frequencies l))))

(defun initial-perm-numbers (freqs)
  (reductions
   #'+
   (map
    (lambda (e)
      (let ((k (key e))
            (v (val e)))
        (count-permutations-from-frequencies (assoc freqs k (dec v)))))
    freqs)
   :initial-value 0))

(defun index-remainder (perm-numbers n)
  (labels ((ir* (pn index)
             (if (and (<= (first pn) n)
                      (< n (second pn)))
                 [index (- n (first pn))]
                 (ir* (rest pn) (inc index)))))
    (ir* perm-numbers 0)))

(defun dec-key (m k)
  (if (= 1 (get m k))
      (dissoc m k)
      (update m k #'dec)))

(defun factorial-numbers-with-duplicates (n fr)
  (labels ((digits* (n digits freqs)
             (let ((skeys (sort (->list (keys freqs)) #'less-than)))
               (if (zero? n) (into digits (take (apply #'+ (->list (vals freqs))) (repeat 0)))
                   (let* ((ir (index-remainder (initial-perm-numbers freqs) n))
                          (index (get ir 0))
                          (remainder (get ir 1)))
                     (digits* remainder
                              (conj digits index)
                              (let ((nth-key (nth skeys index))) (dec-key freqs nth-key))))))))
    (digits* n [] fr)))

(defun nth-permutation-duplicates (l n)
  (when (>= n (count-permutations l))
    (error "~a is too large. Input has only ~a permutations." n  (count-permutations l)))
  (let ((f (frequencies l)))
    (labels ((perm* (freqs indices perm)
               (let ((skeys (sort (->list (keys freqs)) #'less-than)))
                 (if (empty? indices) perm
                     (let* ((i (first indices))
                            (item (nth skeys i)))
                       (perm* (dec-key freqs item)
                              (rest indices)
                              (conj perm item)))))))
      (perm* f (factorial-numbers-with-duplicates n f) []))))

(defun nth-permutation (items n)
  (if (sorted-numbers? items)
      (if (all-different? items)
          (nth-permutation-distinct items n)
          (nth-permutation-duplicates items n))
      (if (all-different? items)
          (let ((v (vec items))
                (perm-indices (nth-permutation-distinct (range (count items)) n)))
            (vec (map (lambda (idx) (get v idx)) perm-indices)))
          (let* ((v (vec (distinct items)))
                 (f (frequencies items))
                 (indices (mapcat (lambda (i) (take (get f (get v i)) (repeat i))) (range (count v)))))
            (vec (map (lambda (idx) (get v idx)) (nth-permutation-duplicates indices n)))))))

(defun drop-permutations (items n)
  (cond
    ((zero? n) (permutations items))
    ((= n (count-permutations items)) ())
    (t (if (sorted-numbers? items)
           (if (all-different? items)
               (vec-lex-permutations (nth-permutation-distinct items n))
               (vec-lex-permutations (nth-permutation-duplicates items n)))
           (if (all-different? items)
               (let ((v (vec items))
                     (perm-indices (nth-permutation-distinct (range (count items)) n)))
                 (map
                  (lambda (idxs)
                    (map
                     (lambda (idx)
                       (get v idx))
                     idxs))
                  (vec-lex-permutations perm-indices)))
               (let* ((v (vec (distinct items)))
                      (f (frequencies items))
                      (indices (mapcat (lambda (i) (take (get f (get v i)) (repeat i))) (range (count v)))))
                 (map
                  (lambda (idxs)
                    (map
                     (lambda (idx)
                       (get v idx))
                     idxs))
                  (vec-lex-permutations (nth-permutation-duplicates indices n)))))))))

(defun n-take-k (n k)
  (cond
    ((< k 0) 0)
    ((> k n) 0)
    ((zero? k) 1)
    ((= k 1) n)
    ((> k (quot n 2)) (n-take-k n (- n k)))
    (t (/ (apply #'* (->list (range (inc (- n k)) (inc n))))
          (apply #'* (->list (range 1 (inc k))))))))

(defun count-combinations-from-frequencies (freqs n)
  (let* ((counts (vals freqs))
         (sum (apply #'+ (->list counts))))
    (cond
      ((zero? n) 1)
      ((= n 1) (count freqs))
      ((every? (lambda (i) (= i 1)) counts) (n-take-k (count freqs) n))
      ((> n sum) 0)
      ((= n sum) 1)
      ((= (count freqs) 1) 1)
      (t (let ((new-freqs (dec-key freqs (first (keys freqs)))))
           (+ (count-combinations-from-frequencies new-freqs (dec n))
              (count-combinations-from-frequencies (dissoc freqs (first (keys freqs))) n)))))))
;;
;;  TODO: figure out how to do the correct memoization (or the same effect) as in the original code
;;
(defun count-combinations (items n)
  (if (all-different? items)
      (n-take-k (count items) n)
      (count-combinations-from-frequencies (frequencies items) n)))

(defun count-subsets (items)
  (cond
    ((empty? items) 1)
    ((all-different? items) (expt 2 (count items)))
    (t (apply #'+ (map (lambda (i) (count-combinations items i)) (range 0 (inc (count items))))))))

(defun nth-combination-distinct (items tt n)
  (labels ((comb* (comb items tt n)
             (if (or (zero? n) (empty? items)) (into comb (take tt items))
                 (let ((dc-dt (n-take-k (dec (count items)) (dec tt))))
                   (if (< n dc-dt)
                       (comb* (conj comb (first items)) (rest items) (dec tt) n)
                       (comb* comb (rest items) tt (- n dc-dt)))))))
    (comb* [] items tt n)))

(defun nth-combination-freqs (freqs tt n)
  (labels ((comb* (comb freqs tt n)
             (let ((skeys (sort (->list (keys freqs)) #'less-than)))
               (if (or (zero? n) (empty? freqs))
                   (into comb (take tt (mapcat
                                        (lambda (freq)
                                          (take (get freq 1) (repeat (get freq 0))))
                                        (map (lambda (k) [k (get freqs k)]) skeys))))
                   (let* ((first-key (first skeys))
                          (remove-one-key (dec-key freqs first-key))
                          (dc-dt (count-combinations-from-frequencies remove-one-key (dec tt))))
                     (if (< n dc-dt)
                         (comb* (conj comb first-key) remove-one-key (dec tt) n)
                         (comb* comb (dissoc freqs first-key) tt (- n dc-dt))))))))
    (comb* [] freqs tt n)))

(defun nth-combination (items tt n)
  (when (>= n (count-combinations items tt))
    (error "~a is too large. Input has only ~a permutations." n  (count-combinations items tt)))
  (if (all-different? items)
      (nth-combination-distinct items tt n)
      (let* ((v (vec (distinct items)))
             (f (frequencies items))
             (indices (mapcat (lambda (i) (take (get f (get v i)) (repeat i))) (range (count v)))))
        (vec (map (lambda (idx) (get v idx)) (nth-combination-freqs (frequencies indices) tt n))))))

(defun nth-subset (items n)
  (when (>= n (count-subsets items))
    (error "~a is too large. Input has only ~a subsets." n  (count-subsets items)))
  (labels ((subset* (size n)
             (let ((num-combinations (count-combinations items size)))
               (if (< n num-combinations)
                   (nth-combination items size n)
                   (subset* (inc size) (- n num-combinations))))))
    (subset* 0 n)))

(defun list-index (l item)
  "The opposite of nth, i.e., from an item in a list, find the n"
  (labels ((n* (ll n)
             (assert (seq ll))
             (if (== item (first ll)) n
                 (n* (rest ll) (inc n)))))
    (n* l 0)))

(defun permutation-index-distinct (l)
  (labels ((index* (ll index n)
             (if (empty? ll) index
                 (index* (rest ll)
                         (+ index (* (factorial n) (list-index (sort (->list ll) #'less-than) (first ll))))
                         (dec n)))))
    (index* l 0 (dec (count l)))))

(defun permutation-index-duplicates (l)
  (labels ((index* (ll index freqs)
             (let ((skeys (sort (->list (keys freqs)) #'less-than)))
               (if (empty? ll) index
                   (index* (rest ll)
                           (reduce
                            #'+
                            (map
                             (lambda (k)
                               (count-permutations-from-frequencies (dec-key freqs k)))
                             (take-while (lambda (k) (neg? (compare k (first ll)))) skeys))
                            :initial-value index)
                           (dec-key freqs (first ll)))))))
    (index* l 0 (frequencies l))))

(defun permutation-index (items)
  "Input must be a sortable collection of items.  Returns the n such that
    (nth-permutation (sort items) n) is items."
  (if (all-different? items)
      (permutation-index-distinct items)
      (permutation-index-duplicates items)))
