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
                     ((<= (- total (dec this-and-to-the-left)) (apply + (subvec m (inc index))))
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
          (dlet (([index this-bucket] d))
                (take (get v index) (repeat this-bucket))))
        q))
     qs)))

(defun combinations (items n)
  (let ((v-items (vec (reverse (->list items)))))
    (if (zero? n) (list ())
        (let ((cnt (count items)))
          (cond ((> n cnt) nil)
                ((= n 1) (map (lambda (item) (list item)) (distinct items)) )
                ((distinct? items) (if (= n cnt)
                                       (->list items)
                                       (map (lambda (c) (map (lambda (d) (get v-items d)) c)) (index-combinations n cnt))))
                (t (multi-comb items n)))))))

;; (setf p::*print-bpvt-items* 100)

;; (n-choose-k 26 6)

;; (into [] (take 100  (combinations '(:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z) 6)))

;; (count (n-choose-k 26 6))

;; (combinations '(:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z) 4)
