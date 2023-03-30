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
;;; data.lisp
;;;
;;; -----

(in-package :data)

(named-readtables:in-readtable persidastricl:syntax)

(defgeneric diff* (d1 d2))

(defun diff-atom (a b)
  (if (== a b) [nil nil b] [a b nil]))

(defmethod diff* (a b)
  (diff-atom a b))

(defun as-set-value (s)
  (if (set? s) s (into #{} s)))

(defun vectorize (m)
  "Convert an associative-by-numeric-index collection into
   an equivalent vector, with nil for any missing keys"
  (when (seq m)
    (reduce
     (lambda (result e)
       (dlet (([k v] (->list e)))
         (assoc result k v)))
     m
     :initial-value (vec (take (apply #'max (->list (keys m))) (repeat nil))))))


(defun diff-associative-key (a b k)
  "Diff associative things a and b, comparing only the key k."
  (dlet ((va (get a k))
         (vb (get b k))
         ([a* b* ab] (diff va vb))
         (in-a (not (== (get a k :not-found) :not-found)))
         (in-b (not (== (get b k :not-found) :not-found)))
         (same (and in-a in-b
                    (or (not (nil? ab))
                        (and (nil? va) (nil? vb))))))
    [(when (and in-a (or (not (nil? a*)) (not same))) {k a*})
     (when (and in-b (or (not (nil? b*)) (not same))) {k b*})
     (when same {k ab})]))

(defun diff-associative (a b ks)
  (vec
   (reduce
    (lambda (diff1 diff2)
      (doall (map #'merge diff1 diff2)))
    (map
     (partial #'diff-associative-key a b)
     ks)
    :initial-value [nil nil nil])))

(defun diff-sequential (a b)
  (vec (map #'vectorize (diff-associative
                         (if (vector? a) a (vec a))
                         (if (vector? b) b (vec b))
                         (range (max (count a) (count b)))))))


(defmethod diff* ((a1 p::hash-map) a2)
  (diff-associative a1 a2 (set:union (set (keys a1)) (set (keys a2)))))

(defmethod diff* ((a1 hash-table) a2)
  (diff-associative a1 a2 (set:union (set (keys a1)) (set (keys a2)))))

(defmethod diff* ((s1 string) (s2 string))
  (diff-sequential s1 s2))

(defmethod diff* ((s1 p::hash-set) s2)
  (let ((s1 (as-set-value s1))
        (s2 (as-set-value s2)))
    [(set:difference s1 s2) (set:difference s2 s1) (set:intersection s1 s2)]))

(defmethod diff* ((s1 sequence) s2)
  (diff-sequential s1 s2))

(defmethod diff* ((s1 p::vector) s2)
  (diff-sequential s1 s2))

(defmethod diff* ((s1 p::lazy-sequence) s2)
  (diff-sequential s1 s2))

(type-of %{:a 1})

(defun diff-type (x)
  (typecase x
    (p::hash-map :associative)
    (hash-table  :associative)
    (p::hash-set :set)
    (p::vector :sequential)
    (simple-array :sequential)
    (list :sequential)
    (string  :string)
    (t :unknown)))

(defun diff (a b)
  (if (== a b)
      [nil nil a]
      (if (== (diff-type a) (diff-type b))
          (diff* a b)
          (diff-atom a b))))
