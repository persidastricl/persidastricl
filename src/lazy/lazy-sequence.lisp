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
;;;   lazy-sequence.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; lazy-sequence
;;
;; -----

(defclass lazy-sequence ()
  ((head :initarg :head :reader :head)
   (tail :initarg :tail :reader :tail)))

(defmacro lseq (head tail)
  `(make-instance 'lazy-sequence :head ,head :tail (delay ,tail)))

(defmethod first ((seq lazy-sequence))
  (slot-value seq 'head))

(defmethod rest ((seq lazy-sequence))
  (when-let ((tail (slot-value seq 'tail))) (force tail)))

(defmethod last ((seq lazy-sequence))
  (let* ((curr (first seq))
         (more (rest seq)))
    (if-not more
            curr
            (last more))))

(defmethod butlast ((seq lazy-sequence) &optional (n 1))
  (when (> (bounded-count (inc n) seq) n)
    (lseq (head seq) (butlast (tail seq) n))))

(defmethod nth ((seq lazy-sequence) n &optional (default nil))
  (or (first (drop n seq)) default))

(defmethod next ((seq lazy-sequence))
  (when-let ((tail (slot-value seq 'tail))) (force tail)))

(defmethod head ((seq lazy-sequence))
  (slot-value seq 'head))

(defmethod tail ((seq lazy-sequence))
  (when-let ((tail (slot-value seq 'tail))) (force tail)))

(defmethod ->list ((obj lazy-sequence))
  (let ((lst))
    (do* ((s obj (tail s))
          (x (head s) (head s)))
         ((empty? s) lst)
      (setf lst (append lst (list x))))))

(defmethod cl-murmurhash:murmurhash ((object lazy-sequence) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (->list object) :seed seed :mix-only mix-only))

(defmethod ->array ((seq lazy-sequence))
  (when seq
    (let ((lst (->list seq)))
      (make-array (cl:length lst) :initial-contents lst))))

(defgeneric lazy-seq (obj)
  (:method (obj) (lazy-seq (list obj)))
  (:method ((s sequence)) (lazy-seq (coerce s 'list)))
  (:method ((obj function)) (lazy-seq (funcall obj)))
  (:method ((obj lazy-sequence)) obj))

(defmethod lazy-seq ((obj list))
  (unless (empty? obj)
    (lseq (head obj) (lazy-seq (tail obj)))))

(defgeneric seq (object)
  (:method ((object list)) (lazy-seq object))
  (:method ((object sequence)) (seq (coerce object 'list)))
  (:method ((object lazy-sequence)) object)
  (:method ((object hash-table)) (seq (->list object))))

(defun take (n coll)
  (labels ((take* (n s)
             (when (seq s)
               (cond
                 ((= n 1) (list (head s)))
                 ((pos? n) (lseq (head s) (take* (1- n) (tail s))))))))
    (take* n (seq coll))))

(defun drop (n seq)
  (labels ((drop* (n s)
             (if (and (seq s) (pos? n))
                 (drop* (1- n) (tail s))
                 s)))
    (drop* n (seq seq))))

(defvar *print-lazy-items* 10)

(defun pprint-lazy-sequence (stream ls &rest other-args)
  (declare (ignore other-args))
  (let ((*print-length* (min *print-lazy-items* (or *print-lines* *print-lazy-items*))))
    (pprint-logical-block (stream (->list (take (inc  *print-length*) ls)) :prefix "(" :suffix ")")
      (pprint-exit-if-list-exhausted)
      (loop
        (write (pprint-pop) :stream stream)
        (pprint-exit-if-list-exhausted)
        (write-char #\space stream)
        (pprint-newline :fill stream)))))

(defmethod print-object ((object lazy-sequence) stream)
  (format stream "~/persidastricl::pprint-lazy-sequence/" object))

(set-pprint-dispatch 'lazy-sequence 'pprint-lazy-sequence)

(defmethod count ((object lazy-sequence))
  "Danger! Endless loop on infinite lazy sequences! Use bounded-count for those"
  (labels ((count* (seq n)
             (if (empty? seq)
                 n
                 (count* (tail seq) (inc n)))))
    (count* object 0)))

(defmethod bounded-count (n thing)
  (labels ((bounded-count* (s i)
             (if (and s (< i n))
                 (bounded-count* (tail s) (inc i))
                 i)))
    (bounded-count* (seq thing) 0)))

(defmethod empty? ((seq lazy-sequence))
  (= (bounded-count 1 seq) 0))

(defmethod compare ((s1 lazy-sequence) (s2 lazy-sequence))
  (cond ((and (nil? s1) (nil? s2)) 0)
        ((nil? s1) -1)
        ((nil? s2)  1)
        (t (let ((cf (compare (first s1) (first s2))))
             (if (= 0 cf) (compare (rest s1) (rest s2)) cf)))))
