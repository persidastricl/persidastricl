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
;;; sub-vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(define-immutable-class sub-vector (metadata collection seqable)
  ((v :initarg :v :reader v)
   (start :initarg :start :reader start)
   (end :initarg :end :reader end))
  (:default-initargs :v [] :start 0 :end 0 :meta nil))

(defun subvec (v start &optional (end (count v)))
  (assert (vector? v))
  (assert (> end start))
  (make-instance 'sub-vector :v v :start start :end (min end (count v))))

(defmethod conj ((sv sub-vector) &rest items)
  (if items
      (with-slots (v start end meta) sv
        (let* ((items (into #() items))
               (n (count items))
               (end (slot-value sv 'end))
               (nv (reduce
                    (lambda (v* i)
                      (assoc v* (+ end i) (get items i)))
                    (range n)
                    :initial-value v)))
          (make-instance 'sub-vector :v  nv :start start :end (+ end n) :meta meta)))
      sv))

(defmethod count ((sv sub-vector))
  (- (slot-value sv 'end) (slot-value sv 'start)))

(defmethod get ((sv sub-vector) index &optional (default nil))
  (if (< index (count sv))
      (get (slot-value sv 'v) (+ (slot-value sv 'start) index) default)
      default))

(defmethod first ((sv sub-vector))
  (get (slot-value sv 'v) (slot-value sv 'start)))

(defmethod peek ((sv sub-vector))
  (get (slot-value sv 'v) (dec (slot-value sv 'end))))

(defmethod seq ((sv sub-vector))
  (when (pos? (count sv))
    (labels ((next* (i)
               (when (< i (count sv))
                 (let ((value (get sv i)))
                   (lseq value (next* (1+ i)))))))
      (lseq (first sv) (next* 1)))))

(defmethod rest ((sv sub-vector))
  (drop 1 (seq sv)))

(defmethod rseq ((sv sub-vector))
  (with-slots (v start) sv
    (let ((count (count sv)))
      (when (pos? count)
        (let ((index (+ start (dec count))))
          (labels ((next* (i)
                     (when (>= i start)
                       (let ((value (get v i)))
                         (lseq value (next* (1- i)))))))
            (lseq (get v index) (next* (1- index)))))))))

(defmethod nth ((sv sub-vector) n &optional (default nil))
  (nth (seq sv) n default))

(defmethod next ((sv sub-vector))
  (next (seq sv)))

(defmethod head ((sv sub-vector))
  (head (seq sv)))

(defmethod tail ((sv sub-vector))
  (tail (seq sv)))

(defmethod ->list ((sv sub-vector))
  (cl:map 'list (lambda (i) (get sv i)) (loop for i from 0 below (count sv) collect i)))

(defmethod cons ((sv sub-vector) value)
  (conj sv value))

(defmethod pop ((sv sub-vector))
  (with-slots (v start end meta) sv
    (make-instance 'sub-vector :v v :start start :end (max start (dec end)) :meta meta)))

(defmethod assoc ((sv sub-vector) index item &rest kv-pairs)
  (with-slots (v start end meta) sv
    (let ((new-items (- (inc (count kv-pairs)) (- (dec end) index))))

      (labels ((adjust (vr k v) (conj vr (+ start k) v)))

        (let* ((kv-pairs (reduce
                          (lambda (v kv-pair)
                            (apply #'adjust v kv-pair))
                          (partition kv-pairs 2)
                          :initial-value (conj [] (+ start index) item)))

               (nv (apply #'assoc v (->list kv-pairs))))

          (make-instance 'sub-vector :v nv :start start :end (+ end (max 0 new-items)) :meta meta))))))

(defun pprint-sub-vector (stream sv &rest other-args)
  (declare (ignore other-args))
  (let ((*print-length* (min *print-bpvt-items* (or *print-lines* *print-bpvt-items*))))
    (pprint-logical-block (stream (->list (take (inc *print-length*) (seq sv))) :prefix "[" :suffix "]")
      (pprint-exit-if-list-exhausted)
      (loop
        (write (pprint-pop) :stream stream)
        (pprint-exit-if-list-exhausted)
        (write-char #\space stream)
        (pprint-newline :fill stream)))))

(defmethod print-object ((object sub-vector) stream)
  (format stream "~/persidastricl::pprint-sub-vector/" object))

(set-pprint-dispatch 'sub-vector 'pprint-sub-vector)

(defmethod with-meta ((sv sub-vector) meta)
  (with-slots (v start end) sv
    (make-instance 'sub-vector :v v :start start :end end :meta meta)))

(defmethod == ((s1 sub-vector) s2)
  (== (seq s1) s2))

(defmethod == (s1 (s2 sub-vector))
  (== (seq s2) s1))
