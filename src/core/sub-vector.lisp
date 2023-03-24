;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; sub-vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(define-immutable-class sub-vector (collection seqable)
  ((v :initarg :v :reader v)
   (start :initarg :start :reader start)
   (end :initarg :end :reader end))
  (:default-initargs :v [] :start 0 :end 0))

(defun subvec (v start &optional (end (count v)))
  (assert (vector? v))
  (assert (> end start))
  (make-instance 'sub-vector :v v :start start :end end))

(defmethod conj ((sv sub-vector) &rest items)
  (if items
      (let* ((items (into #() items))
             (n (count items))
             (end (slot-value sv 'end))
             (nv (reduce
                  (lambda (v i)
                    (assoc v (+ end i) (get items i)))
                  (range n)
                  :initial-value (slot-value sv 'v))))
        (subvec nv (slot-value sv 'start) (+ end n)))
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
  (subvec (slot-value sv 'v) (slot-value sv 'start) (dec (slot-value sv 'end))))

(defmethod assoc ((sv sub-vector) index item &rest kv-pairs)
  (let* ((start (slot-value sv 'start))
         (end (slot-value sv 'end))
         (new-items (- (inc (count kv-pairs)) (- (dec end) index))))

    (labels ((adjust (vr k v) (conj vr (+ start k) v)))

      (let* ((kv-pairs (reduce
                        (lambda (v kv-pair)
                          (apply #'adjust v kv-pair))
                        (partition kv-pairs 2)
                        :initial-value (conj [] (+ start index) item)))

             (nv (apply #'assoc (slot-value sv 'v) (->list kv-pairs))))

        (subvec nv start (+ end (max 0 new-items)))))))

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
