;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   thunk.lisp
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

(defmacro lazy-seq* (&body body)
  `(lazy-seq ,(list* `lambda `() body)) )

(defun take (n coll)
  (labels ((take* (n s)
             (when (and (seq s) (> n 0))
               (lseq (head s) (take* (1- n) (tail s))))))
    (take* n (seq coll))))

(defun drop (n seq)
  (labels ((drop* (n s)
             (if (and (seq s) (> n 0))
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

