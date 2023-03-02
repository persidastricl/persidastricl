;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   core/generics.lisp
;;;
;;;   core (generic) functions
;;;
;;; -----

(in-package #:persidastricl)

(defmethod conj ((l list) &rest items)
  (->list (concat (seq l) items)))

(defgeneric into (obj sequence))

(defmethod into ((l list) sequence)
  (apply #'conj l (->list sequence)))

(defmethod into ((a array) sequence)
  (let ( (size (+ (count (->list a)) (count (->list sequence)))))
    (make-array size :initial-contents (->list (concat (->list a) sequence)))))

(defmethod into ((s1 string) (s2 string))
  (concatenate 'string s1 s2))

(defmethod into ((obj array) seq)
  (let* ((lst (->list seq))
         (size (length lst)))
    (make-array size :fill-pointer size :initial-contents lst)))

(defmethod into ((obj collection) source)
  (lreduce #'conj (seq source) :initial-value obj))

(defmethod into ((obj hash-map) source-seq)
  (let ((seq (seq source-seq)))
    (labels ((twople? (item)
               (= (count item) 2))
             (classify (s)
               (let ((item (first s)))
                 (cond
                   ((and (or (typep item 'entry)
                             (typep item 'simple-array)
                             (typep item 'persistent-vector)
                             (typep item 'list))
                         (twople? item))  :entries)
                   (t :non-entries)))))
      (lreduce
       (lambda (m kv-pair)
         (apply #'assoc m kv-pair))
       (if (== (classify seq) :entries)
           (lmap #'->list seq)
           (partition-all seq 2))
       :initial-value obj))))

;; todo : same as for hash-maps above (use labels around both fns?)
;;
;; (defmethod into ((obj hash-table) sequence)
;;   (lreduce
;;    (lambda (ht entry))))
