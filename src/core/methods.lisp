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

(defmethod into ((lst list) sequence)
  (if sequence
      (apply #'conj lst (->list sequence))
      lst))

(defmethod into ((a array) sequence)
  (if sequence
      (let ((size (+ (length (->list a)) (length (->list sequence)))))
        (make-array size :initial-contents (->list (concat (->list a) sequence))))
      a))

(defmethod into ((s1 string) (s2 string))
  (concatenate 'string s1 s2))

(defmethod into ((col collection) sequence)
  (reduce #'conj (seq sequence) :initial-value col))

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

  (defmethod into ((obj hash-map) sequence)
    (let ((seq (seq sequence)))
      (reduce
       (lambda (m kv-pair)
         (apply #'assoc m kv-pair))
       (if (== (classify seq) :entries)
           (map #'->list seq)
           (partition-all seq 2))
       :initial-value obj)))

  (defmethod into ((obj hash-table) sequence)
    (let ((seq (seq sequence)))
      (reduce
       (lambda (m kv-pair)
         (setf (gethash (first kv-pair) m) (second kv-pair))
         m)
       (if (== (classify seq) :entries)
           (map #'->list seq)
           (partition-all seq 2))
       :initial-value obj))))


;; -----
;;  more equality
;;
;; -----

(defmethod == ((s1 lazy-sequence) s2)
  (or (eq s1 s2)
      (and (= (count s1) (count s2))
           (every?
            (lambda (e1 e2)
              (== e1 e2))
            s1
            (seq s2)))))

(defmethod == (s1 (s2 lazy-sequence))
  (== s2 s1))
