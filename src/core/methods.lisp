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
               ((dotted-pair? item) :alist)
               ((and (or (typep item 'entry)
                         (typep item 'simple-array)
                         (typep item 'persistent-vector)
                         (typep item 'list))
                     (twople? item))  :entries)
               (t :non-entries))))
         (prepare-sequence (seq)
           (case (classify seq)
             (:alist  (map (lambda (dp) (list (car dp) (cdr dp))) seq))
             (:entries (map #'->list seq))
             (otherwise (partition-all seq 2)))))

  (defmethod into ((obj hash-map) sequence)
    (let ((seq (seq sequence)))
      (reduce
       (lambda (m kv-pair)
         (apply #'assoc m kv-pair))
       (prepare-sequence seq)
       :initial-value obj)))

  (defmethod into ((obj hash-table) sequence)
    (let ((seq (seq sequence)))
      (reduce
       (lambda (m kv-pair)
         (setf (gethash (first kv-pair) m) (second kv-pair))
         m)
       (prepare-sequence seq)
       :initial-value obj))))

;; -----
;;  more equality
;;
;; -----

;;
;; lazy sequence equality with sequences and vectors

(defmethod == ((s1 lazy-sequence) (s2 lazy-sequence))
  (or (eq s1 s2)
      (and (= (count s1) (count s2))
           (every?
            (lambda (e1 e2)
              (== e1 e2))
            s1
            s2))))

(defmethod == ((s1 sequence) (s2 lazy-sequence))
  (== (seq s1) s2))

(defmethod == ((s1 lazy-sequence) (s2 sequence))
  (== s1 (seq s2)))

(defmethod == ((s1 vector) (s2 lazy-sequence))
  (== (seq s1) s2))

(defmethod == ((s1 lazy-sequence) (s2 vector))
  (== s1 (seq s2)))

;;
;; vectors
;;

(defmethod == ((s1 vector) (s2 vector))
  (== (seq s1) (seq s2)))

(defmethod == ((s1 sequence) (s2 vector))
  (when s1 (== (seq s2) (seq s1))))

(defmethod == ((s1 vector) (s2 sequence))
  (when s2 (== (seq s1) (seq s2))))

