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
;;; set.lisp
;;;
;;; -----

(in-package #:set)

(named-readtables:in-readtable persidastricl:syntax)

(defun union (&rest sets)
  (reduce #'into sets))

(defun intersection (&rest sets)
  (labels ((intersect* (s1 s2)
             (reduce
              (lambda (s item)
                (if (and (contains? s1 item)
                         (contains? s2 item))
                    (conj s item)
                    s))
              (if (< (count s1) (count s2)) s1 s2)
              :initial-value #{})))
    (if (every (lambda (s) (not (empty? s))) sets)
        (reduce
         #'intersect*
         sets)
        #{})))

(defun difference (&rest sets)
  (labels ((difference* (s1 s2)
             (reduce
              (lambda (s1 item)
                (if (contains? s1 item)
                    (disj s1 item)
                    s1))
              s2
              :initial-value s1)))
    (reduce #'difference* sets)))

(defun index (xrel ks)
  (reduce
   (lambda (m x)
     (let ((ik (select-keys x ks)))
       (assoc m ik (conj (get m ik #{}) x))))
   xrel
   :initial-value {}))

(defun select (s pred)
  (reduce
   (lambda (s item)
     (if (funcall pred item)
         (conj s item)
         s))
   s
   :initial-value #{}))

(defun project (xrel ks)
  (with-meta (set (map (lambda (x) (select-keys x ks)) xrel)) (meta xrel)))

(defun rename-keys (map kmap)
  (reduce-kv
   (lambda (m old new)
     (let ((v (get map old)))
       (if v
           (assoc m new v)
           m)))
   kmap
   :initial-value (apply #'dissoc map (->list (keys kmap)))))


(defun rename (xrel kmap)
  (with-meta (set (map (lambda (x) (rename-keys x kmap)) xrel)) (meta xrel)))


(defun map-invert (m)
  (reduce-kv
   (lambda (m k v)
     (assoc m v k))
   m
   :initial-value {}))

(defun join (xrel yrel &optional km)
  (labels ((natural-join (xrel yrel)
             (if (and (seq xrel) (seq yrel))
                 (let* ((ks (intersection (set (keys (first xrel))) (set (keys (first yrel)))))
                        (rels (if (<= (count xrel) (count yrel)) [xrel yrel] [yrel xrel]))
                        (r (get rels 0))
                        (s (get rels 1))
                        (idx (index r ks)))
                   (reduce
                    (lambda (ret x)
                      (let ((found (get idx (select-keys x ks))))
                        (if found
                            (reduce
                             (lambda (inner-s m)
                               (conj inner-s (merge m x)))
                             found
                             :initial-value ret)
                            ret)))
                    s
                    :initial-value #{}))
                 #{}))
           (key-join (xrel yrel km)
             (let* ((rels (if (<= (count xrel) (count yrel))
                              [xrel yrel (map-invert km)]
                              [yrel xrel km]))
                    (r (get rels 0))
                    (s (get rels 1))
                    (k (get rels 2))
                    (idx (index r (vals k))))
               (reduce
                (lambda (ret x)
                  (let ((found (get idx (rename-keys (select-keys x (keys k)) k))))
                    (if found
                        (reduce
                         (lambda (inner-s m)
                           (conj inner-s (merge m x)))
                         found
                         :initial-value ret)
                        ret)))
                s
                :initial-value #{}))))
    (if km
        (key-join xrel yrel km)
        (natural-join xrel yrel))))

(defun subset? (set1 set2)
  (and (<= (count set1) (count set2))
       (every?
        (lambda (item)
          (contains? set2 item))
        set1)))

(defun superset? (set1 set2)
  (and (>= (count set1) (count set2))
       (every?
        (lambda (item)
          (contains? set1 item))
        set2)))
