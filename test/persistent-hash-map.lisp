;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   test/persistent-hash-map.lisp
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :persistent-hash-map-tests
  :description "testing persistent hash map operations"
  :in master-suite)

(in-suite :persistent-hash-map-tests)

(defparameter *phm* (-> (make-instance 'persistent-hash-map)
                      (assoc :k1 :v1)
                      (assoc :k2 :v2)
                      (assoc :k3 :v3)
                      (assoc :k4 :v4)
                      (assoc :k5 :v5)
                      (assoc :k6 :v6)
                      (assoc :k7 :v7)))
(setf phm (dissoc *phm* :k5))
(setf phm (dissoc phm :k7))

(lookup *phm* :k6)

(defparameter *thm*
  (-> (make-instance 'transient-hash-map)
    (assoc :k1 :v1)
    (assoc :k2 :v2)

    (assoc :k3 :v3)
    (assoc :k4 :v4)
    (assoc :k5 :v5)
    (assoc :k6 :v6)))

(lookup *thm* :k3)

(setf n (make-instance 'transient-hash-map-node))
(with-slots (dmap) n (setf dmap (insert dmap 8 (e:map-entry "k2" "v2"))))



(defparameter *thm* (-> (make-instance 'transient-hash-map)
                      (assoc :k1 :v1)
                      (assoc :k2 :v2)
                      (assoc :k3 :v3)
                      (assoc :k4 :v4)
                      (assoc :k5 :v5)
                      (assoc :k6 :v6)
                      (assoc :k7 :v7)))

(lookup *thm* :k7)

(setf thm (dissoc *thm* :k5))
(setf thm (dissoc thm :k7))

(h:hash nil)

(defparameter phs (-> (make-instance 'persistent-hash-set)))
(setf phs (conj phs 1 2 3 4 5 6))
(setf phs  (conj phs nil))
(setf phs  (conj phs :k5))
(setf phs  (conj phs :k6))
(contains? phs 7)
(contains? phs :k6)
(contains? phs nil)
(contains? phs :k8)

(setf phs (disj phs :k5 :k6))
(conj phs :k8)


(defparameter ths (-> (make-instance 'transient-hash-set)))
(conj ths 1 2 3 4 5 6)
(contains? ths 7)
(conj ths nil)
(conj ths :k5)
(conj ths :k6)
(contains? ths :k5)
(contains? ths :k6)
(contains? ths nil)

(disj ths :k5 :k6)

(defparameter *phs* (-> (make-instance 'persistent-hash-set)
                      (conj nil)
                      (conj "v1")
                      (conj "v2")
                      (conj "v3")
                      (conj "v4")
                      (conj "v5")
                      (conj "v6")
                      (conj "v7")
                      (conj :k1)
                      (conj :k2)
                      (conj :k3)
                      (conj :k4)
                      (conj :k5)
                      (conj :k7)
                      (conj :k8 :k9 :k10 :k11)
                      ))

(defparameter *phs* (-> (make-instance 'persistent-hash-set)
                      (conj nil)
                      (conj "v1")
                      (conj "v2")
                      (conj "v3")
                      (conj "v4")
                      (conj "v5")
                      (conj "v6")
                      (conj "v7")
                      (conj :k1 :k2 :k3 :k4 :k5 :k6 :k7)

                      ))
