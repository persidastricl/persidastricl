;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;; test/entry.lisp
;;;
;;;  testing making/using map entries (vector tuples)
;;;
;;; -----

(in-package #:entry-test)

(def-suite entry-tests
  :description ""
  :in master-suite)

(in-suite entry-tests)

(test entry-creation-tests
  :description "test the making of a map entry 'tuple'"
  (is (equalp #(:k :v)   (e:entry :k  :v)))
  (is (equalp #("k" "v") (e:entry "k" "v")))
  (is (equalp #(k v)   (e:entry 'k  'v)))
  (is (equalp #(1 2)     (e:entry  1  2))))

(test key-tests
  :description "test pulling they key out of map entries"
  (is (eq :k (e:key (e:entry :k :v)))))

(test value-tests
  :description "test pulling the value out of map entries"
  (is (eq :v (e:value (e:entry :k :v)))))
