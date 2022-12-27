;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;; test/entry.lisp
;;;
;;;  testing making/using map entries (vector tuples)
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :entry-tests
  :description "testing map entries"
  :in master-suite)

(in-suite :entry-tests)

(defun proper-entry-p (entry k v)
  (is (equalp k (e:key entry)))
  (is (equalp v (e:value entry))))

(test entry-creation-tests
      :description "test the making of a map entry 'tuple'"
      (is (proper-entry-p (e:map-entry :k  :v)  :k  :v))
      (is (proper-entry-p (e:map-entry "k" "v") "k" "v"))
      (is (proper-entry-p (e:map-entry 'k  'v)  'k  'v))
      (is (proper-entry-p (e:map-entry  1  2)    1   2)))

(test key-tests
      :description "test pulling they key out of map entries"
      (is (eq :k (e:key (e:map-entry :k :v)))))

(test value-tests
      :description "test pulling the value out of map entries"
      (is (eq :v (e:value (e:map-entry :k :v)))))
