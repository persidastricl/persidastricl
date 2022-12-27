;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;; test/node.lisp
;;;
;;;  testing various nodes
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :node-tests
  :description ""
  :in master-suite)

(in-suite :node-tests)

(test simple-node-tests
  :description ""
  (is (= 1 1)))
