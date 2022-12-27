;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;; test/persidastricl.lisp
;;;
;;;  master test suite
;;;
;;; -----

(in-package #:persidastricl)

(shadowing-import '(5am:def-suite
                    5am:in-suite
                    5am:test
                    5am:is-true
                    5am:is-false
                    5am:is
                    5am:signals))

(def-suite master-suite)
(in-suite master-suite)
