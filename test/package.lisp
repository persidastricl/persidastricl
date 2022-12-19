;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;; test/package.lisp
;;;
;;;  common bit operations testing
;;;
;;; -----


(defpackage #:persidastricl-test
  (:use #:cl #:fiveam)
  (:shadowing-import-from #:cl #:and #:or)
  (:export #:master-suite))

(defpackage #:bitop-test
  (:use #:cl #:fiveam #:persidastricl-test #:bitop)
  (:shadowing-import-from #:cl #:and #:or #:set)
  (:export #:bitop-tests))

(defpackage #:entry-test
  (:use #:cl #:fiveam #:persidastricl-test #:entry)
  (:shadowing-import-from #:cl #:and #:or)
  (:export #:entry-tests))

(defpackage #:hash-test
  (:use #:cl #:fiveam #:persidastricl-test #:hash)
  (:shadowing-import-from #:cl #:and #:or)
  (:export #:hash-tests))

(defpackage #:vector-test
  (:use #:cl #:fiveam #:persidastricl-test #:vector)
  (:shadowing-import-from #:cl #:and #:or #:delete)
  (:export #:vector-tests))

(defpackage #:bitmap-vector-test
  (:use #:cl #:fiveam #:persidastricl-test #:bitmap-vector)
  (:shadowing-import-from #:cl #:count #:and #:or #:get #:delete)
  (:export #:vector-tests))

(defpackage #:node-test
  (:use #:cl #:fiveam #:persidastricl-test #:node)
  (:shadowing-import-from #:cl #:and #:or #:get #:delete)
  (:export #:vector-tests))
