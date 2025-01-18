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
;;;   test/random.lisp
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :java-util-random-test
  :description "java.util.Random compatible random number generator"
  :in master-suite)

(in-suite :java-util-random-test)

(named-readtables:in-readtable persidastricl:syntax)

(test equality-of-random-generator-with-java-util-random
  (let* ((expected [50 52 80 31 73 22 58 6 61 46 64 94 39 35 82 51 44 95 16 60 15 47 89 19 38 35
                   61 73 26 2 22 2 61 56 88 80 11 21 95 30 89 48 87 18 41 40 50 84 94 71 6 6 85
                   12 81 32 18 27 49 7 72 0 98 39 52 68 32 75 69 74 19 94 10 22 30 0 49 81 7 82
                   75 80 32 47 93 62 75 76 85 96 48 78 99 24 41 75 31 84 67 11])
         (seed 848484)
         (rnd (random-generator seed))
         (actual (take 100 (drop 1000 (rand-seq 100 rnd)))))
    (is (== expected actual))))

;;(5am:run! :java-util-random-test)
