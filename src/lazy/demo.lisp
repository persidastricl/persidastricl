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
;;;   lazy-seq/examples.lisp
;;;
;;; -----

;;; just for fun :-)

(in-package #:persidastricl)

(time
 (into [] (take 100 fib)))

(time
 (into [] (take 50 trib)))

(time
 (first (take 1 (drop 200 trib))))

(time
 (into [] (take 30 (squares))))

(time
 (into [] (take 30 (triangulars))))

(time
 (into [] (take 30 (hexagonals))))

(time
 (into [] (take 30 (catalan-seq))))

(time
 (into [] (take 30 (catalan-seq 10))))

(time
 (into [] (take 30 (primes-seq))))

(time
 (into [] (take 30 (drop 5000 (primes-seq)))))
