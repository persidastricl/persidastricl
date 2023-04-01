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
;;; test/persidastricl.lisp
;;;
;;;  master test suite
;;;
;;; -----

(in-package #:persidastricl)


(5am:def-suite master-suite)
(5am:in-suite master-suite)


(shadowing-import '(5am:def-suite
                    5am:in-suite
                    5am:test
                    5am:is-true
                    5am:is-false
                    5am:is
                    5am:signals))

