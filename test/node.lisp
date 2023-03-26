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
