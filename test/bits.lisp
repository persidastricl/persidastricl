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
;;; test/bitop.lisp
;;;
;;;  common bit operations testing
;;;
;;; -----

(in-package #:persidastricl/test)

(def-suite :bits-tests
  :description "testing bit operations"
  :in master-suite)

(in-suite :bits-tests)

(test set?-tests
  :description "test the checking of set bit positions in a bitmap"
  (is-true  (b:set? 0 #b01))
  (is-false (b:set? 0 #b10)))

(test set-tests
  :description "test the setting of bit positions in a bitmap"
  (is (= #b1000 (b:set 3 #b0000)))
  (is (= #b0011 (b:set 1 #b0001)))
  (is (= #b0001 (b:set 0 #b0001))))

(test clear-tests
  :description "test the clearing of bit positions in a bitmap"
  (is (= #b0000 (b:clear 3 #b1000)))
  (is (= #b0001 (b:clear 1 #b0011)))
  (is (= #b0010 (b:clear 0 #b0010))))

(test below-tests
  :description "test the clearing of bits above a bit position (return bits below)"
  (is (= #b00000000000000001111111111111111 (b:below 16 #b11111111111111111111111111111111))))

(test index-tests
  :description "given a bitmap return the number of 'on' bits below the bit-position"
  (is (= 0 (b:index 16 #b11111111111111110000000000000000)))
  (is (= 5 (b:index 31 #b11000000110000000000000000000011))))

;; (5am:run! :bits-tests)
