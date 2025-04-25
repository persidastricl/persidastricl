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
;;; test/thunk.lisp
;;;
;;; -----

(in-package #:persidastricl/test)

(named-readtables:in-readtable persidastricl:syntax)

(def-suite :thunk-tests
  :description "testing thunks"
  :in master-suite)

(in-suite :thunk-tests)

(test force-delay-test
  :description "test making a thunk via the delay macro"
  (let* ((m {:a 1})
         (th (delay (update m :a #'inc))))
    (is (= 1 (get m :a)))
    (is (functionp (slot-value th 'p::fn)))
    (is (nil? (slot-value th 'p::r)))

    (let ((m (force th)))
      (is (= 2 (get m :a))))

    (is (nil? (slot-value th 'p::fn)))
    (is (== (slot-value th 'p::r) {:a 2}))))

;; (5am:run! :thunk-tests)
