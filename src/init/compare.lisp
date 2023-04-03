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
;;;   compare.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defgeneric compare (item1 item2))

(defmethod compare ((n1 number) (n2 number))
  (cond ((< n1 n2) -1)
        ((> n1 n2)  1)
        (t 0)))

(defmethod compare ((c1 character) (c2 character))
  (cond ((char< c1 c2) -1)
        ((char> c1 c2)  1)
        (t 0)))

(defmethod compare ((s1 string) (s2 string))
  (cond ((string< s1 s2) -1)
        ((string> s1 s2)  1)
        (t 0)))

(defmethod compare ((l1 list) (l2 list))
  (cond ((and (nil? l1) (nil? l2)) 0)
        ((nil? l1) -1)
        ((nil? l2)  1)
        (t (let ((cf (compare (first l1) (first l2))))
             (if (= 0 cf) (compare (rest l1) (rest l2)) cf)))))

(defmethod compare ((s1 sequence) (s2 sequence))
  (compare (coerce s1 'list) (coerce s2 'list)))

(defmethod compare ((sym1 symbol) (sym2 symbol))
  (compare (name sym1) (name sym2)))
