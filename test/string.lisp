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
;;; test/string.lisp
;;;
;;; -----

(in-package #:persidastricl/test)

(named-readtables:in-readtable persidastricl:syntax)

(def-suite :string-tests
  :description "testing string package operations"
  :in master-suite)

(in-suite :string-tests)

(test split-test
  :description "test splitting strings"
  (is (== '("a" "b") (s:split "a-b" "-")))
  (is (== '("a" "b-c") (s:split "a-b-c" "-" 2)))
  (is (== '("abc") (s:split "abc" "-"))))

(test reverse-test
  :description "test reversing a string"
  (is (== "stressed" (s:reverse "desserts"))))

(test replace-test
  :description "test string replacement"
  (is (== "faabar" (s:replace "foobar" #\o #\a)))
  (is (== "foobar" (s:replace "foobar" #\z #\a)))
  (is (== "barbarbar" (s:replace "foobarfoo" "foo" "bar")))
  (is (== "foobarfoo" (s:replace "foobarfoo" "baz" "bar")))
  (is (== "f$$d" (s:replace "food" "o" "$")))
  (is (== "f\\\\d" (s:replace "food" "o" "\\")))
  (is (== "f\\$\\$d" (s:replace "food" "o" (s:re-quote-replacement "$"))))
  (is (== "f\\\\d" (s:replace "food" "o" (s:re-quote-replacement "\\"))))
  (is (== "FOObarFOO" (s:replace "foobarfoo" "foo" #'s:upper-case)))
  (is (== "foobarfoo" (s:replace "foobarfoo" "baz" #'s:upper-case)))
  (is (== "OObarOO" (s:replace "foobarfoo" "f(o+)" (lambda (mv) (s:upper-case (second mv))))))
  (is (== "baz\\bang\\" (s:replace "bazslashbangslash" "slash" (constantly "\\")))))


(test replace-first-test
  :description "test string replacement (only first match)"
  (is (== "faobar" (s:replace-first "foobar" #\o #\a)))
  (is (== "foobar" (s:replace-first "foobar" #\z #\a)))
  (is (== "z.ology" (s:replace-first "zoology" #\o #\.)))
  (is (== "barbarfoo" (s:replace-first "foobarfoo" "foo" "bar")))
  (is (== "foobarfoo" (s:replace-first "foobarfoo" "baz" "bar")))
  (is (== "f$od" (s:replace-first "food" "o" "$")))
  (is (== "f\\od" (s:replace-first "food" "o" "\\")))
  (is (== "f\\$od" (s:replace-first "food" "o" (s:re-quote-replacement "$"))))
  (is (== "f\\od" (s:replace-first "food" "o" (s:re-quote-replacement "\\"))))
  (is (== "FOObarfoo" (s:replace-first "foobarfoo" "foo" #'s:upper-case)))
  (is (== "foobarfoo" (s:replace-first "foobarfoo" "baz" #'s:upper-case)))
  (is (== "OObarfoo" (s:replace-first "foobarfoo" "f(o+)" (lambda (mv) (s:upper-case (second mv))))))
  (is (== "baz\\bangslash" (s:replace-first "bazslashbangslash" "slash" (constantly "\\")))))

(test join-test-with-no-separator
  :description "testing string joins with sequences of things"
  (is (== "" (s:join nil)))
  (is (== "" (s:join [])))
  (is (== "1" (s:join [1])))
  (is (== "12" (s:join [1 2]))))

(test join-test-with-separator
  (is (== "1,2,3" (s:join #\, [1 2 3])))
  (is (== "" (s:join #\, [])))
  (is (== "1" (s:join #\, [1])))
  (is (== "" (s:join #\, nil)))
  (is (== "1 and-a 2 and-a 3" (s:join " and-a " [1 2 3]))))

(test trim-newline-test
  :description "testing trim-newline"
  (is (== "foo" (s:trim-newline (format nil "foo~c" #\newline))))
  (is (== "foo" (s:trim-newline (format nil "foo~c~c" #\return #\newline))))
  (is (== "foo" (s:trim-newline "foo")))
  (is (== "" (s:trim-newline ""))))

(test capitalize-test
  (is (== "Foobar" (s:capitalize "foobar")))
  (is (== "Foobar" (s:capitalize "FOOBAR"))))

(test triml-test
  (is (== "foo " (s:triml " foo ")))
  (is (== "" (s:triml "   ")))
  #+sbcl
  (is (== "bar" (s:triml (format nil "~c ~cbar" #\U+2002 #\tab)))))

(test trimr-test
  (is (== " foo" (s:trimr " foo ")))
  (is (== "" (s:trimr "   ")))
  #+sbcl
  (is (== "bar" (s:trimr (format nil "bar~c ~c" #\tab #\U+2002)))))

(test trim-test
  (is (== "foo" (s:trim (format nil "  foo  ~c~c" #\return #\newline))))
  (is (== "" (s:trim "   ")))
  #+sbcl
  (is (== "bar" (s:trim (format nil "~cbar~c ~c" #\U+2000 #\tab #\U+2002)))))

(test upper-case-test
  (is (== "FOOBAR" (s:upper-case "Foobar"))))

(test lower-case-test
  (is (== "foobar" (s:lower-case "FooBar"))))

(test nil-handling-test
  :description "test how functions handle nil"
  (signals simple-error (s:condense nil))
  (signals simple-error (s:ends-with? nil "foo"))
  (signals simple-error (s:includes? nil "foo"))
  (signals simple-error (s:index-of nil "foo"))
  (signals simple-error (s:last-index-of nil "foo"))
  (signals simple-error (s:reverse nil))
  (signals simple-error (s:replace nil "foo" "bar"))
  (signals simple-error (s:replace-first nil "foo" "bar"))
  (signals simple-error (s:re-quote-replacement nil))
  (signals simple-error (s:capitalize nil))
  (signals simple-error (s:upper-case nil))
  (signals simple-error (s:lower-case nil))
  (signals simple-error (s:split nil "-"))
  (signals simple-error (s:split nil "-" 1))
  (signals simple-error (s:starts-with? nil "foo"))
  (signals simple-error (s:trim nil))
  (signals simple-error (s:triml nil))
  (signals simple-error (s:trimr nil))
  (signals simple-error (s:trim-newline nil)))

(test escape-test
  (is (== "&lt;foo&amp;bar&gt;"
          (s:escape "<foo&bar>" {#\& "&amp;" #\< "&lt;" #\> "&gt;"})))
  (is (== " \\\"foo\\\" "
          (s:escape " \"foo\" " {#\" "\\\""})))
  (is (== "faabor"
          (s:escape "foobar" {#\a #\o, #\o #\a}))))

(test blank-test
  (is (s:blank? nil))
  (is (s:blank? ""))
  (is (s:blank? " "))
  (is (s:blank? (format nil  " ~c ~c  ~c " #\NEWLINE #\TAB #\RETURN)))
  (is (not (s:blank? "  foo  "))))

(test split-lines-test
  (let ((result (s:split-lines (format nil "one~%two~c~cthree" #\return #\newline))))
    (is (== '("one" "two" "three") result)))
  (is (== '("foo") (s:split-lines "foo"))))

(test index-of-test
  (let ((s "tacos"))
    (is (== 2  (s:index-of s "c")))
    (is (== 2  (s:index-of s #\c)))
    (is (== 1  (s:index-of s "ac")))
    (is (== 3  (s:index-of s "o" 2)))
    (is (== 3  (s:index-of s  #\o  2)))
    (is (== 3  (s:index-of s "o" -100)))
    (is (== nil (s:index-of s "z")))
    (is (== nil (s:index-of s #\z)))
    (is (== nil (s:index-of s "z" 2)))
    (is (== nil (s:index-of s #\z  2)))
    (is (== nil (s:index-of s "z" 100)))
    (is (== nil (s:index-of s "z" -10)))))

(test last-index-of-test
  (let ((s "banana"))
    (is (== 4 (s:last-index-of s "n")))
    (is (== 4 (s:last-index-of s #\n)))
    (is (== 3 (s:last-index-of s "an")))
    (is (== 4 (s:last-index-of s "n" )))
    (is (== 4 (s:last-index-of s "n" 5)))
    (is (== 4 (s:last-index-of s #\n  5)))
    (is (== 4 (s:last-index-of s "n" 500)))
    (is (== nil (s:last-index-of s "z")))
    (is (== nil (s:last-index-of s "z" 1)))
    (is (== nil (s:last-index-of s #\z  1)))
    (is (== nil (s:last-index-of s "z" 100)))
    (is (== nil (s:last-index-of s "z" -10)))))

(test starts-with?-test
  (is (s:starts-with? "common lisp rocks" "common"))
  (is (not (s:starts-with? "persidastricl" "common"))))

(test ends-with?-test
  (is (s:ends-with? "Common Lisp" "Lisp"))
  (is (not (s:ends-with? "persidastricl" "stric"))))

(test includes?-test
  (let ((s "Practical Common Lisp book is practical"))
    (is (s:includes? s "on Lisp"))
    (is (not (s:includes? s "perfection")))))

(test empty-collections
  (is (== "" (str '())))
  (is (== "{}" (str {})))
  (is (== "[]" (str []))))

;;(5am:run! :string-tests)
