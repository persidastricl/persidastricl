;;;; package.lisp

(defpackage #:persidastricl
  (:use #:cl))

(defpackage #:bitop
  (:nicknames #:bit #:b)
  (:use #:cl)
  (:shadow #:cl #:set)
  (:export #:set?
           #:clear
           #:set
           #:bits
           #:below
           #:index))


(defpackage #:entry
  (:nicknames #:e)
  (:use #:cl)
  (:export #:entry
           #:key
           #:value))


(defpackage #:hash
  (:nicknames #:h)
  (:use #:cl #:cl-murmurhash)
  (:export #:hash))


(defpackage #:vector
  (:nicknames #:v)
  (:use #:cl)
  (:shadow #:cl #:delete)
  (:export #:insert
           #:update
           #:delete))


(defpackage #:util
  (:nicknames #:u)
  (:use #:cl))


(defpackage #:counted
  (:nicknames #:c)
  (:use #:cl)
  (:shadow #:cl #:count)
  (:export #:count
           #:counted))


(defpackage #:metadata
  (:nicknames #:m)
  (:use #:cl)
  (:export #:metadata))


(defpackage #:seqable
  (:nicknames #:seq)
  (:use #:cl)
  (:export #:seqable))


(defpackage #:bitmap-vector
  (:nicknames #:bv)
  (:use #:cl #:counted)
  (:shadow #:cl #:count #:get #:delete)
  (:export #:bitmap-vector
           #:empty
           #:insert
           #:update
           #:delete
           #:set?
           #:get))


(defpackage #:node
  (:use #:cl)
  (:shadow #:cl #:put #:get #:delete)
  (:import-from #:arrow-macros #:->)
  (:export #:empty
           #:node
           #:put
           #:get
           #:delete))
