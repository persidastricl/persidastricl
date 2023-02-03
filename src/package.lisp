;;;; package.lisp

(defpackage #:string
  (:nicknames #:s #:str)
  (:use #:cl)
  (:shadow #:cl #:replace)
  (:export #:to-string
           #:str
           #:join
           #:condense
           #:trim
           #:blank?
           #:replace
           #:replace-all
           #:->keyword))

(defpackage #:bits
  (:nicknames #:bit #:b)
  (:use #:cl)
  (:shadow #:cl #:set)
  (:export #:set?
           #:clear
           #:set
           #:bits
           #:below
           #:index))

(defpackage #:hash
  (:nicknames #:h)
  (:use #:cl #:cl-murmurhash)
  (:export #:hash
           #:size))

(defpackage #:entry
  (:nicknames #:e)
  (:use #:cl)
  (:export #:Entry.
           #:map-entry
           #:entry
           #:key
           #:value
           #:->vec
           #:->list
           #:->cons
           #:list->entry))

(defpackage #:vector
  (:nicknames #:v)
  (:use #:cl)
  (:shadow #:append #:cl #:delete)
  (:export #:copy
           #:append
           #:insert
           #:update
           #:delete))


(defpackage #:persidastricl
  (:nicknames :pds :p)
  (:use #:cl #:arrow-macros)
  (:shadow #:cl #:cons #:first #:rest #:append #:assoc #:count #:dissoc #:get #:delete #:remove)
  (:export #:persistent-hash-map
           #:transient-hash-map
           #:persistent-hash-set
           #:transient-hash-set
           #:assoc
           #:dissoc
           #:lookup
           #:==
           #:syntax))
