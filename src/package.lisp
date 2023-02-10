;;;; package.lisp

(defpackage #:equality
  (:use #:cl)
  (:export #:==))

(defpackage #:immutable
  (:use #:cl)
  (:export #:immutable-class
           #:define-immutable-class))

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
  (:shadow #:cl #:append #:delete)
  (:export #:copy
           #:append
           #:insert
           #:update
           #:delete
           #:modify))

(defpackage #:bitmap-vector
  (:nicknames #:bv)
  (:use #:cl #:equality #:immutable)
  (:shadow #:cl #:remove #:count)
  (:export #:at-index
           #:at-position
           #:is-set
           #:count
           #:insert
           #:update
           #:remove
           #:EMPTY-TRANSIENT-BITMAP-VECTOR
           #:EMPTY-TRANSIENT-NODE-BITMAP-VECTOR
           #:EMPTY-TRANSIENT-KEY-VALUE-BITMAP-VECTOR
           #:EMPTY-PERSISTENT-BITMAP-VECTOR
           #:EMPTY-PERSISTENT-NODE-BITMAP-VECTOR
           #:EMPTY-PERSISTENT-KEY-VALUE-BITMAP-VECTOR))

(defpackage #:node
  (:nicknames #:n)
  (:use #:cl #:arrow-macros #:equality #:immutable)
  (:shadow #:cl #:cons #:put #:get #:delete #:insert #:update #:remove #:count)
  (:export #:add-leaf-node
           #:at-index
           #:sub-nodes
           #:count
           #:node
           #:overflow-node
           #:transient-hash-set-node
           #:transient-hash-map-node
           #:persistent-hash-set-node
           #:persistent-hash-map-node
           #:persistent-vector-node
           #:persistent-vector-leaf-node
           #:persistent-vector-tail-node
           #:cons
           #:put
           #:get
           #:delete
           #:insert
           #:update
           #:delete))

(defpackage #:persidastricl
  (:nicknames :pds :p)
  (:use #:cl #:arrow-macros #:equality #:immutable)
  (:shadow #:cl #:cons #:first #:rest #:last #:butlast #:assoc #:dissoc #:get #:delete #:remove #:length #:count #:set)
  (:export #:persistent-hash-map
           #:transient-hash-map
           #:persistent-hash-set
           #:transient-hash-set
           #:assoc
           #:dissoc
           #:lookup
           #:==
           #:syntax))
