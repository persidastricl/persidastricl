;;;; package.lisp

(defpackage #:equality
  (:use #:cl)
  (:export #:==))

(defpackage #:util
  (:use #:cl)
  (:export #:while
           #:if-not
           #:when-not
           #:if-let
           #:when-let
           #:empty?))

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
  (:use #:cl #:arrow-macros #:equality #:immutable #:util)
  (:shadow #:cl #:cons #:put #:get #:delete #:insert #:update #:remove #:count #:pop)
  (:export #:add-leaf-node
           #:get-leaf-node
           #:remove-leaf-node
           #:pop
           #:at-index
           #:sub-nodes
           #:count
           #:node
           #:overflow-node
           #:transient-hash-set-node
           #:transient-hash-map-node
           #:persistent-hash-set-node
           #:persistent-hash-map-node
           #:transient-vector-node
           #:transient-vector-leaf-node
           #:persistent-vector-node
           #:persistent-vector-leaf-node
           #:cons
           #:put
           #:get
           #:delete
           #:insert
           #:update
           #:delete))

(defpackage #:persidastricl
  (:nicknames :pds :p)
  (:use #:cl #:arrow-macros #:equality #:immutable #:util)
  (:shadow #:cl #:atom #:cons #:first #:rest #:last #:butlast #:assoc #:dissoc #:get #:delete #:remove #:length #:count #:set #:vector #:pop)
  (:export #:transient-hash-map
           #:persistent-hash-map
           #:transient-hash-set
           #:persistent-hash-set
           #:transient-vector
           #:persistent-vector
           #:assoc
           #:dissoc
           #:lookup
           #:==
           #:syntax))
