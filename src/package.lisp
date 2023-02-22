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
           #:->keyword
           #:includes?))

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

(defpackage #:persidastricl
  (:nicknames :pds :p)
  (:use #:cl #:arrow-macros)
  (:shadow #:cl #:some #:atom #:cons #:first #:rest #:last #:butlast #:assoc #:dissoc #:get #:delete #:remove #:length #:count #:set #:vector #:pop)
  (:export
   #:->alist
   #:->array
   #:->keys
   #:->list
   #:->plist
   #:->vals
   #:->vector
   #:-vec
   #:==
   #:assoc
   #:assoc-in
   #:atom
   #:butlast
   #:collection?
   #:conj
   #:conj
   #:cons
   #:contains?
   #:count
   #:cycle
   #:dec
   #:deref
   #:disj
   #:dissoc
   #:drop
   #:drop-last
   #:drop-while
   #:empty
   #:empty?
   #:even?
   #:every?
   #:filter
   #:filterv
   #:first
   #:flatten
   #:fn
   #:fnil
   #:get
   #:get-in
   #:inc
   #:int?
   #:integers
   #:interleave
   #:into
   #:iterate
   #:juxt
   #:keep
   #:last
   #:lazy-seq
   #:length
   #:line-seq
   #:lmap
   #:lookup
   #:lreduce
   #:lseq
   #:map-indexed
   #:map?
   #:mapv
   #:meta
   #:named-fn
   #:nat-int?
   #:neg-int?
   #:neg?
   #:odd?
   #:only-valid-values
   #:partition
   #:partition-all
   #:peek
   #:persistent-hash-map
   #:persistent-hash-set
   #:persistent-vector
   #:pop
   #:pos-int?
   #:pos?
   #:put
   #:range
   #:reduce-kv
   #:repeat
   #:repeatedly
   #:reset!
   #:rest
   #:seq
   #:sequential?
   #:set
   #:set?
   #:some
   #:some-fn
   #:split-at
   #:split-with
   #:string?
   #:swap!
   #:syntax
   #:t-set
   #:t-vec
   #:take
   #:take-last
   #:take-while
   #:transient!
   #:transient-hash-map
   #:transient-hash-set
   #:transient-vector
   #:update
   #:update-in
   #:vec
   #:vector?
   #:with-meta
   #:zero?
   #:zipmap
   ))
