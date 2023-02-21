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
  (:export #:->alist
           #:->array
           #:->list
           #:->plist
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
           #:deref
           #:disj
           #:dissoc
           #:drop
           #:empty
           #:empty?
           #:filter
           #:filterv
           #:first
           #:flatten
           #:fn
           #:fnil
           #:get
           #:get-in
           #:integers
           #:interleave
           #:into
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
           #:mapv
           #:map?
           #:meta
           #:named-fn
           #:only-valid-values
           #:partition
           #:partition-all
           #:peek
           #:persistent-hash-map
           #:persistent-hash-set
           #:persistent-vector
           #:pop
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
           #:string?
           #:swap!
           #:syntax
           #:t-set
           #:t-vec
           #:take
           #:transient!
           #:transient-hash-map
           #:transient-hash-set
           #:transient-vector
           #:update
           #:update-in
           #:vec
           #:vector?
           #:with-meta
           #:zipmap
           ))
