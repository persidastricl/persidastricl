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
   #:concat
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
   #:instance?
   #:int?
   #:integers
   #:interleave
   #:interpose
   #:into
   #:iterate
   #:juxt
   #:keep
   #:keep-indexed
   #:last
   #:lazy-seq
   #:length
   #:line-seq
   #:lmap
   #:lookup
   #:lreduce
   #:lseq
   #:map-entry
   #:map-indexed
   #:map?
   #:mapcat
   #:mapv
   #:memoize
   #:meta
   #:named-fn
   #:nat-int?
   #:neg-int?
   #:neg?
   #:nil?
   #:odd?
   #:only-valid-values
   #:partition
   #:partition-all
   #:partition-by
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
   #:run!
   #:seq
   #:sequential?
   #:set
   #:set?
   #:some
   #:some-fn
   #:some?
   #:split-at
   #:split-with
   #:string?
   #:swap!
   #:syntax
   #:t-set
   #:t-vec
   #:take
   #:take-last
   #:take-nth
   #:take-while
   #:transient!
   #:transient-hash-map
   #:transient-hash-set
   #:transient-vector
   #:tree-seq
   #:update
   #:update-in
   #:vec
   #:vector?
   #:with-meta
   #:zero?
   #:zipmap
   ))
