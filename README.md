# persidastricl

## Persistant Data Structures in Common Lisp

This project is very early ALPHA code. Everything! is subject to change so please use/proceed with caution.

This project started out as a learning experience and another implementation of persistent and transient HAMT maps, sets, and vectors in common lisp. After things settled a bit, other things were added a little at a time. These things include lazy sequences, basic atoms, and some common lisp interop around some core functions similar to those found in clojure. The goal was to create a familiar and practical environment where I could work as easily and efficiently in common lisp as I was used to working in clojure.  For my personal uses, the goal was achieved. For anyone else, this might not be the case so use at your own risk.

## Setting up `asdf` to find persidastricl

In order for quicklisp to load the project, `asdf` should be configured to find it. See this [link](https://github.com/lisp/net.comon-lisp.asdf/blob/master/README.source-registry) to configure asdf for searching for source trees and specific projects. There are several ways to configure things. Choose the setup that works for you.


## Building and Testing

Once you have `asdf` configured for your system, you should be able to eval the following forms to load and test the project.

```common-lisp
(ql:quickload :fiveam)
(ql:quickload :persidastricl)
(in-package :persidastricl)
(named-readtables:in-readtable persidastricl:syntax)
(ql:quickload :persidastricl/test)
(asdf:test-system :persidastricl) 
```

There is a `user` package that has been defined where you should be able to explore things without much fuss.

````common-lisp
(in-package :user)
````

If, for some reason, you can't use the sytactic sugar for maps/sets/vectors etc., then make sure the reader macros are loaded up

````common-lisp
(named-readtables:in-readtable persidastricl:syntax)
````

## Data Structures

There are three main data structures that I wanted to use in a similar way to clojure: vectors, maps, and sets.  There are both persistent and transient versions of these data structures.

### Vectors

Vectors contain values in order by an index.

```common-lisp
(defvar v1 (persistent-vector 1 2 3 4 5))
(get v1 0) ;;=> 1
```

### Maps

You add keys and values and then pull them back out when you need them.

```common-lisp
(defvar m1 (peristent-hash-map :a 1 :b 2))
(get m1 :a) ;; => 1
```

### Sets

Sets contain one and only one copy of any values you put in. You can check for set membership with `contains?`

```common-lisp
(defvar s1 (persistent-hash-set :a :b :c))
(contains? s1 :a) ;; => T
```

## Familiar Syntax

When convenient, you may prefer a touch of familiar syntax. For this, there exists a dash of syntactic sugar for these data structures (and other 'old favorites' from LISP to be shown below). I used named-readtables for this.  To turn-on the syntax use:

```common-lisp
(named-readtables:in-readtable persidastricl:syntax)
```

When using the reader-macros defined you can do things like the following:

Vectors
```common-lisp
;; persistent vector
(defvar v1 [1 2 3 4 5])
(get v1 0) ;; => 1

;; transient vector
(defvar v1 @[1 2 3 4 5])
(get v1 0) ;; => 1
```

Maps
```common-lisp
;; persistent map
(defvar m1 {:a 1 :b 2 :c 3})
(get m1 :a) ;; => 1

;; transient map
(defvar m1 @{:a 1 :b 2 :c 3})
(get m1 :a) ;; => 1
```

Sets
```common-lisp
;; persistent set
(defvar s1 #{:a :b :c})
(contains? s1 :a) ;; => :a

;; transient set
(defvar s1 @#{:a :b :c})
(contains? s1 :a) ;; => :a
```

CL hash tables (can be created and used in a similar way to the persistent/transient maps)
```common-lisp
(defvar ht %{:a 1 :b 2})
(get ht :a) ;; => 1
```

## Lazy Sequences

Also, lazy sequences are defined for most of the data structures.  There is a method named `seq` that will take a variety of new (and old) data structures and turn them into lazy sequences.  Lazy sequences have a `print-object` method defined for them that relies on a variable `*print-lazy-items*` which by dfault is set to 10 items. So, when you print a lazy sequence, it will only print out 10 of them (to avoid realizing infinite sequences)

We can make lazy sequences out of any common lisp sequence
```common-lisp
(seq '(1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0))
;; => (1 2 3 4 5 6 7 8 9 0 ...)

(seq #(1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0))
;; => (1 2 3 4 5 6 7 8 9 0 ...)

(seq "testing 1 2 3")
;; => (#\t #\e #\s #\t #\i #\n #\g #\  #\1 #\  ...)
```

For simplicity, `*print-lazy-items*` has been set to 3 for the followng examples.

All of the persistent data structures can also be made into a lazy sequence:

```common-lisp
(seq {:a 1 :b 2 :c 3 :d 4 :e 5 :g 6})
;; => ([:g 6] [:a 1] [:c 3] ...)

(seq [1 2 3 4 5 6 7 8 9 0])
;; => (1 2 3 ...)

(seq #{1 2 3 4 5 6 7 8 9 0})
;; => (7 1 6 ...)
```

More on lazy sequences below in the Examples section.

## Atoms

At the moment, a very simple implementation of atoms exists based on [Shinmera/atomics library](https://github.com/Shinmera/atomics) function `atomics:cas` for portability.

```common-lisp
(def a (atom nil))

(swap! a (fnil #'inc 0)
(deref a)
```

Atoms permit 'watchers' (`add-watch` and `remove-watch` are provided) that are notified on changes.

## Conveniences

Once these three elements were in place (persistent data structures, lazy sequences, and a basic implementation of STM atoms), much of the familiar functionality and conveniences that I wanted could easily be implemented.

Much of what you can do with clojure around maps, sets, vectors, and lazy sequences you can do with this library (examples:  map, reduce, reduce-kv, keep, filter, remove, group-by, partition, interpose, juxt ... etc).  See the core functions and methods for more.  There are other functions and methods in various locations within the code that seemed better defined in those particular locations to me but at the cost of them not being as easy to discover (especially since this library is not yet sufficiently doucmented).  The list of exported functions in `package.lisp` should also help.

## Some common lisp conveniences

Using common lisp data structures in a similar way to the new persistent data structures

```common-lisp
(defvar ht (make-hash-table))
(assoc ht :a 1 :b 2 :c 3)
(get ht :b)


(defvar lst '(1 2 3 4 5))
(assoc lst 3 :new)
(get lst 3)

(defvar v #(1 2 3 4 5))
(assoc v 3 :new)
(get v 3)

(into '() #{:a :b :c :d})
(into #() [:a :b :c :d])
(into ht {:a 5 :b 7})

;; TODO: more examples

;; NOTE: there also exists a pinch of syntactic sugar around common lisp hash tables

 %{:a 1 :b 2} ;; a common lisp hash-table

 ;; this will define a common lisp hash-table when the syntax mentioned above is turned on
```

There are also functions for creating various types of common lisp data structures from the persidastricl data structures (and hash-tables) for ease of interop/convenience.

```common-lisp
(->list [1 2 3 4])
(->plist {:a 1 :b 2}) ;; lazy
(->alist {:a 1 :b 2})
(->array {:a 1 :b 2})
```

Using a bit of a hack (which I am still trying to decide if I should leave in the code or not), you can use keywords as functions with maps.
```common-lisp
(defvar m1 {:a 1 :b 2})
(:a m1) ;; => 1
```

This is done when assoc'ing into the map. A function is created with the keyword as the symbol to look itself up in any map given as a arg to the function (also, a default value is optional). This means that keywords that were not originally used to create a key value pair in the map will not have an associated function definition and lisp will complain. This may be a BAD idea and I may remove it altogether and leave it up to the user to do this explicitly when needed.  This can be done with one of two functions:  a `make-funcallable-keyword` and `make-funcallable-keywords` which will do the same thing under programmers control.

There are a couple of macros to allow a context-dependent use of a map or set as a function as well.

```common-lisp
(let ((m1 {:a 1 :b 2}))
  (with-funcallable-map (m m1)
    (m :a))) ;; => 1

(let ((s1 #{:a :b :c}))
  (with-funcallable-set (s s1)
    (s :a))) ;; => T
```
## Other Examples

#### reduce
`reduce` has been shadowed to also take any sequential and/or lazy sequence. NOTE: This is NOT a clojure-style reduce, rather it maintains the flavor of common lisp's native reduce adding an optional keyword `:initial-value` for setting the initial argument of the reduce.

```common-lisp
(reduce #'+ (range 10))
;; => 45

(reduce #'+ (range 10) :initial-value 2)
;; => 47

;; NOTE that (reduce #'+ 0 (range 10)) with the initial value in front of the sequence will not work as it does in clojure!

```

#### map and mapv
These essentially work as they do in clojure. `map` has been re-defined to take any sequential data structure and lazily 'map' over it applying a fn. `mapv` eagerly does the same returning a persistent vector.

```common-lisp
(map #'identity "test")
;; => (#\t #\e #\s #\t)

(map #'char-code "testing 1 2 3 4")
;; => (116 101 115 116 105 110 103 32 49 32 ...)

(mapv #'inc (range 20)
;; => [1 2 3 4 5 6 7 8 9 0 10]
```

TODO: more examples

NOTE: the arrow macros are re-exported from the common lisp 'arrow-macros' library.

Functions that are in the library and still need to be documented more fully:
```common-lisp
   -<>
   -<>>
   ->
   ->>
   ->alist
   ->array
   ->list
   ->plist
   ->vector
   -vec
   <!>
   <>
   ==
   as->
   assoc
   assoc-in
   atom
   bounded-count
   butlast
   collection?
   comment
   comp
   compare
   concat
   cond->
   cond->>
   conj
   cons
   contains?
   count
   cycle
   dec
   dedup
   def
   defmemoized
   delay
   deref
   destructure
   disj
   dissoc
   distinct
   distinct?
   dlet
   do-n
   doall
   dorun
   dorun-n
   dotted-pair?
   drop
   drop-last
   drop-while
   empty
   empty?
   even?
   every-pred
   every?
   fact
   false?
   fdef
   filter
   filterv
   first
   flatten
   fn
   fnil
   force
   frequencies
   get
   get-in
   group-by
   identical?
   if-let
   if-not
   inc
   instance?
   int?
   integers
   interleave
   interpose
   into
   iterate
   juxt
   keep
   keep-indexed
   key
   keys
   keyword
   last
   lazy-cat
   lazy-seq
   length
   line-seq
   lookup
   lseq
   map
   map-indexed
   map?
   mapcat
   mapv
   max-key
   memoize
   merge
   merge-with
   meta
   metadata
   min-key
   n-choose-k
   name
   nat-int?
   neg-int?
   neg?
   next
   next-int
   nil?
   not-any?
   not-every?
   nth
   odd?
   only-valid-values
   partial
   partition
   partition-all
   partition-by
   peek
   persistent-hash-map
   persistent-hash-set
   persistent-vector
   pop
   pos-int?
   pos?
   put
   quot
   rand-nth
   rand-seq
   random-generator
   range
   re-seq
   reduce
   reduce-kv
   reductions
   repeat
   repeatedly
   replace
   reset!
   rest
   rseq
   run!
   second
   select-keys
   seq
   sequential?
   set
   set?
   shuffle
   slurp
   some
   some-<>
   some-<>>
   some->
   some->>
   some-fn
   some?
   spit
   split-at
   split-with
   str
   string?
   subs
   subseq
   subvec
   swap!
   syntax
   t-set
   t-vec
   take
   take-last
   take-nth
   take-while
   third
   trampoline
   transient!
   transient-hash-map
   transient-hash-set
   transient-vector
   tree-seq
   true?
   update
   update-in
   val
   vals
   value
   vary-meta
   vec
   vector?
   when-first
   when-let
   when-not
   while
   with-meta
   zero?
   zipmap
```


#### string operations

Package name is `string` with nicknames `str` and `s`

```common-lisp
  blank?
  capitalize
  condense
  ends-with?
  escape
  includes?
  index-of
  join
  last-index-of
  lower-case
  re-quote-replacement
  replace
  replace-first
  reverse
  split
  split-lines
  starts-with?
  trim
  trim-newline
  triml
  trimr
  upper-case
```

Example usage:

```common-lisp
(s:split  "this is a test" "\\s+")
```

#### set operations

Package name is `set`

```common-lisp
  difference
  index
  intersection
  join
  map-invert
  project
  rename
  rename-keys
  select
  subset?
  superset?
  union
```

#### data (diff)

Package name is `data`

#### walk

Package name is `walk`

```common-lisp
   keywordize-keys
   macroexpand-all
   postwalk
   postwalk-demo
   postwalk-replace
   prewalk
   prewalk-demo
   prewalk-replace
   stringify-keys
   walk
```

Example usage:

```common-lisp
(walk:keywordize-keys {"a" 1 "b" {"c" 2}})
```

#### combinatorics

Package name is `combinatorics` with nickname `c`

```common-lisp
  combinations
  subsets
  cartesian-product
  selections
  permutations
  permuted-combinations
  count-permutations
  nth-permutation
  drop-permutations
  count-combinations
  count-subsets
  nth-combination
  nth-subset
  permutation-index
```

Example usage:

```common-lisp
(c:permutations [1 2 3 4])
```

NOTE: combinatorics 'partition' functions not implemented quite yet

## Usage

See the `examples` directory for usage examples.

### PDF document

FUTURE: there may or may not be a pdf guide in the works but who knows how long that will take to be presentable. 

## License

  Copyright (c) 2019-2023 Michael D Pendergrass, pupcus.org

  This program and the accompanying materials are made available under the terms of the Eclipse Public License 2.0 which is available at https://www.eclipse.org/legal/epl-2.0/

  SPDX-License-Identifier: EPL-2.0


