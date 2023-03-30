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
;;; test/set.lisp
;;;
;;;  testing set functions
;;;
;;; -----

(in-package #:persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(def-suite :set-tests
  :description "testing set package operations"
  :in master-suite)

(in-suite :set-tests)

(test union-test
  (is (== (set:union) #{}))
  (is (== (set:union #{}) #{}))
  (is (== (set:union #{1}) #{1}))
  (is (== (set:union #{1 2 3}) #{1 2 3}))
  (is (== (set:union #{} #{}) #{}))
  (is (== (set:union #{} #{1}) #{1}))
  (is (== (set:union #{} #{1 2 3}) #{1 2 3}))
  (is (== (set:union #{1} #{}) #{1}))
  (is (== (set:union #{1 2 3} #{}) #{1 2 3}))
  (is (== (set:union #{1} #{2}) #{1 2}))
  (is (== (set:union #{1} #{1 2}) #{1 2}))
  (is (== (set:union #{2} #{1 2}) #{1 2}))
  (is (== (set:union #{1 2} #{3}) #{1 2 3}))
  (is (== (set:union #{1 2} #{2 3}) #{1 2 3}))
  (is (== (set:union #{} #{} #{}) #{}))
  (is (== (set:union #{1} #{} #{}) #{1}))
  (is (== (set:union #{} #{1} #{}) #{1}))
  (is (== (set:union #{} #{} #{1}) #{1}))
  (is (== (set:union #{1 2} #{2 3} #{}) #{1 2 3}))
  (is (== (set:union #{1 2} #{3 4} #{5 6}) #{1 2 3 4 5 6}))
  (is (== (set:union #{1 2} #{2 3} #{1 3 4}) #{1 2 3 4}))
  (is (== (set:union #{1 2}
                     #{:a :b}
                     #{nil}
                     #{nil t}
                     #{'false 'true}
                     #{#\c "abc"}
                     #{[] [1 2]}
                     #{{} {:a 1}}
                     #{#{} #{1 2}})
          #{1 2 :a :b nil t 'false 'true #\c "abc" [] [1 2] {} {:a 1} #{} #{1 2}})))

(test intersection-test
  (signals simple-error (set:intersection))
  (is (== (set:intersection #{}) #{}))
  (is (== (set:intersection #{1}) #{1}))
  (is (== (set:intersection #{1 2 3}) #{1 2 3}))
  (is (== (set:intersection #{} #{}) #{}))
  (is (== (set:intersection #{} #{1}) #{}))
  (is (== (set:intersection #{} #{1 2 3}) #{}))
  (is (== (set:intersection #{1} #{}) #{}))
  (is (== (set:intersection #{1 2 3} #{}) #{}))
  (is (== (set:intersection #{1 2} #{1 2}) #{1 2}))
  (is (== (set:intersection #{1 2} #{3 4}) #{}))
  (is (== (set:intersection #{1 2} #{1}) #{1}))
  (is (== (set:intersection #{1 2} #{2}) #{2}))
  (is (== (set:intersection #{1 2 4} #{2 3 4 5}) #{2 4}))
  (is (== (set:intersection #{} #{} #{}) #{}))
  (is (== (set:intersection #{1} #{} #{}) #{}))
  (is (== (set:intersection #{1} #{1} #{}) #{}))
  (is (== (set:intersection #{1} #{} #{1}) #{}))
  (is (== (set:intersection #{1 2} #{2 3} #{}) #{}))
  (is (== (set:intersection #{1 2} #{2 3} #{5 2}) #{2}))
  (is (== (set:intersection #{1 2 3} #{1 3 4} #{1 3}) #{1 3}))
  (is (== (set:intersection #{1 2 3} #{3 4 5} #{8 2 3}) #{3})))

(test difference-test
  (is (== (set:difference #{}) #{}))
  (is (== (set:difference #{1}) #{1}))
  (is (== (set:difference #{1 2 3}) #{1 2 3}))
  (is (== (set:difference #{1 2} #{1 2}) #{}))
  (is (== (set:difference #{1 2} #{3 4}) #{1 2}))
  (is (== (set:difference #{1 2} #{1}) #{2}))
  (is (== (set:difference #{1 2} #{2}) #{1}))
  (is (== (set:difference #{1 2 4} #{2 3 4 5}) #{1}))
  (is (== (set:difference #{1 2} #{2 3} #{5 2}) #{1}))
  (is (== (set:difference #{1 2 3} #{1 3 4} #{1 3}) #{2}))
  (is (== (set:difference #{1 2 3} #{3 4 5} #{8 2 3}) #{1})))

(test select-test
  (is (== (set:select #'int? #{}) #{}))
  (is (== (set:select #'int? #{1 2}) #{1 2}))
  (is (== (set:select #'int? #{1 2 :a :b :c}) #{1 2}))
  (is (== (set:select #'int? #{:a :b :c}) #{})))

(def compositions #{{:name "Canon in D" :composer "J. S. Bach"}
                    {:name "Jesu, joy of man's desiring" :composer "J. S. Bach"}
                    {:name "Jerusalem" :composer "Giuseppe Verdi"}
                    {:name "Requiem in D minor" :composer "W. A. Mozart"}})

(test project-test
  (is (== (set:project compositions [:name]) #{{:name "Canon in D"}
                                               {:name "Jesu, joy of man's desiring"}
                                               {:name "Jerusalem"}
                                               {:name "Requiem in D minor"}}))
  (is (== (set:project compositions [:composer]) #{{:composer "W. A. Mozart"}
                                                   {:composer "Giuseppe Verdi"}
                                                   {:composer "J. S. Bach"}}))
  (is (== (set:project compositions [:year]) #{{}}))
  (is (== (set:project #{{}} [:name]) #{{}})))

(test rename-test
  (is (== (set:rename compositions {:name :title}) #{{:title "Canon in D" :composer "J. S. Bach"}
                                                     {:title "Jesu, joy of man's desiring" :composer "J. S. Bach"}
                                                     {:title "Jerusalem" :composer "Giuseppe Verdi"}
                                                     {:title "Requiem in D minor" :composer "W. A. Mozart"}}))
  (is (== (set:rename compositions {:year :decade}) #{{:composer "J. S. Bach" :name "Jesu, joy of man's desiring"}
                                                      {:composer "J. S. Bach" :name "Canon in D"}
                                                      {:composer "W. A. Mozart" :name "Requiem in D minor"}
                                                      {:composer "Giuseppe Verdi" :name "Jerusalem"}}))
  (is (== (set:rename #{{}} {:year :decade}) #{{}})))

(test rename-keys-test
  (is (== (set:rename-keys {:a "one" :b "two"} {:a :z}) {:z "one" :b "two"}))
  (is (== (set:rename-keys {:a "one" :b "two"} {:a :z :c :y}) {:z "one" :b "two"}))
  (is (== (set:rename-keys {:a "one" :b "two" :c "three"} {:a :b :b :a}) {:a "two" :b "one" :c "three"})))

(test index-test
  (is (== (set:index  #{{:c 2} {:b 1} {:a 1 :b 2}} [:b]) {{:b 2} #{{:a 1 :b 2}}, {:b 1} #{{:b 1}} {} #{{:c 2}}})))

(test join-test
  (is (== (set:join compositions compositions) compositions))
  (is (== (set:join compositions #{{:name "Canon in D" :genre "Classical"}})
          #{{:name "Canon in D" :composer "J. S. Bach" :genre "Classical"}})))

(test map-invert-test
  (is (== (set:map-invert {:a "one" :b "two"}) {"one" :a "two" :b})))

(test subset?-test
  (is (set:subset? #{} #{}))
  (is (set:subset? #{} #{1}))
  (is (set:subset? #{1} #{1}))
  (is (set:subset? #{1 2} #{1 2}))
  (is (set:subset? #{1 2} #{1 2 42}))
  (is (set:subset? #{'false} #{'false}))
  (is (set:subset? #{nil} #{nil}))
  (is (set:subset? #{nil} #{nil 'false}))
  (is (set:subset? #{1 2 nil} #{1 2 nil 4}))
  (is (not (set:subset? #{1} #{})))
  (is (not (set:subset? #{2} #{1})))
  (is (not (set:subset? #{1 3} #{1})))
  (is (not (set:subset? #{nil} #{'false})))
  (is (not (set:subset? #{'false} #{nil})))
  (is (not (set:subset? #{'false nil} #{nil})))
  (is (not (set:subset? #{1 2 nil} #{1 2}))))

(test superset?-test
  (is (set:superset? #{} #{}))
  (is (set:superset? #{1} #{}))
  (is (set:superset? #{1} #{1}))
  (is (set:superset? #{1 2} #{1 2}))
  (is (set:superset? #{1 2 42} #{1 2}))
  (is (set:superset? #{'false} #{'false}))
  (is (set:superset? #{nil} #{nil}))
  (is (set:superset? #{'false nil} #{'false}))
  (is (set:superset? #{1 2 4 nil 'false} #{1 2 nil}))
  (is (not (set:superset? #{} #{1})))
  (is (not (set:superset? #{2} #{1})))
  (is (not (set:superset? #{1} #{1 3})))
  (is (not (set:superset? #{nil} #{'false})))
  (is (not (set:superset? #{'false} #{nil})))
  (is (not (set:superset? #{nil} #{'false nil})))
  (is (not (set:superset? #{nil 2 3} #{'false nil 2 3}))))

;;(5am:run! :set-tests)
