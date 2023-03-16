;; allow [a b _ &rest rv :as v]
;; allow {:keys [a b c] :as m}
;; allow {a :a b :b :as m}
;; allow {{:keys [a b c]} :top-level-key :as m}
;; allow :strs :syms
;; allow (a b _ &rest rv :as v) ;; list destructuring form (treat same as vector)
;; allow allow :or directive for default values
;; see what else clojure provides that is worth adopting

(in-package #:persidastricl)

(defun destructure (bindings)
  (labels ((pb (bvec b val)
             (labels ((nnext (x) (next (next x)))

                      (pvec (bvec b val)
                        (let ((gvec (gensym "V__"))
                              (gseq (gensym "S__"))
                              (gfirst (gensym "FIRST__"))
                              (has-rest? (some (lambda (s) (contains? #{'&rest} s)) b)))
                          (labels ((pvec* (ret n bs seen-rest?)
                                     (if (seq bs)
                                         (let ((firstb (first bs)))
                                           (cond
                                             ((== firstb '&rest) (pvec* (pb ret (second bs) gseq) n (nnext bs) t))
                                             ((== firstb :as) (pb ret (second bs) gvec))
                                             (:otherwise (if seen-rest?
                                                             (error "Unsupported binding form, only :as can follow &rest parameter")
                                                             (pvec* (pb (if has-rest?
                                                                            (conj ret
                                                                                  gfirst `(first ,gseq)
                                                                                  gseq `(next ,gseq))
                                                                            ret)
                                                                        firstb
                                                                        (if has-rest?
                                                                            gfirst
                                                                            `(nth ,gvec ,n)))
                                                                    (inc n)
                                                                    (next bs)
                                                                    seen-rest?)))))
                                         ret)))
                            (pvec* (let ((ret (conj bvec gvec val)))
                                     (if has-rest?
                                         (conj ret gseq (list `seq gvec))
                                         ret))
                                   0
                                   b
                                   nil))))

                      (pmap (bvec b val)
                        (let ((gmap (gensym "M__"))
                              (defaults (get b :or)))
                          (labels ((pmap* (ret bes)
                                     (if (seq bes)
                                         (let* ((bb (key (first bes)))
                                                (bk (value (first bes)))
                                                (local bb)
                                                (bv (if (and defaults (get defaults local))
                                                        (list `get gmap bk (get defaults local))
                                                        (list `get gmap bk))))
                                           (pmap* (if (symbolp bb)
                                                      (conj ret local bv)
                                                      (pb ret bb bv))
                                                  (next bes)))
                                         ret)))
                            (pmap* (-> (conj bvec gmap)
                                     (conj val)
                                     ;; (conj gmap)
                                     ;; (conj `(if (sequential? ,gmap) (into (persistent-hash-map) (seq ,gmap)) ,gmap))
                                     (lambda (ret)
                                       (let ((vas (get b :as)))
                                         (if vas
                                             (conj ret vas gmap)
                                             ret))))
                                   (let ((transforms (reduce
                                                      (lambda (tfs mk)
                                                        (if (keywordp mk)
                                                            (let ((mkn (string-downcase (symbol-name mk))))
                                                              (cond
                                                                ((string= mkn "keys") (assoc tfs mk #'->keyword))
                                                                ((string= mkn "syms") (assoc tfs mk #'intern))
                                                                ((string= mkn "strs") (assoc tfs mk #'symbol-name))
                                                                (:otherwise tfs)))
                                                            tfs))
                                                      (->keys b)
                                                      :initial-value (persistent-hash-map))))
                                     (reduce
                                      (lambda (bes entry)
                                        (reduce
                                         (lambda (m k)
                                           (assoc m k (funcall (value entry) k) ))
                                         (get bes (key entry))
                                         :initial-value (dissoc bes (key entry))))
                                      transforms
                                      :initial-value (dissoc b :as :or))))))))

               (cond
                 ((symbolp b) (-> bvec (conj b) (conj val)))
                 ((vector? b) (pvec bvec b val))
                 ((listp b) (pvec bvec (into '() b) val))
                 ((map? b) (pmap bvec b val))
                 (:otherwise (error (str "Unsupported binding form" b))))))

           (process-entry (bvec b)
             (pb bvec (first b) (second b))))

    (if (every? #'symbolp (map #'first bindings))
        "bindings"
        (partition (reduce #'process-entry bindings :initial-value []) 2))))

(defmacro dlet (bindings &rest body)
  (let ((bindings (cl:map 'list ()))))
  `(let*  ,(destructure bindings)
     ,@body))


(dlet (((a b c) [1 2 3]))
      (+ a b))

(LET* ((V__778 (PERSISTENT-VECTOR 1 2 3))
       (A (NTH V__778 0))
       (B (NTH V__778 1))
       (C (NTH V__778 2)))
  (+ A B))

(macroexpand `(dlet (((a b c) [1 2 3]))
                    (+ a b)))

(let ((bindings (list (list ['a 'b 'c '&rest 'vr :as 'v] '(1 2 3 4 5 6 7 8 9)))))
  (destructure bindings))

(let ((bindings (list (list (list 'a 'b 'c) '(1 2 3)))))
  (destructure bindings))

(let ((bindings (list (list {:keys ['a 'b 'c 'd] :as 'm :or {'d #{1 2 3}}} {:a 1 :b 2 :c 3}))))
  (destructure bindings))

(let ((bindings (list (list {{:keys ['a 'b 'c 'd] :or {'d #{1 2 3}}} :inner :as 'm :or {'d #{1 2 3}}} {:inner {:a 1 :b 2 :c 3}}))))
  (destructure bindings))

(vector? (first (list (persistent-vector 1 2 3))))


(let ((bindings '((a 1)
                  (b 2)
                  (c 3))))
  (every? #'symbolp (map #'first bindings)))








(symbol-name 'A)
(intern "A")
