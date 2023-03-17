;; allow [a b _ &rest rv :as v]
;; allow {:keys [a b c] :as m}
;; allow {a :a b :b :as m}
;; allow {{:keys [a b c]} :top-level-key :as m}
;; allow :strs :syms
;; allow (a b _ &rest rv :as v) ;; list destructuring form (treat same as vector)
;; allow allow :or directive for default values
;; see what else clojure provides that is worth adopting

(in-package #:persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(defun destructure (bindings)
  (labels ((pb (bvec b val)
             (labels ((nnext (x) (next (next x)))
                      (is-persistent-vector? (b) (== (first b) 'persistent-vector))
                      (is-persistent-map? (b) (== (first b) 'persistent-hash-map))
                      ;; processing vectors and lists
                      (pvec (bvec b val) (let ((gvec (gentemp "V__"))
                                               (gseq (gentemp "S__"))
                                               (gfirst (gentemp "FIRST__"))
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
                      ;; processing maps
                      (pmap (bvec b val) (let ((gmap (gentemp "M__"))
                                               (defaults (into {} (drop 1 (get b :or)))))
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
                                                         (let* ((init (get bes (key entry)))
                                                                (init (if (is-persistent-vector? init) (into [] (drop 1 init)) init)))
                                                           (reduce
                                                            (lambda (m k)
                                                              (assoc m k (funcall (value entry) k) ))
                                                            init
                                                            :initial-value (dissoc bes (key entry)))))
                                                       transforms
                                                       :initial-value (dissoc b :as :or))))))))
               (cond
                 ((symbolp b) (-> bvec (conj b) (conj val)))
                 ((is-persistent-map? b) (pmap bvec (into {} (drop 1 b)) val))
                 ((is-persistent-vector? b) (pvec bvec (into [] (drop 1 b)) val))
                 ((listp b) (pvec bvec (into '() b) val))
                 (:otherwise (error (str "Unsupported binding form" b))))))
           (process-entry (bvec b)
             (pb bvec (first b) (second b))))
    (if (every? #'symbolp (map #'first bindings))
        bindings
        (->list  (partition (reduce #'process-entry bindings :initial-value []) 2)))))



(defmacro dlet (bindings &rest body)
  `(let* ,(destructure bindings)
     ,@body))
