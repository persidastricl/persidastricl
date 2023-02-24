(in-package #:persidastricl)

(defvar my-fact
  (memoize #'fact))

(defun merge (m &rest ms)
  (if (some #'identity ms)
      (lreduce
       (lambda (m1 m2)
         (if m2
             (into m1 (->plist m2))
             m1))
       (if (some? m) ms (tail (filter #'some? ms)))
       :initial-value (if (some? m) m (first (filter #'some? ms))))))

(when (some identity maps)
  (reduce1 #(conj (or %1 {}) %2) maps))

(some #'identity '(nil nil nil nil nil nil nil  1))

(keep #'identity '(nil nil nil nil nil nil nil nil nil 1))

(merge nil nil nil nil nil nil nil {:a 1 :b 2} @{:a 3 :c 4})
