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

;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; json.lisp --- still a [wip]
;;;
;;; -----

(in-package #:json)

(named-readtables:in-readtable persidastricl:syntax)

(labels ((dispatch (stream token)
           (cond
             ((eq token :eof) nil)
             ((eq token :begin-object) (input-object stream))
             ((eq token :begin-array)  (input-array stream))
             (t token)))

         (input-object (stream &optional (object {}))
           (let ((k (json-streams:json-read stream)))
             (cond
               ((eq k :end-object) object)
               ((string? k) (input-object stream (assoc object k (dispatch stream (json-streams:json-read stream)))))
               (:otherwise (error "unexpected token when expected key for object")))))

         (input-array (stream &optional (object []))
           (let ((v (json-streams:json-read stream)))
             (cond
               ((eq v :end-array) object)
               (:otherwise (input-array stream (conj object (dispatch stream v))))))))

  (defun json-stream (json &key close)
    (json-streams:make-json-input-stream json :multiple t :close-stream close))

  (defun json-seq (json)
    (let* ((json (str:trim json))
           (json (if (char= #\[ (first json)) (subseq (str:trim json) 1 (str:last-index-of json "]")) json)))
      (labels ((json-seq* (js)
                 (let ((v (dispatch js (json-streams:json-read js))))
                   (when v (lseq v (json-seq* js))))))
        (json-seq* (json-stream json)))))

  (defun decode-string (json)
    (let ((js (json-stream json)))
      (dispatch js (json-streams:json-read js))))

  (defun decode-file (filename)
    (let ((js (json-stream (slurp filename) :close t)))
      (dispatch js (json-streams:json-read js)))))


(defgeneric encode* (obj))

(defmethod encode* ((v t)) (json-streams:json-output-value v))
(defmethod encode* ((k symbol)) (json-streams:json-output-value (name k)))
(defmethod encode* ((s string)) (json-streams:json-output-value s))

(defmethod encode* ((ls p::lazy-sequence))
  (encode* (->list ls)))

(defmethod encode* ((s sequence)) (json-streams:with-json-array (mapv #'encode* s)))

(defmethod encode* ((hm p::hash-map))
  (json-streams:with-json-object
    (reduce-kv
     (lambda (_ k v)
       (declare (ignore _))
       (encode* k)
       (encode* v))
     hm
     :initial-value nil)))


(defmethod encode* ((hs p::hash-set))
  (json-streams:with-json-array (mapv #'encode* hs)))

(defmethod encode* ((v p::bpvt))
  (json-streams:with-json-array (mapv #'encode* v)))

(defun encode (obj)
  (json-streams:with-json-output (nil :key-encoder #'string-downcase)
    (encode* obj)))

;;(encode {:a 1 :b 2})
;;(encode {:a 1 :b {:c #{1 2 3}}})
;;(encode (map (fn (k v) [k v]) (range 100) (range 100 200 1)))




;; (json-seq "{\"a\" : 1} \"test\" 1 2 3 {\"b\" : 5} ")

;;  (encode (json-seq "[ {\"a\" : 1 } {\"b\" : 2 } \"test\" 1 2 3 {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } ]"))


;; (map #'walk:keywordize-keys (json-seq "[ {\"a\" : 1 } {\"b\" : 2 } \"test\" 1 2 3 {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } ]"))

;; {:b 1}
;; (json-seq "[ {\"a\" : 1 } {\"b\" : 2 } \"test\" 1 2 3 {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } ]")

;; (let* ((json "[ {\"a\" : 1 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } ]")
;;        (js  (subseq json (inc (str:index-of json "\\\[")) (str:last-index-of json "]")))
;;        )
;;   js)

;; (walk:keywordize-keys (decode-file "scratch/colors.json"))
