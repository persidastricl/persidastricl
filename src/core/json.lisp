;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; json.lisp
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

(json-seq "{\"a\" : 1} \"test\" 1 2 3 {\"b\" : 5} ")
(map #'walk:keywordize-keys (json-seq "[ {\"a\" : 1 } {\"b\" : 2 } \"test\" 1 2 3 {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } ]"))


(let* ((json "[ {\"a\" : 1 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } {\"b\" : 2 } ]")
       (js  (subseq json (inc (str:index-of json "\\\[")) (str:last-index-of json "]")))
       )
  js)
