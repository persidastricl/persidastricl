;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   syntax.lisp
;;;
;;; generic functions
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; reader macros for persistent-hash-map
;;
;; -----

(defconstant +hash+ #\#)
(defconstant +at+ #\@)
(defconstant +exclamation+ #\!)
(defconstant +left-brace+ #\{)
(defconstant +right-brace+ #\})
(defconstant +left-bracket+ #\[)
(defconstant +right-bracket+ #\])
(defconstant +comma+ #\,)
(defconstant +space+ #\ )

(defun read-separator (stream char)
  (declare (ignore stream))
  (error "Separator ~S shouldn't be read alone" char))

(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone" char))

(defun read-next-object (separators delimiter
                         &optional (input-stream *standard-input*))
  (flet ((peek-next-char () (peek-char t input-stream t nil t))
         (discard-next-char () (read-char input-stream t nil t)))
    (if (and delimiter (char= (peek-next-char) delimiter))
        (progn
          (discard-next-char)
          nil)
        (let ((next-char (peek-next-char)))
          (cond
            ((and delimiter (char= next-char delimiter)) nil)

            ((member next-char separators)
             (discard-next-char)
             (read-next-object separators delimiter input-stream))

            (t
             (read input-stream t nil t)))))))

(defun transform-primitive (value)
  (if (symbolp value)
      (cond
        ((string-equal (symbol-name value) "true") t)
        ((string-equal (symbol-name value) "false") nil)
        ((string-equal (symbol-name value) "null") nil)
        (t value))
      value))

(defun read-persistent-map-literal (stream char)
  (declare (ignore char))
  (loop
    for object = (read-next-object (list +space+ +comma+) +right-brace+ stream)
    while object
    collect (transform-primitive object) into objects
    finally (return (apply #'persistent-hash-map objects))))

(defun read-transient-map-literal (stream char n-arg)
  (declare (ignore char n-arg))
  (loop
    for object = (read-next-object (list +space+ +comma+) +right-brace+ stream)
    while object
    collect (transform-primitive object) into objects
    finally (return (apply #'transient-hash-map objects))))

(defun read-persistent-set-literal (stream char n-arg)
  (declare (ignore char n-arg))
  (loop
    for object = (read-next-object (list +space+ +comma+) +right-brace+ stream)
    while object
    collect (transform-primitive object) into objects
    finally (return (apply #'persistent-hash-set objects))))

(defun expect (c stream)
  (let ((input-char (read-char stream)))
    (if-not (char= input-char c)
            (error (format nil "Error: unexpected char: '~a' expected, but read '~c' instead~%" c input-char))
            input-char)))

(defun read-transient-set-literal (stream char n-arg)
  (declare (ignore char n-arg))
  (expect +left-brace+ stream)
  (loop
    for object = (read-next-object (list +space+ +comma+) +right-brace+ stream)
    while object
    collect (transform-primitive object) into objects
    finally (return (apply #'transient-hash-set objects))))

(defun read-persistent-vector-literal (stream char)
  (declare (ignore char))
  (loop
    for object = (read-next-object (list +space+ +comma+) +right-bracket+ stream)
    while object
    collect (transform-primitive object) into objects
    finally (return (apply #'persistent-vector objects))))

(named-readtables:defreadtable syntax
  (:merge :standard)
  (:macro-char +at+ :dispatch)
  (:macro-char +exclamation+ :dispatch)
  (:macro-char +right-brace+ #'read-delimiter nil)
  (:macro-char +left-brace+ #'read-persistent-map-literal nil)
  (:dispatch-macro-char +at+ +left-brace+ #'read-transient-map-literal)
  (:dispatch-macro-char +hash+ +left-brace+ #'read-persistent-set-literal)
  (:dispatch-macro-char +at+ +hash+ #'read-transient-set-literal)
  (:macro-char +right-bracket+ #'read-delimiter nil)
  (:macro-char +left-bracket+ #'read-persistent-vector-literal nil))
