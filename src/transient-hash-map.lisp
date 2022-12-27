;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-hash-map.lisp
;;;
;;; transient hash maps
;;;
;;; -----

(in-package :persidastricl)

;; -----
;; transient-key-value-bitmap-vector object
;;
;; -----

(defclass transient-key-value-bitmap-vector (transient-bitmap-vector key-value-bitmap-vector) ())

;; -----
;; methods
;;
;; -----

(defmethod insert ((bv transient-key-value-bitmap-vector) bit-position entry)
  "inserts the key/value from `entry` in the data vector at the 2
calculated index positions determinded by the bit-count of
bit-position with respect to all bits currently set below it in the
bitmap. The indexes in the data vector are: (* 2 determined-bit-count)
for key, and (1+ (* 2 determined-bit-count)) for value"
  (with-slots (bitmap data) bv
    (setf bitmap (b:set bit-position bitmap))
    (setf data (v:insert data (* (b:index bit-position bitmap) 2) (e:key entry) (e:value entry))))
  bv)

(defmethod update ((bv transient-key-value-bitmap-vector) bit-position entry)
  "updates the value from `entry` in the data vector at the calculated
index position determinded by the bit-count of bit-position with
respect to all bits currently set below it in the bitmap. The indexes
in the data vector is: (1+ (* 2 determined-bit-count)) for v"
  (with-slots (bitmap data) bv
    (setf data (v:update data (1+ (* (b:index bit-position bitmap) 2)) (e:value entry))))
  bv)

(defmethod remove ((bv transient-key-value-bitmap-vector) bit-position)
  "removes the key/value from `entry` in the data vector at the 2
calculated index positions determinded by the bit-count of
bit-position with respect to all bits currently set below it in the
bitmap. The indexes in the data vector are: (* 2 determined-bit-count)
for key, and (1+ (* 2 determined-bit-count)) for value"
  (with-slots (bitmap data) bv
    (setf bitmap (b:clear bit-position bitmap))
    (setf data (v:delete data (* (b:index bit-position bitmap) 2) 2)))
  bv)

(defun EMPTY-TRANSIENT-KEY-VALUE-BITMAP-VECTOR ()
  (make-instance 'transient-key-value-bitmap-vector))

;; -----
;; transient-hash-map-node
;;
;; -----

(defclass transient-hash-map-node (transient-node hash-map-node) ()
  (:default-initargs :dmap (EMPTY-TRANSIENT-KEY-VALUE-BITMAP-VECTOR) :nmap (EMPTY-TRANSIENT-NODE-BITMAP-VECTOR)))

;; -----
;; methods
;;
;; -----

(defmethod insert ((node transient-hash-map-node) position item)
  (with-slots (dmap) node
    (setf dmap (insert dmap position item)))
  node)

(defmethod insert ((node transient-hash-map-node) position (new-node transient-hash-map-node))
  (with-slots (nmap) node
    (setf nmap (insert nmap position new-node)))
  node)

(defmethod update ((node transient-hash-map-node) position item)
  (with-slots (dmap) node
    (setf dmap (update dmap position item)))
  node)

(defmethod update ((node transient-hash-map-node) position (new-node transient-hash-map-node))
  (with-slots (nmap) node
    (setf nmap (update nmap position new-node)))
  node)

(defmethod remove ((node transient-hash-map-node) position)
  (with-slots (dmap nmap) node
    (if (is-set dmap position)
        (setf dmap (remove dmap position))
        (setf nmap (remove nmap position))))
  node)


;; -----
;; transient-hash-map
;;
;;  an implementation of a transient/imperative hashed-array-mapped-trie (hamt)
;; -----

(defclass transient-hash-map (metadata)
  ((root :type 'transient-hash-map-node :initarg :root :reader :root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'transient-hash-map-node) :meta nil))

(defmethod lookup ((m transient-hash-map) key &optional (default nil))
  (with-slots (root) m
    (let ((hash (h:hash key)))
      (get root key (list hash 0 default)))))

(defmethod assoc ((m transient-hash-map) key value)
  (let ((current (lookup m key)))
    (when (not (== current value))
      (with-slots (root meta) m
        (let* ((hash (h:hash key))
               (entry (e:map-entry key value))
               (context (list hash 0)))
          (setf root (put root entry context))))))
  m)

(defmethod dissoc ((m transient-hash-map) key)
  (let ((current (lookup m key :not-found)))
    (when (not (== current :not-found))
      (with-slots (root meta) m
        (let* ((hash (h:hash key))
               (context (list hash 0)))
          (setf root (del root key context))))))
  m)
