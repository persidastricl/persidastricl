;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-map.lisp
;;;
;;; persistent hash maps
;;;
;;; -----

(in-package :persidastricl)

;; -----
;; persistent-key-value-bitmap-vector object
;;
;; -----

(define-immutable-class persistent-key-value-bitmap-vector (persistent-bitmap-vector key-value-bitmap-vector) ())

;; -----
;; methods
;;
;; -----

(defmethod insert ((bv persistent-key-value-bitmap-vector) bit-position entry)
  "inserts the key/value from `entry` in the data vector at the 2
calculated index positions determinded by the bit-count of
bit-position with respect to all bits currently set below it in the
bitmap. The indexes in the data vector are: (* 2 determined-bit-count)
for key, and (1+ (* 2 determined-bit-count)) for value"
  (let ((bitmap (b:set bit-position (:bitmap bv))))
    (make-instance 'persistent-key-value-bitmap-vector
                   :bitmap bitmap
                   :data (v:insert (:data bv) (* (b:index bit-position bitmap) 2) (e:key entry) (e:value entry)))))

(defmethod update ((bv persistent-key-value-bitmap-vector) bit-position entry)
  "updates the value from `entry` in the data vector at the calculated
index position determinded by the bit-count of bit-position with
respect to all bits currently set below it in the bitmap. The indexes
in the data vector is: (1+ (* 2 determined-bit-count)) for v"
  (let ((bitmap (:bitmap bv)))
    (make-instance 'persistent-key-value-bitmap-vector
                   :bitmap bitmap
                   :data (v:update (:data bv) (1+ (* (b:index bit-position bitmap) 2)) (e:value entry)))))

(defmethod remove ((bv persistent-key-value-bitmap-vector) bit-position)
  "removes the key/value from `entry` in the data vector at the 2
calculated index positions determinded by the bit-count of
bit-position with respect to all bits currently set below it in the
bitmap. The indexes in the data vector are: (* 2 determined-bit-count)
for key, and (1+ (* 2 determined-bit-count)) for value"
  (let ((bitmap (b:clear bit-position (:bitmap bv))))
    (make-instance 'persistent-key-value-bitmap-vector
                   :bitmap bitmap
                   :data (v:delete (:data bv) (* (b:index bit-position bitmap) 2) 2))))

(defun EMPTY-PERSISTENT-KEY-VALUE-BITMAP-VECTOR ()
  (make-instance 'persistent-key-value-bitmap-vector))


;; -----
;; persistent-hash-map-node
;;
;; -----

(define-immutable-class persistent-hash-map-node (persistent-node hash-map-node) ()
  (:default-initargs :dmap (EMPTY-PERSISTENT-KEY-VALUE-BITMAP-VECTOR) :nmap (EMPTY-PERSISTENT-NODE-BITMAP-VECTOR)))

;; -----
;; methods
;;
;; -----

(defmethod insert ((node persistent-hash-map-node) position item)
  (make-instance 'persistent-hash-map-node :dmap (insert (:dmap node) position item) :nmap (:nmap node)))

(defmethod insert ((node persistent-hash-map-node) position (new-node persistent-hash-map-node))
  (make-instance 'persistent-hash-map-node :dmap (:dmap node) :nmap (insert (:nmap node) position new-node)))

(defmethod update ((node persistent-hash-map-node) position item)
  (make-instance 'persistent-hash-map-node :dmap (update (:dmap node) position item) :nmap (:nmap node)))

(defmethod update ((node persistent-hash-map-node) position (new-node persistent-hash-map-node))
  (make-instance 'persistent-hash-map-node :dmap (:dmap node) :nmap (update (:nmap node) position new-node)))

(defmethod remove ((node persistent-hash-map-node) position)
  (with-slots (dmap nmap) node
    (if (is-set dmap position)
        (make-instance 'persistent-hash-map-node :dmap (remove dmap position) :nmap nmap)
        (make-instance 'persistent-hash-map-node :dmap dmap :nmap (remove nmap position)))))


;; -----
;; persistent-hash-map
;;
;;  an implementation of a persistent/immutable hashed-array-mapped-trie (hamt)
;; -----

(define-immutable-class persistent-hash-map (metadata)
  ((root :type 'persistent-hash-map-node :initarg :root :reader :root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'persistent-hash-map-node) :meta nil))

(defmethod lookup ((m persistent-hash-map) key &optional (default nil))
  (with-slots (root) m
    (let* ((hash (h:hash key)))
      (get root key (list hash 0 default)))))

(defmethod assoc ((m persistent-hash-map) key value)
  (let ((current (lookup m key)))
    (if (== current value)
        m
        (with-slots (root meta) m
          (let* ((hash (h:hash key))
                 (entry (e:map-entry key value))
                 (context (list hash 0)))
            (make-instance 'persistent-hash-map :root (put root entry context) :meta meta))))))

(defmethod dissoc ((m persistent-hash-map) key)
  (let ((current (lookup m key :not-found)))
    (if (== current :not-found)
        m
        (with-slots (root meta) m
          (let* ((hash (h:hash key)))
            (make-instance 'persistent-hash-map :root (del root key (list hash 0)) :meta meta))))))
