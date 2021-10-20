;;;; package.lisp

(defpackage #:persistent-vector
  (:documentation "A fast implementation of the Persistent Vector data structure, based upon Clojure.")
  (:use #:cl)
  (:nicknames #:pv)
  (:export
   ;; Important functions
   :vec
   :pv-append
   :pv-poplast
   :pv-length
   :pv-val-at
   :pv-set-at
   :pv-map
   :pv-reduce
   :pv-equal
   :*max-print-vec-length*
   :with-transient

   ;; Types
   :persistent-vector
   :transient-vector
   :vector-trie
   :persistent-vector-p
   :transient-vector-p
   :vector-trie-p))
