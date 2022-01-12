;;;; package.lisp

(defpackage #:persistent-vector
  (:documentation "A fast implementation of the Persistent Vector data structure, based upon Clojure.")
  (:use #:cl)
  (:nicknames #:pv)
  (:export
   ;; Builders
   :with-transient
   :vec
   :v-append
   :v-pop-last
   :v-set-at

   ;; Accessors and equality
   :v-val-at
   :v-equal
   :v-length

   ;; Looping
   :v-map
   :v-reduce
   :v-for

   ;; Configuration
   :*max-print-vec-length*

   ;; Types
   :persistent-vector
   :transient-vector
   :vector-trie
   :persistent-vector-p
   :transient-vector-p
   :vector-trie-p))
