;;;; package.lisp

(defpackage #:persistent-vector
  (:documentation "A fast implementation of the Persistent Vector data structure, based upon Clojure.")
  (:use #:cl)
  (:nicknames #:pv)
  (:export
   ;; Important functions
   :vec
   :v-append
   :v-pop-last
   :v-length
   :v-val-at
   :v-set-at
   :v-map
   :v-reduce
   :v-equal
   :*max-print-vec-length*
   :with-transient

   ;; Types
   :persistent-vector
   :transient-vector
   :vector-trie
   :persistent-vector-p
   :transient-vector-p
   :vector-trie-p))
