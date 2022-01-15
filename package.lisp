;;;; package.lisp

(defpackage #:persistent-vector
  (:documentation "A fast implementation of the Persistent Vector data structure, based upon Clojure.")
  (:use #:cl)
  (:shadow
   #:length
   #:equal
   #:append
   #:map
   #:reduce)
  (:export
   ;; Builders
   #:with-transient
   #:vec
   #:append
   #:pop-last
   #:set-at

   ;; Accessors and equality
   #:val-at
   #:equal
   #:length

   ;; Looping
   #:map
   #:reduce
   #:dovector

   ;; Types
   #:persistent-vector
   #:transient-vector
   #:vector-trie
   #:persistent-vector-p
   #:transient-vector-p
   #:vector-trie-p))
