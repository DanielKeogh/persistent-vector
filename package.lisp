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
   ;;; api.lisp
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

   ;;; reader-macros.lisp
   ;; reader-macros
   #:enable-reader-macros
   #:disable-reader-macros

   ;;; persistent-vector.lisp
   ;; Types
   #:persistent-vector
   #:transient-vector
   #:vector-trie
   #:persistent-vector-p
   #:transient-vector-p
   #:vector-trie-p))
