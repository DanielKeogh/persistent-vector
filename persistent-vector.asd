;;;; persistent-vector.asd

(asdf:defsystem #:persistent-vector
  :description "Persistent/Immutable vectors based upon Clojure"
  :author "Daniel Keogh"
  :license  "Eclipse 2.0"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "utils")
               (:file "persistent-vector")
	       (:file "reader-macros")
	       (:file "api")))

