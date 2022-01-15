;;;; persistent-vector-tests.asd

(asdf:defsystem #:persistent-vector-tests
  :description "Tests for persistent-vector"
  :author "Daniel Keogh"
  :license  "Eclipse 2.0"
  :depends-on (:persistent-vector :fiveam)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "main")))))
