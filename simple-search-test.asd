(asdf:defsystem simple-search-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "Tests for simple-search"
  :depends-on (:fiveam :simple-search)
  :components
  ((:module test
    :serial t
    :components
    ((:file "main")))))

