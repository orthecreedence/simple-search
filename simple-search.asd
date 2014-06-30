(asdf:defsystem simple-search
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "A simple in-memory full-text search engine."
  :depends-on (#:cl-ppcre)
  :components
  ((:file "package")
   (:file "document" :depends-on ("package"))
   (:file "index" :depends-on ("document" "package"))
   (:file "search" :depends-on ("index" "package"))))

