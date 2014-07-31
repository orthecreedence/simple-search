(asdf:defsystem simple-search
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.1"
  :description "A simple in-memory full-text search engine."
  :depends-on (#:cl-ppcre)
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "document" :depends-on ("package" "util"))
   (:file "stemming" :depends-on ("package" "util"))
   (:file "index" :depends-on ("document" "stemming" "package" "util"))
   (:file "search" :depends-on ("index" "package" "stemming" "util"))))

