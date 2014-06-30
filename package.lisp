(defpackage :simple-search
  (:use :cl)
  (:export #:make-document

           #:make-index
           #:index
           #:unindex
           #:documents
           #:phrase-search

           #:query))

