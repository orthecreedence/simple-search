(defpackage :simple-search
  (:use :cl)
  (:export #:make-document

           #:documents
           #:make-index
           #:index
           #:unindex
           #:phrase-search

           #:query
           #:process-search-string))

