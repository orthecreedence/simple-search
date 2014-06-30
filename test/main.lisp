(defpackage :simple-search-test
  (:use :cl :simple-search :fiveam)
  (:export run-tests))
(in-package :simple-search-test)

(def-suite simple-search :description "Main simple-search test suite.")
(in-suite simple-search)

(defvar *dog-index* nil
  "Holds our global dog index.")

(defparameter *dogs*
  (list (hash '("id" "1"
                "title" "Timmy"
                "body" "Timmy is a three-legged pomeranian. He growls a lot and barks when his mom is gone. Timmy is nutso and sees things that aren't there. Or we're all blind and Timmy actually sees the truth. Timmy is deaf and going blind, but he doesn't seem to care."
                "location" "Duluth, MN"
                "tags" ("bark" "growl" "space cadet" "hoppy" "pomeranian")
                "date" 20020804))
        (hash '("id" "2"
                "name" "Wookie"
                "body" "Wookie is a pomeranian/chow (sp?) mix. He is extremely tempermental and growls like a wookie when grabbed. Wookie is a momma's boy but also enjoys his alone time denning in various hidden locations around the house. Wookie doesn't know how to play."
                "location" "Duluth, MN"
                "tags" ("serious" "pomeranian" "chow" "mutt" "grumpy" "growl")
                "date" 20030507))
        (hash '("id" "3"
                "name" "Lucy"
                "body" "Lucy is a mutt. She is very, very sheepish and tends to avoid children at all costs (but then again, who doesn't). Lucy pees instantly when frightened and has absolutely no concept of personal boundaries. She will stick her snout right into your eye if you don't swat her away."
                "location" "Duluth, MN"
                "tags" ("sheephish" "fast" "shy" "bladder control" "mutt")
                "date" 20080303))
        (hash '("id" "4"
                "name" "Kofi"
                "body" "Kofi is a serious shiba and will rip you limb from limb if you cross him. Many have tried, all have perished. Kofi likes to chase cats and crows and also says \"harrrrr\" a lot. Kofi often wonders why people spend hours in a chair looking at a screen when it's beautiful outside. I often wonder this as well."
                "location" "Santa Clara, CA"
                "tags" ("growl" "harr" "shiba" "serious")
                "date" 20120405))
        (hash '("id" "5"
                "name" "Moses"
                "body" "Moses is a half-pitt mix. He is a sweetheart who makes exaggerated yawning noises to get attention from women in their 20s. It works really well. Moses loves to chase tennis balls. Moses is also a Santa Cruz loc and will send your valley ass packing if you snake his wave, kook. Valley go home, brah."
                "location" "San Francisco, CA"
                "tags" ("yawn" "loc" "santa cruz" "tennis ball" "pitt" "mutt")
                "date" 20090812))))

(defun hash (pairs)
  "Utility for making quick hashes."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (key val) on pairs by #'cddr do
      (setf (gethash key hash) val))
    hash))

(defun find-dog (id)
  (find id *dogs* :test (lambda (x dog) (string= x (gethash "id" dog)))))

(defun test-doc (data)
  "Create a test-standard document using our hardcoded fields."
  (make-document
    '(("id")
      ("title" :tokenize t)
      ("body" :tokenize t)
      ("location" :tokenize t)
      ("tags" :tokenize t)
      ("date" :sort t))
    data))

(test make-doc
  "Do docs work as expected?"
  (let ((doc (test-doc (hash '("id" "187"
                               "title" "The Great Gatsby"
                               "body" "In my younger and more vulnerable years"
                               "tags" ("book" "new york" "high society"))))))
    (is (typep doc 'simple-search::document))))

(test (index :depends-on make-doc)
  "Can we index a bunch of documents?"
  (setf *dog-index* (make-index))
  (dolist (dog *dogs*)
    (index *dog-index* (test-doc dog)))
  (let ((docs (sort (copy-list (documents *dog-index*)) 'string<)))
    (is (equalp '("1" "2" "3" "4" "5") docs))))

(test (query :depends-on index)
  "Can we query our index???????!!!?!?!11"
  (flet ((phrase-fn (doc-id phrase)
           (let* ((dog (find-dog doc-id))
                  (search-str (concatenate 'string
                                           (gethash "body" dog) " "
                                           (gethash "title" dog) " "
                                           (reduce (lambda (a b) (concatenate 'string a " " b)) (gethash "tags" dog) :initial-value ""))))
             (phrase-search phrase search-str))))
    (let ((res1 (query *dog-index* '(:and "pomeranian") :sort '("id")))
          (res2 (query *dog-index* '(:and "mutt" (:not "shy")) :sort '("id" . :desc)))
          (res3 (query *dog-index*
                       '(:or "growl" (:phrase . "he growls a lot"))
                       :sort '("date" . :desc)
                       :phrase-fn #'phrase-fn))
          (res4 (query *dog-index*
                       '(:and "duluth"
                              (:or "chow" "pomeranian")
                              (:not "tempermental"))))
          (res5 (query *dog-index*
                       '(:or (:and (:or "shiba" "mutt") "rip")
                             "loc")
                       :sort '("date")))
          (res6 (query *dog-index*
                       '(:and (:or "yawn" "fast")
                              (:not "shy") "kook")))
          (res7 (query *dog-index*
                       '(:and (:or "three" "serious" "loc")
                              (:not (:or "shiba" "pitt")))
                       :sort '("id" . :asc)))
          (res8 (query *dog-index*
                       '(:not "chow" "shy")
                       :sort '("date")))
          (res9 (query *dog-index*
                       :all
                       :sort '("date" . :desc)
                       :limit 3
                       :offset 1)))
      (is (equalp '("1" "2") res1))
      (is (equalp '("5" "2") res2))
      (is (equalp '("4" "2" "1") res3))
      (is (equalp '("1") res4))
      (is (equalp '("5" "4") res5))
      (is (equalp '("5") res6))
      (is (equalp '("1" "2") res7))
      (is (equalp '("1" "5" "4") res8))
      (is (equalp '("5" "3" "2") res9)))))

(test (unindex :depends-on query)
  "Does unindexing work?"
  (unindex *dog-index* "2")
  (unindex *dog-index* "3")
  (let ((docs (sort (copy-list (documents *dog-index*)) 'string<)))
    (is (equalp '("1" "4" "5") docs))))

(test (unindex-query :depends-on unindex)
  "Does unindexing reflect query results?"
  (flet ((phrase-fn (doc-id phrase)
           (let* ((dog (find-dog doc-id))
                  (search-str (concatenate 'string
                                           (gethash "body" dog) " "
                                           (gethash "title" dog) " "
                                           (reduce (lambda (a b) (concatenate 'string a " " b)) (gethash "tags" dog) :initial-value ""))))
             (phrase-search phrase search-str))))
    (let ((res1 (query *dog-index* '(:and "pomeranian") :sort '("id")))
          (res2 (query *dog-index* '(:and "mutt" (:not "shy")) :sort '("id" . :desc)))
          (res3 (query *dog-index*
                       '(:or "growl" (:phrase . "he growls a lot"))
                       :sort '("date" . :desc)
                       :phrase-fn #'phrase-fn))
          (res4 (query *dog-index*
                       '(:and "duluth"
                              (:or "chow" "pomeranian")
                              (:not "tempermental"))))
          (res5 (query *dog-index*
                       '(:or (:and (:or "shiba" "mutt") "rip")
                             "loc")
                       :sort '("date")))
          (res6 (query *dog-index*
                       '(:and (:or "yawn" "fast")
                              (:not "shy") "kook")))
          (res7 (query *dog-index*
                       '(:and (:or "three" "serious" "loc")
                              (:not (:or "shiba" "pitt")))
                       :sort '("id" . :asc)))
          (res8 (query *dog-index*
                       '(:not "chow" "shy")
                       :sort '("date")))
          (res9 (query *dog-index*
                       :all
                       :sort '("date" . :desc)
                       :limit 3
                       :offset 1)))
      (is (equalp '("1") res1))
      (is (equalp '("5") res2))
      (is (equalp '("4" "1") res3))
      (is (equalp '("1") res4))
      (is (equalp '("5" "4") res5))
      (is (equalp '("5") res6))
      (is (equalp '("1") res7))
      (is (equalp '("1" "5" "4") res8))
      (is (equalp '("5" "1") res9)))))

(defun run-tests ()
  (run! 'simple-search))

