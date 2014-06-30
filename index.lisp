(in-package :simple-search)

(defclass index ()
  ((words :accessor words :initform (make-hash-table :test 'equal))
   (documents :accessor documents :initform nil)
   (sort-fields :accessor sort-fields :initform (make-hash-table :test 'equal))))

(defun make-index ()
  "Make a new index."
  (make-instance 'index))

(defun make-stopwords (words)
  "Make a quick lookup table for stopwords."
  (let* ((words words)
         (hash (make-hash-table :test 'equal :size (length words))))
    (dolist (word words)
      (setf (gethash word hash) t))
    hash))

(defparameter *english-stop-words*
  (make-stopwords '("a" "an" "and" "are" "as" "at" "be" "but" "by" "for" "if"
                    "in" "into" "is" "it" "no" "not" "of" "on" "or" "s" "such"
                    "t" "that" "the" "their" "then" "there" "these"
                    "they" "this" "to" "was" "will" "with")))

(defun remove-stopwords (words)
  "Remove stopwords from a list of words."
  (remove-if (lambda (x)
               (or (string= x "")
                   (gethash x *english-stop-words*)))
             words))

(defun split-words (str)
  "Given a string, split the words up."
  (let ((clean (cl-ppcre:create-scanner "[^a-z0-9\\.\\?!\\s@-]" :case-insensitive-mode t :single-line-mode t))
        (space (cl-ppcre:create-scanner "[\\.\\?!\\s@-]" :single-line-mode t))
        (split (cl-ppcre:create-scanner "\\s+" :single-line-mode t)))
    (let* ((str (cl-ppcre:regex-replace-all clean str ""))
           (str (cl-ppcre:regex-replace-all space str " "))
           (words (cl-ppcre:split split str))
           (words (mapcar 'string-downcase words)))
      words)))

(defun index (index doc)
  "Index a document"
  (let* ((fields (fields doc))
         (doc-meta (meta doc))
         (index-words (words index))
         (doc-id (gethash "id" fields))
         (doc-id (cond ((stringp doc-id) doc-id)
                       (doc-id (write-to-string doc-id)))))
    (unless doc-id
      (error "document being indexed must have \"id\" field"))
    (pushnew doc-id (documents index) :test 'string=)
    (loop for field being the hash-keys of fields
          for value being the hash-values of fields
          for meta = (gethash field doc-meta) do
      ;; if we have a sort field (or this is the "id" field), facilitate sorting
      ;; by saving the value of the field into memory. each document gets its
      ;; own little place in the index for storing its sort values.
      (when (or (getf meta :sort)
                (string= field "id"))
        (let ((doc-sort (gethash doc-id (sort-fields index))))
          ;; if storage for this document doesn't exist, create it
          (unless doc-sort
            (setf (gethash doc-id (sort-fields index)) (make-hash-table :test 'equal :size 4))
            (setf doc-sort (gethash doc-id (sort-fields index))))
          ;; and save our sort value
          (setf (gethash field doc-sort) value)))
      ;; split up our words (if specified) and clean out stopwords
      (let ((words (if (getf meta :tokenize)
                       (remove-stopwords (split-words value))
                       (list value))))
        (dolist (word words)
          (push doc-id (gethash word index-words)))))))

(defun unindex (index doc-id)
  "Unindex a document from this index. Removes all traces of the document and
   also cleans up any places in the index that refer to it."
  (let ((words (words index)))
    (loop for key being the hash-keys of words
          for val being the hash-values of words do
      (setf (gethash key words) (remove doc-id val :test 'string=))
      ;; if removing the doc resulted in an empty list for this word, remove
      ;; the word entry entirely.
      (when (zerop (length (gethash key words)))
        (remhash key words)))
    ;; remove it from the list of indexed docs
    (setf (documents index) (remove doc-id (documents index) :test 'string=))
    ;; remove the sort field entries
    (remhash doc-id (sort-fields index))))

(defun phrase-search (phrase body &key case-sensitive)
  "Determine if the provided phrase is contained in the body string."
  (let ((phrase (if case-sensitive
                    phrase
                    (string-downcase phrase)))
        (body (if case-sensitive
                  body
                  (string-downcase body))))
    (search phrase body)))

