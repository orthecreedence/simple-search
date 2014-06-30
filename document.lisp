(in-package :simple-search)

(defclass document ()
  ((fields :accessor fields :initform (make-hash-table :test 'equal :size 10))
   (meta :accessor meta :initform (make-hash-table :test 'equal :size 10))))

(defun make-document (field-descriptions data)
  "Make a new document type:
    (make-document
      '((\"id\" :tokenize nil)
        (\"title\" :tokenize t))
      my-data-hash)"
  (let ((doc (make-instance 'document)))
    (dolist (field field-descriptions)
      (let* ((name (car field))
             (meta (cdr field))
             (value (gethash name data))
             (value (if (listp value)
                        (reduce (lambda (a b) (concatenate 'string a " " b))
                                value
                                :initial-value "")
                        value)))
        (setf (gethash name (fields doc)) value)
        (setf (gethash name (meta doc)) meta)))
    doc))

