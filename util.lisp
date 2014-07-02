(in-package :simple-search)

(defun hash (pairs)
  "Utility for making quick hashes."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (key val) on pairs by #'cddr do
      (setf (gethash key hash) val))
    hash))

