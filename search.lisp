(in-package :simple-search)

(defun query-op (op index query results &key phrase-fn fuzzy)
  "Recursive query matching function."
  (let ((words (words index))
        (local-results (when (eq op :and) results))
        (is-not-op (eq op :not))
        ;; treat NOT like OR, but negate the results once the function is over
        (op (if (eq op :not)
                :or
                op)))
    (flet ((do-op (result)
             (cond ((eq op :and)
                    (setf local-results (intersection local-results result :test 'string=)))
                   ((eq op :or)
                    (setf local-results (append local-results result))))))
      (dolist (part query)
        (cond ((stringp part)
               (let ((docs (gethash part words)))
                 (do-op (case op
                          (:or docs)
                          (:and (remove-if-not (lambda (x) (find x docs :test 'string=))
                                               local-results))))))
              ((and (listp part)
                    (eq (car part) :phrase))
               (if phrase-fn
                   (let ((phrase-results nil))
                     (dolist (doc (documents index))
                       (when (funcall phrase-fn doc (cdr part))
                         (push doc phrase-results)))
                     (do-op (case op
                              ((:or :not) phrase-results)
                              (:and (remove-if-not (lambda (x) (find x phrase-results :test 'string=))
                                                   local-results)))))
                    ;; hardass "no phrase function? no matches"
                    (setf local-results nil)))
              ((and (listp part)
                    (find (car part) '(:and :or :not)))
               (do-op (query-op (car part)
                                index
                                (cdr part)
                                ;; if we're doing an OR, pass our full result
                                ;; set into the recursive call, otherwise we
                                ;; just pass the current filtered set.
                                (if (eq op :and) local-results results)
                                :phrase-fn phrase-fn))))))
    (if is-not-op
        (remove-if (lambda (x) (find x local-results :test 'string=)) results)
        local-results)))

(defun query (index query &key sort fuzzy phrase-fn (offset 0) limit)
  "Search the given index. Query is given as a plist:

     (query myindex '(:and \"larry\" (:or (:phrase \"nobody thinks you're funny\")
                                          (:phrase \"alright shutup\"))))

   If :fuzzy is specified, some filtering is applied to the search words to make
   them nicer.

   :sort is a pair
   
     '(\"id\" . :desc) '(\"date\" . :asc).
   
   Since the indexer stores no real data, phrases aren't available via a simple
   query. However, you can pass in a :phrase-fn argument which is a function of
   one argument (the id of the document being searched) and returns either t/nil
   meaning the document contains this phrase/phrase not found. You can use the
   provided phrase-search function to determine if a string contains a phrase."
  (let ((results (documents index)))
    (unless (or (eq query :all)
                (find (car query) '(:and :or :not :all)))
      (error "Only '(:and :or :not :all) are allowed for top-level query items. Wrap :phrase/etc in a logic form."))
    (let* ((final (if (eq query :all)
                      (documents index)
                      (query-op (car query) index (cdr query) results :phrase-fn phrase-fn :fuzzy fuzzy)))
           (final (remove-duplicates final :test 'string=))
           (final (if sort
                      (let ((field (if (listp sort) (car sort) sort))
                            (direction (or (when (listp sort) (cdr sort)) :asc)))
                        (sort final
                              (lambda (a b)
                                (let* ((val-a (gethash field (gethash a (sort-fields index))))
                                       (val-b (gethash field (gethash b (sort-fields index))))
                                       (fn (cond ((and (stringp val-a)
                                                       (eq direction :asc))
                                                  'string<)
                                                 ((and (stringp val-a)
                                                       (eq direction :desc))
                                                  'string>)
                                                 ((and (numberp val-a)
                                                       (eq direction :asc))
                                                  '<)
                                                 ((and (numberp val-a)
                                                       (eq direction :desc))
                                                  '>))))
                                  (when (and val-a val-b fn)
                                    (funcall fn val-a val-b))))))
                      final))
           (final (cond ((and offset limit) 
                         (subseq final offset (min (length final)
                                                   (+ offset limit))))
                        (offset (subseq final offset))
                        (limit (subseq final 0 (min (length final) limit)))
                        (t final))))
      final)))

