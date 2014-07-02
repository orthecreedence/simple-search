(in-package :simple-search)

(defun query-op (op index query results &key phrase-fn)
  "Recursive query matching function. This does all of our index searching."
  (let ((words (words index))
        ;; if we have an :and, set our results as all current results. the idea
        ;; is that we widdle away at this list as we perform our operations.
        ;; conversely if we have an :or or :not, we build up our results from
        ;; nothing.
        (local-results (when (eq op :and) results))
        (is-not-op (eq op :not))
        ;; treat NOT like OR, but negate the results once the function is over
        (op (if (eq op :not)
                :or
                op)))
    ;; define a function that correctly handles the results from various sub-
    ;; operations. mainly, :or appends while :and subtracts
    (flet ((do-op (result)
             (cond ((eq op :and)
                    (setf local-results (intersection local-results result :test 'string=)))
                   ((eq op :or)
                    (setf local-results (append local-results result))))))
      ;; loop over each part of our query, running the corresponding operations
      (dolist (part query)
        (cond ((stringp part)
               ;; this is a string, do an index lookup on it, stemming the word
               ;; if the index calls for it
               (let ((docs (if (stemming index)
                               (append (gethash part words)
                                       (gethash (stem part) words))
                               (gethash part words))))
                 (do-op (case op
                          (:or docs)
                          (:and (remove-if-not (lambda (x) (find x docs :test 'string=))
                                               local-results))))))
              ((and (listp part)
                    (eq (car part) :phrase))
               ;; we got a :phrase parameter, do a phrase search
               (if phrase-fn
                   (let ((phrase-results nil))
                     (dolist (doc-id (documents index))
                       (when (funcall phrase-fn doc-id (gethash doc-id (ref-table index)) (cdr part))
                         (push doc-id phrase-results)))
                     (do-op (case op
                              ((:or :not) phrase-results)
                              (:and (remove-if-not (lambda (x) (find x phrase-results :test 'string=))
                                                   local-results)))))
                    ;; hardass "no phrase function? no matches"
                    (setf local-results nil)))
              ((and (listp part)
                    (find (car part) '(:and :or :not)))
               ;; if we got another logic op, recurse
               (do-op (query-op (car part)
                                index
                                (cdr part)
                                ;; if we're doing an :or, pass our full result
                                ;; set into the recursive call, otherwise we
                                ;; just pass the current filtered set.
                                (if (eq op :and) local-results results)
                                :phrase-fn phrase-fn))))))
    ;; if we originally got a :not op, negate our findings, otherwise return as
    ;; is
    (if is-not-op
        (remove-if (lambda (x) (find x local-results :test 'string=)) results)
        local-results)))

(defun query (index query &key sort phrase-fn (offset 0) limit)
  "Search the given index. Query is given as a plist:

     (query myindex '(:and \"larry\" (:or (:phrase \"nobody thinks you're funny\")
                                          (:phrase \"alright shutup\"))))

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
                      (query-op (car query) index (cdr query) results :phrase-fn phrase-fn)))
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

