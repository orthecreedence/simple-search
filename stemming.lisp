;;; It's important to note that most of the code here is adapted from Lunr.js's
;;; stemming support. If things look a bit less lispy than my normal code, it's
;;; because I did a 1:1 conversion in that spot and didn't care enough to change
;;; the structure to be pragmatic lisp.

(in-package :simple-search)

(defparameter *step2-list*
  (hash '("ational" "ate"
          "tional" "tion"
          "enci" "ence"
          "anci" "ance"
          "izer" "ize"
          "bli" "ble"
          "alli" "al"
          "entli" "ent"
          "eli" "e"
          "ousli" "ous"
          "ization" "ize"
          "ation" "ate"
          "ator" "ate"
          "alism" "al"
          "iveness" "ive"
          "fulness" "ful"
          "ousness" "ous"
          "aliti" "al"
          "iviti" "ive"
          "biliti" "ble"
          "logi" "log")))

(defparameter *step3-list*
  (hash '("icate" "ic"
          "ative" ""
          "alize" "al"
          "iciti" "ic"
          "ical" "ic"
          "ful" ""
          "ness" "")))

(let* ((c "[^aeiou]")
       (v "[aeiouy]")
       (cs (concatenate 'string c "[^aeiouy]*"))
       (vs (concatenate 'string v "[aeiou]*"))
       (mgr0 (concatenate 'string "^(" cs ")?" vs cs))
       (meq1 (concatenate 'string "^(" cs ")?" vs cs "(" vs ")?$"))
       (mgr1 (concatenate 'string "^(" cs ")?" vs cs vs cs))
       (sv (concatenate 'string "^(" cs ")?" v))
       (sv2 (concatenate 'string "^" cs v "[^aeiouwxy]$")))
  (defparameter *mgr0* (cl-ppcre:create-scanner mgr0))
  (defparameter *meq1* (cl-ppcre:create-scanner meq1))
  (defparameter *mgr1* (cl-ppcre:create-scanner mgr1))
  (defparameter *sv* (cl-ppcre:create-scanner sv))
  (defparameter *sv2* (cl-ppcre:create-scanner sv2)))

(defun stem (word)
  "Convert a word to its root form. This was taken almost verbatim from lunr.js
   so a lot of setfs are present where let/let* would have sufficed. I don't
   really intend to fix it up to be correct lisp since all the tests are passing
   and I don't really immediately understand everything it's doing anyway."
  (when (< (length word) 3)
    (return-from stem word))
  (let ((stem nil)
        (suffix nil)
        (first-char (aref word 0)))
    (when (eq first-char #\y)
      (setf word (concatenate 'string "Y" (subseq word 1))))

    (let ((re (cl-ppcre:create-scanner "^(.+?)(ss|i)es$"))
          (re2 (cl-ppcre:create-scanner "^(.+?)([^s])s$")))
      (cond ((cl-ppcre:scan re word)
             (setf word (cl-ppcre:regex-replace re word "\\1\\2")))
            ((cl-ppcre:scan re2 word)
             (setf word (cl-ppcre:regex-replace re2 word "\\1\\2")))))

    (let ((re (cl-ppcre:create-scanner "^(.+?)eed$"))
          (re2 (cl-ppcre:create-scanner "^(.+?)(ed|ing)$")))
      (cond ((cl-ppcre:scan re word)
             (let ((fp (aref (nth-value 1 (cl-ppcre:scan-to-strings re word)) 0)))
               (when (cl-ppcre:scan *mgr0* fp)
                 (setf word (cl-ppcre:regex-replace ".$" word "")))))
            ((cl-ppcre:scan re2 word)
             (let ((fp (aref (nth-value 1 (cl-ppcre:scan-to-strings re2 word)) 0)))
               (setf stem fp)
               (when (cl-ppcre:scan *sv* stem)
                 (setf word stem)
                 (cond ((cl-ppcre:scan "(at|bl|iz)$" word)
                        (setf word (concatenate 'string word "e")))
                       ((cl-ppcre:scan "([^aeiouylsz])\\1$" word)
                        (setf word (cl-ppcre:regex-replace ".$" word "")))
                       ((cl-ppcre:scan *sv2* word)
                        (setf word (concatenate 'string word "e")))))))))

    (let ((re (cl-ppcre:create-scanner "^(.+?[^aeiou])y$")))
      (when (cl-ppcre:scan re word)
        (let ((fp (aref (nth-value 1 (cl-ppcre:scan-to-strings re word)) 0)))
          (setf stem fp)
          (setf word (concatenate 'string stem "i")))))
    
    (let ((re (cl-ppcre:create-scanner "^(.+?)(ational|tional|enci|anci|izer|bli|alli|entli|eli|ousli|ization|ation|ator|alism|iveness|fulness|ousness|aliti|iviti|biliti|logi)$")))
      (when (cl-ppcre:scan re word)
        (let* ((res (nth-value 1 (cl-ppcre:scan-to-strings re word))))
          (setf stem (aref res 0))
          (setf suffix (aref res 1))
          (when (cl-ppcre:scan *mgr0* stem)
            (setf word (concatenate 'string stem (gethash suffix *step2-list*)))))))

    (let ((re (cl-ppcre:create-scanner "^(.+?)(icate|ative|alize|iciti|ical|ful|ness)$")))
      (when (cl-ppcre:scan re word)
        (let* ((res (nth-value 1 (cl-ppcre:scan-to-strings re word))))
          (setf stem (aref res 0))
          (setf suffix (aref res 1))
          (when (cl-ppcre:scan *mgr0* stem)
            (setf word (concatenate 'string stem (gethash suffix *step3-list*)))))))
    
    (let ((re (cl-ppcre:create-scanner "^(.+?)(al|ance|ence|er|ic|able|ible|ant|ement|ment|ent|ou|ism|ate|iti|ous|ive|ize)$"))
          (re2 (cl-ppcre:create-scanner "^(.+?)(s|t)(ion)$")))
      (cond ((cl-ppcre:scan re word)
             (let ((fp (aref (nth-value 1 (cl-ppcre:scan-to-strings re word)) 0)))
               (setf stem fp)
               (when (cl-ppcre:scan *mgr1* stem)
                 (setf word stem))))
            ((cl-ppcre:scan re2 word)
             (let ((res (nth-value 1 (cl-ppcre:scan-to-strings re2 word))))
               (setf stem (concatenate 'string (aref res 0) (aref res 1)))
               (when (cl-ppcre:scan *mgr1* stem)
                 (setf word stem))))))
    
    (let ((re (cl-ppcre:create-scanner "^(.+?)e$")))
      (when (cl-ppcre:scan re word)
        (let ((fp (aref (nth-value 1 (cl-ppcre:scan-to-strings re word)) 0)))
          (setf stem fp)
          (when (or (cl-ppcre:scan *mgr1* stem)
                    (and (cl-ppcre:scan *meq1* stem)
                         (not (cl-ppcre:scan *sv2* stem))))
            (setf word stem)))))
    
    (let ((re (cl-ppcre:create-scanner "ll$")))
      (when (and (cl-ppcre:scan re word)
                 (cl-ppcre:scan *mgr1* word))
        (setf word (cl-ppcre:regex-replace ".$" word ""))))

    (when (eq first-char #\y)
      (setf word (concatenate 'string "y" (subseq word 1))))
    word))

