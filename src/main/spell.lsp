(in-package "CL-USER")

(defun fix-spelling (string)
  (setf string (mapcar #'string-downcase string))
  (let ((res nil))
    (loop
      (when (null string) (return))
      (let ((word (car string))
            (rest (cdr string)))
        (setf string (cdr string))
        (if (equal word "a")
            (if (vowel-first-p (car rest))
                (push "an" res)
              (push "a" res))
          (push word res))))
    (nreverse res)))



;;; heuristics for a / an
;;; accessed via the function vowel-first-p


#|

(dolist (word '("abacus" "back" "x-ray" "us" "elephant" "european"
                "udder" "ukelele" "unanimous" "university" "uninformed"
                "oboe" "one" "once" "oncologist"
                "hero" "honour" "homage" "11" "11.3" "111" "18" "8324" 
                "ytterbium" "yacht"))
  (format t "~%~A ~A" (if (vowel-first-p word) "an" "a") word))

(with-open-file (istream "cogen/an-words" :direction :input)
  (let ((wrong 0) (total 0))
  (loop (let ((line (read-line istream nil nil nil)))
          (unless line (return))
          (incf total)
          (let* ((slashpos (position #\/ line)) 
                 (word (subseq line 0 slashpos)))
            (when (not (vowel-first-p word))
              (incf wrong)
              (format t "~%~A" word)))))
  (format t "~%~A wrong out of ~A" wrong total)))

(with-open-file (istream "cogen/a-words" :direction :input)
  (let ((wrong 0) (total 0))
  (loop (let ((line (read-line istream nil nil nil)))
          (unless line (return))
          (incf total)
          (let* ((slashpos (position #\/ line)) 
                 (word (subseq line 0 slashpos)))
            (when (vowel-first-p word)
              (incf wrong)
              (format t "~%~A" word)))))
  (format t "~%~A wrong out of ~A" wrong total)))

(with-open-file (istream "cogen/lob-an-words" :direction :input)
  (let ((wrong 0) (total 0))
  (loop (let ((line (read-line istream nil nil nil)))
          (unless line (return))
          (incf total)
          (let* ((slashpos (position #\_ line)) 
                 (word (subseq line 0 slashpos)))
            (when (not (vowel-first-p word))
              (incf wrong)
              (format t "~%~A" word)))))
  (format t "~%~A wrong out of ~A" wrong total)))

(with-open-file (istream "cogen/lob-a-words" :direction :input)
  (let ((wrong 0) (total 0))
  (loop (let ((line (read-line istream nil nil nil)))
          (unless line (return))
          (incf total)
          (let* ((slashpos (position #\_ line)) 
                 (word (subseq line 0 slashpos)))
            (when (vowel-first-p word)
              (incf wrong)
              (format t "~%~A" word)))))
  (format t "~%~A wrong out of ~A" wrong total)))

|#


(defun spelled-out-abbreviation-p (str letters)
  ;;; obviously needs to be enlarged
  (or (cdr (member #\. letters))
      (let* ((pos (position #\- str))
             (prehyphen (if pos (subseq str 0 pos) str)))
        (or (eql (length prehyphen) 1)
            (member prehyphen 
                    '("us" "uk" "xmas" "x-ray" "nih") :test #'equal)))))

(defun vowel-p (letter)
  (member letter '(#\a #\e #\i #\o #\u)))

(defun vowel-letter-p (letter)
  (member letter '(#\a #\e #\f #\h #\i #\l #\m #\n #\o #\r #\s #\x)))

(defun uni-un-word-p (str)
  (dolist (search-str '("unidentified" "unimproved" "unintended" "uninvited"
                "unimaginativ" "unimaginabl" "unimpeachabl"
                "unimportant" "unimpressed" "uninformed" "uninhabitabl"
                "uninhabited" "uninitiated" "uninspired" "unintelligibl"
                        "unintention" "uninterested" "uninterrupted"
                        "unimpeded" "uninhibited"))
    (if (eql (search search-str str) 0)
        (return t))))

(defun un-un-word-p (str)
  (dolist (search-str '("unanimous"))
    (if (eql (search search-str str) 0)
        (return t))))

(defun dropped-h-p (str)
  (dolist (search-str '("honest" "honor" "honour" "hour" "heir"))
        (if (eql (search search-str str) 0)
        (return t))))
  

(defun vowel-first-p (str)
  (let* ((letters (coerce str 'list))
         (initial-letter (car letters)))
    (cond ((digit-char-p initial-letter) ; digits first
           (vowel-number-p letters))
          ((spelled-out-abbreviation-p str letters)  
           (vowel-letter-p initial-letter))           
          ((member initial-letter '(#\a #\i)) t)
          ((member initial-letter '(#\b #\c #\d #\f #\g #\j 
                                    #\k #\l #\m #\n #\p #\q
                                    #\r #\s #\t #\v #\w #\x
                                    #\z)) nil)
          ((eql initial-letter #\y) 
           (not (vowel-p (cadr letters))))
          ((eql initial-letter #\e) 
           (if (eql (cadr letters) #\u) nil t))
          ((eql initial-letter #\u) 
           (if (eql (cadr letters) #\n)
               (if (eql (caddr letters) #\i)
                   (uni-un-word-p str)
                 (not (un-un-word-p str)))
             (not (vowel-p (caddr letters)))))
          ((eql initial-letter #\o)
           (if (and (eql (cadr letters) #\n)
                    (or (and (eql (caddr letters) #\c)
                             (eql (cadddr letters) #\e))
                        (eql (caddr letters) #\e)))
               nil
             t))
          ((eql initial-letter #\h)
           (dropped-h-p str))
          (t nil))))

(defun vowel-number-p (letters)
  (or
   (eql (car letters) #\8)
   (and (eql (car letters) #\1)
           (member (cadr letters) '(#\1 #\8))
           (or (null (caddr letters))
               (not (digit-char-p (caddr letters)))))))
