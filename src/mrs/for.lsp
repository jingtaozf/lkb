;;; For loop
;;;
;;; Copyright Bran Boguraev, David Carter, John Carroll, Ann Copestake
;;; (c) 1980-1990
;;;
;;; Some-satisfy needs cleaning up (was exists)
;;; Added in-stream (as in Procyon Common Lisp) AAC 9 April 1990
;;; Added on (as in Procyon Common Lisp) AAC 12 October 1993

(in-package :cl-user)

(eval-when
 (compile load eval)
(export '(for in do collect collect-reversed nconc filter
          some-satisfy keep-first car-filter append keep-if
          keep-unless all-satisfy none-satisfy from to
          fixnum downto by on in-stream when until while))
)


(defvar *print-loops* nil)


(defmacro for (&rest lst)
   ($check-for-syntax lst)
   ($for1 ($var-for lst) ($rel-for lst) 
      ($arg-for lst) ($termin-for lst) ($test-for lst) 
      ($type-for lst) ($body-for lst)))


(defun $check-for-syntax (lst)
   (cond
      ((not (symbolp (car lst)))
         (error 
            "Variable in for must be an identifier")))
   (cond
      ((< (length lst) 5)
         (error "Invalid for loop format")))
   (cond
      ((null ($type-for lst))
         (error "Type of for loop not found"))))


(eval-when (load eval)
   (mapc
      #'(lambda (x) (setf (get x 'valid-for-type) t))
      '(do collect collect-reversed nconc filter
          some-satisfy keep-first car-filter append keep-if
          keep-unless all-satisfy none-satisfy)))


(defun $var-for (lst)
   (car lst))


(defun $arg-for (lst)
   (cond
      ((member (cadr lst) '(from fixnum)) 
         (list (caddr lst) 
            (cond
               ((member 'by lst)
                  (cadr (member 'by lst)))
               ((member 'to lst) 1)
               (t -1))
            (cadr 
               (or (member 'to lst) 
                  (member 'downto lst)))))
      ((eq (cadr lst) 'in)
         (caddr lst))
      ((eq (cadr lst) 'on)
         (caddr lst))
      ((eq (cadr lst) 'in-stream)
         (caddr lst))
      (t
         (error "Invalid iteration type - ~A" (cadr lst)))))


(defun $type-for (lst)
   (find-if
      #'(lambda (x)
           (and (symbolp x) (get x 'valid-for-type)))
      lst))


(defun $test-for (lst)
   (or (cadr (member 'when lst))
      (let ((unless (cadr (member 'unless lst))))
         (and unless `(not ,unless)))))


(defun $termin-for (lst)
   (or (cadr (member 'until lst))
      (let ((while (cadr (member 'while lst))))
         (and while `(not ,while)))))


(defun $body-for (lst)
   (prog (revbody)
      (setq revbody 
         (reverse (cdr (member ($type-for lst) lst))))
      (return 
         (cons (reverse (cdr revbody)) 
            (car revbody)))))


(defun $rel-for (lst)
   (cadr lst))


(defun $for1 (var relation arg termin tst type body)
   (prog (ans)
      (setq ans 
         ($for1-proper var relation arg termin tst type 
            body (gensym "$ans") (gensym "$endans") 
            (gensym "$arg") (gensym "$body")))
      (cond
         (*print-loops* (pprint ans)))
      (return ans)))


(defun $for1-proper 
   (var relation arg termin tst type body $ans $endans $arg 
      $body)
   `(prog 
       ,@($for-begin var relation arg type termin
           $ans $endans $arg $body)
       ,@($for-do var relation tst type body 
           $ans $endans $arg $body)
       ,@(cond
           ((member relation '(from fixnum)) 
              `((declare (ignore ,$arg)))
              `((setq ,var (+ ,var ,(cadr arg)))))
           ((eq relation 'in-stream) 
              `((setq ,$arg (read ,arg nil nil))))
           ((eq relation 'on) 
              `((setq ,$arg (cdr ,$arg))))
           ((eq relation 'in) 
              `((setq ,$arg (cdr ,$arg)))))
       (go progloop)
    progout
       ,@(cond
           ((not (eq type 'do)) `((return ,$ans))))))


(defun $for-begin 
   (var relation arg type termin $ans $endans $arg 
      $body)
   `((,var ,$ans 
        ,@(and 
            (member type 
               '(collect filter nconc keep-if append
                   keep-unless))
            `(,$endans))
        ,@(and 
            (member type 
               '(collect collect-reversed filter nconc append
                   car-filter))
            `(,$body))
        ,$arg)
       ,@(cond ((eq type 'do) `((declare (ignore ,$ans) #-cmu (ignore-if-unused ,$arg)))))
                                        ; avoid compiler warnings AAC
                                        ; ignore-if-unused is MCL and ACL specific
       ,@(and 
           (member type '(all-satisfy none-satisfy)) 
           `((setq ,$ans t)))
       ,@(cond
           ((member relation '(from fixnum))
              `((declare (ignore ,$arg)))
              `((setq ,var ,(car arg))))
           ((eq relation 'in-stream)
              `((setq ,$arg (read ,arg nil nil))))
           (t `((setq ,$arg ,arg))))
       progloop 
       ,@(cond
           ((member relation '(from fixnum))
              `((declare (ignore ,$arg)))
              `((cond
                   ((,(cond
                         ((minusp (cadr arg)) '<)
                         (t '>))
                       ,var ,(car (last arg)))
                      (go progout)))))
           ((eq relation 'in-stream)
              `((cond
                   ((null ,$arg) (go progout)))
                (setq ,var ,$arg)))
           ((eq relation 'on)
              `((cond
                   ((null ,$arg) (go progout)))
                (setq ,var ,$arg)))
           (t 
              `((cond
                   ((null ,$arg) (go progout)))
                (setq ,var (car ,$arg)))))
       ,@(cond
            (termin 
               `((cond (,termin (go progout))))))))


(defun $for-do 
   (var relation tst type body $ans $endans $arg 
      $body)
   (prog (res)
      (setq res 
         (append (car body) 
            ($for-do1 var relation type 
               (cdr body) $ans $endans $arg $body)))
      (return 
         (cond
            (tst 
               `((cond
                    (,tst ,@res))))
            (t res)))))


(defun $for-do1 
   (var relation type body $ans $endans $arg 
      $body)
   (cond
      ((eq type 'do) (cons body nil))
      ((eq type 'collect)
         `((setq ,$body ,body) 
             ,($for-do2 $body $ans $endans)))
      ((eq type 'collect-reversed)
         `((setq ,$body ,body) 
             ,($for-do2-reversed $body $ans)))
      ((eq type 'filter) 
         `((setq ,$body ,body) 
             (and ,$body 
                ,($for-do2 $body $ans $endans))))
      ((eq type 'keep-if) 
         `((and ,body 
              ,($for-do2 var $ans $endans))))
      ((eq type 'keep-unless) 
         `((and (not ,body) 
              ,($for-do2 var $ans $endans))))
      ((member type '(nconc append)) 
         `((setq ,$body ,body) 
             ,($for-do2-nconc 
                 (cond
                    ((eq type 'append) 
                       `(copy-list ,$body))
                    (t $body))
                 $ans $endans $body)))
      ((eq type 'some-satisfy) 
         ($for-do3 body 
            (cond
               ((eq relation 'in) $arg)
               ((eq relation 'on) $arg)
               ((eq relation 'in-stream) $arg)
               (t 
                  (error 
"Cannot use some-satisfy keyword with from in for")))
            $ans))
      ((eq type 'keep-first) 
         ($for-do3 body var $ans))
      ((eq type 'car-filter) 
         `((setq ,$body ,body) 
             ,@($for-do3 $body $body $ans)))
      ((eq type 'all-satisfy) 
         ($for-do3 `(not ,body) nil $ans))
      ((eq type 'none-satisfy) 
         ($for-do3 body nil $ans))))


(defun $for-do2 (toadd $ans $endans)
   `(cond
       (,$ans (rplacd ,$endans (cons ,toadd nil)) 
          (setq ,$endans (cdr ,$endans)))
       (t (setq ,$ans (cons ,toadd nil)) 
          (setq ,$endans ,$ans))))


(defun $for-do2-reversed (toadd $ans)
   `(setq ,$ans (cons ,toadd ,$ans)))


(defun $for-do2-nconc (increment $ans $endans $body)
   `(cond
       ((atom ,$body) nil)
       (,$ans (rplacd ,$endans ,increment) 
          (setq ,$endans (last ,$endans)))
       (t (setq ,$ans ,increment) 
          (setq ,$endans (last ,$ans)))))


(defun $for-do3 (condn value $ans)
   `((cond
        (,condn (setq ,$ans ,value) 
           (go progout)))))


;;; End of file
