(in-package :lkb)

;;; Allow for partially bracketed input to constrain parses found
;;; functions are called from parse.lsp

(defun initialise-bracket-list (tokens)
  ;;; tokens is a list of strings, some of which are ( or )
  ;;; this returns a list of strings without the ()s
  ;;; and a list of vertex pairs for matching brackets
  ;;; e.g. ( "Kim" "(" "(" "likes" ")" "Sandy" ")" )
  ;;; returns ("Kim" "likes" "Sandy")
  ;;;         ((1 . 2) (1 . 3))
  (let ((current-vertex 0)
        (bracket-list nil)
        (string-list nil)
        (pending-opens nil)
        (error-state nil))
    (dolist (item tokens)
      (cond ((equal item "(") 
             (push current-vertex pending-opens))
            ((equal item ")")
             (unless (or error-state pending-opens)
               (format t "~%Unmatched brackets - discarding all brackets")
               (setf error-state t))
             (let ((open-paren (pop pending-opens)))
               (unless (eql open-paren current-vertex)
                 (pushnew (cons open-paren current-vertex)
                          bracket-list :test #'equal))))
            ((stringp item) 
             (push item string-list)
             (incf current-vertex))
            (t (error "Unexpected element in tokens ~A" item))))
    (when (and pending-opens (not error-state))
      (format t "~%Unmatched brackets at end - discarding all brackets")
      (setf error-state t))
    (values (nreverse string-list)
            (if error-state nil bracket-list))))
                     


#|

Takes a list of start and end vertices for a candidate set of daughters
for a rule application, and a list of start and end vertices for paired
brackets, and returns t or nil depending on whether the
bracketing is consistent.

e.g.

(consistent-bracketing-p '((0 . 2) (2 . 5)) '((0 . 6)))
=> t

(consistent-bracketing-p '((0 . 2) (2 . 5)) '((0 . 2)))
=> t

(consistent-bracketing-p '((0 . 2) (2 . 5)) '((0 . 1)))
=> t

(consistent-bracketing-p '((0 . 2) (2 . 5)) '((0 . 4)))
=> nil

(consistent-bracketing-p '((0 . 2) (2 . 5)) '((2 . 8)))
=> nil

|#



(defun consistent-bracketing-p (constituent-list bracket-list)
  ;; the constituents are assumed to be already checked
  (let ((consistent t)
        (start (caar constituent-list))
        (end (cdar (last constituent-list))))
    (dolist (bracket-pair bracket-list)
      (unless (consistent-bracket-p constituent-list 
                                    start end
                                    (car bracket-pair)
                                    (cdr bracket-pair))
        (setf consistent nil)
        (return)))
    consistent))


(defun consistent-bracket-p (constituent-list start end 
                             bracket-start bracket-end)
  ;;; obviously this could be more efficient!
  (cond ((>= bracket-start end) t) ; no intersection
        ((<= bracket-end start) t) ; no intersection
        ((< bracket-start start) 
         (>= bracket-end end)) ; OK if bigger
        ((= bracket-start start) 
         (or (>= bracket-end end) ; OK if perfect match
             (<= bracket-end (cdar constituent-list))))
                                        ; OK if part of first constituent
        (t (some #'(lambda (constituent)
                     (and (>= bracket-start (car constituent))
                          (<= bracket-end (cdr constituent))))
                 (cdr constituent-list) ; OK if part of another constituent
                 ))))


  
  
