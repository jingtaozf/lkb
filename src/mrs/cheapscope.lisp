(in-package "MRS")

;;; extracting information from fragments
;;; leqs can either come from the h-cons directly, or
;;; be inferred for a quantifier if there's a predicate which refers 
;;; to a variable which is eq to the quantifier's bv, but which is not
;;; in the restrictor of the quantifier

(defun set-up-cheap-hcons (mrsstruct)
  (let* ((rels (psoa-liszt mrsstruct))
         (hcons (psoa-h-cons mrsstruct))
         (labels nil) (holes nil) (equated-list nil)
         (top-handel (get-var-num (psoa-handel mrsstruct))))
    (for rel in rels
         do
         (let ((var (rel-handel rel)))
           (unless (is-handel-var var)
             (struggle-on-error "~%Relation ~A has incorrect handel ~A"
                                (rel-sort rel) var))
           (pushnew (get-var-num var) labels)))
    (for rel in rels
         do
           (for handel-var in (get-handel-args rel)
                do
                (when (member handel-var labels)
                    (push handel-var equated-list))
                (pushnew handel-var holes)))
    (pushnew top-handel holes) ;; this may be wrong, given the use of prpstn etc
    (process-hcons hcons labels holes equated-list)))

(defun find-cheap-leqs (mrsstruct)
  (let* ((rels (psoa-liszt mrsstruct)) 
         (quant-rels (for rel in rels filter (if (is-quant-rel rel) rel))))
    (append
     (for rel in rels
          append
          (if (member rel quant-rels)
              nil
            (find-necessary-scope-relationships rel quant-rels)))
     ;;; fix - probably needs to be label label relationship
     (for outscpd in *qeqs*
          collect
          (cons (qeq-right outscpd)
                (qeq-left outscpd))))))

(defun get-restr-value (rel)
  ;;; returns the integer value of the handel corresponding to the
  ;;; feature RESTR
  ;;; assumes that there is only one such feature and 
  ;;; that its value is a var 
  (dolist (fvpair (rel-flist rel))
    (when (eql (fvpair-feature fvpair) (vsym 'restr))
      (return (get-var-num (fvpair-value fvpair))))))

(defun get-scope-value (rel)
  ;;; returns the integer value of the handel corresponding to the
  ;;; feature SCOPE
  ;;; assumes that there is only one such feature and 
  ;;; that its value is a var 
  (dolist (fvpair (rel-flist rel))
    (when (eql (fvpair-feature fvpair) (vsym 'scope))
      (return (get-var-num (fvpair-value fvpair))))))
         
(defun find-necessary-scope-relationships (rel quant-rels)
  (let* ((rel-vars (collect-vars-from-rel rel))
        (rel-handel (get-var-num (rel-handel rel)))
        (scopes
    (for var in rel-vars
         append
          (for qrel in quant-rels
               filter
               (let ((quant-var (get-bv-value qrel)))
                 (if (eql quant-var (get-var-num var))
                     (let ((restr-handel (get-restr-value qrel)))
                       (if (eql rel-handel restr-handel)
                           nil
                         (if (get-scope-value qrel)
                             (cons 
                              rel-handel
                              (get-scope-value qrel)))))))))))
    scopes))




  