;; PPCRE sets ppcre:*regex-char-code-limit* to the constant CHAR-CODE-LIMIT
;; under SBCL and CCL, CHAR-CODE-LIMIT is 1114112
;;  - this is way too high for almost any application
;;    (results in massive regex scanners)
;; under Allegro, CHAR-CODE-LIMIT is 65536
;;  - still excessively high, but usable
;; SO we set ppcre:*regex-char-code-limit* to 65536 for now...
#+(or :sbcl :ccl)
(setf ppcre:*regex-char-code-limit* 65536)