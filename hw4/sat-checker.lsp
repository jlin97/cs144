
;
; Helper functions to check the correctness of a truth assignment
;

(defun validate-clause-satisifability (literals clause)
  (cond ((null clause) NIL)
        (t (or (not (null (member (car clause) literals))) 
               (validate-clause-satisifability literals (cdr clause)))))
  )

(defun validate-clauses-satisifability (literals clauses)
  (cond ((null literals) "UNSAT")
        ((null clauses) t)
        ((not (validate-clause-satisifability literals (car clauses))) NIL)
        (t (validate-clauses-satisifability literals (cdr clauses))))
  )

(defun sat-solution-checker (filename)
  (validate-clauses-satisifability (solve-cnf filename) (cadr (parse-cnf filename))))