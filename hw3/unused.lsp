
;
; boxes-in-row (row c)
; Helper function for h1
; Returns the number of boxes in a row
;
(defun count-boxes-in-row (row c)
  (cond ((null row) c)
  		((and (atom row) (isBox row)) (+ 1 c))
  		((atom row) c)
  	    ((and (atom (car row)) (isBox (car row))) (count-boxes-in-row (cdr row) (+ 1 c)))
  	    (t (count-boxes-in-row (cdr row) c)))
  )