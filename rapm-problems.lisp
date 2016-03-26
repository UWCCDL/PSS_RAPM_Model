;;; ------------------------------------------------------------------
;;; RAPM PROBLEMS
;;; ------------------------------------------------------------------
;;; Lisp definition of the RAPM problems used by Lauren Graham
;;; ------------------------------------------------------------------

(setq problem
      '(((number 1 shape triangle)
	 (number 2 shape triangle)
	 (number 3 shape triangle))
	
	((number 1 shape square)
	 (number 2 shape square)
	 (number 3 shape square))
	
	((number 1 shape circle)
	 (number 2 shape circle)
	 nil)))

(setq simple-problem
      '(((shape triangle)
	 (shape triangle)
	 (shape triangle))
	
	((shape triangle)
	 (shape triangle)
	 (shape triangle))
	
	((shape triangle)
	 (shape triangle)
	 nil)))

(setf trial
      (list problem '(number 3 shape circle)
	    '((number 4 shape cirle)
	      (number 3 shape circle)
	      (number 6 shape triangle)
	      (number 2 shape diamond))))

(defparameter  *simple-trial*
  (list simple-problem
	'(shape triangle)
	'((shape circle)
	  (shape square)
	  (shape triangle)
	  (shape diamond))))

(defparameter *trials* (list simple-trial))
