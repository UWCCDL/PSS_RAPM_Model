;;; ------------------------------------------------------------------
;;; RAPM PROBLEMS
;;; ------------------------------------------------------------------
;;; Lisp definition of the RAPM problems used by Lauren Graham
;;; ------------------------------------------------------------------

(defparameter *test-problem*
  '(((number 1 shape triangle)
     (number 2 shape triangle)
     (number 3 shape triangle))
    
    ((number 1 shape square)
     (number 2 shape square)
     (number 3 shape square))
    
    ((number 1 shape circle)
     (number 2 shape circle)
     nil)))

(defparameter *simple-problem*
  '(((shape triangle)
     (shape triangle)
     (shape triangle))
    
    ((shape triangle)
     (shape triangle)
     (shape triangle))
    
    ((shape triangle)
     (shape triangle)
     nil)))

(defparameter *simple-problem-2*
  '(((shape circle)
     (shape circle)
     (shape circle))
    
    ((shape circle)
     (shape circle)
     (shape circle))
    
    ((shape circle)
     (shape circle)
     nil)))

;;; Two features, sharing the same rule.

(defparameter *simple-problem-2-features*
  '(((number 1 shape triangle)
     (number 1 shape triangle)
     (number 1 shape triangle))
    
    ((number 1 shape square)
     (number 1 shape square)
     (number 1 shape square))
    
    ((number 1 shape circle)
     (number 1 shape circle)
     nil)))

(defparameter  *simple-trial-2-features*
  (list *simple-problem-2-features*
	'(shape circle number 1)
	'((shape circle number 1)
	  (shape square number 2)
	  (shape triangle number 1)
	  (shape diamond number 2))))

;;; Three features, sharing the same rule

(defparameter *simple-problem-3-features*
  '(((number 1 shape triangle texture solid)
     (number 1 shape triangle texture solid)
     (number 1 shape triangle texture solid))
    
    ((number 1 shape square texture solid)
     (number 1 shape square texture solid)
     (number 1 shape square texture solid))
    
    ((number 1 shape circle texture solid)
     (number 1 shape circle texture solid)
     nil)))

(defparameter  *simple-trial-3-features*
  (list *simple-problem-3-features*
	'(shape circle number 1 texture solid)
	'((shape circle number 1 texture solid)
	  (shape square number 2 texture solid)
	  (shape triangle number 3 texture solid)
	  (shape diamond number 4 texture solid))))


(defparameter *simple-problem-2-rules*
  '(((number 1 shape triangle)
     (number 2 shape triangle)
     (number 3 shape triangle))
    
    ((number 1 shape square)
     (number 2 shape square)
     (number 3 shape square))
    
    ((number 1 shape circle)
     (number 2 shape circle)
     nil)))


(defparameter *test-trial*
  (list *test-problem* '(number 3 shape circle)
	'((number 4 shape cirle)
	  (number 3 shape circle)
	  (number 6 shape triangle)
	  (number 2 shape diamond))))

(defparameter  *simple-trial*
  (list *simple-problem*
	'(shape triangle)
	'((shape circle)
	  (shape square)
	  (shape triangle)
	  (shape diamond))))

(defparameter  *simple-trial-2*
  (list *simple-problem-2*
	'(shape circle)
	'((shape circle)
	  (shape square)
	  (shape triangle)
	  (shape diamond))))



;(defparameter *trials* (mapcar #'make-trial
;			       (list *simple-trial* *simple-trial-2*)))

(defparameter *trials* (mapcar #'make-trial
			       (list *simple-trial-3-features*)))
