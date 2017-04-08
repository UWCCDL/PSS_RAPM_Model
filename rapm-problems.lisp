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

;; Two features, two rules

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

(defparameter  *simple-trial-2-rules*
  (list *simple-problem-2-rules*
	'(shape circle number 3)
	'((shape circle number 3)
	  (shape square number 2)
	  (shape triangle number 1)
	  (shape diamond number 3))))

;; Three features, three rules

(defparameter *simple-problem-3-rules*
  '(((number 1 shape triangle texture solid)
     (number 2 shape triangle texture striped)
     (number 3 shape triangle texture dotted))
    
    ((number 1 shape square texture solid)
     (number 2 shape square texture striped)
     (number 3 shape square texture dotted))
    
    ((number 1 shape circle texture solid)
     (number 2 shape circle texture striped)
     nil)))

(defparameter *simple-trial-3-rules*
  (list *simple-problem-3-rules*
	'(number 3 shape circle texture dotted)
	'((number 3 shape circle texture dotted)
	  (number 2 shape square texture striped)
	  (number 1 shape circle texture solid)
	  (number 3 shape triangle texture solid))))
	  
;;; Four feature/rules

(defparameter *simple-problem-4-rules*
  '(((number 1 shape triangle texture solid background white)
     (number 2 shape triangle texture striped background black)
     (number 3 shape triangle texture dotted background grey))
    
    ((number 1 shape square texture solid background white)
     (number 2 shape square texture striped background black)
     (number 3 shape square texture dotted background grey))
    
    ((number 1 shape circle texture solid background white)
     (number 2 shape circle texture striped background black)
     nil)))

(defparameter *simple-trial-4-rules*
  (list *simple-problem-4-rules*
	'(number 3 shape circle texture dotted background grey)
	'((number 3 shape circle texture dotted background grey)
	  (number 2 shape circle texture striped background white)
	  (number 3 shape triangle texture dotted background grey)
	  (number 1 shape square texture dotted background black))))


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

;(defparameter *trials* (mapcar #'make-trial
;			       (list *simple-trial-3-features*)))
(defparameter *trials* (mapcar #'make-trial
			       (list *simple-trial-2-rules*
				     *simple-trial-3-rules*
				     *simple-trial-4-rules*)))
