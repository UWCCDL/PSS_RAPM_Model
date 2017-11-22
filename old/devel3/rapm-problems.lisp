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

(defparameter *simple-problem-4-rules-1*
  '(((number 1 shape triangle texture solid background white)
     (number 2 shape triangle texture striped background black)
     (number 3 shape triangle texture dotted background grey))
    
    ((number 1 shape square texture solid background white)
     (number 2 shape square texture striped background black)
     (number 3 shape square texture dotted background grey))
    
    ((number 1 shape circle texture solid background white)
     (number 2 shape circle texture striped background black)
     nil)))

(defparameter *simple-trial-4-rules-1*
  (list *simple-problem-4-rules-1*
	'(number 3 shape circle texture dotted background grey)
	'((number 3 shape circle texture dotted background grey)
	  (number 2 shape circle texture striped background white)
	  (number 3 shape triangle texture dotted background grey)
	  (number 1 shape square texture dotted background black))))

(defparameter *simple-problem-4-rules-2*
  '(((number 1 shape curve texture solid background white)
     (number 2 shape curve texture striped background black)
     (number 3 shape curve texture dotted background grey))
    
    ((number 1 shape wave texture solid background white)
     (number 2 shape wave texture striped background black)
     (number 3 shape wave texture dotted background grey))
    
    ((number 1 shape circle texture solid background white)
     (number 2 shape circle texture striped background black)
     nil)))

(defparameter *simple-trial-4-rules-2*
  (list *simple-problem-4-rules-2*
	'(number 3 shape circle texture dotted background grey)
	'((number 3 shape circle texture dotted background grey)
	  (number 2 shape wave texture striped background black)
	  (number 3 shape curve texture dotted background grey)
	  (number 1 shape square texture solid background black))))


(defparameter *simple-problem-4-rules-3*
  '(((number 1 shape curve texture solid background white)
     (number 1 shape curve texture striped background black)
     (number 1 shape curve texture dotted background grey))
    
    ((number 2 shape wave texture solid background white)
     (number 2 shape wave texture striped background black)
     (number 2 shape wave texture dotted background grey))
    
    ((number 3 shape circle texture solid background white)
     (number 3 shape circle texture striped background black)
     nil)))

(defparameter *simple-trial-4-rules-3*
  (list *simple-problem-4-rules-3*
	'(number 3 shape circle texture dotted background grey)
	'((number 3 shape circle texture dotted background grey)
	  (number 3 shape wave texture striped background black)
	  (number 3 shape curve texture dotted background grey)
	  (number 3 shape square texture solid background black))))


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
			       (list *simple-trial-4-rules-3*
				     *simple-trial-4-rules-2*
				     *simple-trial-4-rules-1*)))

(defparameter *trial-1-3*
  '((((line-geometry diamond line-number two line-distance large)
      (line-geometry diamond line-number two line-distance medium)
      (line-geometry diamond line-number two line-distance close))
     ((line-geometry circle line-number two line-distance large)
      (line-geometry circle line-number two line-distance medium)
      (line-geometry circle line-number two line-distance close))
     ((line-geometry hexagon line-number two line-distance large)
      (line-geometry hexagon line-number two line-distance medium)
      nil))
    
    ;; Solution
    (line-geometry hexagon line-number two line-distance close)

    ;; Distractors
    ((line-geometry mixed line-number two line-distance medium)
     (line-geometry mixed line-number two line-distance medium)
     (line-geometry hexagon line-number two line-distance medium)
     (line-geometry hexagon line-number two line-distance close))))

(defparameter *problem-2-10*
  '((((background-shape cross foreground-size small foreground-distance close)
      (background-shape cross foreground-size small foreground-distance medium)
      (background-shape cross foreground-size small foreground-distance large))
     ((background-shape cross foreground-size medium foreground-distance close)
      (background-shape cross foreground-size medium foreground-distance medium)
      (background-shape cross foreground-size medium foreground-distance large))
     ((background-shape cross foreground-size large foreground-distance close)
      (background-shape cross foreground-size large foreground-distance medium)
      nil))

    ;; Solution
    (background-shape cross foreground-size large foreground-distance large)

    ;; Alternatives
    (((background-shape cross foreground-size large foreground-distance medium)
      (background-shape cross foreground-size medim foreground-distance medium)
      (background-shape cross foreground-size large foreground-distance large)
      (background-shape cross foreground-size small foreground-distance large)))))


(defparameter *trial-3-12*
  '((((element1 circle element2 up-triangle element3 down-triangle element4 dot)
      (element1 circle element2 nil element3 down-triangle element4 nil)
      (element1 nil element2 up-triangle element3 down-triangle element4 nil))
   
     ((element1 nil element2 up-triangle element3 down-triangle element4 nil)
      (element1 nil element2 nil element3 down-triangle element4 nil)
      (element1 nil element2 up-triangle  element3 nil element4 nil))

     ((element1 circle element2 nil element3 nil element4 dot)
      (element1 circle element2 nil element3 nil element4 nil)
      nil))

    (element1 nil element2 nil  element3 nil element4 dot)
    
    ((element1 nil element2 nil element3 down-triangle element4 dot)
     (element1 nil element2 nil  element3 nil element4 dot)
     (element1 circle element2 up-triangle element3 nil element4 nil)
     (element1 circle element2 up-triangle element3 nil element4 dot))))


		   



    
