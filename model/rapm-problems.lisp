;;; ------------------------------------------------------------------
;;; RAPM PROBLEMS
;;; ------------------------------------------------------------------
;;; Lisp definition of the RAPM problems used by Lauren Graham
;;; ------------------------------------------------------------------

(defun seq (start end &optional (step 1))
  "Creates a ranges"
  (let ((results nil)
	(partial start))
    (cond ((and (< start end)
		(plusp step))
	   (loop while (< partial end) do
	     (push partial results)
	     (incf partial step)))
	  ((and (> start end)
		(minusp step))
	   (loop while (> partial end) do
	     (push partial results)
	     (incf partial step)))
	  (t
	   nil))
    (reverse results)))

(defparameter *rapm-features*
  '(number shape texture background)
  "Arbitrarily named features")

(defparameter *feature-values*
  '((number . ((1 . one)
	       (2 . two)
	       (3 . three)))
    (shape . ((1 . triangle)
	      (2 . square)
	      (3 . circle)))
    (background . ((1 . white)
		   (2 . grey)
		   (3 . black)))
    (texture . ((1 . empty)
		(2 . striped)
		(3 . filled))))
  "Feature-specific mappings from internal values to symbolic values")

(defun assoc-value (feature int-value)
  "Returns the symbol associated to the numeric value of a feature"
  (let ((mappings (cdr (assoc feature *feature-values*))))
    (when mappings
      (cdr (assoc int-value mappings)))))  

(defun rassoc-value (feature s-value)
  "Returns the numeric value (e.g., 2) associated with the name of a feature's (e.g., shape) value (e.g., square"
  (let ((mappings (cdr (assoc feature *feature-values*))))
    (when mappings
      (car (rassoc s-value mappings)))))  

(defparameter *rapm-rules* '(rule-same rule-progression rule-constant)
  "the simple rules implemented in the model. Can be extended easily")

(defun print-mat (mat)
  "Pretty-prints a matrix"
  (dotimes (j (length mat))
    (print (nth j mat))))

(defun rotate-matrix (mat)
  "Rotates a matrix"
  (let ((n (length mat))
        (newmat nil))
    (dotimes (row n (reverse newmat))
      (let ((newrow nil))
	(dotimes (col n)
	  (push  (nth row (nth col mat)) newrow))
	(push (reverse newrow) newmat)))))

(defun rule-progression ()
  '((1 2 3)
    (1 2 3)
    (1 2 3)))

(defun rule-constant ()
  (scramble* '((1 1 1) (2 2 2) (3 3 3))))

(defun rule-same ()
  (let ((template (scramble* '(1 2 3)))
	(result nil))
    (dotimes (j 3 result)
      (push template result))))

(defun rule-distribution ()
  (scramble* '((1 2 3) (2 3 1) (3 1 2))))

(defun generate-problem (nfeatures)
  (let* ((feats (subseq (scramble* *rapm-features*)
			0 nfeatures))
	 (problem nil))
    ;; Set up an empty problem
    (dotimes (r 3)
      (let ((row nil))
	(dotimes (cell 3)
	  (push nil row))
	(push row problem)))

    ;; Create features
    (dolist (feature feats problem)
      (let ((matrix (funcall (pick *rapm-rules*))))
	(when (pick (scramble '(t nil)))
	  (setf matrix (rotate-matrix matrix)))
	(dotimes (row 3)
	  (dotimes (col 3)
	    (let* ((int-val (nth col (nth row matrix)))
		   (feat-val (assoc-value feature int-val)))
	      (setf (nth col (nth row problem))
		    (append (list feature feat-val)
			    (nth col (nth row problem)))))))))))

(defun feature-number (problem)
  "Determines the number of features in a problem"
  (let* ((flat (apply #'append problem))
	 (n (apply #'max (mapcar #'length flat))))
    (/ n 2)))


(defun generate-options (problem &optional (solution-position nil))
  (let* ((correct (problem-cell problem 2 2))
	 (paired (divide-into-pairs correct))
	 (abstracted (mapcar #'(lambda (pair)
				 (list (first pair)
				       (rassoc-value (first pair)
						     (second pair))))
			     paired))
	 (options (list correct)))
    ;; Now generate options with 1, 2, or 3 different features
    (dolist (changes '(1 2 3) options)
      (let ((f-indices (subseq (scramble (seq 0 (length abstracted))) 0 (min changes (feature-number problem))))
	    (newoption (mapcar #'(lambda (x) (copy-seq x))
			       abstracted)))
	;; modify the new option
	(dolist (f-index f-indices)
	  (let* ((val (second (nth f-index newoption)))
		 (newval (pick (remove val '(1 2 3)))))
	    ;;(print (list 'index f-index 'val val 'newval newval)) 
	    (setf (second (nth f-index newoption)) newval)))
	
	;; Now, we need to retransform the features from int-values
	;; to symbolic values and flatten it before adding it to
	;; the list of options
	(let* ((newpaired (mapcar #'(lambda (pair)
				      (list (first pair)
					    (assoc-value (first pair)
							 (second pair))))
				  newoption))
	       (foil (flatten newpaired)))
	  (push foil options))))
    
    ;; If we want the solution in a specific position, we swap the values

    (if (and solution-position
	     options
	     (numberp solution-position)
	     (< solution-position 4))
	(let ((alternatives (subseq options 0 3)))
	  (append (subseq alternatives 0 solution-position)
		  (list (nth 3 options))
		  (subseq alternatives solution-position)))
	(scramble options))))

    
      
(defun generate-trial (nfeatures &optional
				   (metadata "Randomly Generated Raven Problem")
				   (solution-position nil))
  "Generates a random trial"
  (let* ((problem (generate-problem nfeatures))
	 (correct (problem-cell problem 2 2))
	 (options (generate-options problem solution-position)))
	   
    (setf (nth 2 (nth 2 problem)) nil)
    (list problem correct options metadata)))

(defun generate-trials (num)
  "Generates @NUM random trials with 4 features each"
  (let ((res nil))
    (dotimes (i num res)
      (push (generate-trial 4) res)))) 

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


		   



    
