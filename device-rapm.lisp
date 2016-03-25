;;; Definition of a problem in RAPM

(setq problem
      '(((number 1 shape triangle)
	 (number 2 shape triangle)
	 (number 3 shape triangle))
	
	((number 1 shape square)
	 (number 2 shape square)
	 (number 3 shape square))
	
	((number 1 shape cirle)
	 (number 2 shape cicle)
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

(defparameter *number-names* '((0 . zero) (1 . one) (2 . two)
			       (3 . three) (4 . four) (5 . five)
			       (6 . six) (7 . seven) (8 . eight)
			       (9 . nine) (10 . ten)))

(defun convert-to-number (token)
  (car (rassoc token *number-names*)))

(defun convert-to-name (number)
  (cdr (assoc number *number-names*)))

(defparameter *pid* nil)

(defun generate-pid (problem)
  "Generates an ID for a problem"
  (if (assoc problem *pid* :test #'equal-problem)
      (rest (assoc problem *pid* :test #'equal-problem))
      (let ((id (1+ (apply #'max (cons 0 (mapcar #'rest *pid*))))))
	(push (cons problem id) *pid*)
	id)))
  

(defun divide-into-pairs (lst &optional (partial nil) (open nil))
  (cond ((null lst)
	 (append partial open))
	((= (length (car open)) 2)
	 (divide-into-pairs (rest lst)
			    (append partial open)
			    (list (list (first lst)))))
	((= (length (car open)) 1)
	 (divide-into-pairs (rest lst)
			    partial
			    (list (list (caar open)
					(first lst)))))
	(t
	 (divide-into-pairs (rest lst)
			    partial
			    (list (list (first lst)))))))
	  

(defun same-elements (lst1 lst2)
  (and (= (length lst1) (length lst2))
       (every #'(lambda (x) (member x lst2 :test #'equalp)) lst1)
       (every #'(lambda (x) (member x lst1 :test #'equalp)) lst2)))

(defun valid-cell? (c)
  (and (listp c)
       (evenp (length c))
       (every #'atom c)))

(defun equal-cell (c1 c2)
  (and (valid-cell? c1)
       (valid-cell? c2)
       (same-elements (divide-into-pairs c1)
		      (divide-into-pairs c2))))

(defun valid-coordinates? (x y)
  (and (>= x 0)
       (< x 3)
       (>= y 0)
       (< y 3)))

(defun valid-problem? (p)
  (and (= 3 (length p))
       (every #'(lambda (x) (and (= 3 (length x))
				 (every #'valid-cell? x)))
	      p)))

(defun equal-row (row1 row2)
  (every #'equal-cell row1 row2))

(defun equal-problem (p1 p2)
  "two problems are the same when they have the same cells in the same order"
  (and (valid-problem? p1)
       (valid-problem? p2)
       (every #'equal-row p1 p2)))


(defun problem-cell (problem row col)
  (when (and (valid-coordinates? row col)
	     (valid-problem? problem))
    (nth col (nth row problem))))


(setf trial
      (list problem '(number 3 shape circle)
	    '((number 4 shape cirle)
	      (number 3 shape circle)
	      (number 6 shape triangle)
	      (number 2 shape diamond))))

(setf simple-trial
      (list simple-problem
	    '(shape triangle)
	    '((shape circle)
	      (shape square)
	      (shape triangle)
	      (shape diamond))))

(defun trial-problem (trl)
  (first trl))

(defun trial-solution (trl)
  (second trl))


(defun trial-options (trl)
  (third trl))

  

(defun valid-options? (opt)
  "A list of options is valid if every member is a valid cell" 
  (every #'valid-cell opt))

(defun valid-trial? (trl)
  "A trial is valid is it is made of a valid problem, a valid cell (solution), and a list of valid options" 
  (and (= (length trl) 3)
       (valid-problem? (first trl))
       (valid-cell? (second trl))
       (valid-options? (third trl))))

;; Quick device. Unfortunately, we do need the model to look at something

(defmethod device-handle-keypress ((tm list) key)
  "Converts the key into a symbol and passes it on to the task manager"
  (let ((val (read-from-string (format nil "~a" key))))
    (record-response tm val)))
			   
(defmethod device-handle-click ((device list))
  "Does nothing"
  (declare (ignore device))
  nil)

(defmethod device-move-cursor-to ((device list) pos)
  "Does nothing"
  (declare (ignore device))
  nil)


(defmethod get-mouse-coordinates ((device list))
  "Does nothing"
  (declare (ignore device))
  (vector 0 0))

(defmethod cursor-to-vis-loc ((device list))
  (declare (ignore device))
  nil)

(defmethod build-vis-locs-for ((device list) vismod)
  "Creates a list of visual locations"
  (let ((problem (first device))
	(results nil))
    (dotimes (i 3)
      (dotimes (j 3)
	(let ((cell (problem-cell trial i j)))
	  (push  `(isa visual-location 
		       kind rapm-cell
		       row ,(convert-to-name i)
		       column ,(convert-to-name j)
		       row-num ,i
		       column-num ,j
		       screen-x ,(* j 200)
		       screen-y ,(* i 200)
		       height 200 
		       width 200
		       ,@cell)
		 results))))
    (push `(isa visual-location
		kind rapm-problem
		id ,(generate-pid problem)
		screen-x 0
		screen-y 0
		height 400
		width 600)
	  results)
    (funcall #'define-chunks-fct results)))
		    


(defmethod vis-loc-to-obj ((device list) vis-loc)
  "Transforms a visual-loc into a visual object"
  (let ((kind (chunk-slot-value-fct vis-loc 'kind))
	(new-chunk nil))
    (cond ((equal kind 'rapm-cell)
	   (let* ((row (chunk-slot-value-fct vis-loc 'row))
		  (column (chunk-slot-value-fct vis-loc 'column))
		  (r (convert-to-number row))
		  (c (convert-to-number column))
		  (cell (problem-cell (trial-problem device)
				      r
				      c)))
	     (setf new-chunk
		   (first (define-chunks-fct 
			      `((isa rapm-cell
				     kind ,kind 
				     row ,row
				     column ,column
				     row-num ,r
				     column-num ,c
				     ,@cell
				     )))))))
	  ((equal kind 'rapm-problem)
	   (let ((id (chunk-slot-value-fct vis-loc 'id)))
	     (setf new-chunk
		   (first (define-chunks-fct 
			      `((isa rapm-problem
				     kind ,kind 
				     id ,id
				     ))))))))
    (fill-default-vis-obj-slots new-chunk vis-loc)
    new-chunk))

(defun schedule-task-update (tm)
  "Schedules the next update of the trial manager"
  (when (act-r-loaded?)
    (next tm)
    (proc-display :clear t)))


(defmethod device-update-attended-loc ((tm list) xyloc)
  "Updates the attention focus on the window"
  (when *window*
    (device-update-attended-loc *window* xyloc)))


;(with-open-file (file "results.csv" :direction :output
;		      :if-exists :overwrite) 
;    (loop for egs from 0 to 2.0 by 0.5 do
;	 (loop for ga from 1 to 2.0 by 0.25 do
;	      (reload)
;	      (sgp-fct `(:egs ,egs :ga ,ga))
					;(run 1000 :real-time nil)
	      ;(let* ((data (analyze-data (current-device)))
	;	     (row (append (list egs ga)
					;			  data)))
;		(format file "~{~a~^, ~}~%" row)))))

     
