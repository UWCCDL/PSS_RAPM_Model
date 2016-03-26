;;; ------------------------------------------------------------------
;;; RAPM-DEVICE.LISP
;;; ------------------------------------------------------------------
;;; A class that provide an ACT-R GUI interface for a modified
;;; version of Raven's Advanced Progressive Matrices
;;; ------------------------------------------------------------------

(defun act-r-loaded? ()
  "Cheap hack to check whether ACTR is loaded"
  (and (fboundp 'run-n-events)
       (fboundp 'start-environment)))

;; ---------------------------------------------------------------- ;;
;; Some utilities
;; ---------------------------------------------------------------- ;;

(defun pick (lst)
  "Picks up an element from a list"
  (when  (listp lst)
    (elt lst (random (length lst)))))


(defun scramble (lst &optional (sofar nil))
  "Scrambles a list of different elements"
  (if (null lst)
      sofar
    (let ((picked (pick lst)))
      (scramble (remove picked lst) (cons picked sofar)))))

(defun scramble* (lst)
  "Scrambles any list of objects"
  (let ((l (length lst))
        (pos nil))
    (dotimes (i l)
      (push i pos))
    (mapcar #'(lambda (x) (elt lst x)) (scramble pos))))

(defun mean (&rest nums)
  (when (every #'numberp nums)
    (/ (reduce #'+ nums)
       (length nums))))


;;; Definition of a problem in RAPM

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
  "Tests whether the coordinate of a cell are valid (i.e., [0, 1, 2])"
  (and (>= x 0)
       (< x 3)
       (>= y 0)
       (< y 3)))

(defun valid-problem? (p)
  "Tests whether a problem is valid"
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
  "Returns the problem in a RAPM trial"
  (first trl))


(defun trial-solution (trl)
  "Returns the solution of problem in a RAPM trial"
  (second trl))


(defun trial-options (trl)
  "Returns the response options available for a trial" 
  (third trl))

  
(defun valid-options? (opt)
  "A list of options is valid if every member is a valid cell" 
  (every #'valid-cell opt))

(defun trial-correct-response (trl)
  "Returns the correct answer of a trial"
  (nth 3 trl))

(defun set-trial-correct-response (trl val)
  "Returns the correct answer of a trial"
  (setf (nth 3 trl) val))


(defun trial-actual-response (trl)
  "Returns the answer given by the model"
  (nth 4 trl))

(defun set-trial-actual-response (trl val)
  "Returns the answer given by the model"
  (setf (nth 4 trl) val))


(defun trial-problem-rt (trl)
  (nth 5 trl))

(defun set-trial-problem-rt (trl val)
  (setf (nth 5 trl) val))


(defun trial-choice-rt (trl)
  (nth 6 trl))

(defun set-trial-choice-rt (trl val)
  (setf (nth 6 trl) val))


(defun valid-trial? (trl)
  "A trial is valid is it is made of a valid problem, a valid cell (solution), and a list of valid options" 
  (and (>= (length trl) 3)
       (valid-problem? (first trl))
       (valid-cell? (second trl))
       (valid-options? (third trl))))

(defparameter *responses* '(("j" . 0) ("k" . 1) ("l" . 2) (";" . 3)))

(defun make-trial (trl)
  (let* ((new-options (scramble* (trial-options trl)))
	 (correct (position (trial-solution trl) new-options)))
    (list (trial-problem trl)              ; Problem
	  (trial-solution trl)             ; Solution
	  new-options                      ; Options
	  correct                          ; Correct response
	  nil                              ; Actual response
	  0                                ; Problem RT
	  0)))                             ; Choice RT

(defun trial-accuracy (trl)
  (if (and (>= (length trl) 7)
	   (= (trial-correct-response trl)
	      (trial-actual-response trl)))
      1
      0))


;;; ------------------------------------------------------------------
;;; ACT-R DEVICE INTERFACE
;;; ------------------------------------------------------------------

;;; ------------
;;; Task Manager
;;; ------------
;;;
(defclass rapm-task ()
  ((task-phase :accessor task-phase
	       :initform nil)
   (index :accessor index
	  :initform nil)
   (trials :accessor training-trials
	   :initform *trials*)
   (current-trial :accessor current-trial
		  :initform nil)
   (experiment-log :accessor experiment-log
		   :initform nil))
  (:documentation "A manager for the PSS task"))

(defmethod init ((task rapm-task))
  "Initializes the PSS task manager"
  (when (not (null (trials task)))
    (setf (index task) 0)
    (setf (experiment-log task) nil)
    (setf (trials task) (scramble* (trials task)))
    (setf (current-trial task) (make-trial (nth (index task) (trials task))))
    (setf (task-phase task) 'problem)))

(defmethod respond ((task rapm-task) key)
  "Records a response in the PSS task"
  (unless (null (current-trial task))
    (let* ((trial (current-trial task))
	   (choice (trial-choice trial))
	   (chosen (nth (cdr (assoc key *key-mappings*))
			choice))
	   (n (random 1.0))
	   (prob (cdr (assoc chosen *probabilities*)))
	   
	   (feedback (< n prob)))

      (set-trial-chosen-option trial chosen)
      (set-trial-best-option trial (trial-best-option trial))
      (set-trial-feedback trial feedback))

    ;; If ACT-R is loaded, we need to sync the visicon with the
    ;; state of the task.

    (when (act-r-loaded?)
      (cond ((equal (phase task) 'test)
	     (schedule-event-relative 0 #'next :params (list task)))
	    ((equal (phase task) 'training)
	     (schedule-event-relative 0 #'proc-display :params nil)
	     (schedule-event-relative 3 #'next :params (list task)))
	    (t
	     (schedule-event-relative 0 #'proc-display :params nil))))))


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
  "Does nothing"
  (declare (ignore device))
  nil)

(defmethod build-vis-locs-for ((device list) vismod)
  "Creates a list of visual locations for a problem"
  (let* ((problem (first device))
	 (pid (generate-pid problem))
	 (results nil))

    ;; The cells within each problem
    
    (dotimes (i 3)
      (dotimes (j 3)
	(let ((cell (problem-cell problem i j)))
	  (push  `(isa rapm-cell-location 
		       kind rapm-cell
		       row ,(convert-to-name i)
		       column ,(convert-to-name j)
		       row-num ,i
		       column-num ,j
		       screen-x ,(* j 200)
		       screen-y ,(* i 200)
		       problem ,pid
		       height 200 
		       width 200
		       ,@cell)
		 results))))
    
    ;; Now the problem  

    (push `(isa problem-location
		kind rapm-problem
		id ,(generate-pid problem)
		screen-x 0
		screen-y 0
		height 400
		width 600)
	  results)

    ;; Creates the chunks
    
    (funcall #'define-chunks-fct results)))
		    


(defmethod vis-loc-to-obj ((device list) vis-loc)
  "Transforms a visual-loc into a visual object"
  (let ((kind (chunk-slot-value-fct vis-loc 'kind))
	(new-chunk nil))
    (cond ((equal kind 'rapm-cell)
	   
	   ;; If the location was a cell

	   (let* ((row (chunk-slot-value-fct vis-loc 'row))
		  (column (chunk-slot-value-fct vis-loc 'column))
		  (r (convert-to-number row))
		  (c (convert-to-number column))
		  (pid (chunk-slot-value-fct vis-loc 'problem))
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
				     problem ,pid
				     ,@cell
				     )))))))

	  ;; If the locations was a rapm-problem 

	  ((equal kind 'rapm-problem)
	   (let ((id (chunk-slot-value-fct vis-loc 'id)))
	     (setf new-chunk
		   (first (define-chunks-fct 
			      `((isa rapm-problem
				     kind ,kind 
				     id ,id
				     ))))))))
    
    ;; No matter what, fill in the slots and return the new chunk
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

     
