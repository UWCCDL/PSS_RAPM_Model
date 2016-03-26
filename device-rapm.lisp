;;; ------------------------------------------------------------------
;;; RAPM-DEVICE.LISP
;;; ------------------------------------------------------------------
;;; A class that provide an ACT-R GUI interface for a modified
;;; version of Raven's Advanced Progressive Matrices
;;; ------------------------------------------------------------------

(load "rapm-problems.lisp")

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

;;; ------------------------------------------------------------------
;;; PARAMETERS
;;; ------------------------------------------------------------------

(defparameter *number-names* '((0 . zero) (1 . one) (2 . two)
			       (3 . three) (4 . four) (5 . five)
			       (6 . six) (7 . seven) (8 . eight)
			       (9 . nine) (10 . ten))
  "Maps numbers onto their symbolic names")

(defparameter *responses* '(("j" . 0) ("k" . 1) ("l" . 2) (";" . 3))
  "Maps finger responses onto screen options")

      
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
  
;;; ------------------------------------------------------------------
;;; CELLS AND PROBLEMS: PROCESSING AND DEFINITION
;;; ------------------------------------------------------------------

(defun divide-into-pairs (lst &optional (partial nil) (open nil))
  "Recursively divides a list into pairs"
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
  "Quickly determines whether two lists contains the same elements (barring repetitions)"
  (and (= (length lst1) (length lst2))
       (every #'(lambda (x) (member x lst2 :test #'equalp)) lst1)
       (every #'(lambda (x) (member x lst1 :test #'equalp)) lst2)))

(defun valid-cell? (c)
  (and (listp c)
       (evenp (length c))
       (every #'atom c)))

(defun equal-cell (c1 c2)
  "Equality for RAPM cells"
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
  "A problem is valid if it is made of three rows of cells"
  (and (= 3 (length p))
       (every #'(lambda (x) (and (= 3 (length x))
				 (every #'valid-cell? x)))
	      p)))

(defun equal-row (row1 row2)
  "Two rows are equal if their cells are equal"
  (every #'equal-cell row1 row2))

(defun equal-problem (p1 p2)
  "two problems are the same when they have the same cells in the same order"
  (and (valid-problem? p1)
       (valid-problem? p2)
       (every #'equal-row p1 p2)))


(defun problem-cell (problem row col)
  "Returns the cell in position ROW/COL"
  (when (and (valid-coordinates? row col)
	     (valid-problem? problem))
    (nth col (nth row problem))))


;;; ------------------------------------------------------------------
;;; EXPERIMENTAL TRIALS
;;; ------------------------------------------------------------------
;;; A trial is a list containing  a problem, a solution, a list of
;;; options, and a set of optional values to measure accuracy and RTs.
;;; ------------------------------------------------------------------

;;; Checks whether a trial is valid

(defun valid-trial? (trl)
  "A trial is valid is it is made of a valid problem, a valid cell (solution), and a list of valid options" 
  (and (>= (length trl) 3)
       (valid-problem? (first trl))
       (valid-cell? (second trl))
       (valid-options? (third trl))))


(defun make-trial (trl)
  "Generates a fully useable trial from a three-item list"
  (let* ((new-options (scramble* (trial-options trl)))
	 (correct (position (trial-solution trl)
			    new-options
			    :test #'equal-cell)))
    (list (trial-problem trl)              ; Problem
	  (trial-solution trl)             ; Solution
	  new-options                      ; Options
	  correct                          ; Correct response
	  nil                              ; Actual response
	  0                                ; Problem onset
	  0                                ; Problem offset
	  0                                ; Choice onset
	  0)))                             ; Choice response time


;;; The trial problem

(defun trial-problem (trl)
  "Returns the problem in a RAPM trial"
  (nth 0 trl))

(defun set-trial-problem (trl p)
  "Returns the problem in a RAPM trial"
  (setf (nth 0 trl) p))

;;; The trial solution

(defun trial-solution (trl)
  "Returns the solution of problem in a RAPM trial"
  (nth 1 trl))

(defun set-trial-solution (trl sol)
  "Returns the solution of problem in a RAPM trial"
  (setf (nth 1 trl) sol))

;;; The trial's list of options to choose from

(defun trial-options (trl)
  "Returns the response options available for a trial" 
  (nth 2 trl))

(defun trial-options (trl opts)
  "Returns the response options available for a trial" 
  (setf (nth 2 trl) opts))

(defun valid-options? (opt)
  "A list of options is valid if every member is a valid cell" 
  (every #'valid-cell opt))

;;; The correct response

(defun trial-correct-response (trl)
  "Returns the correct answer of a trial"
  (nth 3 trl))

(defun set-trial-correct-response (trl val)
  "Returns the correct answer of a trial"
  (setf (nth 3 trl) val))

;;; The actual responses

(defun trial-actual-response (trl)
  "Returns the answer given by the model"
  (nth 4 trl))

(defun set-trial-actual-response (trl val)
  "Returns the answer given by the model"
  (setf (nth 4 trl) val))

;;; The problem onset

(defun trial-problem-onset (trl)
  (nth 5 trl))

(defun set-trial-problem-onset (trl val)
  (setf (nth 5 trl) val))

;;; The problem response time

(defun trial-problem-response-time (trl)
  (nth 6 trl))

(defun set-trial-problem-response-time (trl val)
  (setf (nth 6 trl) val))


;;; The choice onset

(defun trial-choice-onset (trl)
  (nth 7 trl))

(defun set-trial-choice-onset (trl val)
  (setf (nth 7 trl) val))


;;; The choice response time

(defun trial-choice-response-time (trl)
  (nth 8 trl))

(defun set-trial-choice-onset (trl val)
  (setf (nth 8 trl) val))


;;; Accuracy and RTs

(defun trial-accuracy (trl)
  "Returns 1 if the choice was correct, 0 otherwise" 
  (if (and (>= (length trl) 7)
	   (= (trial-correct-response trl)
	      (trial-actual-response trl)))
      1
      0))

(defun trial-problem-rt (trl)
  "Calculates the problem's RT"
  (- (trial-problem-response-time trl)
     (trial-problem-onset trl)))

(defun trial-chocie-rt (trl)
  "Calculates the problem's RT"
  (- (trial-choice-response-time trl)
     (trial-choice-onset trl)))



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

(defparameter *phase-transitions* '((problem . pause1) (pause1 . choice)
				    (choice . pause2) (pause2 . problem)))

(defun get-next-phase (phase)
  (cdr assoc phase *phase-transitions*))

(defmethod next ((task rapm-task))
  "Moves on to the next stage of the task"
  (unless (null (index task))  ; If it nil, the tast is not initialized yetr
    (incf (index task))  ; Increament the index. This is easy
    (push (current-trial task) (experiment-log task))
    (let* ((current-phase (task-phase task))
	   (next-phase (get-next-phase current-phase)))
      (if  (and (equal current-phase 'choice)
		(>= (index task) (length (trials task))))
	   (setf (task-phase task) 'done)
	   (setf (task-phase task) next-phase))
      (when (act-r-loaded?)
	(schedule-event-relative 0 #'proc-display :params nil)
	(when (member next-phase '(pause1 pause2))
	  (schedule-event-relative 1 #'next :params (list task)))))))
	      
	   
		



(defmethod respond ((task rapm-task) response)
  "Records a response in the PSS task"
  (unless (null (current-trial task))
    (let* ((trial (current-trial task)))
      (set-trial-actual-response trial response))
      
    ;; If ACT-R is loaded, we need to record response times
    ;; and sync the visicon 

    (when (act-r-loaded?)
      (let ((tme (mp-time)))
	(cond ((equal (phase task) 'problem)
	       (set-trial-problem-response-time tme))
	      ((equal (phase task) 'choice)
	       (set-trial-problem-choice-time tme)))
	(schedule-event-relative 0 #'next :params (list task))))))

(defmethod next ((task rapm-task))
  "Advances to the next stage"
  ())

(defmethod device-handle-keypress ((tm list) key)
  "Converts the key into a symbol and passes it on to the task manager"
  (let ((val (cdr (assoc (format nil "~a" key)
			 *responses
			 :test #'string-equal))))
    (respond tm val)))
			   
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

     
