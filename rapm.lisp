;; ---------------------------------------------------------------- ;;
;; a model of RAPM
;; ---------------------------------------------------------------- ;;
;; Lots of things happen. At the top level, the strategy looks like
;; this:
;;
;;   1. Observe a cell
;;   2. Find a feature that has not been verified.
;;   3.   Collect the values of tyhe property across first row/column
;;   4.   Find a property.
;;   5.     Find a rule for that property.
;;          Verify rule on second row/column
;;             If rule is verified, create a chunk that links feature, direction, and rule. Go
;;          How do we know that we have examined all the features?
;;             when we cannot retrieve a single feature, for instance. Or when we cannot retrieve a single feature that has not been examined.



(clear-all)
(define-model bar

(sgp :style-warnings nil :model-warnings nil :auto-attend t :er t)

;; Chnunk types. Not needed, technically, but save lots of warnings.

(chunk-type (rapm-problem (:include visual-object))
	    kind id)

(chunk-type (rapm-cell (:include visual-object))
	    kind row column row-num column-num problem
	    shape number)

(chunk-type (rapm-cell-location (:include visual-location))
	    row column row-num column-num problem
	    shape number)

(chunk-type (problem-location (:include visual-location))
	    id)

(chunk-type sketchpad nature verified problem)

(chunk-type rapm-goal step routine direction span direction-num span-num)

;;; Declarative memory

(add-dm (rapm-cell isa chunk)
	(rapm-problem isa chunk)

	; Goal states
	(verify isa chunk)
	(examine isa chunk)
	(collect isa chunk)
	(find-rule isa  chunk)

	; Simple feature
	
	(yes isa chunk)
	(no isa chunk)
	(zero isa chunk)
	(one isa chunk)
	(two isa chunk)
	(end isa chunk)
	(start isa chunk)

	;; Slot names
	
	(row isa chunk)
	(row-num isa chunk)
	(column isa chunk)
	(column-num isa chunk)
	(same isa chunk)
	(solution isa chunk)

	;; Other
	(sketchpad isa chunk)
	
	
	(do-rapm isa rapm-goal
		 step start
		 direction row
		 span column
		 direction-num row-num
		 span-num column-num))
	
;(p attend-cell
;   ?visual-location>
;      buffer empty
;==>
;   +visual-location>
;      screen-x lowest
;      screen-y lowest
;)


;; ---------------------------------------------------------------- ;;
;; START
;; ---------------------------------------------------------------- ;;

(p attend-problem
   =goal>
      step start
   ?visual-location>
      buffer empty
==>
   +visual-location>
      kind rapm-problem

)

(p start*retrieve-solutions
   =goal>
     step start
   =visual>
     kind rapm-problem
     id =PID
   ?retrieval>
     buffer empty
     state free
==>
   =goal>
     step check-solutions
      
   +retrieval>
     nature solution
     problem =PID

   =visual>
)

(p start*solution-found
   "If a solution has been found, check its satisfaction"
   =goal>
     step check-solutions

   =visual>
     kind  rapm-problem
     id  =PID

   =retrieval>
     nature solution
     problem =PID
   
==>
   =goal>
     step satisfaction

   @imaginal>
     =retrieval
   
   =visual>
)


(p start*solution-not-found
   "If a solution has NOT been found, the create a bogus solution (and begin examining features) "
   =goal>
     step check-solutions
   
   =visual>
     kind  rapm-problem
     id  =PID
   ?retrieval>
     state error
   ?imaginal>
     state free
     buffer empty
==>
   +imaginal>
     nature solution

   =goal>
     step satisfaction

   -retrieval>
   =visual>
)



;;; -------------------------------------------------------------- ;;;
;;; SATISFIED
;;; -------------------------------------------------------------- ;;;
;;; Checks whether our solutions are satisfying or not.
;;; -------------------------------------------------------------- ;;;

(p satisfy*no-because-no-row
   "If you have just examined a solution, examine more features"
   =goal>
     step satisfaction
   =visual>
     kind  rapm-problem
     id  =PID
   =imaginal>
     nature solution  
     row nil
     ;column nil
==>
   =goal>
     step examine
     direction row
     routine collect

   =visual>
)

(p satisfy*no-because-no-column
   "If you have just examined a solution, examine more features"
   =goal>
     step satisfaction
   =visual>
     kind  rapm-problem
     id  =PID
   =imaginal>
     nature solution  
    ;row nil
     column nil
==>
   =goal>
     step examine
     direction row
     routine collect

   =visual>
)

(p satisfy*yes
   "If you have just examined a solution, examine more features"
   =goal>
     step satisfaction
   =visual>
     kind  rapm-problem
     id  =PID
   =imaginal>
     nature solution  
    - row nil
    - column nil
==>
   =goal>
     step end
   =visual>
)

;; ----------------------------------------------------------------
;; EXAMINE
;; ----------------------------------------------------------------

(p examine*look-at-cells
   =goal>
      step examine
      routine collect
      direction =DIR
   =visual>
      kind rapm-problem
==>
   +visual-location>
     kind rapm-cell
     screen-x lowest
     screen-y lowest
)


;; ----------------------------------------------------------------
;; The COLLECT routine
;; ----------------------------------------------------------------
;; This is a routine that needs to be executed over and over.
;; It consists of scaning a row or column collecting feauture to
;; put in the imaginal buffer.
;; ----------------------------------------------------------------


(p collect*by-row
   "Collects the value of a feature while scanning horizontally"
   =goal>
      routine collect

   ?visual>
      state free

   =visual>
      kind rapm-cell
      row =ROW
      column =COL
      =FEATURE =VAL

   =imaginal>
      direction row
      value =ROW
      feature =FEATURE
      =COL nil
      two nil   ; The last value must be still free
==>
   =imaginal>
      =COL =VAL
   
   +visual-location>
      screen-y current
    > screen-x current
      screen-x lowest    
   
)

(p collect*by-column
   "Collects the values of a feature vertically"
   =goal>
      routine collect

   ?visual>
      state free

   =visual>
      kind rapm-cell
      row =ROW
      column =COL
      =FEATURE =VAL

   =imaginal>
      direction column
      value =COL
      feature =FEATURE
      =ROW nil
      two nil   ; The last value must be still free
==>
   =imaginal>
      =COL =VAL
   
   +visual-location>
      screen-y current
    > screen-x current
      screen-x lowest    
   
)

;; ----------------------------------------------------------------
;; RULE FINDING
;; ----------------------------------------------------------------
;; These are the routines to examine a proposed rule
;; ----------------------------------------------------------------


(p examine*examine-pattern
   "Examine a pattern and try to find a rule"
   ?visual>
   state free;
   
   =visual>
      kind rapm-cell
      row =R
      column =C
      =F =VAL

   =imaginal>
      direction row
      value =R
      feature =F
      =C nil
      two =VAL
==>
   =imaginal>
      =C =VAL
   
   +visual-location>
      screen-y current
    > screen-x current
      screen-x lowest    
   
)

   



;; ----------------------------------------------------------------
;; Verify rules
;; ----------------------------------------------------------------
;; A rule is verified when it is consistent with the features of
;; three cells.
;; ----------------------------------------------------------------


(p verify*init
   =goal>
     step verify
     direction =DIR    
      
   ?retrieval>
      state free
      buffer empty
      
   =imaginal>
      rule =SOMETHING
      verified nil
   ?imaginal>
      state free   
==>
   @retrieval> =imaginal

   +imaginal>   ; Create a new imaginal buffer
      nature sketchpad
      direction =DIR
      focus zero
       
   +visual-location>
      kind rapm-cell
      row zero
      column zero
)

;; This is a bogus production. Will always verify everything
;;
(p verify*verify-cell-bogus
   =goal>
     step verify
     direction =DIR
     span =INDEX
      
   =retrieval>
     rule =SOMETHING
     verified nil

   =visual>
     kind rapm-cell
     =INDEX =VAL

   =imaginal>
     focus =VAL
     direction =DIR
     verified nil  

==>
   =imaginal>
     focus =VAL
     verified yes
   =retrieval>
   =visual>
)

(p verify*attend-next-cell
   "After verifying one cell, moves in the same direction (indicated by SPAN) to the next one"
   =goal>
     step verify
     span =COORDINATE
     span-num =INDEX
      
   =retrieval>
     rule =SOMETHING
     verified nil

   =visual>
     kind rapm-cell
     =COORDINATE =VAL
   - =COORDINATE two
   
   =imaginal>
     focus =VAL
     verified yes  

==>
   =retrieval>  ; Keep focus
     
   =imaginal>
     verified nil
     focus nil
     
   +visual-location>
     kind rapm-cell
     > =INDEX current
     :nearest current 
)

(p verify*focus-on-attended-cell
   "If I am looking at a new cell, make it the focus of verification"
   =goal>
     step verify
     span =SPAN
      
   =retrieval>
     rule =SOMETHING
     verified nil

   =visual>
     kind rapm-cell
     =SPAN =VAL
   
   =imaginal>
     nature sketchpad
     focus nil
     verified nil
 
==>
   =imaginal>
     focus =VAL

   =retrieval>
   =visual>

)


(p verify*attend-next-line
   "Notes when an entire line satisfies a rule"
   =goal>
     step verify
     direction =DIR
     span =SPAN
      
   =retrieval>
     rule =SOMETHING
     verified nil

   =visual>
     kind rapm-cell
    =SPAN two
    =DIR zero 
   
   =imaginal>
     nature sketchpad
     focus two
     verified yes
==>
   =retrieval>  ; Keep focus
     
   =imaginal>
     verified nil
     focus nil
     
  +visual-location>
    kind rapm-cell
    =SPAN zero  ; Beginning of the line 
    =DIR one    ; Next line (row or col) 
)

(p verify*success
   "Notes when an entire line satisfies a rule"
   =goal>
     step verify
     direction =DIR
     span =SPAN
      
   =retrieval>
     rule =SOMETHING
     verified nil

   =visual>
     kind rapm-cell
    =SPAN two
    =DIR one
     problem =PID
   
   =imaginal>
     nature sketchpad
     focus two
     verified yes

   ?imaginal>
     state free  
==>
;; We are done. The rule is verified. We just need to create a
;; a chunk that links the rule to the problem.
     
   =imaginal>
      nature solution   
      problem =PID
      rule =SOMETHING
      direction =DIR
      
)

(p memorize-solution
   =goal>
     step verify

   =imaginal>
     nature solution

   ?visual>
     state free   
==>
   =goal>
     step start
   
   +visual-location>
      kind rapm-problem   
)



;; ---------------------------------------------------------------- ;;
;; FEATURE SELECTION
;; ---------------------------------------------------------------- ;;
;; Here are all the productions that compete for feature selection
;; ---------------------------------------------------------------- ;;

;; This is the crucial part.
(p select-feature*shape
   =goal>
      step examine
      direction =DIR
   =visual>
      shape =S
      row =R
   ?imaginal>
      state free   
==> 
   =goal>
      step find-rule
   
   +imaginal>
      feature shape
      direction =DIR
      value =R
      
   =visual>
)


(p select-feature*number
   =goal>
      step examine
      direction =DIR
      
   =visual>
      number =N
      row =R
   ?imaginal>
      state free   
==> 
   =goal>
      step find-rule
   
   +imaginal>
      feature number
      direction =DIR
      value =R
      
   =visual>
)


;; ----------------------------------------------------------------
;; RULE SELECTION
;; ----------------------------------------------------------------
;; Here are the productions that compete for selecting rules
;; ----------------------------------------------------------------

;; Now, how do we 'propose' a rule?

(p propose*same-rule
   "Rule to be suggested when a feauture remains the same"
   =goal>
     step find-rule

   =imaginal>
     zero =P
     one =P
     two =P
     rule nil
==>
   =imaginal>
     rule same
   
   =goal>
     step verify  
)

;; ----------------------------------------------------------------
;; Special verification rules
;; ----------------------------------------------------------------


(goal-focus do-rapm)

)


(defun my-reload ()
  (reload)
  (install-device simple-trial)
  (proc-display)
  (print-visicon))
