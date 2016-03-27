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

(sgp :style-warnings t :model-warnings t :auto-attend t :er t)

;; Chnunk types. Not needed, technically, but save lots of warnings.

(chunk-type (rapm-screen (:include visual-object))
	    kind id)

(chunk-type (rapm-cell (:include visual-object))
	    kind row column row-num column-num problem
	    phase shape number 
	    feature0 feature1 feature2 feature3 feature4
	    feature5 feature6 feature7 feature8 feature9)

(chunk-type (rapm-cell-location (:include visual-location))
	    row column row-num column-num problem
	    shape number
	    feature0 feature1 feature2 feature3 feature4
	    feature5 feature6 feature7 feature8 feature9)

(chunk-type (rapm-screen-location (:include visual-location))
	    id)

(chunk-type sketchpad nature verified problem)

(chunk-type rapm-goal step routine pid direction span direction-num span-num)

(chunk-type missing-cell kind nature pid) 

(chunk-type feature kind feature)

;;; Declarative memory

(add-dm (rapm-cell isa chunk)
	(rapm-problem isa chunk)
	(rapm-pause isa chunk)

	;; Goal states

	(verify isa chunk)
	(examine isa chunk)
	(collect isa chunk)
	(find-rule isa  chunk)

	;; Simple chunks
	
	(yes isa chunk)
	(no isa chunk)	
	(zero isa chunk)
	(one isa chunk)
	(two isa chunk)
	(three isa chunk)
	(end isa chunk)
	(start isa chunk)
	(pause1 isa chunk)
	(pause2 isa chunk)
	(done isa chunk)

	;; Features
	(triangle isa chunk)
	(square isa chunk)
	(circle isa chunk)
	(diamond isa chunk)
	(thick isa chunk)
	(think isa chunk)
	

	;; Slot names
	(row isa chunk)
	(row-num isa chunk)
	(column isa chunk)
	(column-num isa chunk)
	(same isa chunk)
	(solution isa chunk)

	;; Other

	(sketchpad isa chunk)

	;; Feature chunks
	(shape-feature isa feature
		       kind feature
		       feature shape)

	(number-feature isa feature
			kind feature
			feature number)
	
	(do-rapm isa rapm-goal
		 pid nil
		 step start
		 direction row
		 span column
		 direction-num row-num
		 span-num column-num))
	


;;; ---------------------------------------------------------------- ;;
;;; START
;;; ---------------------------------------------------------------- ;;
;;;
;;; New algorithm:
;;;   1. Pick a feature.
;;;   2. If you cannot find a solution associated to that feature,
;;;      examine the feature (randomly going column or row).
;;;   3. If you can find a feature, then, check
;;;      3.1 If it's enough, end

(p start*attend-problem
   ?goal>
     buffer empty
   
   ?visual-location>
      buffer empty
==>
   +visual-location>
      kind rapm-problem
)


(p start*create-goal
   ?goal>
     buffer empty
   
   =visual>
     kind rapm-problem
     id =PID
==>
   +goal>
      isa rapm-goal
      step start
      kind rapm-problem
      pid =PID
   =visual>
)


(p start*retrieve-solution-row
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
     direction row
     span column
     direction-num row-num
     span-num column-num
      
   +retrieval>
     nature solution
     problem =PID
     direction row
  
   =visual>
)

(p start*retrieve-solution-column
   =goal>
     step start
     direction =DIR
   
   =visual>
     kind rapm-problem
     id =PID

   =retrieval>
     nature solution
     direction row

   ?retrieval>
      state free  
==>
   =goal>
     direction column
     span row
     direction-num column-num
     span-num row-num

   +retrieval>
     nature solution
     problem =PID
     direction column
  
   =visual>
)

(p start*solution-not-found
   =goal>
     step start
     direction =DIR
     
   =visual>
     kind rapm-problem
     id =PID

   ?retrieval>
     state error
==>
   =goal>
     step examine
     routine collect
   
   +imaginal>
     direction =DIR
   
   =visual>

   -retrieval>  
)

(p start*satisfied
   =goal>
     step start
     direction column
   
   =visual>
     kind rapm-problem
     id =PID

   =retrieval>
     nature solution
     direction column
==>
   =goal>
     step respond
   
   =visual>
)   

(p start*respond
   =goal>
     step respond
   
   =visual>
     kind rapm-problem
     id =PID
   
   ?manual>
     preparation free
     processor free
     execution free
     
==>

  +manual>
     isa punch
     hand right
     finger index
)

;;; ----------------------------------------------------------------
;;; EXAMINE
;;; ----------------------------------------------------------------
;;; The examination part is where a feature is selected to collect
;;; its variation across row or columns.
;;; ------------------------------------------------------------------


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
      =ROW =VAL
   
   +visual-location>
      screen-x current
    > screen-y current
      screen-y lowest    
   
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

;;; ----------------------------------------------------------------
;;; RULE VERIFICATION
;;; ----------------------------------------------------------------
;;; A rule is verified when it is consistent with the features of
;;; three cells. The verification process roughly follows this
;;; algorithm:
;;;   1. Move the tenativ solution (contents of the imaginal buffer)
;;;      to the retrieval. This free the resources for the imaginal.
;;;   2. Scan the first row, cell by cell.
;;;   3. For each cell a special 'verify*RULE' production should be
;;;      called (need to write one for each rule)
;;;   4. Repeat for the second row.
;;;   5. If the second row is finished, the rule is verified.
;;;
;;; ----------------------------------------------------------------


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


;;; ------------------------------------------------------------------
;;; SOLUTION 
;;; ------------------------------------------------------------------
;;; This is the part where the model identifies a solution
;;; ------------------------------------------------------------------


(p generate*retrieve-solution
   =goal>
     step generate
     pid =PID

   =imaginal>
     nature missing-cell
     completed nil

   ?imaginal>
     state free
   
   ?retrieval>
     buffer empty
     state free
==>
   +retrieval>
     nature solution
     pid =PID
     :recently-retrieved nil
   =imaginal> ; Keep the imaginal buffer
)

;;; In order to generate a solution, we need to be able to predict
;;; a feature based on the value of the same feature across rows or
;;; columns. To do this, we need again to collect features. 

(p generate*collect-features
   =goal>
     step generate
     routine nil
   
   =retrieval>
     nature solution
     direction row

==>
   +visual-location>
     kind rapm-cell
     row zero
     column two
)     
     
(p generate*done
   "When no more solution rules can be retrieved, we are done"
   =goal>
     step generate

   =imaginal>
     nature missing-cell
     completed nil
     
   ?retrieval>
     state error

   ?imaginal>
     state free  
==>

   =imaginal>
     completed yes  
   ;; Clean-up the retrieval buffer
   -retrieval>
)

(p generate*switch-to-respond
   "Done with the generation, switches to responding"
   =goal>
     step generate
   
   =imaginal>
     nature missing-cell
     completed yes
==>
   =goal>
     step respond
)


;; ---------------------------------------------------------------- ;;
;; FEATURE SELECTION
;; ---------------------------------------------------------------- ;;
;; Here are all the productions that compete for feature selection
;; ---------------------------------------------------------------- ;;

;; This is the crucial part.

(p feature*pick-shape
   "REtrieves a feature"
   =goal>
      step examine
      direction =DIR
      
   =visual>
      shape =S
            
   ?retrieval>
     state free
     buffer empty
==>
   +retrieval>
     isa feature
     kind feature
     feature shape
     
   =visual>
)


(p feature*select-feature
   "Once a feature has been retrieved, select it for examination"
   =goal>
      step examine
      direction =DIR
   =visual>
      shape =S
      row =R      
   =retrieval>
      isa feature
      kind feature
      feature =FEATURE
==> 
   =goal>
      step find-rule

   =visual>
      
   +imaginal>
      feature =FEATURE
      direction =DIR
      value =R
)

#|
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
|#

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

;;; ----------------------------------------------------------------
;;; Special verification rules
;;; ----------------------------------------------------------------


;;; ==================================================================
;;; CHOICE
;;; ==================================================================
;;; The choice procedure is essentially a hack. The model simply
;;; retrieves the generated missing cell. Then, it scans the options.
;;; Then it simply retrieves the option that best matches.
;;; Crude but should be effective.
;;; ==================================================================

(p choice*initiate-response
   "When the options show up, prepare to respond"
;   =goal>
;   - step respond

   =visual>
     kind rapm-choice
     id =PID
     
   ?retrieval>
     buffer empty
     state free
 ==>
;   =goal>
;     step respond

   +retrieval>
     nature missing-cell
     pid =PID
   =visual>
)

(p choice*rehearse-solution
   "When the solution has been found, put it in the imaginal buffer "
   =goal>
     step respond

   =visual>
     kind rapm-choice
     id =PID
     
   =retrieval>
     nature missing-cell
   
   ?imaginal>
     state free
==>
   @imaginal> =retrieval>

   +visual-location>
     kind rapm-cell
;     phase choice
     screen-x lowest

)


(p choice*scan-options
   "When we have a solution, scan the available options"
   =goal>
     step respond

   =visual>
     kind rapm-cell
     phase choice
        
   =imaginal>
     nature missing-cell
   
   ?visual>
     state free  
==>

   +visual-location>
     kind rapm-cell
;     phase choice
   > screen-x current
   ;;screen-x lowest
     :nearest current-x
       
   =imaginal>  ; keep the imaginal
)

(p choice*finish-scanning
   "When we have scanned all the options, we retrieve the most similar"
   =goal>
     step respond
 
   =imaginal>
     nature missing-cell
     pid =PID
   
   ?visual-location>
     state error
   
   ?retrieval>
     state free  
==>

   +retrieval>  
     kind rapm-cell
     problem =PID
     phase choice

   ;; Needs to look at the screen, so that when the
   ;; pauses show up, they can be encoded. This permits
   ;; to detect the 'done screen.
   +visual-location>
     kind rapm-choice  
)

(p choice*respond-index
   "Responds with index to a cell with column index 0"
   =goal>
     step respond
 
   =retrieval>  
     kind rapm-cell
     phase choice
     column zero 

   ?manual>
     preparation free
     processor free
     execution free
==>
   
   +manual>
     isa punch
     hand right
     finger index

   -goal>
)
  
(p choice*respond-middle
   "Responds with middle finger to a cell with column index 1"
   =goal>
     step respond
 
   =retrieval>  
     kind rapm-cell
     phase choice
     column one

   ?manual>
     preparation free
     processor free
     execution free
==>
   
   +manual>
     isa punch
     hand right
     finger middle
     
   -goal>
)


(p choice*respond-ring
   "Responds with ring finger to a cell with column index 2"
   =goal>
     step respond
 
   =retrieval>  
     kind rapm-cell
     phase choice
     column two

   ?manual>
     preparation free
     processor free
     execution free
==>
   
   +manual>
     isa punch
     hand right
     finger ring
     
   -goal>
)

(p choice*respond-pinkie
   "Responds with pinkie finger to a cell with column index 3"
   =goal>
     step respond
 
   =retrieval>  
     kind rapm-cell
     phase choice
     column three

   ?manual>
     preparation free
     processor free
     execution free
==>
   
   +manual>
     isa punch
     hand right
     finger pinkie
    
   -goal>
)


(p choice*respond-random
   "When no solution has been found, pick one at random "
   =goal>
     step respond

   =visual>
     kind rapm-choice
     id =PID
     
   ?retrieval>
     state error
   
   ?imaginal>
     state free
     buffer empty
   ?visual>
     state free  
==>
   ;; Create a neutral missing cell, with no expectations      
   +imaginal>
     isa missing-cell
     kind nothing
     nature missing-cell
     pid =PID
     
   +visual-location>
     kind rapm-cell
;     phase choice
     screen-x lowest

)


;;; ------------------------------------------------------------------
;;; DONE!
;;; ------------------------------------------------------------------

(p done
   "Neatly stops when the screen says 'done"
   =visual>
     kind rapm-pause
     value done
==>
   !stop!
)

;(goal-focus do-rapm)
 
)


(defun my-reload ()
  (reload)
  (install-device (make-instance 'rapm-task))
  (init (current-device))
  (proc-display)
  (print-visicon))



