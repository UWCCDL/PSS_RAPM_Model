;;; ==================================================================
;;; A model of RAPM
;;; ==================================================================
;;; Devel 7
;;; 
;;; This is a complicated algorithm.
;;; ==================================================================

(clear-all)
(written-for-act-r-version "7.5.0")
(define-model bar-devel-model7

(sgp :style-warnings nil
     :model-warnings nil
     :style-warnings nil
     :auto-attend t
     :er t
     :ans 0.05
     :record-ticks nil
     :esc t
     :mas 8.0
     :bll nil 
     :blc 100.0  ;; Assumes all chunks are incredibly active
     :lf 0.01
     :ul t
     :reward-hook bg-reward-hook-anticorrelated
     :alpha 0.2
     :egs 0.01
     :imaginal-activation 10
     :visual-activation 10
     :trace-filter production-firing-only
     )
  

;;; CHUNK TYPES
;;;
;;; Chunk types. Not needed... but they save lots of warnings.
;;;
(chunk-type (rapm-screen (:include visual-object))
	    kind id)

(chunk-type (rapm-cell (:include visual-object))
	    kind row column row-num column-num problem
	    phase shape number background texture
	    feature0 feature1 feature2 feature3 feature4
	    feature5 feature6 feature7 feature8 feature9)

(chunk-type (rapm-cell-location (:include visual-location))
	    row column row-num column-num problem
	    shape number background texture
	    feature0 feature1 feature2 feature3 feature4
	    feature5 feature6 feature7 feature8 feature9)

(chunk-type (rapm-screen-location (:include visual-location))
	    id)

(chunk-type sketchpad nature verified problem)

(chunk-type rapm-goal step routine problem direction span direction-num span-num)

(chunk-type missing-cell kind nature pid) 

(chunk-type feature kind feature)

(chunk-type solution problem feature rule direction predicted-value)

(chunk-type rule kind same different progression name)

;; (chunk-type direction kind direction span direction-num span-num) 

;;; DECLARATIVE MEMORY
;;; 
;;; A list of chunks in DM.
;;;
(add-dm (rapm-cell isa chunk)
	(rapm-problem isa chunk)
	(rapm-pause isa chunk)
	(rapm-choice isa chunk)

	;; The coordinates

	(row0-col0 isa chunk)
	(row0-col1 isa chunk)
	(row0-col2 isa chunk)
	(row1-col0 isa chunk)
	(row1-col1 isa chunk)
	(row1-col2 isa chunk)
	(row2-col0 isa chunk)
	(row2-col1 isa chunk)
	(row2-col2 isa chunk)
	
	;; Goal states

	(verify isa chunk)
	(examine isa chunk)
	(collect isa chunk)
	(find-rule isa  chunk)
	(choice isa chunk)
	(check isa chunk)
	(start isa chunk)
	(generate isa chunk)

	;; Simple chunks
	
	(yes isa chunk)
	(no isa chunk)	
	(zero isa chunk)
	(one isa chunk)
	(two isa chunk)
	(three isa chunk)
	(pause1 isa chunk)
	(pause2 isa chunk)
	(done isa chunk)
	(problem isa chunk)
	(nothing isa chunk)
	(missing-cell isa chunk)
	(sketchpad isa chunk)

	;; Features and values
	(feature isa chunk)
	(shape isa chunk)
	(triangle isa chunk)
	(square isa chunk)
	(circle isa chunk)
	(diamond isa chunk)

	(number isa chunk)

	(texture isa chunk)
	(solid isa chunk)
	(transparent isa chunk)
	(dotted isa chunk)
	(striped isa chunk)

	(size isa chunk)
	(small isa chunk)
	(medium isa chunk)
	(large isa chunk)
	
	;; Slot names
	(row isa chunk)
	(row-num isa chunk)
	(column isa chunk)
	(column-num isa chunk)
	(same isa chunk)
	(solution isa chunk)

	;; Feature chunks
	(shape-feature isa feature
		       kind feature
		       feature shape)

	(number-feature isa feature
			kind feature
			feature number)

	(texture-feature isa feature
			 kind feature
			 feature texture)

	(background-feature isa feature
			    kind feature
			    feature background)

	(orientation-feature isa feature
			     kind feature
			     feature orientation)

	;; Rules
	(same-rule isa rule
		   kind rule
		   name same
		   same possible
		   progression nil
		   different nil)

	(progression-rule isa rule
			  kind rule
			  name progression
			  same nil
			  progression possible
			  different possible)

	(constant-rule isa rule
		       kind rule
		       name constant
		       same possible
		       progression possible
		       different possible)

	(disjoint-rule isa rule
		       kind rule
		       name disjoint
		       same nil
		       progression possible
		       different possible)
	)
	


;;; ---------------------------------------------------------------- ;;
;;; START
;;; ---------------------------------------------------------------- ;;
;;;
;;; General solution algorithm:
;;;   1. Pick a feature.
;;;   2. Retrieve a solution for that feature.
;;;   3. If you cannot find a solution associated to that feature,
;;;      examine the feature (randomly going column or row).
;;;   4. If you can find a solution, then, check:
;;;      4.1 If it's been a long time since you have found a new
;;;          feature, end
;;;      4.2 If not, go back to 1
;;;
;;;          Pick Feature <------+
;;;               |              |
;;;         Find Solution        |
;;;               |              |
;;;             Found?           |
;;;          /         \         |
;;;        No          Yes     No
;;;        |             |     /
;;;    Mark Time      Long time?
;;;        |                 \  
;;;   *EXAMINE*              Yes
;;;                           |
;;;                         *END*
;;; ------------------------------------------------------------------

(p start*attend-problem
   "Attends a problem"
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
      problem =PID
      
   +visual-location>
      kind rapm-cell
      screen-x lowest
      screen-y lowest
   +temporal>
      isa time
      ticks 0
   ;; Cleans up the reward signals
   !eval! (trigger-reward nil)
)

;;; ---------------------------------------------------------------- ;;
;;; 1.1 FEATURE SELECTION
;;; ---------------------------------------------------------------- ;;
;;; Here are all the productions that compete for feature selection.
;;; Feature selection consists of two steps only, picking a feature
;;; to examine and encoding it into WM. The process of picking a
;;; feature consists of multiple Pick/Don't Pick productions, one
;;; pair for each feature.
;;;
;;;   Pick    Don't   Pick  Don't  Pick   ... Don't
;;;   Shape   Shape   Num   Num    ...        ...
;;;       \      \    \    /     /          /
;;;          Encode The Selected Feature
;;;
;;; ---------------------------------------------------------------- ;;

;; This is the crucial part.

(p feature*prepare
   =goal>
      step start
      
   =visual>
     isa rapm-cell

   ?imaginal>
     state free
     buffer empty
 ==>
   =visual>    

   +imaginal>
     kind attention
   
;   +temporal>
;     isa time  
;     ticks 0  
)


(p feature*pick-shape
   "Retrieves a feature"
   =goal>
      step start
      
   =visual>
    - shape nil

   =imaginal>
     kind attention
     feature nil
     shape nil

   ?imaginal>
     state free  
==>
   *imaginal>
     feature shape
     
   =visual>
)

(p feature*dont-pick-shape
   "Retrieves a feature"
   =goal>
      step start
      
   =visual>
   - shape nil

   =imaginal>
     feature nil
     shape nil

   ?imaginal>
     state free  
==>
   *imaginal>
     shape no
     
   =visual>
)

(p feature*pick-number
   "Retrieves a feature"
   =goal>
      step start
      
   =visual>
   - number nil

   =imaginal>
     kind attention
     feature nil
     number nil

   ?imaginal>
     state free  
==>
   *imaginal>
     feature number
     
   =visual>
)


(p feature*dont-pick-number
   "Does not select number"
   =goal>
      step start
      
   =visual>
   - number nil

   =imaginal>
     feature nil
     number nil

   ?imaginal>
     state free  
==>
   *imaginal>
     number no
     
   =visual>
)


(p feature*pick-texture
   "Retrieves a feature"
   =goal>
      step start
      
   =visual>
   - texture nil

   =imaginal>
     kind attention
     feature nil
     texture nil

   ?imaginal>
     state free  
==>
   *imaginal>
     feature texture
     
   =visual>
)


(p feature*dont-pick-texture
   "Does not select texture"
   =goal>
      step start
      
   =visual>
   - texture nil

   =imaginal>
     feature nil
     texture nil

   ?imaginal>
     state free  
==>
   *imaginal>
     texture no
     
   =visual>
)



(p feature*pick-background
   "Retrieves a feature"
   =goal>
      step start
      
   =visual>
   - background nil

   =imaginal>
     kind attention
     feature nil
     background nil

   ?imaginal>
     state free  
==>
   *imaginal>
     feature background
     
   =visual>
)


(p feature*dont-pick-background
   "Does not select texture"
   =goal>
      step start
      
   =visual>
   - background nil

   =imaginal>
     feature nil
     background nil

   ?imaginal>
     state free  
==>
   *imaginal>
     background no
     
   =visual>
)

(p feature*restart
   =goal>
     step start

   =imaginal>
     feature nil

;   =temporal>
;     isa time
;   > ticks 10

   ?imaginal>
     state free  

==>
  ; -temporal>
  -imaginal>
   ;*imaginal>
    ; background nil
     ;texture nil
     ;number nil
     ;shape nil
)

(p feature*end-of-selection
   =goal>
     step start

   =visual>
     row =R
   
   =imaginal>
   - feature nil
==>
;   +temporal>
;      isa time
;      ticks 0
   =visual>
   =imaginal>
     value =R
   *goal>
     step check
)

;;; ---------------------------------------------------------------- ;;
;;; 1.2 FEATURE CHECK
;;; ---------------------------------------------------------------- ;;
;;; Feature check is the process by which a feature is considered as
;;; somethinbg to examine. It consists of two steps, retrieving the
;;; solution for a feature (in a given problem), and deciding what
;;; to do when the feature has been found:
;;;
;;;         Retrieve Solution with Feature
;;;                      |
;;;               Solution Found?
;;;                /           \
;;;             (No)          (Yes)
;;;              /               \
;;;         *COLLECT*           Is time since last
;;;                             examination > 20?
;;;                               /             \
;;;                            (No)            (Yes)
;;;                             /                 \
;;;                          Try another       *GENERATE SOLUTION*
;;;                          Feature
;;;
;;; ---------------------------------------------------------------- ;;


(p check*retrieve-solution-for-feature
   "Once a feature has been picked, examine whether a solution exists "
   =goal>
      step check
      problem =PID
      
   =visual>
      kind rapm-cell

   ?retrieval>
      state free
      buffer empty

   =imaginal>
      feature =FEATURE

 ==>
   =visual>      ; Keep looking
   =imaginal>    ; keep in WM
   +retrieval>
     nature solution
     problem =PID
     feature =FEATURE
)
      

(p check*solution-found-and-time-elapsed
   "If a solution has been found and time has passed, proceed"
   =goal>
      step check
      problem =PID
      
   =visual>
      kind rapm-cell

   =retrieval>
      nature solution
      problem =PID
      feature =FEATURE

   =imaginal>
      feature =FEATURE

   !bind! =MAX *ticks*
   =temporal>
      isa time
    > ticks =MAX  ; Totally random value
 ==>
   =goal>
   ;;step respond
     step generate

   +visual-location>
      kind rapm-problem

   -temporal>   ; Stop counting
   !eval! (reset-declarative-finsts)
;   !eval! (trigger-reward (* -1 *reward*))
)


(p check*solution-found-and-time-not-elapsed
   "If a solution has been found and time has not passed, back to selecting"
   =goal>
      step check
      problem =PID
      
   =visual>
      kind rapm-cell

   =retrieval>
      isa solution
      problem =PID
      feature =FEATURE

   =imaginal>
      feature =FEATURE

   !bind! =MAX *ticks*
   =temporal>
      isa time
    < ticks =MAX   ; Totally random value! What does it translate to?
 ==>
   =goal>
      step start

   =visual>
   ;; This is ugly but I cannot find a better way to do it.
   ;; Should ask Dan...
   !eval! (reset-declarative-finsts)   
;   !eval! (trigger-reward (* -1 *reward*))
)

(p check*solution-not-found-row
   "If a previous solution cannot be found, initiate the process of finding one by row"
   =goal>
      step check
      problem =PID
      
   =visual>
      kind rapm-cell

   =imaginal>
      feature =FEATURE

   ?retrieval>
      state error

 ==>
   =goal>
      step find-rule
      direction row   
      span column
      direction-num row-num
      span-num column-num
      routine collect
      value zero
      
   =imaginal>
      direction row
   
   =visual>   

   -retrieval> ; Clear retrieval error
)


(p check*solution-not-found-column
   "If a previous solution cannot be found, initiate the process of finding one by column"
   =goal>
      step check
      problem =PID
      
   =visual>
      kind rapm-cell

   =imaginal>
      feature =FEATURE

   ?retrieval>
      state error

 ==>
   =goal>
      step find-rule
      direction column
      span row
      direction-num column-num
      span-num row-num
      routine collect
      
   =imaginal>
      direction column
   
   =visual>   

   -retrieval> ; Clear retrieval error
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
     
  -temporal>
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
;; RULE SELECTION
;; ----------------------------------------------------------------
;; Here are the productions that compete for selecting rules
;; ----------------------------------------------------------------

;; Now, how do we 'propose' a rule?


(p find-rule*ppick-same
   "Rule to be suggested when a feauture remains the same"
   =goal>
     step find-rule

   =imaginal>
     zero =P
     one =P
     two =P
     rule nil
     
   ?retrieval>
     state free
     buffer empty
==>
   =imaginal>
   +retrieval>
     isa rule
     kind rule
     name same
     same possible

)

#| ;; Not yet
(p find-rule*dont-pick-same
   "Rule to be suggested when a feauture remains the same"
   =goal>
     step find-rule

   =imaginal>
     zero =P
     one =P
     two =P
     rule nil
     
   ?retrieval>
     state free
     buffer empty
==>
   +retrieval>
     isa rule
     kind rule
   _ name same
     same predicted 
)
|#

(p find-rule*ppick-progression
   "Rule to be suggested when a feauture increases"
   =goal>
     step find-rule

   =imaginal>
     zero =X
     one =Y
   > one =X
   > two =Y
     rule nil
     
   ?retrieval>
     state free
     buffer empty
==>
  =imaginal>

   +retrieval>
     isa rule
     kind rule
     name progression
     progression possible

)


(p find-rule*ppick-constant
   "Rule to be suggested when a feauture increases"
   =goal>
     step find-rule

   =imaginal>
     zero =X
     one =Y
   - one =X
   - two =Y
     rule nil
     
   ?retrieval>
     state free
     buffer empty
==>
  =imaginal>

   +retrieval>
     isa rule
     kind rule
     name constant
     progression possible
)

#|
(p find-rule*dont-pick-constant
   "Rule to be suggested when a feauture increases"
   =goal>
     step find-rule

   =imaginal>
     zero =X
     one =Y
   - one =X
   - two =Y
     rule nil
     
   ?retrieval>
     state free
     buffer empty
==>
  =imaginal>

   +retrieval>
     isa rule
     kind rule
   - name constant
     progression possible
     )

(p find-rule*dont-pick-progression
   "Rule to be suggested when a feauture increases"
   =goal>
     step find-rule

   =imaginal>
     zero =X
     one =Y
   > one =X
   > two =Y
     rule nil
     
   ?retrieval>
     state free
     buffer empty
==>
  =imaginal>

   +retrieval>
     isa rule
     kind rule
   - name progression
     progression possible
)

(p find-rule*dont-pick-same
   "Rule to be suggested when a feauture increases"
   =goal>
     step find-rule

   =imaginal>
     zero =P
     one =P
     two =P
     rule nil
     
   ?retrieval>
     state free
     buffer empty
==>
  =imaginal>

   +retrieval>
     isa rule
     kind rule
   - name same
     progression possible
)
|#

(p find-rule*accept-suggestion
   "Very neutral. Accepts any suggestion"
   =goal>
     step find-rule

   =imaginal>
   - zero nil
   - one nil
   - two nil
     rule nil
     
   =retrieval>
     kind rule
     name =RULE-NAME
==>
   =imaginal>
     rule =RULE-NAME
   
   =goal>
     step verify
     routine nil
)



;;; ----------------------------------------------------------------
;;; RULE VERIFICATION
;;; ----------------------------------------------------------------
;;; A rule is verified when it is consistent with the features of
;;; three cells. The verification process roughly follows this
;;; algorithm:
;;;   1. Move the tentative solution (contents of the imaginal buffer)
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

   +imaginal>   ; Create a new imaginal chunk
      nature sketchpad
      direction =DIR
      focus zero
       
   +visual-location>
      kind rapm-cell
      row zero
      column zero
)


(p verify*verify-cell
   =goal>
     step verify
     direction =DIR
     span =INDEX
      
   =retrieval>
     rule =SOMETHING
     verified nil
     feature =FEATURE

   =visual>
     kind rapm-cell
     =INDEX =VAL
     =FEATURE =X

   =imaginal>
     focus =VAL
     direction =DIR
     verified nil  

   ?imaginal>
     state free

   ?imaginal-action>
     state free  
==>
   +imaginal-action>
     action verify-current-value  

   =imaginal>
     =VAL =X  
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
     zero nil
     one nil
     two nil
     
  +visual-location>
    kind rapm-cell
    =SPAN zero  ; Beginning of the line 
    =DIR one    ; Next line (row or col) 
)

(p verify*successful
   "Notes when an entire line satisfies a rule"
   =goal>
     step verify
     direction =DIR
     span =SPAN
      
   =retrieval>
     rule =SOMETHING
     feature =FEATURE
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
      feature =FEATURE
      focus nil  ;; Remove the 'focus' slot. Makes for cleaner chunks
)


(p verify*not-successful
   "When a cell is not verified by the rule, forget and go to start"
   =goal>
     step verify
     direction =DIR
     span =SPAN
         
   =imaginal>
     nature sketchpad
     verified no

   =retrieval>
   - rule nil
   
   ?imaginal>
     state free  
==>
   =goal>
     step start

   ;; Moves attention to the first cell
   +visual-location>
     kind rapm-cell
     row zero
     column zero
   -retrieval>
)


(p verify*memorize-solution
   "Memorizes the solution and resets the temporal counter"
   =goal>
     step verify

   =imaginal>
     nature solution

   ?visual>
     state free   
==>
   =goal>
     step start

   ;; Moves attention to the first cell
   +visual-location>
     kind rapm-cell
     row zero
     column zero
   
   +temporal>
     isa time
     ticks 0
   !eval! (trigger-reward nil)
)


;;; ------------------------------------------------------------------
;;; GENERATE MISSING CELL 
;;; ------------------------------------------------------------------
;;; This is the part where the model generates a hypothetical
;;; missing cell, based on the partial solutions identified so far.
;;;
;;;              Create a missing cell chunk
;;;                          |
;;;          Retrieve a solution for the problem 
;;;                          |
;;;                New solution available?
;;;                  /                 \
;;;               (No)                (Yes)
;;;               /                       \
;;;          *RESPOND*                Collect first two
;;;                                features in third row/col
;;;                                           |
;;;                                Generate missing feature
;;;
;;; ------------------------------------------------------------------

(p generate*init
   "Prepare the imaginal buffer to generate a predicted missing cell"   
   =goal>
     step generate
     problem =PID
     
   ?imaginal>
     state free
     buffer empty
     
 ==>
   +imaginal>
     isa missing-cell
     nature missing-cell
     problem =PID
     completed nil
  !eval! (reset-declarative-finsts)   

)

(p generate*retrieve-solution
   "Prepare the imaginal buffer to generate a predicted missing cell"   
   =goal>
     step generate
     problem =PID
     
   =imaginal>
     nature missing-cell
     completed nil
     
   ?retrieval>
     buffer empty
     state free
==>
   =imaginal>
      
   +retrieval>
     nature solution
     problem =PID
     :recently-retrieved nil

   +temporal>
     isa time
     ticks 0
)

;;; In order to generate a solution, we need to be able to predict
;;; a feature based on the value of the same feature across rows or
;;; columns. To do this, we need again to collect features. 

(p generate*collect-features-by-row
   "Begin the collection process during the generation of a solution"
   =goal>
     step generate
     routine nil
     
   =retrieval>
     nature solution
     direction row
     feature =FEATURE

   =imaginal>
     nature missing-cell

 ==>
   =goal>
     routine collect
   
   =imaginal>
     feature =FEATURE
     direction row
     value two
     
   =retrieval>

   +visual-location>
     kind rapm-cell
     row two
     column zero
)     

(p generate*collect-features-by-column
   "Begins the collection process during the generation of a solution" 
   =goal>
     step generate
     routine nil
     
   =retrieval>
     nature solution
     direction column
     feature =FEATURE

   =imaginal>
     nature missing-cell

==>
   =goal>
     routine collect
     
  =imaginal>
     feature =FEATURE
     direction column
     value two
     
  =retrieval>

  +visual-location>
     kind rapm-cell
     row zero
     column two
)     

(p generate*predict-feature-value
   "Predicts the value of the missing cell based a rule. Uses Lisp code"
   =goal>
     step generate
     routine collect
   
   =retrieval>
     nature solution
     feature =FEATURE

   ?imaginal>
     state free
   
   ?visual-location>
     state error
 ==>

   =goal>
     routine nil
   
   +imaginal-action>
     action predict-feature-value  
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

   ;; Cleans up the retrieval buffer  
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

   +visual-location>
     kind rapm-problem  
)



#|
(p propose*rule-progression
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
     routine nil
)
|#

;;; ==================================================================
;;; CHOICE
;;; ==================================================================
;;; The choice procedure is essentially a hack. The model simply
;;; retrieves the generated missing cell. Then, it scans the options.
;;; Then it simply retrieves the option that best matches.
;;; Crude, but should be effective.
;;;
;;;                         Initiate response
;;;                                |
;;;                        Hold on to solution
;;;                                |
;;;                       Scan next option cell <-----+
;;;                                |                  |
;;;                           Option found? -- (Yes) -+
;;;                                |
;;;                              (No)
;;;                                |
;;;                      Retrieve best option
;;;                      /      /   \      \
;;;                  Press  Press   Press   Press
;;;                  Index  Middle   Ring   Pinkie
;;; ==================================================================

(p choice*initiate-response
   "When the options show up, prepare to respond"
   =goal>
     step respond

   =visual>
     kind rapm-choice
     id =PID
     
   ?retrieval>
     buffer empty
     state free
     
   ?manual>
     preparation free
     processor free
     execution free
 ==>
   =goal>
     step choice

   +retrieval>
     nature missing-cell
     problem =PID
   =visual>
)

(p choice*hold-on-to-solution
   "When the solution has been found, put it in the imaginal buffer "
   =goal>
     step choice

   =visual>
     kind rapm-choice
     id =PID
     
   =retrieval>
     nature missing-cell
   
   ?imaginal>
     state free
==>
   @imaginal> =retrieval

   +visual-location>
     kind rapm-cell
;     phase choice
     screen-x lowest

)


(p choice*scan-options
   "When we have a solution, scan the available options"
   =goal>
     step choice

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

(p choice*retrieve-best-option
   "When we have scanned all the options, we retrieve the most similar"
   =goal>
     step choice
 
   =imaginal>
     nature missing-cell
     problem =PID
   
   ?visual-location>
     state error
   
   ?retrieval>
     state free  
==>
   =imaginal>

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
     step choice

   =imaginal>
     nature missing-cell
   
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
     step choice

   =imaginal>
     nature missing-cell

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
     step choice

   =imaginal>
     nature missing-cell

     
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
     step choice
 
   =imaginal>
     nature missing-cell
     
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
     step choice

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
     problem =PID
     
   +visual-location>
     kind rapm-cell
;     phase choice
     screen-x lowest

)


;;; ------------------------------------------------------------------
;;; DONE!
;;; ------------------------------------------------------------------
;;; End of the RAPM procedure.
;;;
;;;                             Done
;;;                               |
;;;                             (END)
;;;------------------------------------------------------------------

(p done
   "Neatly stops ACT-R when the screen says 'done"
   =visual>
     kind rapm-pause
     value done
==>
   !stop!
)

 
)  ; End of the Model


(spp feature*restart :reward -1)
(spp check*solution-found-and-time-elapsed :reward -1)
(spp check*solution-found-and-time-not-elapsed :reward -1)
(spp check*solution-not-found-row :reward 1)
(spp check*solution-not-found-column :reward 1)
(spp verify*successful :reward 1)
;(spp verify*not-successful :reward -1)
;(spp feature*restart :reward -1)

(spp feature*restart :u -1000 :fixed-utility t)

;(spp-fct `((feature*pick-shape :u ,*bias*)))
;(spp-fct `((feature*pick-number :u ,*bias*)))
;(spp-fct `((feature*pick-texture :u ,*bias*)))
;(spp-fct `((feature*pick-background :u ,*bias*)))


;;; RAPM-RELOAD
;;;
;;; Quick reload function that also installs and sets the device properly. 
;;;
(defun rapm-reload (&optional (visicon t))
  "Reloads the model and sets up the experiment"
  (reload)
  (install-device (make-instance 'rapm-task))
  (init (current-device))
  (proc-display)
  (when visicon
    (print-visicon)))