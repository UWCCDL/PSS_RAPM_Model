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
