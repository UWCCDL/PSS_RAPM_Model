(require-compiled "GOAL-STYLE-MODULE" "ACT-R-support:goal-style-module")

(define-module dopamine  
  (d1-filter d2-filter)  
  nil                    
  :version "1.0"
  :documentation "Declarative memory filters based on D1/D2 parameters"
  :query goal-style-query
  :request goal-style-request
  :buffer-mod goal-style-mod-request)
