(clear-all)

(define-model negative-spreading

(sgp :ga -10
     :imaginal-activation 10
     :esc t
     :er t
;     :ans 0.5
     :mas 10.0
;     :bll 0.5
     :blc 1
     :lf 0.4
     :rt -2
     :act nil
)
    

(chunk-type test field1 field2)
(chunk-type content value)
(add-dm (r isa chunk)
	(b isa chunk)
	(test1 isa test field1 b field2 r)
	(test2 isa test field1 r field2 r)
	(test3 isa test field1 r field2 b)
	(test4 isa test field1 b field2 r))

(p setup
   ?goal>
     buffer empty
     state free
   ?imaginal>
     buffer empty
     state free
   ==>
   +goal>
     isa content
     value r
   +imaginal>
     isa content
     value b
     )



(p retrieve
   =goal>
   - value nil
   =imaginal>
   - value nil
   ==>
   +retrieval>)
)
