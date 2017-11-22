(clear-all)

(define-model two-extra-modules

(sgp :ga 0
     :imaginal-activation 0
     :d2-filter-activation -10
     :d1-filter-activation 10 
     :esc t
     :er t
     :mas 10.0
     :blc 0
     :lf 0.4
     :rt -2
     :act nil
     :ans nil
)
    

(chunk-type test field1 field2)
(chunk-type content value)
(add-dm (r isa chunk)
	(b isa chunk)
	(test1 isa test field1 b field2 r)
	(test2 isa test field1 r field2 r)
	(test3 isa test field1 r field2 b)
	(test4 isa test field1 b field2 b))

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
   +d1-filter>
     isa content
     value b

   +d2-filter>
     isa content
     value r
     )



(p retrieve
   =goal>
   - value nil
   =imaginal>
   - value nil
   ==>
   +retrieval>)

)
