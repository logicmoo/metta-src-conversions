
  (= 
    (learn $Concept) 
    ( (findbag $X 
        (example $X) $L) (learn $L $Concept)))
; 


  (= 
    (learn 
      (Cons  $FirstExample $Examples) $ConceptDesc) 
    ( (initialize $FirstExample $InitialHypothesis) (process-examples $InitialHypothesis $Examples $ConceptDesc)))
; 

	

  (= 
    (initialize 
      (+ (object $Parts $Rels)) 
      (concept $Parts Nil $Rels Nil)) 
    (namevars $Parts 
      (:: part1 part2 part3 part4 part5 part6)))
; 

	

  (= 
    (namevars $List $NameList) 
    (append $List $_ $NameList))
; 



  (= 
    (process_examples  $ConceptDesc () $ConceptDesc) True)
; 

  (= 
    (process-examples $CurDesc 
      (Cons  $Example $Examples) $FinDesc) 
    ( (object-type $Example $Object $Type) 
      (match $Object $CurDesc $Difference) 
      (update $Type $Difference $CurDesc $NewDesc) 
      (process-examples $NewDesc $Examples $FinDesc)))
; 

	

  (= 
    (object_type  
      (+  $Object) $Object positive) True)
; 

  (= 
    (object_type  
      (-  $Object) $Object negative) True)
; 



  (= 
    (match 
      (object $OParts $ORels) 
      (concept $CParts $Musts $Rels $MustNots) 
      (+ $Missing $Extras)) 
    ( (list-diff $ORels $Musts 
        (+ Nil $RestRels)) 
      (short-lists (+ $Missing $Extras)) 
      (list-diff $OParts $CParts 
        (+ Nil Nil)) 
      (list-diff $RestRels $Rels 
        (+ $Missing $Extras)) 
      (list-diff $Extras $MustNots 
        (+ $MustNots $_))))
; 

	

  (= 
    (list_diff  $List () 
      (+  () $List)) True)
; 

  (= 
    (list-diff $List1 
      (Cons  $X $List2) 
      (+ $Miss $Extras)) 
    ( (delete $List1 $List11 $X $Miss11 $Miss) (list-diff $List11 $List2 (+ $Miss11 $Extras))))
; 

	

  (= 
    (delete  () () $X $Dels 
      (Cons  $X $Dels)) True)
; 
  
  (= 
    (delete 
      (Cons  $Y $L) $L $X $Dels $Dels) 
    ( (== $X $Y) (set-det)))
; 

  (= 
    (delete 
      (Cons  $Y $L) $L $X $Dels $Dels) 
    (= $X $Y))
; 

  (= 
    (delete 
      (Cons  $Y $L) 
      (Cons  $Y $L1) $X $Dels $Dels1) 
    (delete $L $L1 $X $Dels $Dels1))
; 

	

  (= 
    (short-lists (+ $L1 $L2)) 
    ( (append $L $_ 
        (:: $_ $_ $_)) (append $L1 $L2 $L)))
; 



  (= 
    (update  negative 
      (+  $_ 
        ($ExtraRelation)) 
      (concept  $Parts $Musts $Rels $MustNots) 
      (concept  $Parts $Musts $Rels 
        (Cons  $ExtraRelation $MustNots))) True)
; 

  (= 
    (update negative 
      (+ $Missing $_) 
      (concept $Parts $Musts $Rels $MustNots) 
      (concept $Parts $NewMusts $NewRels $MustNots)) 
    ( (= $Missing 
        (Cons  $_ $_)) 
      (append $Missing $Musts $NewMusts) 
      (list-diff $Rels $Missing 
        (+ $_ $NewRels))))
; 

  (= 
    (update negative 
      (+ 
        (:: $MissR) 
        (:: $ExtraR)) $CurDesc $NewDesc) 
    ( (update negative 
        (+ Nil 
          (:: $ExtraR)) $CurDesc $InterDesc) (update negative (+ (:: $MissR) Nil) $InterDesc $FinDesc)))
; 

  (= 
    (update positive 
      (+ 
        (:: (isa $Object $Class1)) 
        (:: (isa $Object $Class2))) 
      (concept $Parts $Musts $Rels $MustNots) 
      (concept $Parts $Musts $NewRels $MustNots)) 
    ( (climb $Class1 $Class) 
      (climb $Class2 $Class) 
      (set-det) 
      (replace 
        (isa $Object $Class1) $Rels 
        (isa $Object $Class) $NewRels)))
; 

	

  (= 
    (replace $Item $List $NewItem 
      (Cons  $NewItem $List1)) 
    (delete $List $List1 $Item $_ $_))
; 

	

  (= 
    (climb  $Class $Class) True)
; 

  (= 
    (climb $Class $SuperClass) 
    ( (get-symbols  &self 
        (= 
          (ako  $Class1 $Class) true)) (climb $Class1 $SuperClass)))
; 



  !(dynamic (/ found 1))
; 



  (= 
    (findbag $X $G $_) 
    ( (add-symbol  &self 
        (found  mark)) 
      (call $G) 
      (add-symbol  &self 
        (found  $X)) 
      (fail)))
; 
                            
  (= 
    (findbag $_ $_ $L) 
    (collect-found Nil $L))
; 
                                     
                                                              

  (= 
    (collect-found $L $L1) 
    ( (getnext $X) (collect-found (Cons  $X $L) $L1)))
; 
                      
  (= 
    (collect_found  $L $L) True)
; 
                                        
                                                              
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  arch2/arch2.pl 268 30 14766))



  (= 
    (help) 
    ( (write 'Load data set with command: [Filename].') 
      (nl) 
      (write 'Start arch2   with command: learn(X).') 
      (nl)))
; 



  !(help *)
; 


