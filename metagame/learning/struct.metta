;                                           (convert_to_metta_file  struct $_119626 metagame/learning/struct.pl metagame/learning/struct.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; building all tables.


  (= (build-tables)  
    (new-empty-state $State)
    (build-tables $State))
; ; for testing

  (= (build-tables $State)  
    (runtime (build-tables $State $Tables)))

  (= (build-tables $State $T)  
    (anal-table $T)
    (promsq-matrix $T $PromSqMatrix)
    (promotion-matrix $T $PromMatrix)
    (prom-distance-matrix $T $PromDistMatrix)
    (transition-matrix $T $TransMatrix)
    (mobility-matrix $T $MobMatrix)
    (eventual-matrix $T $EventualMatrix)
    (distance-matrix $T $DistMatrix)
    (distance-table $T $DistTable)
    (active-advisor-table $T $Advisors)
    (static-matrix $T $StatMatrix)
    (compile-basic-tables)
    (tracing-anal-format tables "Building <active advisor> tables ...~n" Nil)
    (tracing-anal-timing tables 
      (build-advisor-table $Advisors))
    (tracing-anal-format tables "Building <promotion transition> tables ...~n" Nil)
    (tracing-anal-timing tables 
      (build-promotion-matrix $PromMatrix))
    (tracing-anal-format tables "Building <promotion distance> tables ...~n" Nil)
    (tracing-anal-timing tables 
      (build-prom-distance-matrix $PromMatrix $PromDistMatrix))
    (tracing-anal-format tables "Building <transition> tables ...~n" Nil)
    (tracing-anal-timing tables 
      (build-transition-matrix $TransMatrix $State))
    (tracing-anal-format tables "Building <mobility> tables ...~n" Nil)
    (tracing-anal-timing tables 
      (build-mobility-matrix $TransMatrix $MobMatrix))
    (tracing-anal-format tables "Building <distance> matrices ...~n" Nil)
    (tracing-anal-timing tables 
      (build-distance-matrix $TransMatrix $DistMatrix))
    (tracing-anal-format tables "Building <distance> tables ...~n" Nil)
    (tracing-anal-timing tables 
      (build-distance-table $DistMatrix $DistTable))
    (tracing-anal-format tables "Building <eventual mobility> tables ...~n" Nil)
    (tracing-anal-timing tables 
      (build-eventual-matrix $DistMatrix $EventualMatrix))
    (tracing-anal-format tables "Building <promotion square distance> tables ...~n" Nil)
    (tracing-anal-timing tables 
      (build-promsq-matrix $DistTable $PromSqMatrix))
    (tracing-anal-format tables "Building <independent piece value> tables ...~n" Nil)
    (tracing-anal-timing tables 
      (build-static-matrix $StatMatrix $T))
    (tracing-anal-format tables "Tables completed!~n" Nil)
    (save-tables $T))





  (= (clear-tables)  
    (remove-all-atoms  &self 
      (advice_tables  $_)))


  (= (save-tables $Tables)  
    ( (clear-tables) (add-is-symbol  &self (advice_tables  $Tables))))


  (= (dump-tables $File)  
    (with-output-file $File write dump-tables))

  (= (dump-tables)  
    (listing (/ advice-tables 1)))


  (= (load-tables)  
    (read (advice-tables $Tables))
    (save-tables $Tables)) 

  (= (load-tables $File)  
    (see $File)
    (load-tables)
    (seen))


  (= (find-advice-tables-if $T)  
    (det-if-then-else 
      (var $T) 
      (find-advice-tables $T) True))



  (= (find-advice-tables $T)  
    (det-if-then-else 
      (current-predicate advice-tables 
        (advice-tables $_)) 
      (advice-tables $T) 
      (= $T none)))


;
; Stripped down version, when we just want the basic structure
;
; without the real analysis.


  (= (build-dummy-tables)  
    (build-dummy-tables $State $Tables))

  (= (build-dummy-tables $State $Tables)  
    (anal-table $T)
    (active-advisor-table $T $Advisors)
    (compile-basic-tables)
    (tracing-anal-format tables "Building <active advisor> tables ...~n" Nil)
    (tracing-anal-timing tables 
      (build-advisor-table $Advisors))
    (save-tables $T))
	


;
; ============================================================================
;
; ANAL_TABLE data structure
;
; ============================================================================

;
; Contains a few extra slots for future development.

  (= (anal-table $N)  
    (functor $N tables 15))


  (= (promsq-matrix $T $M)  
    (arg 1 $T $M))

  (= (promotion-matrix $T $M)  
    (arg 2 $T $M))

  (= (prom-distance-matrix $T $M)  
    (arg 3 $T $M))

  (= (transition-matrix $T $M)  
    (arg 4 $T $M))

  (= (mobility-matrix $T $M)  
    (arg 5 $T $M))

  (= (eventual-matrix $T $M)  
    (arg 6 $T $M))

  (= (distance-matrix $T $M)  
    (arg 7 $T $M))

  (= (distance-table $T $M)  
    (arg 8 $T $M))


  (= (piece-value-table $T $M)  
    (arg 9 $T $M))

  (= (piece-square-table $T $M)  
    (arg 10 $T $M))


  (= (active-advisor-table $T $M)  
    (arg 11 $T $M))


  (= (capturing-table $T $M)  
    (arg 12 $T $M))

  (= (moving-table $T $M)  
    (arg 13 $T $M))

  (= (static-matrix $T $M)  
    (arg 14 $T $M))




  (= (promsq-matrix $M)  
    (advice-tables $T)
    (promsq-matrix $T $M))

  (= (promotion-matrix $M)  
    (advice-tables $T)
    (promotion-matrix $T $M))

  (= (prom-distance-matrix $M)  
    (advice-tables $T)
    (prom-distance-matrix $T $M))

  (= (transition-matrix $M)  
    (advice-tables $T)
    (transition-matrix $T $M))

  (= (mobility-matrix $M)  
    (advice-tables $T)
    (mobility-matrix $T $M))

  (= (eventual-matrix $M)  
    (advice-tables $T)
    (eventual-matrix $T $M))

  (= (distance-matrix $M)  
    (advice-tables $T)
    (distance-matrix $T $M))

  (= (distance-table $M)  
    (advice-tables $T)
    (distance-table $T $M))


  (= (piece-value-table $M)  
    (advice-tables $T)
    (piece-value-table $T $M))

  (= (piece-square-table $M)  
    (advice-tables $T)
    (piece-square-table $T $M))

  (= (active-advisor-table $M)  
    (advice-tables $T)
    (active-advisor-table $T $M)) 

  (= (static-matrix $M)  
    (advice-tables $T)
    (static-matrix $T $M))



  (= (add-portray-anal-table)  
    ( (anal-table $T) (add-is-symbol  &self (:-  (portray  $T) (portray_anal_table  $T)))))


  (= (portray-anal-table $T)  
    (format "<Anal Table>" Nil))

;
; ============================================================================
;
; ACTIVE_ADVISOR_TABLE data structure
;
; ============================================================================


  (= (shutdown-advisor $Advisor $Table)  
    (active-advisor-table $Table $M)
    (advisor-number $Advisor $Number)
    (arg $Number $M off))


  (= (active-advisor $Advisor $Table)  
    (active-advisor-table $Table $M)
    (advisor-number $Advisor $Number)
    (arg $Number $M $Status)
    (\== $Status off))


  (= (advisor_number  threat 1)  True)
  (= (advisor_number  prom 2)  True)
  (= (advisor_number  dynamic_mobility 3)  True)


  (= (number_of_advisors  3)  True)


  (= (build-advisor-table $T)  
    (number-of-advisors $N)
    (functor $T active-advisors $N))


	
;
; ================================================================================
;
; Interface
;
; ================================================================================


  (= (build-top)  
    (build-tables))


  (= (showstatic-top)  
    (print-static-matrix))


