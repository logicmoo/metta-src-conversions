;
; 

;
; 

;
; 

;
; 


;
; 


;
; 

;
; 

;
; 


;
; 

;
; 

;
; 


  (= 
    (load-eval $Name) 
    ( (find-eval-file $Name $File) (file-make-test-eval $File)))
; 



  (= 
    (find-eval-file $Name $File) 
    (find-suffixed-library-file $Name eval $File))
; 


;
; 

;
; 


  (= 
    (file-make-test-eval $File) 
    ( (abolish (/ piece-value 2)) 
      (abolish (/ piece-square-value 2)) 
      (compile $File)))
; 




;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (successor-pos $Move $State $State2 $Tables) 
    (legal $Move $State $State2))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


  (= 
    (terminal-pos-value $Pos $Val $Tables) 
    (terminal-game-outcome $Pos $Val))
; 



;
; 

;
; 


  (= 
    (terminal-game-outcome $Pos $Val) 
    ( (game-outcome $Outcome $Pos) 
      (value-of-outcome $Outcome $Val) 
      (set-det)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


  (= 
    (staticval $Pos $Val $Tables) 
    (evaluation $Val $Pos $Tables))
; 


;
; 

;
; 

;
; 

  (= 
    (staticval $Pos $Val) 
    (evaluation $Val $Pos))
; 




;
; 

;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (evaluation $Value $Position) 
    ( (find-advice-tables $Tables) (evaluation $Value $Position $Tables)))
; 


  (= 
    (evaluation $Value $Position $Tables) 
    ( (get-advices $Advice $Position $Tables) (mediate-advices $Advice $Value $Tables)))
; 



;
; 

;
; 

;
; 

;
; 


  (= 
    (local-evaluation $Piece $Sq $Value $Position $Tables) 
    ( (get-local-advices $Piece $Sq $Advice $Position $Tables) (mediate-advices $Advice $Value $Tables)))
; 


	

  (= 
    (get-advices $Advices $Position) 
    ( (find-advice-tables $Tables) (get-advices $Advices $Position $Tables)))
; 

	
  (= 
    (get-advices $Advices $Position $Tables) 
    (findall $Advice 
      (, 
        (add-dynamic-tables $Position $Tables) 
        (value $Advice $Position $Tables)) $Advices))
; 




  (= 
    (get-local-advices $Piece $Sq $Advices $Position $Tables) 
    (findall $Advice 
      (value $Piece $Sq $Advice $Position $Tables) $Advices))
; 



  (= 
    (show-advices-name $PosName) 
    ( (checkpoint $PosName $Pos) (show-advices $Pos)))
; 




  (= 
    (show-advices $Pos) 
    ( (get-advices $As $Pos) (ppl $As)))
; 


  (= 
    (show-advices $Pos $Tables) 
    ( (get-advices $As $Pos $Tables) (ppl $As)))
; 




  (= 
    (show-local-advices $Piece $Sq $Pos) 
    ( (find-advice-tables $Tables) (show-local-advices $Piece $Sq $Pos $Tables)))
; 



  (= 
    (show-local-advices $Piece $Sq $Pos $Tables) 
    ( (on $Piece $Sq $Pos) 
      (get-local-advices $Piece $Sq $Advices $Pos $Tables) 
      (ppl $Advices)))
; 






  (= 
    (mediate-advices $Advices $Value $Tables) 
    ( (weigh-advices $Advices $Vals $Tables) (sumlist $Vals $Value)))
; 



  (= 
    (weigh_advices  () () $_) True)
; 

  (= 
    (weigh-advices 
      (Cons  $A $As) 
      (Cons  $V $Vals) $Tables) 
    ( (weigh-advice $A $V $Tables) (weigh-advices $As $Vals $Tables)))
; 

	

  (= 
    (weigh-advice 
      (advice $Advisor $Comment $V) $Val $Tables) 
    ( (advisor-weight $Advisor $Weight $Tables) (is $Val (* $V $Weight))))
; 
 

;
; 


  (= 
    (advisor-weight $Advisor $Weight $Tables) 
    (parameter $Advisor $Weight))
; 




  (= 
    (advisor $A $B) 
    (dynamic-advisor $A $B))
; 

  (= 
    (advisor $A $B) 
    (static-advisor $A $B))
; 




  (= 
    (dynamic_advisor  gen_material 0) True)
; 

  (= 
    (dynamic_advisor  material 0) True)
; 

  (= 
    (dynamic_advisor  square 0) True)
; 

  (= 
    (dynamic_advisor  lthreat 0) True)
; 

  (= 
    (dynamic_advisor  potent 0) True)
; 

  (= 
    (dynamic_advisor  dynamic_mobility 0) True)
; 


  (= 
    (dynamic_advisor  gmovmob 0) True)
; 

  (= 
    (dynamic_advisor  gcapmob 0) True)
; 

  (= 
    (dynamic_advisor  gthreat 0) True)
; 

  (= 
    (dynamic_advisor  pthreat 0) True)
; 


  (= 
    (dynamic_advisor  static_mobility 0) True)
; 

  (= 
    (dynamic_advisor  eventual_mobility 0) True)
; 

  (= 
    (dynamic_advisor  arrive_distance 0) True)
; 

  (= 
    (dynamic_advisor  promote_distance 0) True)
; 

  (= 
    (dynamic_advisor  possess 0) True)
; 

  (= 
    (dynamic_advisor  initprom 0) True)
; 

;
; 

  (= 
    (dynamic_advisor  eradicate 0) True)
; 

  (= 
    (dynamic_advisor  vital 0) True)
; 
 


  (= 
    (static_advisor  random 0) True)
; 

  (= 
    (static_advisor  static 0) True)
; 

  (= 
    (static_advisor  victims 0) True)
; 

  (= 
    (static_advisor  max_static_mob 0) True)
; 

  (= 
    (static_advisor  max_eventual_mob 0) True)
; 

  (= 
    (static_advisor  avg_static_mob 0) True)
; 

  (= 
    (static_advisor  avg_eventual_mob 0) True)
; 

  (= 
    (static_advisor  eradicate 0) True)
; 

  (= 
    (static_advisor  stalemate 0) True)
; 

  (= 
    (static_advisor  arrive 0) True)
; 

  (= 
    (static_advisor  giveaway 0) True)
; 

  (= 
    (static_advisor  immunity 0) True)
; 

;
; 




  (= 
    (initialize-advisors) 
    ( (whenever 
        (advisor $Name $Default) 
        (add-parameter $Name $Default)) (add-symbol  &self initialized_advisors)))
; 



  (= 
    (active-parameter $P) 
    ( (advisor $P $_) 
      (parameter $P $Val) 
      (\== $Val 0)))
; 



  !(det-if-then-else 
    (current-predicate initialized-advisors $_) True initialize-advisors)
; 



;
; 

;
; 

;
; 



  (= 
    (movtable) 
    (det-if-then 
      (, 
        (needs-movtable $Param) 
        (active-parameter $Param)) True))
; 



  (= 
    (needs_movtable  dynamic_mobility) True)
; 

  (= 
    (needs_movtable  gmovmob) True)
; 




  (= 
    (captable) 
    (det-if-then 
      (, 
        (needs-captable $Param) 
        (active-parameter $Param)) True))
; 


;
; 


  (= 
    (needs_captable  gcapmob) True)
; 

  (= 
    (needs_captable  vital) True)
; 

  (= 
    (needs_captable  gthreat) True)
; 

  (= 
    (needs_captable  pthreat) True)
; 

  (= 
    (needs_captable  lthreat) True)
; 

  (= 
    (needs_captable  gcapmob) True)
; 

  (= 
    (needs_captable  eradicate) True)
; 

  (= 
    (needs_captable  potent) True)
; 



;
; 

;
; 

;
; 



;
; 


  (= 
    (value 
      (advice gmovmob $Player $Value) $Position $Tables) 
    ( (active-parameter gmovmob) 
      (gmobility $Player $Val $Position $Tables) 
      (negate-for-player $Player $Val $Value)))
; 

	

;
; 

  (= 
    (value 
      (advice gcapmob $Player $Value) $Position $Tables) 
    ( (active-parameter gcapmob) 
      (gcapmobility $Player $Val1 $Position $Tables) 
      (favor-control $Player $Val1 $Val $Position) 
      (negate-for-player $Player $Val $Value)))
; 


;
; 

  (= 
    (value 
      (advice gthreat $Player $Value) $Position $Tables) 
    ( (active-parameter gthreat) (gthreat $Player $Value $Position $Tables)))
; 


;
; 

  (= 
    (value 
      (advice pthreat $Player $Value) $Position $Tables) 
    ( (active-parameter pthreat) (pthreat $Player $Value $Position $Tables)))
; 



;
; 

  (= 
    (value $Value $Position $Tables) 
    ( (active-parameter possess) 
      (in-hand $Piece $Player $Position) 
      (not (still-assigning $Position)) 
      (current-predicate possess-value 
        (possess-value $_ $_ $_ $_ $_)) 
      (possess-value $Piece $Player $Value $Position $Tables)))
; 


;
; 

;
; 

  (= 
    (value $Value $Position $Tables) 
    ( (active-parameter initprom) 
      (current-predicate initprom-value 
        (initprom-value $Value $Position $Tables)) 
      (initprom-value $Value $Position $Tables)))
; 


;
; 

;
; 

;
; 

  (= 
    (value 
      (advice vital 
        (, $Goal 
          (@ $Piece $Square)) $Value) $Position $Tables) 
    ( (active-parameter vital) (threatened-vital-piece-value $Piece $Square $Player $Goal $Value $Position $Tables)))
; 




;
; 

;
; 

;
; 

  (= 
    (value 
      (advice random 
        (range $Min $Max) $Value) $Position $Tables) 
    ( (active-parameter random) (random-eval $Min $Max $Value)))
; 



;
; 

;
; 

  (= 
    (value $Value $Position $Tables) 
    ( (on $Piece $Player $Square $Position) (value $Piece $Square $Value $Position $Tables)))
; 





;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

  (= 
    (value $Piece $Sq 
      (advice gen-material $Type $Val) $Position $Tables) 
    ( (active-parameter gen-material) (gen-material-value $Piece $Player $Type $Val)))
; 




;
; 

;
; 

;
; 

  (= 
    (value $Piece $Square 
      (advice static $Piece $Value) $Pos $Tables) 
    ( (active-parameter static) (piece-player-static $Piece $Player $Value $Tables)))
; 



;
; 

  (= 
    (value $Piece $Square 
      (advice dynamic-mobility 
        (@ $Piece $Square) $Value) $Position $Tables) 
    ( (active-parameter dynamic-mobility) 
      (active-advisor dynamic-mobility $Tables) 
      (dynamic-piece-mobility $Piece $Square $Value $Position $Tables)))
; 


;
; 

  (= 
    (value $Piece $Square 
      (advice static-mobility 
        (@ $Piece $Square) $Value) $Position $Tables) 
    ( (active-parameter static-mobility) (static-piece-mobility $Piece $Square $Value $Position $Tables)))
; 


;
; 

  (= 
    (value $Piece $Square 
      (advice eventual-mobility 
        (@ $Piece $Square) $Value) $Position $Tables) 
    ( (active-parameter eventual-mobility) (eventual-piece-mobility $Piece $Square $Value $Position $Tables)))
; 


;
; 

;
; 

  (= 
    (value $Piece $Square 
      (advice lthreat 
        (captures 
          (@ $Piece $Square) 
          (@ $PieceV $SqV)) $Value) $Position $Tables) 
    ( (active-parameter lthreat) 
      (active-advisor threat $Tables) 
      (local-threat-value $Piece $Square $PieceV $SqV $Value $Position $Tables)))
; 



;
; 

  (= 
    (value $Piece $Square 
      (advice potent 
        (captures 
          (@ $Piece $Square) 
          (@ $PieceV $SqV)) $Value) $Position $Tables) 
    ( (active-parameter potent) 
      (active-advisor threat $Tables) 
      (potent-threat-mover $Piece $Square $PieceV $SqV $Value $Position $Tables)))
; 





;
; 

  (= 
    (value $Piece $Square 
      (advice arrive-distance 
        (, $Goal 
          (@ $Piece $Square) $SqT) $Value) $Position $Tables) 
    ( (active-parameter arrive-distance) (arrive-value $Piece $Square $SqT $Goal $Value $Position $Tables)))
; 


;
; 

  (= 
    (value $Piece $Square 
      (advice promote-distance 
        (, 
          (@ $Piece $Square) $SqT) $Value) $Position $Tables) 
    ( (active-parameter promote-distance) 
      (active-advisor prom $Tables) 
      (prom-value $Piece $Square $SqT $Value $Position $Tables)))
; 



;
; 

  (= 
    (value $Piece $Square 
      (advice dominate 
        (, $Goal 
          (@ $Piece $Square) 
          (@ $PieceV $SqV)) $Value) $Position $Tables) 
    ( (active-parameter dominate) (dominate-value $Piece $Square $PieceV $SqV $Goal $Value $Position $Tables)))
; 



;
; 

;
; 

  (= 
    (value $Piece $Square 
      (advice dominate 
        (, $Goal 
          (@ $Piece $Square)) $Value) $Position $Tables) 
    ( (active-parameter dominate) (eradicate-safety $Player $Piece $Square $Goal $Value $Position $Tables)))
; 



;
; 

  (= 
    (value $Piece $Square 
      (advice material $Piece $Value) $Position $Tables) 
    ( (active-parameter material) 
      (current-predicate $_ 
        (piece-value $_ $_)) 
      (piece-value $Piece $Value)))
; 



;
; 

  (= 
    (value $Piece $Square 
      (advice square 
        (@ $Piece $Square) $Value) $Position $Tables) 
    ( (active-parameter square) (piece-square-value $Piece $Square $Value $Tables)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (random-eval $Min $Max $Value) 
    ( (parameter random-min $Min) 
      (parameter random-max $Max) 
      (random $Min $Max $Value)))
; 
 


;
; 


  (= 
    (gen-material-value $Piece $Player $Type $Val) 
    ( (owns $Piece $Player) 
      (piece-name $Piece $Type) 
      (negate-for-player $Player 1 $Val)))
; 

	

;
; 

;
; 

;
; 


  (= 
    (dynamic-piece-mobility $Piece $Square $Value $Position $Tables) 
    ( (owns $Piece $Player) 
      (dynamic-piece-mob $Piece $Player $Square $Val $Position $Tables) 
      (> $Val 0) 
      (negate-for-player $Player $Val $Value)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


  (= 
    (dynamic-piece-mob $Piece $Player $Square $Count $Position $Tables) 
    ( (moving-table $Tables $Moves) 
      (unique-moves $Player $Piece $Square $SqT $Moves $Targets) 
      (length $Targets $Count) 
      (tracing-path-format moves "Dynamic: <~p>: ~p -> ~p~n" 
        (:: $Piece $Square $Count))))
; 





;
; 


  (= 
    (static-piece-mobility $Piece $Square $Value $Position $Tables) 
    ( (owns $Piece $Player) 
      (static-piece-mob $Piece $Player $Square $Val $Position $Tables) 
      (negate-for-player $Player $Val $Value)))
; 



  (= 
    (static-piece-mob $Piece $Player $Square $Value $Position $Tables) 
    ( (square-piece-mobility $Square $Piece $Value $Tables) (tracing-path-format static "In 1: <~p>: ~p -> ~p~n" (:: $Piece $Square $Value))))
; 



;
; 


  (= 
    (eventual-piece-mobility $Piece $Square $Value $Position $Tables) 
    ( (owns $Piece $Player) 
      (eventual-piece-mob $Piece $Player $Square $Val $Position $Tables) 
      (negate-for-player $Player $Val $Value)))
; 




  (= 
    (eventual-piece-mob $Piece $Player $Square $Value $Position $Tables) 
    ( (square-piece-reachability $Square $Piece $Value $Tables) (tracing-path-format eventual "In 4: <~p>: ~p -> ~p~n" (:: $Piece $Square $Value))))
; 



;
; 

;
; 

;
; 



  (= 
    (initiative-offset $Control $Player $Offset) 
    (det-if-then-else 
      (= $Control $Player) 
      (= $Offset 0.9) 
      (= $Offset 0.7)))
; 


	

  (= 
    (favor-control $Player $Val1 $Value $Position) 
    ( (control $Control $Position) 
      (initiative-offset $Control $Player $Offset) 
      (is $Value 
        (* $Val1 $Offset))))
; 
 
	
;
; 

;
; 


  (= 
    (favorable-to-owner player $Val) 
    (> $Val 0))
; 

  (= 
    (favorable-to-owner opponent $Val) 
    (< $Val 0))
; 



;
; 

;
; 


  (= 
    (control-cost $Player $Cost $Position) 
    ( (control $Control $Position) (det-if-then-else (= $Control $Player) (= $Cost 0) (= $Cost 1))))
; 


	


  (= 
    (negate-if-same $Player1 $Player2 $Val1 $Val) 
    (det-if-then-else 
      (\== $Player1 $Player2) 
      (= $Val1 $Val) 
      (is $Val 
        (- $Val1))))
; 




  (= 
    (negate-if-different $Player1 $Player2 $Val1 $Val) 
    (det-if-then-else 
      (= $Player1 $Player2) 
      (= $Val1 $Val) 
      (is $Val 
        (- $Val1))))
; 



  (= 
    (negate_for_player  player $Val $Val) True)
; 

  (= 
    (negate-for-player opponent $Val $Val1) 
    (is $Val1 
      (- $Val)))
; 



  (= 
    (negate-advice-for-player $Player 
      (advice $A $C $V) 
      (advice $A $C $V1)) 
    (negate-for-player $Player $V $V1))
; 



;
; 


  (= 
    (max-for-player player $List $Best) 
    (max $List $Best))
; 

  (= 
    (max-for-player opponent $List $Best) 
    (min $List $Best))
; 


;
; 


  (= 
    (min-for-player player $List $Best) 
    (min $List $Best))
; 

  (= 
    (min-for-player opponent $List $Best) 
    (max $List $Best))
; 


	
;
; 

;
; 

;
; 



  (= 
    (evalfile-top $Game) 
    (load-eval $Game))
; 



  (= 
    (evalfile-com $_ $_ $_ $Game) 
    (load-eval $Game))
; 



  (= 
    (evaluate-com $Move $SIn $_) 
    ( (timing (evaluation $Value $SIn)) (format "Position's value (positive favors white):  ~p~n" (:: $Value))))
; 




  (= 
    (advice-com $Move $SIn $_) 
    ( (timing (get-advices $As $SIn)) (ppl $As)))
; 



  (= 
    (advice-com $_ $SIn $SOut $Row $Col) 
    ( (with-alpha-squares (gsquare $Square (:: ( $Row , $Col )) Nil)) (timing (show-local-advices $Piece $Square $SIn))))
; 

	  


  (= 
    (advisor-weight $Adv $Weight) 
    ( (advisor $Adv $_) (advisor-weight $Adv $Weight $_)))
; 



  (= 
    (show-advisors) 
    ( (format "Advisors: ~n" Nil) (whenever (advisor-weight $Adv $Weight) (format "<~p>: ~p~n" (:: $Adv $Weight)))))
; 



  (= 
    (alladvisors-com $_ $_ $_) 
    (show-advisors))
; 


  (= 
    (alladvisors-top) 
    (show-advisors))
; 




  (= 
    (seta-com $_ $_ $_) 
    (show-active-advisors))
; 



  (= 
    (seta-top) 
    (show-active-advisors))
; 

	

  (= 
    (active-com $_ $_ $_) 
    (show-active-advisors))
; 



  (= 
    (active-top) 
    (show-active-advisors))
; 




  (= 
    (show-active-advisors) 
    ( (format "Active Advisors: ~n" Nil) (whenever (, (advisor-weight $Adv $Weight) (\== $Weight 0)) (format "<~p>: ~p~n" (:: $Adv $Weight)))))
; 


