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
    (move-threat $Piece $Player $SqF $SqT $Tables) 
    ( (moving-table $Tables $MTable) (member (move $Piece $Player $SqF $SqT) $MTable)))
; 



  (= 
    (capture-threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect $Tables) 
    ( (capturing-table $Tables $MTable) (member (threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect) $MTable)))
; 



;
; 


  (= 
    (done-move-threat $Piece $Player $SqF $SqT $Pos $PosOut $Tables) 
    ( (move-threat $Piece $Player $SqF $SqT $Tables) (do-move $Piece $SqF $SqT $Pos $PosOut)))
; 



  (= 
    (done-capture-threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect $Pos $PosOut $Tables) 
    ( (capture-threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect $Tables) (do-capture $PieceA $SqA $SqT $PieceV $SqV $Pos $PosOut)))
; 





  (= 
    (do-move $Piece $SqF $SqT $Pos $PosOut) 
    ( (lift-piece $Piece $SqF $Pos $Pos1) (place-piece $Piece $SqT $Pos1 $PosOut)))
; 




  (= 
    (do-capture $Piece $SqF $SqT $PieceV $SqV $Pos $PosOut) 
    ( (lift-piece $Piece $SqF $Pos $Pos1) 
      (det-if-then-else 
        (\== $SqV $SqT) 
        (lift-piece $PieceV $SqV $Pos1 $Pos2) 
        (= $Pos1 $Pos2)) 
      (place-piece $Piece $SqT $Pos2 $PosOut)))
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
    (capture-table $Table $S) 
    ( (findall 
        (threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect) 
        (threatens $PieceA $Player $SqA $SqT $PieceV $SqV $Effect $S) $Table1) (remove-duplicates $Table1 $Table)))
; 

	  

  (= 
    (unique-threats $Threats $Unique) 
    ( (findall 
        (target $Player $PieceV $SqV $Effect) 
        (member 
          (threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect) $Threats) $Squares) (remove-duplicates $Squares $Unique)))
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
    (unique-threats $Player $Threats $Unique) 
    ( (player-role $Player) 
      (findall 
        (target $Player $PieceV $SqV $Effect) 
        (member 
          (threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect) $Threats) $Squares) 
      (remove-duplicates $Squares $Unique)))
; 




  (= 
    (player-threats $Player $Threats $PThreats) 
    ( (player-role $Player) (findall (threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect) (member (threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect) $Threats) $PThreats)))
; 



;
; 

;
; 


  (= 
    (threatens $PieceA $Player $SqA $SqT $PieceV $SqV $Effect $S) 
    ( (on $PieceA $SqA $S) 
      (owns $PieceA $Player) 
      (put-control-if $Player $S $S1) 
      (captures $PieceA $Player $SqA $SqT $Effect $Captured $S1) 
      (not (excluded-to $PieceA $SqT)) 
      (captured-piece $PieceV $SqV $Captured)))
; 


	
;
; 

;
; 

;
; 



  (= 
    (gthreat $Player $Value $Position $Tables) 
    ( (capturing-table $Tables $MTable) 
      (unique-threats $Player $MTable $Unique) 
      (\== $Unique Nil) 
      (eval-threats $Unique $Player $Evaled $Position $Tables) 
      (keysort-for-player $Player $Evaled $Ordered) 
      (threat-outcome $Player $Ordered $Value $Position $Tables)))
; 



  (= 
    (eval-threats $Unique $Player $Evaled $Position $Tables) 
    (findall 
      (- $Val $Threat) 
      (, 
        (member $Threat $Unique) 
        (= $Threat 
          (target $Player $PieceV $SqV $Effect)) 
        (effect-threat-evaluation $Effect $Player $PieceV $SqV $Val $Position $Tables) 
        (tracing-anal-format gthreat "Threatened: <~p> can capture <~p> ~p (~p) [~p]~n" 
          (:: $Player $PieceV $SqV $Effect $Val))) $Evaled))
; 

	    

;
; 

;
; 


  (= 
    (local-threat-evaluation $Victim $SqV $Val $S $Tables) 
    ( (shutdown-advisor threat $Tables) 
      (= $S1 $S) 
      (local-evaluation $Victim $SqV $Val $S1 $Tables)))
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
    (effect-threat-evaluation $Effect $Player $PieceV $SqV $Value $Position $Tables) 
    ( (local-threat-evaluation $PieceV $SqV $LVal $Position $Tables) 
      (is $RemVal 
        (- $LVal)) 
      (effect-val $Effect $Player $PieceV $LVal $EVal $Position $Tables) 
      (is $Value 
        (+ $RemVal $EVal)) 
      (favorable-to-owner $Player $Value)))
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
    (effect_val  remove $Player $PieceV $Val 0 $Position $Tables) True)
; 

  (= 
    (effect-val 
      (possess $Possessor) $Player $PieceV $LVal $EValue $Position $Tables) 
    (estimate-possess-value $Possessor $PieceV $LVal $EValue $Position $Tables))
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
    (threat-outcome $Player $Ordered $Value $Position $Tables) 
    (det-if-then-else 
      (control $Player $Position) 
      (, 
        (= $Ordered 
          (Cons  
            (- $Val1 $Threat1) $Rest)) 
        (favor-control $Player $Val1 $Value $Position)) 
      (, 
        (length $Ordered $Num) 
        (det-if-then-else 
          (> $Num 1) 
          (, 
            (nth 2 $Ordered 
              (- $Val1 $Threat1)) 
            (favor-control $Player $Val1 $Value $Position)) 
          (, 
            (= $Ordered 
              (Cons  
                (- $Val1 $Threat1) $Rest)) 
            (favor-control $Player $Val1 $Val2 $Position) 
            (favor-control $Player $Val2 $Value $Position))))))
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
    (local-threat-value $Piece $Square $PieceV $SqV $Value $Pos $Tables) 
    ( (owns $Piece $Player) 
      (capture-threat $Piece $Player $Square $SqT $PieceV $SqV $Effect $Tables) 
      (local-threat-value $Player $Piece $Square $SqT $PieceV $SqV $Effect $Value $Pos $Tables)))
; 


  (= 
    (local-threat-value $Player $Piece $Square $SqT $Victim $SqV $Effect $Value $S $Tables) 
    ( (effect-threat-evaluation $Effect $Player $Victim $SqV $Val $S $Tables) 
      (favor-control $Player $Val $Value $S) 
      (tracing-anal-format lthreat "<~p>: ~p -> ~p x ~p ~p (~p) [~p]~n" 
        (:: $Piece $Square $SqT $Victim $SqV $Effect $Value))))
; 
   


;
; 

;
; 

;
; 





  (= 
    (put-control-if $P $S $S1) 
    (det-if-then-else 
      (control $P $S) 
      (= $S $S1) 
      (put-control $P $S $S1)))
; 





;
; 

;
; 


  (= 
    (move-table $Table $S) 
    ( (findall 
        (move $PieceA $Player $SqA $SqT) 
        (could-move $PieceA $Player $SqA $SqT $S) $Table1) (remove-duplicates $Table1 $Table)))
; 

	  

  (= 
    (unique-moves $Moves $Unique) 
    ( (findall 
        (target $Player $PieceA $SqA $SqT) 
        (member 
          (move $PieceA $Player $SqA $SqT) $Moves) $Squares) (remove-duplicates $Squares $Unique)))
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
    (unique-moves $Player $Moves $Targets) 
    ( (player-role $Player) (unique-moves $Player $PieceA $SqA $SqT $Moves $Targets)))
; 


  (= 
    (unique-moves $Player $PieceA $SqA $SqT $Moves $Targets) 
    (findall 
      (target $Player $PieceA $SqA $SqT) 
      (member 
        (move $PieceA $Player $SqA $SqT) $Moves) $Targets))
; 



;
; 

;
; 


  (= 
    (could-move $PieceA $Player $SqA $SqT $S) 
    ( (on $PieceA $SqA $S) 
      (owns $PieceA $Player) 
      (put-control-if $Player $S $S1) 
      (moves $PieceA $Player $SqA $SqT $S1) 
      (not (excluded-to $PieceA $SqT))))
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
    (gmobility $Player $Val $Position $Tables) 
    ( (moving-table $Tables $MTable) 
      (unique-moves $Player $MTable $Moves) 
      (length $Moves $Val)))
; 


;
; 

;
; 

;
; 


  (= 
    (gcapmobility $Player $Val $Position $Tables) 
    ( (capturing-table $Tables $MTable) 
      (player-threats $Player $MTable $Moves) 
      (length $Moves $Val)))
; 






;
; 



  (= 
    (add-dynamic-tables-if $S $Tables) 
    (det-if-then-else 
      (or 
        (, 
          (capturing-table $Tables $Table) 
          (var $Table)) 
        (, 
          (moving-table $Tables $Table) 
          (var $Table))) 
      (add-dynamic-tables $S $Tables) True))
; 



;
; 


  (= 
    (add-dynamic-tables $S $Tables) 
    ( (det-if-then-else movtable 
        (add-moving-table-if $S $Tables) True) (det-if-then-else captable (add-capturing-table-if $S $Tables) True)))
; 




  (= 
    (add-capturing-table-if $S $Tables) 
    (det-if-then-else 
      (, 
        (capturing-table $Tables $Table) 
        (var $Table)) 
      (, 
        (tracing-anal-format dynamic "Building <capture> table ...~n" Nil) 
        (tracing-anal-timing dynamic 
          (add-capturing-table $S $Tables))) True))
; 




  (= 
    (add-moving-table-if $S $Tables) 
    (det-if-then-else 
      (, 
        (moving-table $Tables $Table) 
        (var $Table)) 
      (, 
        (tracing-anal-format dynamic "Building <move> table ...~n" Nil) 
        (tracing-anal-timing dynamic 
          (add-moving-table $S $Tables))) True))
; 



  (= 
    (add-moving-table $S $Tables) 
    ( (move-table $Table $S) (moving-table $Tables $Table)))
; 



  (= 
    (add-capturing-table $S $Tables) 
    ( (capture-table $Table $S) (capturing-table $Tables $Table)))
; 






