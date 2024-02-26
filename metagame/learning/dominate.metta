;
; 

;
; 

;
; 

;
; 



  (= 
    (attacks-i $PieceAI $SqAI $SqTI $PieceVI $SqVI $S) 
    ( (attacks $PieceA $SqA $SqT $PieceV $SqV $S) 
      (piece-index $PieceA $PieceAI) 
      (piece-index $PieceV $PieceVI) 
      (square-index $SqA $SqAI) 
      (square-index $SqV $SqVI)))
; 





  (= 
    (attacks $PieceA $SqA $SqT $PieceV $SqV $S) 
    ( (det-if-then-else 
        (or 
          (nonvar $SqA) 
          (nonvar $SqT)) True 
        (board-square $SqA)) (attacks1 $PieceA $SqA $SqT $PieceV $SqV $S)))
; 



;
; 

;
; 

;
; 



  (= 
    (attacks0 $PieceA $SqA $SqT $PieceV $SqV $S) 
    ( (blank-state $S) 
      (player-role $Player) 
      (control $Player $S) 
      (owns $PieceA $Player) 
      (on $PieceA $SqA $S) 
      (captures $PieceA $Player $SqA $SqT $Effect $Captured $S) 
      (captured-piece $PieceV $SqV $Captured)))
; 



  (= 
    (attacks1 $PieceA $SqA $SqT $PieceV $SqV $S) 
    ( (blank-state-if $S) 
      (player-role $Player) 
      (put-control $Player $S $S1) 
      (owns $PieceA $Player) 
      (captures $PieceA $Player $SqA $SqT $Effect $Captured $S1) 
      (captured-piece $PieceV $SqV $Captured)))
; 


;
; 

;
; 

;
; 


  (= 
    (attackshow $PieceA $SqA $SqT $PieceV $SqV $S) 
    ( (attacks $PieceA $SqA $SqT $PieceV $SqV $S) (on $PieceA $SqA $S)))
; 




  (= 
    (blank-state-if $S) 
    (det-if-then-else 
      (var $S) 
      (blank-state $S) True))
; 




  (= 
    (blank-state $S) 
    (blank-state $Player $Stage 0 $S))
; 


  (= 
    (blank-state $Player $Stage $Move $S) 
    ( (new-state $S0) (initialize-state-properties $Stage $Player $Move $S0 $S)))
; 



;
; 

;
; 

;
; 


  (= 
    (attack-distance $PA $SqA $SqI $PV $SqV $Dist $State) 
    ( (attacks $PA $SqI $SqT $PV $SqV $State) (square-piece-distance $SqA $PA $SqI $Dist)))
; 



  (= 
    (closest-attack $PA $SqA $SqI $PV $SqV $Dist $State $Tables) 
    ( (setof 
        (- 
          (- $Dist1 $SqI) $State) 
        (attack-distance $PA $SqA $SqI $PV $SqV $Dist1 $State) $Places) (= $Places (Cons  (- (- $Dist $SqI) $State) $Rest))))
; 

	   



  (= 
    (testa $Dist $State) 
    (attack-distance 
      (piece king player) 
      (square 5 3) $SqI 
      (piece man opponent) 
      (square 2 2) $Dist $State))
; 




  (= 
    (dominate-value $PieceA $SqA $PieceV $SqV $Goal $Value $Pos $Tables) 
    ( (blank-state-if $Pos) 
      (game-player-has-goal $_ $Player $Goal) 
      (opposite-role $Player $Opponent) 
      (eradicate-goal $Goal $Opponent $Type) 
      (eradicate-goal $Goal $Descr) 
      (weighted-dominate $Descr $PieceA $Player $SqA $PieceV $SqV $Value $Pos $Tables)))
; 


;                                        (error
;                                          (syntax_error  operator_expected)
;                                          (file  metagame/learning/dominate.pl 158 13 5055))
  

;                                         (error
;                                           (syntax_error  operator_expected)
;                                           (file  metagame/learning/dominate.pl 163 13 5214))




  (= 
    (dominate-val1 $PieceA $Player $SqA $PieceV $SqV $Value $Pos $Tables) 
    ( (blank-state-if $Pos) 
      (closest-attack $PieceA $SqA $SqI $PieceV $SqV $Dist $State $Tables) 
      (distance-value $Dist $Val) 
      (negate-for-player $Player $Val $Value)))
; 

	



  (= 
    (matching-square $Piece $Sq $Descr $Pos) 
    ( (on $Piece $_ $Sq $Pos) (matches $Descr $Piece)))
; 

	


  (= 
    (enough-target-urgency $Targets $Weight) 
    ( (length $Targets $Length) 
      (< $Length 4) 
      (is $Weight 
        (/ 1 $Length))))
; 





  (= 
    (test2 $PV $SV $Val $S) 
    ( (checkpoint test $S) (dominate-value (piece king player) (square 4 2) $PV $SV $Goal $Val $S)))
; 



  (= 
    (test3) 
    ( (setof 
        (, $A $B $C $D $E) 
        (^ $S 
          (attacks $A $B $C $D $E $S)) $Sets) (ppl $Sets)))
; 



  (= 
    (test4) 
    ( (setof 
        (, $D $A $E $B) 
        (^ 
          (, $C $S) 
          (attacks $A $B $C $D $E $S)) $Sets) (ppl $Sets)))
; 




  (= 
    (test5) 
    ( (setof 
        (, $D $A $E $B) 
        (^ 
          (, $C $S) 
          (attacks-i $A $B $C $D $E $S)) $Sets) (ppl $Sets)))
; 



  (= 
    (attackset $Attacks $State) 
    (setof 
      (^ $PieceV 
        (^ $PieceA 
          (^ $SqV $SqA))) 
      (^ 
        (, $SqT $State) 
        (attacks-i $PieceA $SqA $SqT $PieceV $SqV $State)) $Attacks))
; 



;
; 


  (= 
    (dom-table $DomTable $State) 
    ( (attackset $Attacks $State) 
      (do-graph $Attacks $GroupedTargs) 
      (group-attacks $GroupedTargs $DomTable)))
; 


;
; 



  (= 
    (group_attacks  () ()) True)
; 

  (= 
    (group-attacks 
      (Cons  
        (- $T $As) $Rest) 
      (Cons  
        (- $T $GroupedAs) $GRest)) 
    ( (do-graph $As $Grouped1) 
      (group-targsq $Grouped1 $GroupedAs) 
      (group-attacks $Rest $GRest)))
; 



  (= 
    (group_targsq  () ()) True)
; 

  (= 
    (group-targsq 
      (Cons  
        (- $A $Sqs) $Rest) 
      (Cons  
        (- $A $GroupedSqs) $GRest)) 
    ( (do-graph $Sqs $GroupedSqs) (group-targsq $Rest $GRest)))
; 




