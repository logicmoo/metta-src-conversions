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
    (goal-square $Piece $Sq $Player $Goal) 
    ( (game-player-has-goal $_ $Player $Goal) 
      (arrive-goal $Goal $Descr $Squares) 
      (matches $Descr $Piece) 
      (member1 $Sq $Squares)))
; 





  (= 
    (safe-transitions-type $Piece $PieceIndex $Sq $SqIndex $Type $SqTIndices $State) 
    ( (square-index $Sq $SqIndex) (det-if-then-else (setof $SqTIndex (^ $SqT (safe-transition-type $Piece $PieceIndex $Sq $SqIndex $SqT $SqTIndex $Type $State)) $SqTIndices) True (= $SqTIndices Nil))))
; 



  (= 
    (safe-transition $Piece $Sq $SqT $State) 
    (safe-transition $Piece $PieceIndex $Sq $SquareIndex $SqT $SqTIndex $State))
; 

	  
  (= 
    (safe-transition $Piece $PieceIndex $Sq $SquareIndex $SqT $SqTIndex $State) 
    (safe-transition-type $Piece $PieceIndex $Sq $SquareIndex $SqT $SqTIndex moving $State))
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
    (safe-transition-type $Piece $PieceIndex $Sq $SquareIndex $SqT $SqTIndex $Type $State) 
    ( (square-index $Sq $SquareIndex) 
      (piece-index $Piece $PieceIndex) 
      (not (excluded-from $Piece $Sq)) 
      (piece-move-for-type $Type $Piece $Sq $SqT $State) 
      (not (excluded-to $Piece $SqT)) 
      (square-index $SqT $SqTIndex) 
      (tracing-anal-format detailed "~p: ~p -> ~p~n" 
        (:: $Piece $Sq $SqT))))
; 




;
; 

;
; 


  (= 
    (excluded-from $Piece $Sq) 
    (goal-square $Piece $Sq $Player $Goal))
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
    (excluded-to $Piece $SqT) 
    ( (owns $Piece $Player) 
      (goal-square $Piece $SqT $Opp $_) 
      (opposite-role $Player $Opp) 
      (not (goal-square $Piece $SqT $Player $_)) 
      (not (safe-promotion $Piece $Player $SqT)) 
      (tracing-anal-format filter "Filter: <~p> can't safely move to ~p~n" 
        (:: $Piece $SqT))))
; 

  (= 
    (excluded-to $Piece $SqT) 
    ( (owns $Piece $Player) 
      (player-promotion-square $Player $SqT) 
      (not (safe-prom1 $Piece $Player $SqT)) 
      (tracing-anal-format filter "Filter: <~p> can't safely promote on ~p~n" 
        (:: $Piece $SqT))))
; 


	
;
; 

;
; 

;
; 



  (= 
    (safe-promotion $Piece $Player $SqT) 
    ( (owns $Piece $Player) 
      (player-promotion-square $Player $SqT) 
      (safe-prom1 $Piece $Player $SqT)))
; 


;
; 

;
; 


  (= 
    (safe-prom1 $Piece $Player $SqT) 
    (player-safe-prom $Piece $Player $SqT))
; 

  (= 
    (safe-prom1 $Piece $Player $SqT) 
    (opponent-safe-prom $Piece $Player $SqT))
; 




  (= 
    (player-safe-prom $Piece $Player $SqT) 
    ( (promotes-into $Piece $PieceT $Player $Player) 
      (opposite-role $Player $Opp) 
      (not (goal-square $PieceT $SqT $Opp $_))))
; 




  (= 
    (opponent-safe-prom $Piece $Player $SqT) 
    ( (opponent-promotes $Piece) (not (opponent-wins-prom $Piece $Player $SqT))))
; 



  (= 
    (opponent-wins-prom $Piece $Player $SqT) 
    ( (promotes-into $Piece $PieceT $Player $Opp) 
      (opposite-role $Player $Opp) 
      (goal-square $PieceT $SqT $Opp $_)))
; 



	

;
; 

;
; 

;
; 

;
; 


