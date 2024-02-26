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
    (safe-flight-square $Piece $Player $SqF $SqT $Pos $Tables) 
    ( (opposite-role $Player $Opp) 
      (flight-square $Piece $Player $SqF $SqT $Pos $PosOut $Tables) 
      (not (threatens $PieceA $Opp $SqA $SqTA $Piece $SqT $Effect $PosOut))))
; 



;
; 


  (= 
    (flight-square $Piece $Player $SqF $SqT $Pos $PosOut $Tables) 
    ( (blank-state-if $Pos) 
      (on $Piece $SqF $Pos) 
      (piece-index $Piece $_) 
      (general-move $Piece $Player $SqF $SqT $Pos $PosOut $Tables)))
; 




  (= 
    (general-move $Piece $Player $SqF $SqT $Pos $PosOut $Tables) 
    (done-move-threat $Piece $Player $SqF $SqT $Pos $PosOut $Tables))
; 

  (= 
    (general-move $Piece $Player $SqF $SqT $Pos $PosOut $Tables) 
    (done-capture-threat $Piece $Player $SqF $SqT $PieceV $SqV $Effect $Pos $PosOut $Tables))
; 




  (= 
    (testflight) 
    ( (checkpoint cap $Pos) 
      (flight-square $Piece player $SqF $SqT $Pos $ZOut $Tables) 
      (format "~p->~p~n" 
        (:: $SqF $SqT))))
; 





;
; 

;
; 

;
; 




  (= 
    (flight-square $Piece $Player $SqF $SqT $Pos $Tables) 
    ( (blank-state-if $Pos) 
      (on $Piece $SqF $Pos) 
      (piece-index $Piece $_) 
      (general-move $Piece $Player $SqF $SqT $Pos $Tables)))
; 

	

;
; 


  (= 
    (general-move $Piece $Player $SqF $SqT $Pos $Tables) 
    (move-threat $Piece $Player $SqF $SqT $Tables))
; 

  (= 
    (general-move $PieceA $Player $SqA $SqT $Pos $Tables) 
    (capture-threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect $Tables))
; 



;
; 

;
; 

;
; 



  (= 
    (eradicate-safety $Player $Piece $Sq $Goal $Value $Pos $Tables) 
    ( (opposite-role $Player $Opponent) 
      (game-player-has-goal $_ $Opponent $Goal) 
      (eradicate-goal $Goal $Player $Type) 
      (eradicate-goal $Goal $Descr) 
      (owns $Piece $Player) 
      (weighted-dominate $Descr $Player $Piece $Sq $Value $Pos $Tables)))
; 


;                                       (error
;                                         (syntax_error  operator_expected)
;                                         (file  metagame/learning/flight.pl 105 12 3822))
  

;                                        (error
;                                          (syntax_error  operator_expected)
;                                          (file  metagame/learning/flight.pl 110 13 3967))



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
    (dominate-val1 $Player $Piece $SqF $Value $Pos $Tables) 
    ( (blank-state-if $Pos) 
      (safe-flight-square $Piece $Player $SqF $SqT $Pos $Tables) 
      (eventual-piece-mobility $Piece $SqT $Val $Pos $Tables) 
      (negate-for-player $Player $Val $Value)))
; 

	



  (= 
    (matching-square $Piece $Sq $Descr $Pos) 
    ( (on $Piece $_ $Sq $Pos) (matches $Descr $Piece)))
; 

	

;
; 

;
; 


  (= 
    (enough-target-urgency $Targets $Weight) 
    ( (parameter vital-number $N) 
      (length $Targets $Length) 
      (=< $Length $N) 
      (is $Weight 
        (/ 1 $Length))))
; 



;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  metagame/learning/flight.pl 150 12 5319))

	
	

  (= 
    (eradicate-goal-targets $Player $Goal $Targets $Pos) 
    ( (opposite-role $Player $Opponent) 
      (game-player-has-goal $_ $Player $Goal) 
      (eradicate-goal $Goal $Opponent $Type) 
      (eradicate-goal $Goal $Descr) 
      (dominate-targets $Descr $Targets $Pos)))
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
    (threatened-vital-piece $Piece $Sq $Player $Goal $Value $Pos $Tables) 
    ( (opposite-role $Player $Opponent) 
      (vital-piece $Piece $Sq $Player $Goal $Weight $Pos $Tables) 
      (det-if-then 
        (capture-threat $PieceA $Opponent $SqA $SqT $Piece $Sq $Effect $Tables) True) 
      (negate-for-player $Opponent $Weight $Value)))
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
    (threatened-vital-piece-value $Piece $Sq $Player $Goal $Value $Pos $Tables) 
    ( (not (still-assigning $Pos)) 
      (threatened-vital-piece $Piece $Sq $Player $Goal $Val1 $Pos $Tables) 
      (det-if-then-else 
        (or 
          (control $Player $Pos) 
          (< 
            (abs $Val1) 1)) 
        (= $Value $Val1) 
        (is $Value 
          (* 10000 $Val1)))))
; 

	
	
	

