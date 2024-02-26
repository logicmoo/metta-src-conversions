;                                           (convert_to_metta_file  flight $_216976 metagame/learning/flight.pl metagame/learning/flight.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================


;
; SAFE_FLIGHT_SQUARE(Piece,Player,SqF,SqT,Pos,Tables)
;
; A safe flight square is one where the piece can move (perhaps by capture),
;
; the new square doesn't win for the enemy, and in the resulting position
;
; no piece is attacking it.
;
; We could restrict attacking pieces to those which wouldn't be attacked in the
;
; next position, to pieces which could safely move onto their dest square when
;
; capturing us, etc., but this should do as a first cut!


  (= (safe-flight-square $Piece $Player $SqF $SqT $Pos $Tables)  
    (opposite-role $Player $Opp)
    (flight-square $Piece $Player $SqF $SqT $Pos $PosOut $Tables)
    (not (threatens $PieceA $Opp $SqA $SqTA $Piece $SqT $Effect $PosOut)))


;
; The exclusion check now in global.pl when we build tables in the first place.

  (= (flight-square $Piece $Player $SqF $SqT $Pos $PosOut $Tables)  
    (blank-state-if $Pos)
    (on $Piece $SqF $Pos)
    (piece-index $Piece $_)
    (general-move $Piece $Player $SqF $SqT $Pos $PosOut $Tables))
; ; find_advice_tables_if(Tables), ; add_dynamic_tables_if(Pos,Tables),



  (= (general-move $Piece $Player $SqF $SqT $Pos $PosOut $Tables)  
    (done-move-threat $Piece $Player $SqF $SqT $Pos $PosOut $Tables))
  (= (general-move $Piece $Player $SqF $SqT $Pos $PosOut $Tables)  
    (done-capture-threat $Piece $Player $SqF $SqT $PieceV $SqV $Effect $Pos $PosOut $Tables))



  (= (testflight)  
    (checkpoint cap $Pos)
    (flight-square $Piece player $SqF $SqT $Pos $ZOut $Tables)
    (format "~p->~p~n" 
      (:: $SqF $SqT)))




;
; ================================================================================
;
; Flight_square without changing position (not used).
;
; ================================================================================



  (= (flight-square $Piece $Player $SqF $SqT $Pos $Tables)  
    (blank-state-if $Pos)
    (on $Piece $SqF $Pos)
    (piece-index $Piece $_)
    (general-move $Piece $Player $SqF $SqT $Pos $Tables))
; ; find_advice_tables_if(Tables), ; add_dynamic_tables_if(Pos,Tables),
	

;
; use routines in global.pl

  (= (general-move $Piece $Player $SqF $SqT $Pos $Tables)  
    (move-threat $Piece $Player $SqF $SqT $Tables))
  (= (general-move $PieceA $Player $SqA $SqT $Pos $Tables)  
    (capture-threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect $Tables))


;
; ================================================================================
;
; Using flight info for dominate goals
;
; ================================================================================


  (= (eradicate-safety $Player $Piece $Sq $Goal $Value $Pos $Tables)  
    (opposite-role $Player $Opponent)
    (game-player-has-goal $_ $Opponent $Goal)
    (eradicate-goal $Goal $Player $Type)
    (eradicate-goal $Goal $Descr)
    (owns $Piece $Player)
    (weighted-dominate $Descr $Player $Piece $Sq $Value $Pos $Tables))
; /* A piece is tightly-dominated to the extent that its flight squares are covered by enemy pieces. Domination is slightly worse for the player who has to move, because of Zugswang. [?] Also important for domination is the number of pieces left to capture before the goal is achieved. Thus when a player has just one king, this is vital to protect, whereas when he has lots of checkers, dominating any 1 isn't that crucial until the numbers are smaller. To model this, we should first count the number matching this goal, and then weight the resulting domination value for each target piece by some value which decreases as the number of such targets increases. In fact, maybe don't even think about domination until the number of targets is smaller. */

;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  metagame/learning/flight.pl 105 12 3822))
  

;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  metagame/learning/flight.pl 110 13 3967))



;
; A dominated piece gets points for its owner for each safe
;
; flight square it has, with each square weighted by the
;
; eventual mobility the piece would have from that square.
;
; Thus, a king near the corner, with 8 flight squares, may still be
;
; worse than a king in the center with only 3 flight squares.
;
; Also, this says that if we want to take away just 1 flight square
;
; from the piece, take the more central one to force it backward!
;
; If this is annoying we can change it to just count the number of
;
; moves it has.

  (= (dominate-val1 $Player $Piece $SqF $Value $Pos $Tables)  
    (blank-state-if $Pos)
    (safe-flight-square $Piece $Player $SqF $SqT $Pos $Tables)
    (eventual-piece-mobility $Piece $SqT $Val $Pos $Tables)
    (negate-for-player $Player $Val $Value))
	



  (= (matching-square $Piece $Sq $Descr $Pos)  
    (on $Piece $_ $Sq $Pos)
    (matches $Descr $Piece))
	

;
; VITAL_NUMBER parameter controls max number left st we consider
;
; remaining eradicate targets to be vital to our safety.

  (= (enough-target-urgency $Targets $Weight)  
    (parameter vital-number $N)
    (length $Targets $Length)
    (=< $Length $N)
    (is $Weight 
      (/ 1 $Length)))


;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  metagame/learning/flight.pl 150 12 5319))

	
	

  (= (eradicate-goal-targets $Player $Goal $Targets $Pos)  
    (opposite-role $Player $Opponent)
    (game-player-has-goal $_ $Player $Goal)
    (eradicate-goal $Goal $Opponent $Type)
    (eradicate-goal $Goal $Descr)
    (dominate-targets $Descr $Targets $Pos))
	
	
;
; May want to take the particular effect into accout, as not as
;
; serious if we possess the piece afterward.
;
; Just checks if there is some threat to this piece, doesn't count
;
; duplicates.
;
; If black needs to kill N more white pieces to achieve his goal, the
;
; value to white for having each of these pieces threatened is -1/N.
;
; So when down to last vital piece, he scores -1 for having it threatened.
;
; Thus by setting the parameter which uses this function to have high value,
;
; it only kicks into effect when we get threatened, and can override all other
;
; considerations.
;
; Also, when there are multiple goals threatened, we take more seriously the ones
;
; which are closer to achievement.
;
; 

  (= (threatened-vital-piece $Piece $Sq $Player $Goal $Value $Pos $Tables)  
    (opposite-role $Player $Opponent)
    (vital-piece $Piece $Sq $Player $Goal $Weight $Pos $Tables)
    (det-if-then 
      (capture-threat $PieceA $Opponent $SqA $SqT $Piece $Sq $Effect $Tables) True)
    (negate-for-player $Opponent $Weight $Value))
; ; find_advice_tables_if(Tables), ; add_dynamic_tables_if(Pos,Tables),
	
;
; If opponent to move and threatening our last piece, this is
;
; absolutely terrible, so make value much more intense (override all else).
;
; Otherwise use normal value.
;
; Note this only applies after we've assigned all the pieces, as
;
; eradicate goals are not checked until then.

  (= (threatened-vital-piece-value $Piece $Sq $Player $Goal $Value $Pos $Tables)  
    (not (still-assigning $Pos))
    (threatened-vital-piece $Piece $Sq $Player $Goal $Val1 $Pos $Tables)
    (det-if-then-else 
      (or 
        (control $Player $Pos) 
        (< 
          (abs $Val1) 1)) 
      (= $Value $Val1) 
      (is $Value 
        (* 10000 $Val1))))
	
	
	

