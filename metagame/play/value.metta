;                                           (convert-to-metta-file  value $_36932 metagame/play/value.pl metagame/play/value.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; value.pl

;
; =============================================================================
;
; Loading evaluation files
;
; =============================================================================

;
; LOAD_EVAL(+GameName)
;
; Finds a file GameName.eval in a library directory.
;
; Then loads this file as the current evaluation function.

  (= (load-eval $Name)  
    (find-eval-file $Name $File)
    (file-make-test-eval $File))


  (= (find-eval-file $Name $File)  
    (find-suffixed-library-file $Name eval $File))

;
; Maybe abolishing these is a bit strict, should
;
; just turn off redefinitions ...

  (= (file-make-test-eval $File)  
    (abolish (/ piece-value 2))
    (abolish (/ piece-square-value 2))
    (compile $File))



;
; =============================================================================
;
; Search-Specific Predicates used in search engine
;
; =============================================================================

;
; ---------------------------------------
;
; SUCCESSOR_POS(Move,State,State2,Tables)
;
; ---------------------------------------
;
; External predicate used by alphabeta search engine.
;
; Backtracks over all successor STATE2 reachable from STATE.
;
; Move should be a unique name of this transition.
;
; Uses evaluation tables.


  (= (successor-pos $Move $State $State2 $Tables)  
    (legal $Move $State $State2))


;
; -----------------------------------
;
; TERMINAL_POS_VALUE(Pos,Val,Tables)
;
; -----------------------------------
;
; External predicate for alphabeta code.
;
; Returns a Val for a Pos if it is terminal, fails otherwise.
;
; 

  (= (terminal-pos-value $Pos $Val $Tables)  
    (terminal-game-outcome $Pos $Val))


;
; value-of-outcome defined in alphabeta.pl
;
; 

  (= (terminal-game-outcome $Pos $Val)  
    (game-outcome $Outcome $Pos)
    (value-of-outcome $Outcome $Val)
    (set-det))



;
; -------------------------
;
; STATICVAL(Pos,Val,Tables)
;
; -------------------------
;
; This defines the external predicate for search engine.
;
; Determines what evaluation procedure will be used.
;
; 

  (= (staticval $Pos $Val $Tables)  
    (evaluation $Val $Pos $Tables))

;
; ------------------
;
; staticval(Pos,Val)
;
; ------------------
  (= (staticval $Pos $Val)  
    (evaluation $Val $Pos))



;
; =============================================================================
;
; Defining value function for Symmetric Chess-Like Games
;
; =============================================================================


;
; Each advisor is a separate rule making a comment about a position.
;
; They all contribute their advice to the position.
;
; These advices are then weighed appropriately (by the mediation routine),
;
; and then summed to give the final value of the position.
;
; [Note: the term "advices" is due to:
;
; Professor Professor H. J. van den Herik.
;
; This may or may not be an official word in the English language.]


  (= (evaluation $Value $Position)  
    (find-advice-tables $Tables)
    (evaluation $Value $Position $Tables))

  (= (evaluation $Value $Position $Tables)  
    (get-advices $Advice $Position $Tables)
    (mediate-advices $Advice $Value $Tables))


;
; Like the global version, but just considers the evaluation if
;
; Piece were to be on Sq in the Position.
;
; It might be worth actually pretending to put the piece there
;
; to be more accurate, but that is somewhat more costly.

  (= (local-evaluation $Piece $Sq $Value $Position $Tables)  
    (get-local-advices $Piece $Sq $Advice $Position $Tables)
    (mediate-advices $Advice $Value $Tables))

	

  (= (get-advices $Advices $Position)  
    (find-advice-tables $Tables)
    (get-advices $Advices $Position $Tables))
	
  (= (get-advices $Advices $Position $Tables)  
    (findall $Advice 
      (, 
        (add-dynamic-tables $Position $Tables) 
        (value $Advice $Position $Tables)) $Advices))



  (= (get-local-advices $Piece $Sq $Advices $Position $Tables)  
    (findall $Advice 
      (value $Piece $Sq $Advice $Position $Tables) $Advices))
; ; bagof(Advice,


  (= (show-advices-name $PosName)  
    (checkpoint $PosName $Pos)
    (show-advices $Pos))



  (= (show-advices $Pos)  
    (get-advices $As $Pos)
    (ppl $As))

  (= (show-advices $Pos $Tables)  
    (get-advices $As $Pos $Tables)
    (ppl $As))



  (= (show-local-advices $Piece $Sq $Pos)  
    (find-advice-tables $Tables)
    (show-local-advices $Piece $Sq $Pos $Tables))


  (= (show-local-advices $Piece $Sq $Pos $Tables)  
    (on $Piece $Sq $Pos)
    (get-local-advices $Piece $Sq $Advices $Pos $Tables)
    (ppl $Advices))





  (= (mediate-advices $Advices $Value $Tables)  
    (weigh-advices $Advices $Vals $Tables)
    (sumlist $Vals $Value))


  (= (weigh-advices  () () $_)  True)
  (= (weigh-advices (Cons  $A $As) (Cons  $V $Vals) $Tables)  
    (weigh-advice $A $V $Tables)
    (weigh-advices $As $Vals $Tables))
	

  (= (weigh-advice (advice $Advisor $Comment $V) $Val $Tables)  
    (advisor-weight $Advisor $Weight $Tables)
    (is $Val 
      (* $V $Weight))) 

;
; Change this to use tables explicitly.

  (= (advisor-weight $Advisor $Weight $Tables)  
    (parameter $Advisor $Weight))



  (= (advisor $A $B)  
    (dynamic-advisor $A $B))
  (= (advisor $A $B)  
    (static-advisor $A $B))



  (= (dynamic-advisor  gen_material 0)  True)
  (= (dynamic-advisor  material 0)  True)
  (= (dynamic-advisor  square 0)  True)
  (= (dynamic-advisor  lthreat 0)  True)
  (= (dynamic-advisor  potent 0)  True)
  (= (dynamic-advisor  dynamic_mobility 0)  True)

  (= (dynamic-advisor  gmovmob 0)  True)
  (= (dynamic-advisor  gcapmob 0)  True)
  (= (dynamic-advisor  gthreat 0)  True)
  (= (dynamic-advisor  pthreat 0)  True)

  (= (dynamic-advisor  static_mobility 0)  True)
  (= (dynamic-advisor  eventual_mobility 0)  True)
  (= (dynamic-advisor  arrive_distance 0)  True)
  (= (dynamic-advisor  promote_distance 0)  True)
  (= (dynamic-advisor  possess 0)  True)
  (= (dynamic-advisor  initprom 0)  True)
;
; dynamic_advisor(dominate,0).
  (= (dynamic-advisor  eradicate 0)  True)
  (= (dynamic-advisor  vital 0)  True) 


  (= (static-advisor  random 0)  True)
  (= (static-advisor  static 0)  True)
  (= (static-advisor  victims 0)  True)
  (= (static-advisor  max_static_mob 0)  True)
  (= (static-advisor  max_eventual_mob 0)  True)
  (= (static-advisor  avg_static_mob 0)  True)
  (= (static-advisor  avg_eventual_mob 0)  True)
  (= (static-advisor  eradicate 0)  True)
  (= (static-advisor  stalemate 0)  True)
  (= (static-advisor  arrive 0)  True)
  (= (static-advisor  giveaway 0)  True)
  (= (static-advisor  immunity 0)  True)
;
; advisor(,0).



  (= initialize-advisors  
    ( (whenever 
        (advisor $Name $Default) 
        (add-parameter $Name $Default)) (add-atom  &self initialized_advisors)))


  (= (active-parameter $P)  
    (advisor $P $_)
    (parameter $P $Val)
    (\== $Val 0))


  !(det-if-then-else 
    (current-predicate initialized-advisors $_) True initialize-advisors)


;
; ----------------------------------------
;
; Advisor requirements
;
; ----------------------------------------


  (= movtable  
    (det-if-then 
      (, 
        (needs-movtable $Param) 
        (active-parameter $Param)) True))


  (= (needs-movtable  dynamic_mobility)  True)
  (= (needs-movtable  gmovmob)  True)



  (= captable  
    (det-if-then 
      (, 
        (needs-captable $Param) 
        (active-parameter $Param)) True))

;
; needs_captable(threat).

  (= (needs-captable  gcapmob)  True)
  (= (needs-captable  vital)  True)
  (= (needs-captable  gthreat)  True)
  (= (needs-captable  pthreat)  True)
  (= (needs-captable  lthreat)  True)
  (= (needs-captable  gcapmob)  True)
  (= (needs-captable  eradicate)  True)
  (= (needs-captable  potent)  True)


;
; ----------------------------------------
;
; VALUE/3
;
; ----------------------------------------


;
; Global moving mobility

  (= (value (advice gmovmob $Player $Value) $Position $Tables)  
    (active-parameter gmovmob)
    (gmobility $Player $Val $Position $Tables)
    (negate-for-player $Player $Val $Value))
	

;
; Global capturing mobility
  (= (value (advice gcapmob $Player $Value) $Position $Tables)  
    (active-parameter gcapmob)
    (gcapmobility $Player $Val1 $Position $Tables)
    (favor-control $Player $Val1 $Val $Position)
    (negate-for-player $Player $Val $Value))

;
; Global Threats
  (= (value (advice gthreat $Player $Value) $Position $Tables)  
    (active-parameter gthreat)
    (gthreat $Player $Value $Position $Tables))

;
; Best Potent Threat
  (= (value (advice pthreat $Player $Value) $Position $Tables)  
    (active-parameter pthreat)
    (pthreat $Player $Value $Position $Tables))


;
; One source of value comes from POSSESSING a piece.
  (= (value $Value $Position $Tables)  
    (active-parameter possess)
    (in-hand $Piece $Player $Position)
    (not (still-assigning $Position))
    (current-predicate possess-value 
      (possess-value $_ $_ $_ $_ $_))
    (possess-value $Piece $Player $Value $Position $Tables))

;
; One source of value comes from being able to
;
; OPPONENT-PROMOTE a piece.
  (= (value $Value $Position $Tables)  
    (active-parameter initprom)
    (current-predicate initprom-value 
      (initprom-value $Value $Position $Tables))
    (initprom-value $Value $Position $Tables))

;
; VITAL
;
; Losing points when our vital pieces (those enemy wants to remove and is near
;
; to achieving) are threatened.
  (= (value (advice vital (, $Goal (@ $Piece $Square)) $Value) $Position $Tables)  
    (active-parameter vital)
    (threatened-vital-piece-value $Piece $Square $Player $Goal $Value $Position $Tables))



;
; RANDOM
;
; Augments a position's evaluation by a random number in the range
;
; [MIN,MAX] as set by the parameters RANDOM_MIN and RANDOM_MAX.
  (= (value (advice random (range $Min $Max) $Value) $Position $Tables)  
    (active-parameter random)
    (random-eval $Min $Max $Value))


;
; One source of value comes from having a piece on a square.
;
; Using on/4 here ensures it is a piece struct instead of empty.
  (= (value $Value $Position $Tables)  
    (on $Piece $Player $Square $Position)
    (value $Piece $Square $Value $Position $Tables))




;
; ----------------------------------------
;
; VALUE/5
;
; ----------------------------------------

;
; General material: 1 point for player's piece, -1 for opponents.
;
; value(piece(Type,player),_Square,advice(gen_material,Type,1),_Position,_Tables).
;
; value(piece(Type,opponent),_Square,advice(gen_material,Type,-1),_Position,_Tables).
;
; 
  (= (value $Piece $Sq (advice gen-material $Type $Val) $Position $Tables)  
    (active-parameter gen-material)
    (gen-material-value $Piece $Player $Type $Val))



;
; INDEPENDENT STATIC PIECE VALUE
;
; This isn't the right way to do this (use table instead), but for now
;
; will cut down on extra eval stuff.
  (= (value $Piece $Square (advice static $Piece $Value) $Pos $Tables)  
    (active-parameter static)
    (piece-player-static $Piece $Player $Value $Tables))
; ; independent_advice(_Player,Piece,Advice).


;
; Dynamic Piece mobility
  (= (value $Piece $Square (advice dynamic-mobility (@ $Piece $Square) $Value) $Position $Tables)  
    (active-parameter dynamic-mobility)
    (active-advisor dynamic-mobility $Tables)
    (dynamic-piece-mobility $Piece $Square $Value $Position $Tables))

;
; Static piece mobility
  (= (value $Piece $Square (advice static-mobility (@ $Piece $Square) $Value) $Position $Tables)  
    (active-parameter static-mobility)
    (static-piece-mobility $Piece $Square $Value $Position $Tables))

;
; Eventual piece mobility
  (= (value $Piece $Square (advice eventual-mobility (@ $Piece $Square) $Value) $Position $Tables)  
    (active-parameter eventual-mobility)
    (eventual-piece-mobility $Piece $Square $Value $Position $Tables))
; ; current_predicate(eventual_piece_mobility,_),

;
; Piece attacks
;
; Now *requires* that the opponent have the tables constructed.
  (= (value $Piece $Square (advice lthreat (captures (@ $Piece $Square) (@ $PieceV $SqV)) $Value) $Position $Tables)  
    (active-parameter lthreat)
    (active-advisor threat $Tables)
    (local-threat-value $Piece $Square $PieceV $SqV $Value $Position $Tables))


;
; Potent Threats
  (= (value $Piece $Square (advice potent (captures (@ $Piece $Square) (@ $PieceV $SqV)) $Value) $Position $Tables)  
    (active-parameter potent)
    (active-advisor threat $Tables)
    (potent-threat-mover $Piece $Square $PieceV $SqV $Value $Position $Tables))




;
; Arrive Distance
  (= (value $Piece $Square (advice arrive-distance (, $Goal (@ $Piece $Square) $SqT) $Value) $Position $Tables)  
    (active-parameter arrive-distance)
    (arrive-value $Piece $Square $SqT $Goal $Value $Position $Tables))
; ; current_predicate(_,arrive_value(_,_,_,_,_,_)),

;
; Promotion Distance
  (= (value $Piece $Square (advice promote-distance (, (@ $Piece $Square) $SqT) $Value) $Position $Tables)  
    (active-parameter promote-distance)
    (active-advisor prom $Tables)
    (prom-value $Piece $Square $SqT $Value $Position $Tables))
; ; current_predicate(_,prom_value(_,_,_,_,_)),


;
; Domination Value (not supported)
  (= (value $Piece $Square (advice dominate (, $Goal (@ $Piece $Square) (@ $PieceV $SqV)) $Value) $Position $Tables)  
    (active-parameter dominate)
    (dominate-value $Piece $Square $PieceV $SqV $Goal $Value $Position $Tables))
; ; current_predicate(_,dominate_value(_,_,_,_,_,_,_)),


;
; Eradicating enemy pieces
;
; Call this dominate advisor here, as we use static eradicate in step.pl
  (= (value $Piece $Square (advice dominate (, $Goal (@ $Piece $Square)) $Value) $Position $Tables)  
    (active-parameter dominate)
    (eradicate-safety $Player $Piece $Square $Goal $Value $Position $Tables))
; ; active_advisor(prom,Tables), ; current_predicate(_,prom_value(_,_,_,_,_)),


;
; Specific material: refers to table.
  (= (value $Piece $Square (advice material $Piece $Value) $Position $Tables)  
    (active-parameter material)
    (current-predicate $_ 
      (piece-value $_ $_))
    (piece-value $Piece $Value))


;
; Piece-Square tables
  (= (value $Piece $Square (advice square (@ $Piece $Square) $Value) $Position $Tables)  
    (active-parameter square)
    (piece-square-value $Piece $Square $Value $Tables))
; ; current_predicate(_,piece_square_value(_,_,_)),


;
; ============================================================================
;
; Specific Piece-value tables
;
; ============================================================================
;
; To change this for another game, make a new file containing just
;
; facts like these but with piece names specialized for your new game.
;
; Then load this file separately.
;
; Examples files are games/chess.eval, and games/turncoat.eval


  (= (random-eval $Min $Max $Value)  
    (parameter random-min $Min)
    (parameter random-max $Max)
    (random $Min $Max $Value))
; /* piece_value(piece(king,player),15). piece_value(piece(king,opponent),-15). piece_value(piece(queen,player),9). piece_value(piece(queen,opponent),-9). piece_value(piece(rook,player),5). piece_value(piece(rook,opponent),-5). piece_value(piece(night,player),3). piece_value(piece(night,opponent),-3). piece_value(piece(bishop,player),3.25). piece_value(piece(bishop,opponent),-3.25). piece_value(piece(pawn,player),1). piece_value(piece(pawn,opponent),-1). */
; ;============================================================================ ; Piece-Square tables ;============================================================================ ; This model is again for chess. Make a separate file containing ; just rules of these types for your game, and load it. ; This could of course be the same file at that used for specific ; piece material values above.
; /* ; Piece-Square tables piece_square_value(piece(night,player),square(4,4),2). piece_square_value(piece(night,player),square(1,1),-5). ; Pawns given value as they move closer to their promotion ; rank. piece_square_value(piece(pawn,player),square(_X,Y),Val) :- Val is (Y-1)/6. piece_square_value(piece(pawn,opponent),square(_X,Y),Val) :- Val is (Y-8)/6. */
; ;-------------------------------------------------------------------------------- ; Support routines ;--------------------------------------------------------------------------------
; ; RANDOM_EVAL ; Augments a position's evaluation by a random number in the range ; [MIN,MAX] as set by the parameters RANDOM_MIN and RANDOM_MAX. 


;
; +1 if piece owned by white, -1 if owned by black.

  (= (gen-material-value $Piece $Player $Type $Val)  
    (owns $Piece $Player)
    (piece-name $Piece $Type)
    (negate-for-player $Player 1 $Val))
	

;
; Dynamic Piece Mobility
;
; Only returns advice for pieces with non-0 value.
;
; To just count the moves a piece has, use dynamic_piece_mob.

  (= (dynamic-piece-mobility $Piece $Square $Value $Position $Tables)  
    (owns $Piece $Player)
    (dynamic-piece-mob $Piece $Player $Square $Val $Position $Tables)
    (> $Val 0)
    (negate-for-player $Player $Val $Value))


;
; Note: this currently uses dynamic moving tables. It will
;
; thus only return values for pieces actually on the board.
;
; (We know the tables exist by this point, because of the requires
;
; statement declaring that this advisor needs moving tables.
;
; 
;
; What we should really do is:
;
; 1. Check if tables exist, and piece in current position, whenever call
;
; things which might be used locally.
;
; 2. If either not true, compute it ourself.
;
; 
;
; However, this is not a problem in the current architecture, because:
;
; a. local eval only used in threat and promotion analysis.
;
; b. In threat anal, we know the piece is on the square already.
;
; c. In prom analysis, we shutdown dynamic mob, because don't have time!
;
; So, if we ever change this for prom analysis, we must incorporate the
;
; change above. This is easy, but slows things down a little bit.
;
; 

  (= (dynamic-piece-mob $Piece $Player $Square $Count $Position $Tables)  
    (moving-table $Tables $Moves)
    (unique-moves $Player $Piece $Square $SqT $Moves $Targets)
    (length $Targets $Count)
    (tracing-path-format moves "Dynamic: <~p>: ~p -> ~p~n" 
      (:: $Piece $Square $Count)))




;
; Static Piece Mobility

  (= (static-piece-mobility $Piece $Square $Value $Position $Tables)  
    (owns $Piece $Player)
    (static-piece-mob $Piece $Player $Square $Val $Position $Tables)
    (negate-for-player $Player $Val $Value))


  (= (static-piece-mob $Piece $Player $Square $Value $Position $Tables)  
    (square-piece-mobility $Square $Piece $Value $Tables)
    (tracing-path-format static "In 1: <~p>: ~p -> ~p~n" 
      (:: $Piece $Square $Value)))


;
; Eventual Piece Mobility

  (= (eventual-piece-mobility $Piece $Square $Value $Position $Tables)  
    (owns $Piece $Player)
    (eventual-piece-mob $Piece $Player $Square $Val $Position $Tables)
    (negate-for-player $Player $Val $Value))



  (= (eventual-piece-mob $Piece $Player $Square $Value $Position $Tables)  
    (square-piece-reachability $Square $Piece $Value $Tables)
    (tracing-path-format eventual "In 4: <~p>: ~p -> ~p~n" 
      (:: $Piece $Square $Value)))


;
; ================================================================================
;
; Generic 2-player-game Support Predicates
;
; ================================================================================


  (= (initiative-offset $Control $Player $Offset)  
    (det-if-then-else 
      (= $Control $Player) 
      (= $Offset 0.9) 
      (= $Offset 0.7)))

	

  (= (favor-control $Player $Val1 $Value $Position)  
    (control $Control $Position)
    (initiative-offset $Control $Player $Offset)
    (is $Value 
      (* $Val1 $Offset))) 
	
;
; The player will only execute the threat if doing so might net
;
; him an increase in value.

  (= (favorable-to-owner player $Val)  
    (> $Val 0))
  (= (favorable-to-owner opponent $Val)  
    (< $Val 0))


;
; How many moves extra it costs a player before it is his turn to
;
; move. In 2-player game, just 0 or 1, of course.

  (= (control-cost $Player $Cost $Position)  
    (control $Control $Position)
    (det-if-then-else 
      (= $Control $Player) 
      (= $Cost 0) 
      (= $Cost 1)))

	


  (= (negate-if-same $Player1 $Player2 $Val1 $Val)  
    (det-if-then-else 
      (\== $Player1 $Player2) 
      (= $Val1 $Val) 
      (is $Val 
        (- $Val1))))



  (= (negate-if-different $Player1 $Player2 $Val1 $Val)  
    (det-if-then-else 
      (= $Player1 $Player2) 
      (= $Val1 $Val) 
      (is $Val 
        (- $Val1))))


  (= (negate-for-player  player $Val $Val)  True)
  (= (negate-for-player opponent $Val $Val1)  
    (is $Val1 
      (- $Val)))


  (= (negate-advice-for-player $Player (advice $A $C $V) (advice $A $C $V1))  
    (negate-for-player $Player $V $V1))


;
; Of some list of values, PLAYER wants the max, OPPONENT wants the min.

  (= (max-for-player player $List $Best)  
    (max $List $Best))
  (= (max-for-player opponent $List $Best)  
    (min $List $Best))

;
; Of some list of values, PLAYER wants the min, OPPONENT wants the max.

  (= (min-for-player player $List $Best)  
    (min $List $Best))
  (= (min-for-player opponent $List $Best)  
    (max $List $Best))

	
;
; ================================================================================
;
; Interface
;
; ================================================================================


  (= (evalfile-top $Game)  
    (load-eval $Game))


  (= (evalfile-com $_ $_ $_ $Game)  
    (load-eval $Game))


  (= (evaluate-com $Move $SIn $_)  
    (timing (evaluation $Value $SIn))
    (format "Position's value (positive favors white):  ~p~n" 
      (:: $Value)))



  (= (advice-com $Move $SIn $_)  
    (timing (get-advices $As $SIn))
    (ppl $As))


  (= (advice-com $_ $SIn $SOut $Row $Col)  
    (with-alpha-squares (gsquare $Square (:: ( $Row , $Col )) Nil))
    (timing (show-local-advices $Piece $Square $SIn)))
	  


  (= (advisor-weight $Adv $Weight)  
    (advisor $Adv $_)
    (advisor-weight $Adv $Weight $_))


  (= show-advisors  
    (format "Advisors: ~n" Nil)
    (whenever 
      (advisor-weight $Adv $Weight) 
      (format "<~p>: ~p~n" 
        (:: $Adv $Weight))))


  (= (alladvisors-com $_ $_ $_)  show-advisors)

  (= alladvisors-top  show-advisors)



  (= (seta-com $_ $_ $_)  show-active-advisors)


  (= seta-top  show-active-advisors)
	

  (= (active-com $_ $_ $_)  show-active-advisors)


  (= active-top  show-active-advisors)



  (= show-active-advisors  
    (format "Active Advisors: ~n" Nil)
    (whenever 
      (, 
        (advisor-weight $Adv $Weight) 
        (\== $Weight 0)) 
      (format "<~p>: ~p~n" 
        (:: $Adv $Weight))))

