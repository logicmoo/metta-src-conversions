;                                           (convert-to-metta-file  exclude $_215464 metagame/learning/exclude.pl metagame/learning/exclude.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; exclude.pl
;
; ;; Filtering piece movement transitions based on goals.
;
; ;; The only external module here is SAFE_TRANSITION_TYPE/8.

;
; GOAL_SQUARE(Piece,Sq,Player,Goal)
;
; Player has goal Goal to get Piece to Sq (in the current game).

  (= (goal-square $Piece $Sq $Player $Goal)  
    (game-player-has-goal $_ $Player $Goal)
    (arrive-goal $Goal $Descr $Squares)
    (matches $Descr $Piece)
    (member1 $Sq $Squares))




  (= (safe-transitions-type $Piece $PieceIndex $Sq $SqIndex $Type $SqTIndices $State)  
    (square-index $Sq $SqIndex)
    (det-if-then-else 
      (setof $SqTIndex 
        (^ $SqT 
          (safe-transition-type $Piece $PieceIndex $Sq $SqIndex $SqT $SqTIndex $Type $State)) $SqTIndices) True 
      (= $SqTIndices Nil)))


  (= (safe-transition $Piece $Sq $SqT $State)  
    (safe-transition $Piece $PieceIndex $Sq $SquareIndex $SqT $SqTIndex $State))
	  
  (= (safe-transition $Piece $PieceIndex $Sq $SquareIndex $SqT $SqTIndex $State)  
    (safe-transition-type $Piece $PieceIndex $Sq $SquareIndex $SqT $SqTIndex moving $State))


;
; SAFE_TRANSITION_TYPE(Piece,PieceIndex,Sq,SquareIndex,SqT,SqTIndex,Type,State)
;
; 
;
; Check here before calling piece-movement that Sq is not a goal square for Piece
;
; (for either player), as if it is these transitions are irrelevant as the game will
;
; end first.
;
; Then check that target square SqT is not necessarily a *loss* (goal square for the
;
; enemy alone) as if it is this transition will likely never be made.
;
; Not sure about this:
;
; I might capture your piece by landing on a losing square, thus ending the
;
; game in a draw (but we're probably not doing captures here!).
;
; 

  (= (safe-transition-type $Piece $PieceIndex $Sq $SquareIndex $SqT $SqTIndex $Type $State)  
    (square-index $Sq $SquareIndex)
    (piece-index $Piece $PieceIndex)
    (not (excluded-from $Piece $Sq))
    (piece-move-for-type $Type $Piece $Sq $SqT $State)
    (not (excluded-to $Piece $SqT))
    (square-index $SqT $SqTIndex)
    (tracing-anal-format detailed "~p: ~p -> ~p~n" 
      (:: $Piece $Sq $SqT)))
; ; new_empty_state(State), ; for testing



;
; A piece will never move from a goal square for either player,
;
; as the game will already have ended.

  (= (excluded-from $Piece $Sq)  
    (goal-square $Piece $Sq $Player $Goal)) 
	
;
; A piece will never move to a goal square for the opponent,
;
; unless immediately on that square it is able to force promotion into
;
; a piece which is not *only* an opponent goal on that square.
;
; 
;
; Similarly, a piece will never move onto a square where it is forced to promote
;
; into a goal for the opponent.

  (= (excluded-to $Piece $SqT)  
    (owns $Piece $Player)
    (goal-square $Piece $SqT $Opp $_)
    (opposite-role $Player $Opp)
    (not (goal-square $Piece $SqT $Player $_))
    (not (safe-promotion $Piece $Player $SqT))
    (tracing-anal-format filter "Filter: <~p> can't safely move to ~p~n" 
      (:: $Piece $SqT)))
  (= (excluded-to $Piece $SqT)  
    (owns $Piece $Player)
    (player-promotion-square $Player $SqT)
    (not (safe-prom1 $Piece $Player $SqT))
    (tracing-anal-format filter "Filter: <~p> can't safely promote on ~p~n" 
      (:: $Piece $SqT)))

	
;
; A safe promotion occurs when we are in promotion zone and either:
;
; 1. We choose the promotion, and there is some non-losing choice.
;
; 2. Opponent chooses, and there is no losing choice.


  (= (safe-promotion $Piece $Player $SqT)  
    (owns $Piece $Player)
    (player-promotion-square $Player $SqT)
    (safe-prom1 $Piece $Player $SqT))

;
; Case 1: we can choose safely.
;
; Case 2: opponent must choose safely.

  (= (safe-prom1 $Piece $Player $SqT)  
    (player-safe-prom $Piece $Player $SqT))
  (= (safe-prom1 $Piece $Player $SqT)  
    (opponent-safe-prom $Piece $Player $SqT))



  (= (player-safe-prom $Piece $Player $SqT)  
    (promotes-into $Piece $PieceT $Player $Player)
    (opposite-role $Player $Opp)
    (not (goal-square $PieceT $SqT $Opp $_)))



  (= (opponent-safe-prom $Piece $Player $SqT)  
    (opponent-promotes $Piece)
    (not (opponent-wins-prom $Piece $Player $SqT)))
; /* Moved this to prom.pl. ; Player can promote Piece to PieceT on SqT. ; Backtracks over all SqT. player_safe_new_prom(Piece,Player,PieceT,SqT) :- promotes_into(Piece,PieceT,Player,Player), Piece \== PieceT, player_promotion_square(Player,SqT), opposite_role(Player,Opp), \+ goal_square(PieceT,SqT,Opp,_). */


  (= (opponent-wins-prom $Piece $Player $SqT)  
    (promotes-into $Piece $PieceT $Player $Opp)
    (opposite-role $Player $Opp)
    (goal-square $PieceT $SqT $Opp $_))


	

;
; arrive_value(Piece,Sq,SqT,Goal,Value,Pos) :-
;
; game_player_has_goal(_,Player,Goal),
;
; arrive_goal(Goal,Player,_Type,_Sqs),
;
; arrive_goal(Goal,Descr,Squares),

