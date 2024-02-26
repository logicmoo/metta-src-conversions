;                                           (convert_to_metta_file  dominate $_206796 metagame/learning/dominate.pl metagame/learning/dominate.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================


  (= (attacks-i $PieceAI $SqAI $SqTI $PieceVI $SqVI $S)  
    (attacks $PieceA $SqA $SqT $PieceV $SqV $S)
    (piece-index $PieceA $PieceAI)
    (piece-index $PieceV $PieceVI)
    (square-index $SqA $SqAI)
    (square-index $SqV $SqVI))




  (= (attacks $PieceA $SqA $SqT $PieceV $SqV $S)  
    (det-if-then-else 
      (or 
        (nonvar $SqA) 
        (nonvar $SqT)) True 
      (board-square $SqA))
    (attacks1 $PieceA $SqA $SqT $PieceV $SqV $S))


;
; PieceA is on SqA in the
;
; position, and he can take PieceV on SqV by moving to SqT.
;
; (Need to set the right player in control to do this correctly).


  (= (attacks0 $PieceA $SqA $SqT $PieceV $SqV $S)  
    (blank-state $S)
    (player-role $Player)
    (control $Player $S)
    (owns $PieceA $Player)
    (on $PieceA $SqA $S)
    (captures $PieceA $Player $SqA $SqT $Effect $Captured $S)
    (captured-piece $PieceV $SqV $Captured))


  (= (attacks1 $PieceA $SqA $SqT $PieceV $SqV $S)  
    (blank-state-if $S)
    (player-role $Player)
    (put-control $Player $S $S1)
    (owns $PieceA $Player)
    (captures $PieceA $Player $SqA $SqT $Effect $Captured $S1)
    (captured-piece $PieceV $SqV $Captured))
; /* attacks1(PieceA,SqA,SqT,PieceV,SqV,S) :- blank_state_if(S), player_role(Player), control(Player,S), owns(PieceA,Player), captures(PieceA,Player,SqA,SqT,_Effect,Captured,S), captured_piece(PieceV,SqV,Captured). */
; ; If PieceA were on SqA and Player in control in the ; position, he could take PieceV on SqV by moving to SqT. ;

;
; This is very inefficient if we know the position already,
;
; as it first genetes hypothetical attacks, and then sees if the
;
; piece really is on the board!

  (= (attackshow $PieceA $SqA $SqT $PieceV $SqV $S)  
    (attacks $PieceA $SqA $SqT $PieceV $SqV $S)
    (on $PieceA $SqA $S))



  (= (blank-state-if $S)  
    (det-if-then-else 
      (var $S) 
      (blank-state $S) True))



  (= (blank-state $S)  
    (blank-state $Player $Stage 0 $S))

  (= (blank-state $Player $Stage $Move $S)  
    (new-state $S0)
    (initialize-state-properties $Stage $Player $Move $S0 $S))


;
; PA on SqA is Dist moves from a square SqI on which it
;
; would attack PV on SqV, in State.
;
; 

  (= (attack-distance $PA $SqA $SqI $PV $SqV $Dist $State)  
    (attacks $PA $SqI $SqT $PV $SqV $State)
    (square-piece-distance $SqA $PA $SqI $Dist))
; ; attackshow(PA,SqI,_SqT,PV,SqV,State),


  (= (closest-attack $PA $SqA $SqI $PV $SqV $Dist $State $Tables)  
    (setof 
      (- 
        (- $Dist1 $SqI) $State) 
      (attack-distance $PA $SqA $SqI $PV $SqV $Dist1 $State) $Places)
    (= $Places 
      (Cons  
        (- 
          (- $Dist $SqI) $State) $Rest)))
	   



  (= (testa $Dist $State)  
    (attack-distance 
      (piece king player) 
      (square 5 3) $SqI 
      (piece man opponent) 
      (square 2 2) $Dist $State))



  (= (dominate-value $PieceA $SqA $PieceV $SqV $Goal $Value $Pos $Tables)  
    (blank-state-if $Pos)
    (game-player-has-goal $_ $Player $Goal)
    (opposite-role $Player $Opponent)
    (eradicate-goal $Goal $Opponent $Type)
    (eradicate-goal $Goal $Descr)
    (weighted-dominate $Descr $PieceA $Player $SqA $PieceV $SqV $Value $Pos $Tables))
; /* ; A piece is dominated to the extent that there are enemy pieces close to ; its square and squares it can move to. ; The simplest way to do this analysis may be to build a graph which says for ; each square, the pieces (now on the board?) which would attack it if they got to ; some square. ; Well, first find the target squares. Consider each one separately. ; Dominating a target square: ; For each piece type, find the squares it would be on to attack the target. ; Then consider the pieces on the board separately. Look up their entry (set of ; squares) in this table, find the distance to each of these, and take the minimum. ; Score some points which decrease as the distance gets farther. This can be done most simply to start by having just 1 piece on the board. Domination is slightly worse for the player who has to move, because of Zugswang. [?] Also important for domination is the number of pieces left to capture before the goal is achieved. Thus when a player has just one king, this is vital to protect, whereas when he has lots of checkers, dominating any 1 isn't that crucial until the numbers are smaller. To model this, we should first count the number matching this goal, and then weight the resulting domination value for each target piece by some value which decreases as the number of such targets increases. In fact, maybe don't even think about domination until the number of targets is smaller. */
; /* dominate_value(PieceA,SqA,PieceV,SqV,Goal,Value,Pos,Tables) :- game_player_has_goal(_,Player,Goal), eradicate_goal(Goal,Opponent,_Type), eradicate_goal(Goal,Descr), opposite_role(Player,Opponent), dominate_val1(PieceA,Player,SqA,PieceV,SqV,Descr,Value,Pos,Tables). dominate_val1(PieceA,Player,SqA,PieceV,SqV,Descr,Value,Pos,Tables) :- blank_state_if(Pos), on(PieceV,_,SqV,Pos), matches(Descr,PieceV), closest_attack(PieceA,SqA,_SqI,PieceV,SqV,Dist,_State,Tables), distance_value(Dist,Val), negate_for_player(Player,Val,Value). */

;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  metagame/learning/dominate.pl 158 13 5055))
  

;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  metagame/learning/dominate.pl 163 13 5214))




  (= (dominate-val1 $PieceA $Player $SqA $PieceV $SqV $Value $Pos $Tables)  
    (blank-state-if $Pos)
    (closest-attack $PieceA $SqA $SqI $PieceV $SqV $Dist $State $Tables)
    (distance-value $Dist $Val)
    (negate-for-player $Player $Val $Value))
	



  (= (matching-square $Piece $Sq $Descr $Pos)  
    (on $Piece $_ $Sq $Pos)
    (matches $Descr $Piece))
	


  (= (enough-target-urgency $Targets $Weight)  
    (length $Targets $Length)
    (< $Length 4)
    (is $Weight 
      (/ 1 $Length)))
; ; Make a param




  (= (test2 $PV $SV $Val $S)  
    (checkpoint test $S)
    (dominate-value 
      (piece king player) 
      (square 4 2) $PV $SV $Goal $Val $S))


  (= (test3)  
    (setof 
      (, $A $B $C $D $E) 
      (^ $S 
        (attacks $A $B $C $D $E $S)) $Sets)
    (ppl $Sets))


  (= (test4)  
    (setof 
      (, $D $A $E $B) 
      (^ 
        (, $C $S) 
        (attacks $A $B $C $D $E $S)) $Sets)
    (ppl $Sets))
; /* black king ,( a , 1 ) ,( c , 3 ) ,white king ,( b , 2 ) black king ,( a , 1 ) ,( c , 3 ) ,white man ,( b , 2 ) black king ,( a , 2 ) ,( c , 4 ) ,white king ,( b , 3 ) black king ,( a , 2 ) ,( c , 4 ) ,white man ,( b , 3 ) */



  (= (test5)  
    (setof 
      (, $D $A $E $B) 
      (^ 
        (, $C $S) 
        (attacks-i $A $B $C $D $E $S)) $Sets)
    (ppl $Sets))


  (= (attackset $Attacks $State)  
    (setof 
      (^ $PieceV 
        (^ $PieceA 
          (^ $SqV $SqA))) 
      (^ 
        (, $SqT $State) 
        (attacks-i $PieceA $SqA $SqT $PieceV $SqV $State)) $Attacks))
; /* 1,3,45,38 1,3,45,52 1,3,45,54 1,3,46,37 1,3,46,39 */


;
; Attackset gives us: [Targ^Attack^TargSq^AttackSq|Rest]

  (= (dom-table $DomTable $State)  
    (attackset $Attacks $State)
    (do-graph $Attacks $GroupedTargs)
    (group-attacks $GroupedTargs $DomTable))

;
; Grouped attacks gives us: [Targ-[Attack^TargSq^AttackSq|RestAttacks]|RestTargs]


  (= (group_attacks  () ())  True)
  (= (group-attacks (Cons  (- $T $As) $Rest) (Cons  (- $T $GroupedAs) $GRest))  
    (do-graph $As $Grouped1)
    (group-targsq $Grouped1 $GroupedAs)
    (group-attacks $Rest $GRest))


  (= (group_targsq  () ())  True)
  (= (group-targsq (Cons  (- $A $Sqs) $Rest) (Cons  (- $A $GroupedSqs) $GRest))  
    (do-graph $Sqs $GroupedSqs)
    (group-targsq $Rest $GRest))



