;                                           (convert-to-metta-file  global $_316216 metagame/learning/global.pl metagame/learning/global.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; global.pl


  (= (move-threat $Piece $Player $SqF $SqT $Tables)  
    (moving-table $Tables $MTable)
    (member 
      (move $Piece $Player $SqF $SqT) $MTable))
; /* Dealing with global analysis of threats and piece moves. This should build a table listing which squares each piece could move to (if present as feature) and which squares each piece could capture. Then we can analyze threats for player (to move) and opponent (next move). Player gets the max piece he is attacking, plus any special effects. One special effect is that if he threatens to achieve a goal and it is his move, he wins. Next mover gets 2nd max piece he is threatening. If only threatening 1 piece, gets a wee fraction of value for it. */
; ;================================================================================ ; Moves and Captures using Dynamic Move-Tables ;================================================================================ ; Note these are the same as you would get using the position, but ; instead rely on the tables built from the position. ; However they don't include ones which lose an arrival goal immediately. ; Also, the capturing routines only accurate when just 1 piece would be ; captured, as they leave the rest on the board.


  (= (capture-threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect $Tables)  
    (capturing-table $Tables $MTable)
    (member 
      (threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect) $MTable))


;
; These actually provide the new output state.

  (= (done-move-threat $Piece $Player $SqF $SqT $Pos $PosOut $Tables)  
    (move-threat $Piece $Player $SqF $SqT $Tables)
    (do-move $Piece $SqF $SqT $Pos $PosOut))


  (= (done-capture-threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect $Pos $PosOut $Tables)  
    (capture-threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect $Tables)
    (do-capture $PieceA $SqA $SqT $PieceV $SqV $Pos $PosOut))




  (= (do-move $Piece $SqF $SqT $Pos $PosOut)  
    (lift-piece $Piece $SqF $Pos $Pos1)
    (place-piece $Piece $SqT $Pos1 $PosOut))



  (= (do-capture $Piece $SqF $SqT $PieceV $SqV $Pos $PosOut)  
    (lift-piece $Piece $SqF $Pos $Pos1)
    (det-if-then-else 
      (\== $SqV $SqT) 
      (lift-piece $PieceV $SqV $Pos1 $Pos2) 
      (= $Pos1 $Pos2))
    (place-piece $Piece $SqT $Pos2 $PosOut))
	



;
; ================================================================================
;
; Global Captures
;
; ================================================================================


;
; The unique set of capturing moves available in the
;
; current position.

  (= (capture-table $Table $S)  
    (findall 
      (threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect) 
      (threatens $PieceA $Player $SqA $SqT $PieceV $SqV $Effect $S) $Table1)
    (remove-duplicates $Table1 $Table))
	  

  (= (unique-threats $Threats $Unique)  
    (findall 
      (target $Player $PieceV $SqV $Effect) 
      (member 
        (threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect) $Threats) $Squares)
    (remove-duplicates $Squares $Unique))


;
; UNIQUE is the set of target<Player,PieceV,SqV,Effect>
;
; tuples for a given PLAYER, where he has some piece
;
; which can capture PIECEV on SqV with effect Effect.
;
; Uses the capture table already constructed.
;
; Thus unlike player_threats/3 below, here we just collect
;
; the unique victim/effect pairs, even if a victim could
;
; be captured by multiple attackers.

  (= (unique-threats $Player $Threats $Unique)  
    (player-role $Player)
    (findall 
      (target $Player $PieceV $SqV $Effect) 
      (member 
        (threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect) $Threats) $Squares)
    (remove-duplicates $Squares $Unique))



  (= (player-threats $Player $Threats $PThreats)  
    (player-role $Player)
    (findall 
      (threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect) 
      (member 
        (threat $PieceA $Player $SqA $SqT $PieceV $SqV $Effect) $Threats) $PThreats))


;
; We check destination square not excluded, as otherwise not a real
;
; threat.

  (= (threatens $PieceA $Player $SqA $SqT $PieceV $SqV $Effect $S)  
    (on $PieceA $SqA $S)
    (owns $PieceA $Player)
    (put-control-if $Player $S $S1)
    (captures $PieceA $Player $SqA $SqT $Effect $Captured $S1)
    (not (excluded-to $PieceA $SqT))
    (captured-piece $PieceV $SqV $Captured))

	
;
; ================================================================================
;
; Global Threats
;
; ================================================================================


  (= (gthreat $Player $Value $Position $Tables)  
    (capturing-table $Tables $MTable)
    (unique-threats $Player $MTable $Unique)
    (\== $Unique Nil)
    (eval-threats $Unique $Player $Evaled $Position $Tables)
    (keysort-for-player $Player $Evaled $Ordered)
    (threat-outcome $Player $Ordered $Value $Position $Tables))
; /* 1. If piece attacks > 1 target, value is 2nd max target value. 2. If 2 pieces attack 1 target, no clear value (they may not protect each other from defenders). 3. If 2 pieces attacks different targets, value is 2nd max value. Thus we don't need this local to each piece. Just count targets/effects, score each, take top 2, allow for defense of the first one, score (discounted?) value of 2nd one. If there was only one, give some annoyance credit based on its value, perhaps as we currently do. Actually, that whole analysis applies only to player not on move -- player on move can just take max-valued piece with no worries, and then position probably changes so much that other threats are irrelevant. So if you fork my knight and rook, and instead of moving one of them I attack your queen with a pawn, I gain some points for the queen attack, but know that I am going to lose the knight almost certainly. Note this whole analysis is based on assumption that player can only block 1 threat, and also that we can only execute 1 threat at a time. But suppose we capture by hopping over some whole row of pieces. Then if he moves 1 we may still be able to take the rest. Then again, what do you want for a heuristic anyway? */


  (= (eval-threats $Unique $Player $Evaled $Position $Tables)  
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
; Shutting down the threat feature to avoid nesting,
;
; which was too costly although it gave some finer discriminations.

  (= (local-threat-evaluation $Victim $SqV $Val $S $Tables)  
    (shutdown-advisor threat $Tables)
    (= $S1 $S)
    (local-evaluation $Victim $SqV $Val $S1 $Tables))



;
; EFFECT_THREAT_EVALUATION(+Effect,+Player,+PieceV,+SqV,-Value,+Position,+Tables)
;
; 
;
; The value of having a threat against a piece:
;
; 
;
; 1. Piece has some value V on the board.
;
; 2. Piece gets removed from board. This negates the value: -V.
;
; 3. Piece may get possessed by some player. We then really want
;
; possess_value(Possessor,Piece,PosVal)
;
; Just as defined in possess routine when we really have it in hand.
;
; but this seems too expensive to use in threats.
;
; A simple approximation would be to use its static value, but we won't
;
; always have computed that if it is not being used.
;
; a. So instead, we could just say for now that if the same player
;
; possesses it after as before, its original val gets added back:
;
; PosVal = V.
;
; In effect, this means there is no threat in capturing an opponent's
;
; piece if he possesses it right back.
;
; b. If the capturing player gets it, he in effect makes the value at
;
; least twice negated: value: -2V; once for removing it, and once for
;
; having it in his own hand.
;
; 4. If this is not good for player in the end, we just fail, as no threat.
;
; 
;
; Note that we must negate value on board here, as a threat for a player is the negative
;
; of the value of the threatened piece.
;
; Also, we ensure it would be favorable at all to player, else it is not a threat.
;
; [** If must_capture, perhaps should consider even bad values we may have to take]
;
; 
;
; 
;
; [More thoughts ...]
;
; If a piece has a certain value to the opponent, and we are threatening
;
; to capture it under some effect, what is the value to us?
;
; a. Suppose we remove it.
;
; - Removing it negates the value (ie eliminates that component from the position).
;
; - If it was an eradicate target for us, that adds some value
;
; (the more the closer to last it is?)
;
; b. Suppose we possess it.
;
; - Same as remove, but perhaps multiplied for giving us more options?
;
; - Could give big bonus if we can place it on goal square.
;
; c. Suppose opponent possess it.
;
; - Could help to get rid of our pieces as they become opponent's.
;
; 
;
; 

  (= (effect-threat-evaluation $Effect $Player $PieceV $SqV $Value $Position $Tables)  
    (local-threat-evaluation $PieceV $SqV $LVal $Position $Tables)
    (is $RemVal 
      (- $LVal))
    (effect-val $Effect $Player $PieceV $LVal $EVal $Position $Tables)
    (is $Value 
      (+ $RemVal $EVal))
    (favorable-to-owner $Player $Value))
	

;
; Estimating possession value defined in possess.pl.
;
; Mainly, a threat to give a piece back to its owner has no effect.
;
; And swapping ownership doubles the threat value, as it first removes it from
;
; the victim, and then gives it to the attacker.
;
; 

  (= (effect-val  remove $Player $PieceV $Val 0 $Position $Tables)  True)
  (= (effect-val (possess $Possessor) $Player $PieceV $LVal $EValue $Position $Tables)  
    (estimate-possess-value $Possessor $PieceV $LVal $EValue $Position $Tables))


;
; If player in control, gets (discounted?) max threat value.
;
; If player not:
;
; If > 1 threat, gets (discounted?) 2nd max threat value.
;
; If 1 threat only, gets (more discounted?) value of it??
;
; Could only use for case when >1 threats, and take only threat
;
; as just capmobility??


  (= (threat-outcome $Player $Ordered $Value $Position $Tables)  
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
; ================================================================================
;
; Local Threats
;
; ================================================================================

;
; The value of having a local threat on a piece is the amount which would be increased
;
; for the threatener if that piece disappeared (or 0 if that is negative),
;
; discounted by an offset based on whether the threat can be executed now or
;
; if the victim gets a chance to defend.
;
; 
;
; This advisor was generally replaced by the global threat (gthreat),
;
; but is kept intact for comparison purposes. Perhaps in some game this would
;
; be the better option.


  (= (local-threat-value $Piece $Square $PieceV $SqV $Value $Pos $Tables)  
    (owns $Piece $Player)
    (capture-threat $Piece $Player $Square $SqT $PieceV $SqV $Effect $Tables)
    (local-threat-value $Player $Piece $Square $SqT $PieceV $SqV $Effect $Value $Pos $Tables))

  (= (local-threat-value $Player $Piece $Square $SqT $Victim $SqV $Effect $Value $S $Tables)  
    (effect-threat-evaluation $Effect $Player $Victim $SqV $Val $S $Tables)
    (favor-control $Player $Val $Value $S)
    (tracing-anal-format lthreat "<~p>: ~p -> ~p x ~p ~p (~p) [~p]~n" 
      (:: $Piece $Square $SqT $Victim $SqV $Effect $Value)))   


;
; ================================================================================
;
; Global Moves
;
; ================================================================================




  (= (put-control-if $P $S $S1)  
    (det-if-then-else 
      (control $P $S) 
      (= $S $S1) 
      (put-control $P $S $S1)))




;
; The unique set of non-capturing moves available in the
;
; current position.

  (= (move-table $Table $S)  
    (findall 
      (move $PieceA $Player $SqA $SqT) 
      (could-move $PieceA $Player $SqA $SqT $S) $Table1)
    (remove-duplicates $Table1 $Table))
	  

  (= (unique-moves $Moves $Unique)  
    (findall 
      (target $Player $PieceA $SqA $SqT) 
      (member 
        (move $PieceA $Player $SqA $SqT) $Moves) $Squares)
    (remove-duplicates $Squares $Unique))


;
; UNIQUE is the set of target<Player,Piece,SqF,SqT>
;
; tuples for a given PLAYER, where he has a
;
; PIECE move from SqF to SqT.
;
; Uses the move table already constructed.
;
; 
;
; Don't need to remove-dups here because the move-table
;
; already has done this; we're just sorting through the
;
; ones which belong to a given player.

  (= (unique-moves $Player $Moves $Targets)  
    (player-role $Player)
    (unique-moves $Player $PieceA $SqA $SqT $Moves $Targets))

  (= (unique-moves $Player $PieceA $SqA $SqT $Moves $Targets)  
    (findall 
      (target $Player $PieceA $SqA $SqT) 
      (member 
        (move $PieceA $Player $SqA $SqT) $Moves) $Targets))


;
; We check destination square not excluded, as otherwise not a real
;
; possibility.

  (= (could-move $PieceA $Player $SqA $SqT $S)  
    (on $PieceA $SqA $S)
    (owns $PieceA $Player)
    (put-control-if $Player $S $S1)
    (moves $PieceA $Player $SqA $SqT $S1)
    (not (excluded-to $PieceA $SqT)))



;
; ========================================================================
;
; Gmob counts the UNIQUE moving-moves a player has.
;
; (ie if can move piece by 2 paths to target, doesn't
;
; increase mobility.


  (= (gmobility $Player $Val $Position $Tables)  
    (moving-table $Tables $MTable)
    (unique-moves $Player $MTable $Moves)
    (length $Moves $Val))

;
; Gcapmob counts all the threats, possibly counting
;
; the same victim multiply when attacked by different
;
; pieces, or under different effects.

  (= (gcapmobility $Player $Val $Position $Tables)  
    (capturing-table $Tables $MTable)
    (player-threats $Player $MTable $Moves)
    (length $Moves $Val))





;
; ========================================================================


  (= (add-dynamic-tables-if $S $Tables)  
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
; Should change some names here to avoid confusion!

  (= (add-dynamic-tables $S $Tables)  
    (det-if-then-else movtable 
      (add-moving-table-if $S $Tables) True)
    (det-if-then-else captable 
      (add-capturing-table-if $S $Tables) True))
; ; tracing_anal_format(dynamic,"Building <dynamic> tables ...~n",[]),



  (= (add-capturing-table-if $S $Tables)  
    (det-if-then-else 
      (, 
        (capturing-table $Tables $Table) 
        (var $Table)) 
      (, 
        (tracing-anal-format dynamic "Building <capture> table ...~n" Nil) 
        (tracing-anal-timing dynamic 
          (add-capturing-table $S $Tables))) True))



  (= (add-moving-table-if $S $Tables)  
    (det-if-then-else 
      (, 
        (moving-table $Tables $Table) 
        (var $Table)) 
      (, 
        (tracing-anal-format dynamic "Building <move> table ...~n" Nil) 
        (tracing-anal-timing dynamic 
          (add-moving-table $S $Tables))) True))


  (= (add-moving-table $S $Tables)  
    (move-table $Table $S)
    (moving-table $Tables $Table))


  (= (add-capturing-table $S $Tables)  
    (capture-table $Table $S)
    (capturing-table $Tables $Table))





