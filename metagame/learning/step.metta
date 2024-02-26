;                                           (convert-to-metta-file  step $_27416 metagame/learning/step.pl metagame/learning/step.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; step.pl


;
; --------------------------------------------------------------------------------
;
; Piece has independent static advice for each player.

;
; --------------------------------------------------------------------------------
;
; ind_advice(Player,Piece,Advice,Tables)
;
; --------------------------------------------------------------------------------
;
; 
;
; Nondeterm routine, tells us various sources of static advice accrued to Player
;
; by having a piece on the board owned by him, independent of the board
;
; and of possible interactions between Piece and other pieces.
;
; 
;
; o Useful to have pieces which can capture many victims (including pieces of
;
; his own). The more pieces, the better. (victims)
;
; o Useful to have piece which cannot be captured by many enemy pieces. (immunity)
;
; o Useful to have piece which can be captured by many of our own pieces (giveaway)
;
; o Useful to have a piece with high max static mobility, or (max_static_mob)
;
; high max eventual mobility. (max_eventual_mob)
;
; o Useful to have a piece with high average static mobility, or (avg_static_mob)
;
; high average eventual mobility. (avg_eventual_mob)
;
; o Bad to have a piece when *we* have a goal to eradicate it. (Also converse) (eradicate)
;
; o Bad to have a piece when goal is stalemate ourself. (Also converse) (stalemate)
;
; o Good to have piece if we have an arrival goal for it, (arrive)
;
; or if it can generate such a one by promotion.
;
; -- In this case, the value of this as a target decreases with the number of intermediate proms
;
; necessary to create a piece matching the arrive goal. (Also converse)
;
; A separate rule does the opposite, when we generate a goal for the opponent, and
;
; encourages us to stay away.
;
; *. Also good to player-possess-capture a piece (owned by either player) for which we have
;
; an arrive goal! Perhaps use generator fn to measure how good to capture.
;
; Note: There is a recurrence here which says useful to capture enemy piece if it can capture
;
; our piece which is useful to capture it.

  (= (ind-advice $Player $Piece (advice victims $Piece $Value) $Tables)  
    (victim-counts $Piece $Value))
  (= (ind-advice $Player $Piece (advice immunity $Piece $Value) $Tables)  
    (immunity-value $Player $Piece $Value))
  (= (ind-advice $Player $Piece (advice giveaway $Piece $Value) $Tables)  
    (giveaway-value $Player $Piece $Value))
  (= (ind-advice $Player $Piece (advice max-static-mob $Piece $Value) $Tables)  
    (max-static-mob $Piece $Value $Tables))
  (= (ind-advice $Player $Piece (advice max-eventual-mob $Piece $Value) $Tables)  
    (max-eventual-mob $Piece $Value $Tables))
  (= (ind-advice $Player $Piece (advice avg-static-mob $Piece $Value) $Tables)  
    (average-static-mob $Piece $Value $Tables))
  (= (ind-advice $Player $Piece (advice avg-eventual-mob $Piece $Value) $Tables)  
    (average-eventual-mob $Piece $Value $Tables))
  (= (ind-advice $Player $Piece (advice eradicate (- $Player $Piece) $Value) $Tables)  
    (player-eradicate-target $Player $Piece)
    (= $Value -1))
  (= (ind-advice $Player $Piece (advice eradicate (- $Opp $Piece) $Value) $Tables)  
    (opposite-role $Player $Opp)
    (player-eradicate-target $Opp $Piece)
    (= $Value 1))
  (= (ind-advice $Player $Piece (advice stalemate $Player $Value) $Tables)  
    (player-stalemate-target $Player $Piece)
    (= $Value -1))
  (= (ind-advice $Player $Piece (advice stalemate $Opp $Value) $Tables)  
    (opposite-role $Player $Opp)
    (player-stalemate-target $Opp $Piece)
    (= $Value 1))
  (= (ind-advice $Player $Piece (advice arrive (- $Player $Piece) $Value) $Tables)  
    (player-arrive-generator $Player $Piece $Dist $Tables)
    (is $Value 
      (/ 1 
        (+ 1 $Dist))))
  (= (ind-advice $Player $Piece (advice arrive (- (- $Opp $Piece) $Dist) $Value) $Tables)  
    (opposite-role $Player $Opp)
    (player-arrive-generator $Opp $Piece $Dist $Tables)
    (is $Value 
      (/ -1 
        (+ 1 $Dist))))

	
  (= (ind-advice $Player $Piece $Value)  
    (advice-tables $Tables)
    (ind-advice $Player $Piece $Value $Tables))



  (= (independent-advice $Player $Piece $Value $Tables)  
    (owns $Piece $Player)
    (piece-index $Piece $_)
    (ind-advice $Player $Piece $Val1 $Tables)
    (negate-advice-for-player $Player $Val1 $Value)) 

  (= (independent-advice $Player $Piece $Value)  
    (advice-tables $Tables)
    (independent-advice $Player $Piece $Value $Tables))



;
; --------------------------------------------------------------------------------
;
; Mediating independent advice
;
; --------------------------------------------------------------------------------
;
; 
;
; Just like the advice meditation done dynamically, but here applying only to the
;
; static advisors.


  (= (static-evaluation $Piece $Sq $Value $Position $Tables)  
    (get-static-advices $Piece $Sq $Advice $Position $Tables)
    (mediate-advices $Advice $Value $Tables))

	

  (= (get-static-advices $Piece $Sq $Advices $Position $Tables)  
    (piece-index $Piece $_)
    (findall $Advice 
      (independent-advice $Player $Piece $Advice $Tables) $Advices))

  (= (get-static-advices $Piece $Advices)  
    (find-advice-tables $Tables)
    (get-static-advices $Piece $Sq $Advices $Position $Tables))


  (= (show-static-advices $Piece)  
    (get-static-advices $Piece $As)
    (ppl $As))



  (= (show-static-advices-total $Piece)  
    (get-static-advices $Piece $As)
    (ppl $As)
    (mediate-advices $As $Total $_)
    (format "Total (Mediated) for <~p>: ~p~n" 
      (:: $Piece $Total)))
	


  (= (advice-top $Color $Type)  
    (piece $Piece 
      (:: $Color $Type) Nil)
    (show-static-advices-total $Piece))


  (= (adviceold-top $Color $Type)  
    (piece $Piece 
      (:: $Color $Type) Nil)
    (show-static-advices $Piece))
	


;
; --------------------------------------------------------------------------------
;
; Piece has relative static value for each player, based on the independent
;
; values of pieces and certain possible relations between them.
;
; One of these rels is capturing.
;
; 
;
; 
;
; Giving points for pieces based on which other piece types they can capture,
;
; perhaps with a measure of how effective they will be at making these
;
; captures.

;
; Value added to a piece for being able to capture a Victim.
;
; This is a *static* measure, which can be computed before the game
;
; ever starts!
;
; 
;
; At moment we aren't conditioning on different possible effects of the capture,
;
; which may be important.
;
; 

  (= (cap-value $Piece $Player $Victim $Value $Tables)  
    (find-advice-tables-if $Tables)
    (piece-victim $Piece $Player $Victim $VPlayer $Effect)
    (victim-value $Player $Victim $VPlayer $Value $Tables))
	
  (= (cap-value $Piece $Player $Victim $Value)  
    (advice-tables $Tables)
    (cap-value $Piece $Player $Victim $Value $Tables))


;
; How good for Player to be able to capture Piece owned by VPlayer.
;
; If Piece has Pos val for VPlayer, and we remove it, we make
;
; neg val for VPlayer. Then we negate again if VPLAYER \== PLAYER,
;
; as the value will have been wrt the wrong perspective.
;
; 
;
; If this is optional, we don't force the player to take unacceptable
;
; pieces.

  (= (victim-value $Player $Piece $VPlayer $Value $Tables)  
    (independent-advice $VPlayer $Piece $Advice $Tables)
    (advice-victim-value $Advice $Val1 $Tables)
    (remove-option-value $Player $VPlayer $Val1 $Value))


  (= (advice-victim-value $Advice $Val $Tables)  
    (weigh-advice $Advice $Val $Tables))


;
; Removal may be optional or forced. When forced, he must accept
;
; whatever the consequences. When optional, he can choose to accept
;
; only the positive ones.

  (= (remove-option-value $Player $Owner $Val1 $Value)  
    (det-if-then-else 
      (forced-remove $Player $Owner) 
      (forced-remove-value $Player $Val1 $Value) 
      (optional-remove-value $Player $Val1 $Value)))
	
;
; Removal may be forced when the game has a global must_capture rule,
;
; and the victim piece is owned by the enemy. This is becasue he may throw
;
; his pieces at us and force us to absorb the value of them.

  (= (forced-remove $Player $Owner)  
    (current-game-must-capture (\== $Player $Owner)))

;
; In this case, the player has no choice but to accept the loss of material.

  (= (forced-remove-value $Player $Val1 $Value)  
    (is $Value 
      (- $Val1)))
	  
;
; Player has the option of removing Val1 points from the board.
;
; When this is optional, WHITE prefers to take 0 points over
;
; removing positive points, and BLACK prefers to take 0 points
;
; over removing negative points. The result of this decision thus
;
; gives only not-unfavorable points for Player for having this option.
;
; 

  (= (optional-remove-value $Player $Val1 $Value)  
    (is $RemoveVal 
      (- $Val1))
    (negate-for-player $Player $RemoveVal $PVal)
    (max 0 $PVal $OptVal)
    (negate-for-player $Player $OptVal $Value))
	



  (= (piece-victim $Piece $Victim)  
    (piece-victim $Piece $Victim $Effect))

  (= (piece-victim $Piece $Victim $Effect)  
    (piece-victim $Piece $Player $Victim $VPlayer $Effect))

  (= (piece-victim $Piece $Player $Victim $VPlayer $Effect)  
    (piece-victim $Piece $Player $Victim $VPlayer $Effect $Capture))

  (= (piece-victim $Piece $Player $Victim $VPlayer $Effect $Capture)  
    (piece-index $Piece $PI)
    (owns $Piece $Player)
    (owns $Victim $VPlayer)
    (current-game-for-player $Player $Game)
    (game-piece-has-capture $Piece $Capture $Game)
    (capture-type $Capture $Type)
    (capture-effect $Capture $Effect)
    (matches $Type $Victim))



;
; True when the effect of capturing the victim is not opponent possesses.

  (= (threat-piece-victim $Piece $Player $Victim $VPlayer $Effect $Capture)  
    (piece-victim $Piece $Player $Victim $VPlayer $Effect $Capture)
    (not (, (opposite-role $Player $VPlayer) (= $Effect (possess $VPlayer)))))



  (= (victim-counts $Piece $VCount)  
    (unique-victims $Piece $Unique)
    (length $Unique $VCount))
	       


  (= (unique-victims $Piece $Unique)  
    (unique-victims $Piece $Player $Victim $VPlayer $Effect $Unique))

  (= (unique-victims $Piece $Player $Victim $VPlayer $Effect $Unique)  
    (findall $Victim 
      (threat-piece-victim $Piece $Player $Victim $VPlayer $Effect $Cap) $Victims)
    (remove-duplicates $Victims $Unique))



  (= (unique-victimizers $Piece $Unique)  
    (unique-victimizers $Piece $Player $Victim $VPlayer $Effect $Unique))

  (= (unique-victimizers $Piece $Player $Victim $VPlayer $Effect $Unique)  
    (findall $Piece 
      (threat-piece-victim $Piece $Player $Victim $VPlayer $Effect $Cap) $Victims)
    (remove-duplicates $Victims $Unique))




  (= (giveaway-value $Player $Piece $Value)  
    (unique-victimizers $CapPiece $Player $Piece $Player $Effect $Unique)
    (length $Unique $VCount)
    (is $Value $VCount))
; ; piece_type_count(Count),



  (= (immunity-value $Player $Piece $Value)  
    (piece-type-count $Count)
    (opposite-role $Player $Opp)
    (unique-victimizers $CapPiece $Opp $Piece $Player $Effect $Unique)
    (length $Unique $VCount)
    (is $Value 
      (- $Count $VCount)))



;
; True when Player has goal to eradicate piece Piece.

  (= (player-eradicate-target $Player $Piece)  
    (game-player-has-goal $_ $Player $Goal)
    (eradicate-goal $Goal $Descr)
    (matches $Descr $Piece))

;
; True when Player has goal to eradicate piece Piece,
;
; because that helps him to stalemate its owner.

  (= (player-stalemate-target $Player $Piece)  
    (game-player-has-goal $_ $Player $Goal)
    (stalemate-goal $Goal $Owner)
    (owns $Piece $Owner))


;
; True when Player has goal to arrive either Piece or something
;
; Piece could promote into without changing ownership.

  (= (player-arrive-generator $Player $Piece $Dist $Tables)  
    (game-player-has-goal $_ $Player $Goal)
    (arrive-goal $Goal $Descr $Squares)
    (piece-player-prom-distance $Piece $Player $PieceT $Dist $Tables)
    (matches $Descr $PieceT))
	






  (= (max-static-mob $Piece $Val $Tables)  
    (findall $Val1 
      (square-piece-mobility $Sq $Piece $Val1 $Tables) $Mobs)
    (max $Mobs $Val))		   
	       



  (= (max-eventual-mob $Piece $Val $Tables)  
    (findall $Val1 
      (square-piece-reachability $Sq $Piece $Val1 $Tables) $Mobs)
    (max $Mobs $Val))		   





  (= (average-static-mob $Piece $Val $Tables)  
    (findall $Val1 
      (square-piece-mobility $Sq $Piece $Val1 $Tables) $Mobs)
    (average $Mobs $Val))		   
	       



  (= (average-eventual-mob $Piece $Val $Tables)  
    (findall $Val1 
      (square-piece-reachability $Sq $Piece $Val1 $Tables) $Mobs)
    (average $Mobs $Val))		   





;
; --------------------------------------------------------------------------------
;
; Mediating capture advice
;
; --------------------------------------------------------------------------------
;
; 
;
; Just like the advice meditation done dynamically, but here applying only to the
;
; capture advisors.


  (= (capture-evaluation $Piece $Sq $Value $Position $Tables)  
    (get-capture-advices $Piece $Sq $Advice $Position $Tables)
    (mediate-capture-advices $Advice $Value $Tables))


  (= (mediate-capture-advices $Advice $Value $Tables)  
    (sumlist $Advice $Value))



  (= (get-capture-advices $Piece $Sq $Advices $Position $Tables)  
    (piece-index $Piece $_)
    (findall $Advice 
      (cap-value $Piece $Player $Victim $Advice $Tables) $Advices))

  (= (get-capture-advices $Piece $Advices)  
    (find-advice-tables $Tables)
    (get-capture-advices $Piece $Sq $Advices $Position $Tables))


  (= (show-capture-advices $Piece)  
    (get-capture-advices $Piece $As)
    (ppl $As))





;
; ==============================================================================
;
; INDEPENDENT PIECE VALUE MATRIX
;
; ==============================================================================


  (= (build-static-matrix $Matrix)  
    (map-piece-table static-matrix Nil $Matrix))

  (= (build-static-matrix $Matrix $Tables)  
    (map-piece-table static-matrix 
      (:: $Tables) $Matrix))



  (= print-static-matrix  
    (print-static-matrix $Piece $Player $StatVal))

  (= (print-static-matrix $Piece $Player $StatVal)  
    (format "Player Piece Static Matrix:~n" Nil)
    (or 
      (, 
        (piece-player-static $Piece $Player $StatVal) 
        (format "~p: ~p -> ~p~n" 
          (:: $Player $Piece $StatVal)) fail) True))


  (= (pps $Player $Piece $StatVal)  
    (piece-player-static $Piece $Player $StatVal))

;
; PIECE_PLAYER_STATIC(?Piece,?Player,?StatVal)
;
; Very nice table indicating static value for Piece owned by Player.

  (= (piece-player-static $Piece $Player $StatVal)  
    (advice-tables $Tables)
    (piece-player-static $Piece $Player $StatVal $Tables))

;
; PIECE_PLAYER_STATIC(?Piece,?Player,?StatVal,+Tables)
;
; Very nice table indicating static value for Piece owned by Player.
  (= (piece-player-static $Piece $Player $StatVal $Tables)  
    (piece-player-static $Piece $PieceI $Player $PlayerI $StatVal $Tables))

  (= (piece-player-static $Piece $PieceI $Player $PlayerI $StatVal $Tables)  
    (piece-index $Piece $PieceI)
    (owns $Piece $Player)
    (piece-static-value $PieceI $StatVal $Tables))


  (= (piece-static-value $PieceI $StatVal $Tables)  
    (static-matrix $Tables $M)
    (piece-static-val1 $PieceI $StatVal $M))


  (= (piece-static-val1 $PieceI $StatVal $M)  
    (pindex-table-entry $PieceI $M $StatVal))



  (= (static-matrix $Piece $PIndex $Value)  
    (find-advice-tables $Tables)
    (static-matrix $Piece $PIndex $Value $Tables))

;
; Tables not last so can be used with map_piece_table.
  (= (static-matrix $Piece $PIndex $Tables $Value)  
    (piece-index $Piece $PIndex)
    (static-evaluation $Piece $Sq $Value $Position $Tables)
    (tracing-anal-format tables "Built independent matrix for <~p>~n" 
      (:: $Piece)))


