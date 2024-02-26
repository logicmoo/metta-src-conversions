;                                           (convert_to_metta_file  prom $_490622 metagame/learning/prom.pl metagame/learning/prom.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; prom.pl

;
; PROMOTION Strategy
;
; A portion of a piece's value on a square is its possibility
;
; to move and promote into something else.
;
; The value comes from the value derived by getting that promoted piece
;
; on the promotion square, and then deriving whatever intrinsic
;
; value such a piece would have there.
;
; However, we don't actually get the intrinsic value immediately.
;
; 1. Clear the promotion square (and a path to it, perhaps)
;
; 2. Move the piece to the promotion square.
;
; 3. Execute a number of moves in promotion region to create the
;
; desired promoted piece (assuming we could do each in just
;
; 1 move for now).
;
; 
;
; Note that as promoting is usually optional, we should discard those
;
; possibilities which would give the promoting player negative utility.
;
; 
;
; The value of a piece to be promoted is the max value of anything we might
;
; promote it into. This avoids the situation where we don't promote a piece
;
; because we prefer the option of all possible promotions to any one, but then
;
; we get a chess pawn sitting on the 7th rank enjoying its options!
;
; 
;
; For the moment, we may ignore the possibility to promote into the opponent's
;
; on a square which would win us the game, and instead consider only those
;
; promotions which keep the piece in our control.
;
; ==> No, now we even consider promoting to opponent's piece if that gives us
;
; better value.
;
; 
;
; Also, we choose the most accessible square, but independent of choice of
;
; promotion piece. Thus, it may not be the best square for some piece which
;
; would be our best choice piece on some other square.
;
; 


  (= (prom-value $Piece $Sq $SqT $Value $Pos $Tables)  
    (owns $Piece $Player)
    (useful-promotes $Piece $Player $Tables)
    (focussed-promsq-cost $Piece $Player $Sq $SqT $PathCost $Pos $Tables)
    (best-promotion-choice-value $Piece $Player $SqT $PathCost $PieceT $Value $Pos $Tables))




  (= (best-promotion-choice-value $Piece $Player $SqT $PathCost $BestPiece $BestValue $Pos $Tables)  
    (findall 
      (- $Value $PieceT) 
      (promotion-choice-value $Piece $Player $SqT $PathCost $PieceT $Value $Pos $Tables) $Pairs)
    (best-player-choice $Player $Pairs $BestPiece $BestValue))	   


  (= (promotion-choice-value $Piece $Player $SqT $PathCost $PieceT $Value $Pos $Tables)  
    (piece-player-prom-distance $Piece $Player $PieceT $PromDist $Tables)
    (\== $PieceT $Piece)
    (intrinsic-value $PieceT $SqT $IVal $Pos $Tables)
    (prom-likelihood $PromDist $PathCost $Prob)
    (reasonable-likelihood $Prob)
    (expected-value $Prob $IVal $Value)
    (favorable-to-owner $Player $Value))
; ; Ie PromDist > 0




  (= (useful-promotes $Piece $Player $Tables)  
    (det-if-then 
      (player-safe-new-prom $Piece $Player $PieceT $SqT $Tables) True)) 


;
; Player can promote Piece to PieceT on SqT.
;
; Backtracks over all SqT.

  (= (player-safe-new-prom $Piece $Player $PieceT $SqT $Tables)  
    (piece-player-promotion $Piece $Player $PieceT $Tables)
    (\== $Piece $PieceT)
    (opposite-role $Player $Opp)
    (player-promotion-square $Player $SqT)
    (not (goal-square $PieceT $SqT $Opp $_)))


	
;
; Of the nearest prom squares, choose the one most accessible
;
; in the current position.
;
; This is the one with the lowest cost to clear it.
;
; 
;
; [ Technically we shouldn't choose among only the nearest,
;
; as one may be farther but empty, and this would be preferable.
;
; However, we don't want to search through all of promotion region
;
; and do this comparison if we can avoid it. ]


  (= (focussed-promsq-cost $Piece $Player $Sq $SqT $Cost $Pos $Tables)  
    (square-index $Sq $SqI)
    (piece-index $Piece $PieceI)
    (square-piece-psqs $SqI $PieceI $SqTIs $Dist $Tables)
    (cheapest-destination $Piece $Player $Sq $SqTIs $Dist $SqT $Cost $Pos $Tables))

;
; More efficient if we store the target squares, instead of indices,
;
; in promsq matrix.

  (= (cheapest-destination $Piece $Player $Sq $SqTIs $Dist $SqT $Cost $Pos $Tables)  
    (findall 
      (- $Cost1 $SqT1) 
      (, 
        (member $SqTI1 $SqTIs) 
        (square-index $SqT1 $SqTI1) 
        (cost-to-promsq $Piece $Player $Sq $SqT1 $Dist $Cost1 $Pos $Tables)) $Dests)
    (sort $Dests 
      (Cons  
        (- $Cost $SqT) $_)))



  (= (cost-to-promsq $Piece $Player $Sq $SqT $SqDist $PathCost $Pos $Tables)  
    (clear-path-cost $Piece $Player $Sq $SqT $SqDist $PathCost $Pos))
; ; distance_to_promsq(Piece,Sq,SqT,SqDist,Tables),



  (= (distance-to-promsq $Piece $Sq $SqT $SqDist $Tables)  
    (square-piece-promsq $Sq $Piece $SqT $SqDist $Tables))



;
; The likelihood of achieving the promotion decreases
;
; exponentially with the number of moves required to
;
; achieve it.
;
; We subtract 1 from the total as if a piece promotes
;
; immediately into another, the cost is just the path
;
; distance.

  (= (prom-likelihood $PromDist $PathCost $Prob)  
    (is $MinMoves 
      (- 
        (+ $PromDist $PathCost) 1))
    (distance-value $MinMoves $Prob))



;
; This must measure the value independent of promotions
;
; this piece could make, as these will already be taken
;
; into account.
;
; Thus we shut down both the threat analysis and the
;
; other promotions.
;
; Also shutdown dynamic-mob, to save time (if have more time, keep it).

  (= (intrinsic-value $Piece $Sq $Val $Pos $Tables)  
    (shutdown-advisor threat $Tables)
    (shutdown-advisor prom $Tables)
    (shutdown-advisor dynamic-mobility $Tables)
    (local-evaluation $Piece $Sq $Val $Pos $Tables))



  (= (testp)  
    (checkpoint init $S)
    (get-advices $A $S)
    (ppl $A))





;
; ============================================================================
;
; Tables for Promotion Goals
;
; ============================================================================

;
; promotes_into(OrigPiece,PromPiece,Distance)
;
; OrigPiece, if it could use its promotion power each turn,
;
; could promote into PromPiece after Distance turns.
;
; 
;
; We first make a graph, where each node is a piece(Player,Type)
;
; for each type of piece in the game def. We add an edge from P1 to P2
;
; if P1 could promote directly into P2.
;
; From this we can then answer several of these types of questions by
;
; standard graph algorithms.


  (= (promotes-into $Piece $PieceI $Prom $PromI $Player $Chooser)  
    (promotes-into $Piece $Prom $Player $Chooser)
    (piece-index $Piece $PieceI)
    (piece-index $Prom $PromI))
	


  (= (opponent-promotes $Piece)  
    (promotes-into $Piece $_ $Player $Chooser)
    (opposite-role $Player $Chooser))  
	


  (= (promotes-into $OrigPiece $PromPiece $Player $Chooser)  
    (owns $OrigPiece $Player)
    (current-game-for-player $Player $Game)
    (game-piece-promoting $OrigPiece $Promoting $Game)
    (promote-choice $Promoting $Player $Chooser $PromPiece))


  (= (promote-choice $Promoting $Player $Player $PromPiece)  
    (simple-promote $Promoting $Player $PromPiece))
  (= (promote-choice $Promoting $Player $Chooser $PromPiece)  
    (= $Promoting 
      (promote $Chooser $Descr))
    (matches $Descr $PromPiece))

	

  (= (forced-promote $Player $PlI $Piece $PieceI $Prom $PromI)  
    (player-index $Player $PlI)
    (forced-promote $Player $Piece $PieceI $Prom $PromI))

  (= (forced-promote $Player $Piece $PieceI $Prom $PromI)  
    (promotes-into $Piece $PieceI $Prom $PromI $Player $Player))
; ; player_role(Player),


;
; ==============================================================================
;
; Piece-Piece (Immediate Player_Promotion) Matrix
;
; ==============================================================================


  (= (build-promotion-matrix $Matrix)  
    (map-player-table promotion-matrix Nil $Matrix))


  (= (print-promotion-matrix)  
    (print-promotion-matrix $Piece $Player $PieceT))

  (= (print-promotion-matrix $Piece $Player $PieceT)  
    (format "Player Piece Promotion Matrix:~n" Nil)
    (or 
      (, 
        (piece-player-promotion $Piece $Player $PieceT) 
        (format "~p: ~p -> ~p~n" 
          (:: $Player $Piece $PieceT)) 
        (fail)) True))
; ; format("matrix[~p][~p] = ~p~n",[Piece,Player,PieceT]),


  (= (ppp $Player $Piece $PieceT)  
    (piece-player-promotion $Piece $Player $PieceT))

;
; PIECE_PLAYER_PROMOTION(?Piece,?Player,?PieceT)
;
; Very nice table indicating when a player could
;
; move directly from one piece to another on an empty board.

  (= (piece-player-promotion $Piece $Player $PieceT)  
    (advice-tables $Tables)
    (piece-player-promotion $Piece $Player $PieceT $Tables))

;
; PIECE_PLAYER_PROMOTION(?Piece,?Player,?PieceT,+Tables)
;
; Very nice table indicating when a player could
;
; move directly from one piece to another on an empty board.
  (= (piece-player-promotion $Piece $Player $PieceT $Tables)  
    (piece-player-promotion $Piece $PieceI $Player $PlayerI $PieceT $PieceTI $Tables))

  (= (piece-player-promotion $Piece $PieceI $Player $PlayerI $PieceT $PieceTI $Tables)  
    (player-index $Player $PlayerI)
    (piece-index $Piece $PieceI)
    (piece-player-piece $PieceI $PlayerI $PieceTI $Tables)
    (piece-index $PieceT $PieceTI))


  (= (piece-player-piece $PieceI $PlayerI $PieceTI $Tables)  
    (promotion-matrix $Tables $M)
    (piece-player-piece1 $PieceI $PlayerI $PieceTI $M))


  (= (piece-player-piece1 $PieceI $PlayerI $PieceTI $M)  
    (player-table-entry $_ $PlayerI $M $Entry)
    (member1-pair 
      (- $PieceI $Ts) $Entry)
    (member $PieceTI $Ts))


  (= (piece-player-pieces $PieceI $PlayerI $TIs $Tables)  
    (promotion-matrix $Tables $M)
    (piece-player-pieces $PieceI $PlayerI $TIs $M))


  (= (piece-player-pieces1 $PieceI $PlayerI $TIs $M)  
    (player-table-entry $_ $PlayerI $M $Entry)
    (member1-pair 
      (- $PieceI $TIs) $Entry))


;
; Reversed order of args to use map table.
;
; promotion_matrix(Player,PlIndex,Matrix) :-

  (= (promotion-matrix $Player $PlIndex $Matrix)  
    (player-index $Player $PlIndex)
    (setof 
      (- $PieceIndex $PieceTIndices) 
      (^ $Piece 
        (player-pieces $Player $PlIndex $Piece $PieceIndex $PieceTIndices)) $Matrix)
    (tracing-anal-format tables "Built promotion matrix for <~p>~n" 
      (:: $Player)))



  (= (player-pieces $Player $PlIndex $Piece $PieceIndex $PieceTIndices)  
    (piece-index $Piece $PieceIndex)
    (det-if-then-else 
      (setof $PieceTIndex 
        (^ $PieceT 
          (player-piece-piece $Player $PlIndex $Piece $PieceIndex $PieceT $PieceTIndex)) $PieceTIndices) True 
      (= $PieceTIndices Nil)))


  (= (player-piece-piece $Player $Piece $PieceT)  
    (player-piece-piece $Player $PlIndex $Piece $PieceIndex $PieceT $PieceTIndex))
	  
  (= (player-piece-piece $Player $PlI $Piece $PieceI $PieceT $PieceTI)  
    (piece-index $Piece $PieceI)
    (player-index $Player $PlI)
    (forced-promote $Player $PlI $Piece $PieceI $PieceT $PieceTI)
    (piece-index $PieceT $PieceTI)
    (tracing-anal-format detailed "~p: ~p -> ~p~n" 
      (:: $Player $Piece $PieceT)))



;
; ==============================================================================
;
; Promotion Distance Matrix
;
; ==============================================================================

;
; Requires transition-matrix (at the moment).

  (= (build-prom-distance-matrix $Trans $Matrix)  
    (map-player-table prom-distance-matrix 
      (:: $Trans) $Matrix))




  (= (prom-distance-matrix $Player $PlayerIndex $Matrix)  
    (promotion-matrix $Trans)
    (prom-distance-matrix $Player $PlayerIndex $Trans $Matrix))

  (= (prom-distance-matrix $Player $PlayerIndex $Trans $Matrix)  
    (player-table-entry $Player $PlayerIndex $Trans $Matrix1)
    (s-floyd $Matrix1 $Matrix)
    (tracing-anal-format tables "Built prom_distance matrix for <~p>~n" 
      (:: $Player)))



  (= (piece-player-list-dist $PieceI $PlayerI $PieceTI $Dist $Tables)  
    (prom-distance-matrix $Tables $Matrix)
    (piece-player-list-dist1 $PieceI $PlayerI $PieceTI $Matrix $Dist))
	

  (= (piece-player-list-dist1 $PieceI $PlayerI $PieceTI $Table $Dist)  
    (player-table-entry $_ $PlayerI $Table $Entry)
    (piece-matrix-distance $PieceI $PieceTI $Entry $Dist)) 


  (= (piece-player-list-prom-distance $PieceI $PlayerI $PieceTI $Dist $Tables)  
    (prom-distance-matrix $Tables $Matrix)
    (piece-player-list-prom-distance1 $PieceI $PlayerI $PieceTI $Matrix $Dist))
	

  (= (piece-player-list-prom-distance1 $PieceI $PlayerI $PieceTI $Table $Dist)  
    (player-table-entry $_ $PlayerI $Table $Entry)
    (piece-matrix-distance $PieceI $PieceTI $Entry $Dist))	




  (= (print-prom-distance-matrix)  
    (print-prom-distance-matrix $_ $_ $_))

  (= (print-prom-distance-matrix $Piece $Player $PieceT)  
    (format "Piece Player Prom_distance Matrix:~n" Nil)
    (or 
      (, 
        (piece-player-prom-distance $Piece $Player $PieceT $Dist) 
        (format "~p: ~p -> ~p <~p>~n" 
          (:: $Player $Piece $PieceT $Dist)) 
        (fail)) True))
; ; format("matrix[~p][~p] = ~p~n",[Piece,Player,PieceT]),



  (= (ppd $P $Pl $PieceT $V)  
    (piece-player-prom-distance $P $Pl $PieceT $V))

;
; PIECE_PLAYER_PROM_DISTANCE(?Piece,?Player,?PieceT,?Dist)
;
; Very nice table indicating the minimum number of promotions
;
; (prom_distance) a player would
;
; take to move from one piece to another on an empty board.

  (= (piece-player-prom-distance $Piece $Player $PieceT $Dist)  
    (advice-tables $Tables)
    (piece-player-prom-distance $Piece $Player $PieceT $Dist $Tables))

;
; PIECE_PLAYER_PROM_DISTANCE(?Piece,?Player,?PieceT,?Dist,+Tables)
;
; Very nice table indicating the minimum number of promotions
;
; (prom_distance) a player would
;
; take to move from one piece to another on an empty board.
  (= (piece-player-prom-distance $Piece $Player $PieceT $Dist $Tables)  
    (player-index $Player $PlayerI)
    (piece-index $Piece $PieceI)
    (piece-index $PieceT $PieceTI)
    (piece-player-list-prom-distance $PieceI $PlayerI $PieceTI $Dist $Tables))


	
;
; ==============================================================================
;
; Piece Promotion Square Distance (Promsq) Table
;
; ==============================================================================
;
; Requires distance table now.


  (= (build-promsq-matrix $Matrix)  
    (distance-table $DTable)
    (build-promsq-matrix $DTable $Matrix))

;
; Uses saved distance-table.
  (= (build-promsq-matrix $DTable $Matrix)  
    (map-piece-table promsq-matrix 
      (:: $DTable) $Matrix))


  (= (promsq-matrix $Piece $PieceIndex $Matrix)  
    (distance-table $DTable)
    (promsq-matrix $Piece $PieceIndex $DTable $Matrix))
	
  (= (promsq-matrix $Piece $PieceIndex $DTable $Matrix)  
    (piece-index $Piece $PieceIndex)
    (map-square-table square-promsq 
      (:: $Piece $PieceIndex $DTable) $Matrix)
    (tracing-anal-format tables "Built promsq table for <~p>~n" 
      (:: $Piece)))



;
; If nothing found, puts a variable in that entry.

  (= (square-promsq $Sq $SqI $Piece $PieceI $DTable (/ $SqTIs $Dist))  
    (nearest-promotion-squares $Piece $PieceI $Player $Sq $SqI $SqTs $SqTIs $DTable $Dist)
    (set-det))
  (= (square_promsq  $Sq $SqI $Piece $PieceI $DTable $Ignore)  True)




;
; Returns the list of SqTIs (all with the minimum Dist).

  (= (square-piece-psqs $SqI $PieceI $SqTIs $Dist $Tables)  
    (promsq-matrix $Tables $M)
    (pindex-table-entry $PieceI $M $Entry)
    (slindex-table-entries $SqI $Entry $SqTIs $Dist))


;
; Backtracks over each SqTI (all with the minimum Dist).

  (= (square-piece-psq $SqI $PieceI $SqTI $Dist $Tables)  
    (promsq-matrix $Tables $M)
    (pindex-table-entry $PieceI $M $Entry)
    (slindex-table-entry $SqI $Entry $SqTI $Dist))




  (= (spq $S $P $SqT $D)  
    (square-piece-promsq $S $P $SqT $D))

;
; SQUARE_PIECE_PROMSQ(?Sq,?Piece,?SqT,?Dist)
;
; Very nice table indicating for a piece on a square,
;
; one of the nearest promotion squares SqT is Dist squares away.
;
; Backtracks over all nearest squares.

  (= (square-piece-promsq $Sq $Piece $SqT $Dist)  
    (advice-tables $Tables)
    (square-piece-promsq $Sq $Piece $SqT $Dist $Tables))

;
; SQUARE_PIECE_PROMSQ(?Sq,?Piece,?SqT,?Dist,Tables)
;
; Very nice table indicating for a piece on a square,
;
; one of the nearest promotion squares SqT is Dist squares away.
  (= (square-piece-promsq $Sq $Piece $SqT $Dist $Tables)  
    (piece-index $Piece $PieceI)
    (square-index $Sq $SqI)
    (square-piece-psq $SqI $PieceI $SqTI $Dist $Tables)
    (square-index $SqT $SqTI))




;
; Printing promsq matrix
;
; 

  (= (print-promsq-matrix)  
    (print-promsq-matrix $_ $_))

  (= (print-promsq-matrix $Sq $Piece)  
    (format "Square Piece Promsq Matrix:~n" Nil)
    (or 
      (, 
        (square-piece-promsq $Sq $Piece $SqT $Dist) 
        (format "matrix[~p][~p] = ~p (~p)~n" 
          (:: $Sq $Piece $SqT $Dist)) 
        (fail)) True))



;
; =============================================================================

  (= (nearest-promotion-squares $Piece $Player $Sq $SqTs $Dist)  
    (nearest-promotion-squares $Piece $PieceI $Player $Sq $SqI $SqTs $SqTIs $Dist))
; /* Nearest promotion square/s: The closest square a piece could move to and promote (on an empty board). We could either use just the nearest one, or if there are ties the nearest maximizing some value, etc. */
; ; NEAREST_PROMOTION_SQUARES(?Piece,?Player,?Sq,-SqTs,-Dist) ; SqTs are the nearest promotion square from Sq for Player's Piece, ; and getting to any of them would require Dist moves on an empty board.

  (= (nearest-promotion-squares $Piece $PieceI $Player $Sq $SqI $SqTs $SqTIs $Dist)  
    (distance-table $DTable)
    (nearest-promotion-squares $Piece $PieceI $Player $Sq $SqI $SqTs $SqTIs $DTable $Dist))

  (= (nearest-promotion-squares $Piece $PieceI $Player $Sq $SqI $SqTs $SqTIs $DTable $Dist)  
    (piece-index $Piece $PieceI)
    (square-index $Sq $SqI)
    (owns $Piece $Player)
    (prom-square-indices $Player $PromSquares)
    (nearest-squares $PromSquares $PieceI $SqI $SqTs $SqTIs $DTable $Dist))


;
; NEAREST_SQUARES(Squares,PieceI,SqI,SqTs,SqTIs,DTable,Dist) :-
;
; SqTIs are the indices of the nearest members SqTs of Squares (list of
;
; indices) for piece index PieceI from SqI, and Dist is the
;
; min distance.
;
; If no reachable square was found, this fails.
;
; DTable is the distance table.


  (= (nearest-squares $Squares $PieceI $SqI $SqTs $SqTIs $DTable $Dist)  
    (square-distances $Squares $PieceI $SqI $DTable $AllDists)
    (closest-dists $AllDists $Dist $SqTIs)
    (maplist square-index $SqTs $SqTIs))



  (= (square-distances $Squares $PieceI $SqI $DTable $AllDists)  
    (findall 
      (- $Dist $SqTI) 
      (, 
        (member $SqTI $Squares) 
        (\== $SqTI $SqI) 
        (square-piece-sq-dist1 $SqI $PieceI $SqTI $DTable $Dist)) $AllDists))  
	   

  (= (closest-dists $AllDists $V $Es)  
    (p-to-s-graph $AllDists $Graph)
    (first-connected $Graph 
      (- $V $Es)))


  (= (first-connected (Cons  $H $Rest) $First)  
    (first1 $H $Rest $First))


  (= (first1 (- $V $Es) $Rest $First)  
    (det-if-then-else 
      (= $Es Nil) 
      (first-connected $Rest $First) 
      (= $First 
        (- $V $Es))))
 

