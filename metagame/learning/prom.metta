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
    (prom-value $Piece $Sq $SqT $Value $Pos $Tables) 
    ( (owns $Piece $Player) 
      (useful-promotes $Piece $Player $Tables) 
      (focussed-promsq-cost $Piece $Player $Sq $SqT $PathCost $Pos $Tables) 
      (best-promotion-choice-value $Piece $Player $SqT $PathCost $PieceT $Value $Pos $Tables)))
; 





  (= 
    (best-promotion-choice-value $Piece $Player $SqT $PathCost $BestPiece $BestValue $Pos $Tables) 
    ( (findall 
        (- $Value $PieceT) 
        (promotion-choice-value $Piece $Player $SqT $PathCost $PieceT $Value $Pos $Tables) $Pairs) (best-player-choice $Player $Pairs $BestPiece $BestValue)))
; 
	   


  (= 
    (promotion-choice-value $Piece $Player $SqT $PathCost $PieceT $Value $Pos $Tables) 
    ( (piece-player-prom-distance $Piece $Player $PieceT $PromDist $Tables) 
      (\== $PieceT $Piece) 
      (intrinsic-value $PieceT $SqT $IVal $Pos $Tables) 
      (prom-likelihood $PromDist $PathCost $Prob) 
      (reasonable-likelihood $Prob) 
      (expected-value $Prob $IVal $Value) 
      (favorable-to-owner $Player $Value)))
; 





  (= 
    (useful-promotes $Piece $Player $Tables) 
    (det-if-then 
      (player-safe-new-prom $Piece $Player $PieceT $SqT $Tables) True))
; 
 


;
; 

;
; 


  (= 
    (player-safe-new-prom $Piece $Player $PieceT $SqT $Tables) 
    ( (piece-player-promotion $Piece $Player $PieceT $Tables) 
      (\== $Piece $PieceT) 
      (opposite-role $Player $Opp) 
      (player-promotion-square $Player $SqT) 
      (not (goal-square $PieceT $SqT $Opp $_))))
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
    (focussed-promsq-cost $Piece $Player $Sq $SqT $Cost $Pos $Tables) 
    ( (square-index $Sq $SqI) 
      (piece-index $Piece $PieceI) 
      (square-piece-psqs $SqI $PieceI $SqTIs $Dist $Tables) 
      (cheapest-destination $Piece $Player $Sq $SqTIs $Dist $SqT $Cost $Pos $Tables)))
; 


;
; 

;
; 


  (= 
    (cheapest-destination $Piece $Player $Sq $SqTIs $Dist $SqT $Cost $Pos $Tables) 
    ( (findall 
        (- $Cost1 $SqT1) 
        (, 
          (member $SqTI1 $SqTIs) 
          (square-index $SqT1 $SqTI1) 
          (cost-to-promsq $Piece $Player $Sq $SqT1 $Dist $Cost1 $Pos $Tables)) $Dests) (sort $Dests (Cons  (- $Cost $SqT) $_))))
; 




  (= 
    (cost-to-promsq $Piece $Player $Sq $SqT $SqDist $PathCost $Pos $Tables) 
    (clear-path-cost $Piece $Player $Sq $SqT $SqDist $PathCost $Pos))
; 




  (= 
    (distance-to-promsq $Piece $Sq $SqT $SqDist $Tables) 
    (square-piece-promsq $Sq $Piece $SqT $SqDist $Tables))
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
    (prom-likelihood $PromDist $PathCost $Prob) 
    ( (is $MinMoves 
        (- 
          (+ $PromDist $PathCost) 1)) (distance-value $MinMoves $Prob)))
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
    (intrinsic-value $Piece $Sq $Val $Pos $Tables) 
    ( (shutdown-advisor threat $Tables) 
      (shutdown-advisor prom $Tables) 
      (shutdown-advisor dynamic-mobility $Tables) 
      (local-evaluation $Piece $Sq $Val $Pos $Tables)))
; 




  (= 
    (testp) 
    ( (checkpoint init $S) 
      (get-advices $A $S) 
      (ppl $A)))
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
    (promotes-into $Piece $PieceI $Prom $PromI $Player $Chooser) 
    ( (promotes-into $Piece $Prom $Player $Chooser) 
      (piece-index $Piece $PieceI) 
      (piece-index $Prom $PromI)))
; 

	


  (= 
    (opponent-promotes $Piece) 
    ( (promotes-into $Piece $_ $Player $Chooser) (opposite-role $Player $Chooser)))
; 
  
	


  (= 
    (promotes-into $OrigPiece $PromPiece $Player $Chooser) 
    ( (owns $OrigPiece $Player) 
      (current-game-for-player $Player $Game) 
      (game-piece-promoting $OrigPiece $Promoting $Game) 
      (promote-choice $Promoting $Player $Chooser $PromPiece)))
; 



  (= 
    (promote-choice $Promoting $Player $Player $PromPiece) 
    (simple-promote $Promoting $Player $PromPiece))
; 

  (= 
    (promote-choice $Promoting $Player $Chooser $PromPiece) 
    ( (= $Promoting 
        (promote $Chooser $Descr)) (matches $Descr $PromPiece)))
; 


	

  (= 
    (forced-promote $Player $PlI $Piece $PieceI $Prom $PromI) 
    ( (player-index $Player $PlI) (forced-promote $Player $Piece $PieceI $Prom $PromI)))
; 


  (= 
    (forced-promote $Player $Piece $PieceI $Prom $PromI) 
    (promotes-into $Piece $PieceI $Prom $PromI $Player $Player))
; 



;
; 

;
; 

;
; 



  (= 
    (build-promotion-matrix $Matrix) 
    (map-player-table promotion-matrix Nil $Matrix))
; 



  (= 
    (print-promotion-matrix) 
    (print-promotion-matrix $Piece $Player $PieceT))
; 


  (= 
    (print-promotion-matrix $Piece $Player $PieceT) 
    ( (format "Player Piece Promotion Matrix:~n" Nil) (or (, (piece-player-promotion $Piece $Player $PieceT) (format "~p: ~p -> ~p~n" (:: $Player $Piece $PieceT)) (fail)) True)))
; 



  (= 
    (ppp $Player $Piece $PieceT) 
    (piece-player-promotion $Piece $Player $PieceT))
; 


;
; 

;
; 

;
; 


  (= 
    (piece-player-promotion $Piece $Player $PieceT) 
    ( (advice-tables $Tables) (piece-player-promotion $Piece $Player $PieceT $Tables)))
; 


;
; 

;
; 

;
; 

  (= 
    (piece-player-promotion $Piece $Player $PieceT $Tables) 
    (piece-player-promotion $Piece $PieceI $Player $PlayerI $PieceT $PieceTI $Tables))
; 


  (= 
    (piece-player-promotion $Piece $PieceI $Player $PlayerI $PieceT $PieceTI $Tables) 
    ( (player-index $Player $PlayerI) 
      (piece-index $Piece $PieceI) 
      (piece-player-piece $PieceI $PlayerI $PieceTI $Tables) 
      (piece-index $PieceT $PieceTI)))
; 



  (= 
    (piece-player-piece $PieceI $PlayerI $PieceTI $Tables) 
    ( (promotion-matrix $Tables $M) (piece-player-piece1 $PieceI $PlayerI $PieceTI $M)))
; 



  (= 
    (piece-player-piece1 $PieceI $PlayerI $PieceTI $M) 
    ( (player-table-entry $_ $PlayerI $M $Entry) 
      (member1-pair 
        (- $PieceI $Ts) $Entry) 
      (member $PieceTI $Ts)))
; 



  (= 
    (piece-player-pieces $PieceI $PlayerI $TIs $Tables) 
    ( (promotion-matrix $Tables $M) (piece-player-pieces $PieceI $PlayerI $TIs $M)))
; 



  (= 
    (piece-player-pieces1 $PieceI $PlayerI $TIs $M) 
    ( (player-table-entry $_ $PlayerI $M $Entry) (member1-pair (- $PieceI $TIs) $Entry)))
; 



;
; 

;
; 


  (= 
    (promotion-matrix $Player $PlIndex $Matrix) 
    ( (player-index $Player $PlIndex) 
      (setof 
        (- $PieceIndex $PieceTIndices) 
        (^ $Piece 
          (player-pieces $Player $PlIndex $Piece $PieceIndex $PieceTIndices)) $Matrix) 
      (tracing-anal-format tables "Built promotion matrix for <~p>~n" 
        (:: $Player))))
; 




  (= 
    (player-pieces $Player $PlIndex $Piece $PieceIndex $PieceTIndices) 
    ( (piece-index $Piece $PieceIndex) (det-if-then-else (setof $PieceTIndex (^ $PieceT (player-piece-piece $Player $PlIndex $Piece $PieceIndex $PieceT $PieceTIndex)) $PieceTIndices) True (= $PieceTIndices Nil))))
; 



  (= 
    (player-piece-piece $Player $Piece $PieceT) 
    (player-piece-piece $Player $PlIndex $Piece $PieceIndex $PieceT $PieceTIndex))
; 

	  
  (= 
    (player-piece-piece $Player $PlI $Piece $PieceI $PieceT $PieceTI) 
    ( (piece-index $Piece $PieceI) 
      (player-index $Player $PlI) 
      (forced-promote $Player $PlI $Piece $PieceI $PieceT $PieceTI) 
      (piece-index $PieceT $PieceTI) 
      (tracing-anal-format detailed "~p: ~p -> ~p~n" 
        (:: $Player $Piece $PieceT))))
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
    (build-prom-distance-matrix $Trans $Matrix) 
    (map-player-table prom-distance-matrix 
      (:: $Trans) $Matrix))
; 





  (= 
    (prom-distance-matrix $Player $PlayerIndex $Matrix) 
    ( (promotion-matrix $Trans) (prom-distance-matrix $Player $PlayerIndex $Trans $Matrix)))
; 


  (= 
    (prom-distance-matrix $Player $PlayerIndex $Trans $Matrix) 
    ( (player-table-entry $Player $PlayerIndex $Trans $Matrix1) 
      (s-floyd $Matrix1 $Matrix) 
      (tracing-anal-format tables "Built prom_distance matrix for <~p>~n" 
        (:: $Player))))
; 




  (= 
    (piece-player-list-dist $PieceI $PlayerI $PieceTI $Dist $Tables) 
    ( (prom-distance-matrix $Tables $Matrix) (piece-player-list-dist1 $PieceI $PlayerI $PieceTI $Matrix $Dist)))
; 

	

  (= 
    (piece-player-list-dist1 $PieceI $PlayerI $PieceTI $Table $Dist) 
    ( (player-table-entry $_ $PlayerI $Table $Entry) (piece-matrix-distance $PieceI $PieceTI $Entry $Dist)))
; 
 


  (= 
    (piece-player-list-prom-distance $PieceI $PlayerI $PieceTI $Dist $Tables) 
    ( (prom-distance-matrix $Tables $Matrix) (piece-player-list-prom-distance1 $PieceI $PlayerI $PieceTI $Matrix $Dist)))
; 

	

  (= 
    (piece-player-list-prom-distance1 $PieceI $PlayerI $PieceTI $Table $Dist) 
    ( (player-table-entry $_ $PlayerI $Table $Entry) (piece-matrix-distance $PieceI $PieceTI $Entry $Dist)))
; 
	




  (= 
    (print-prom-distance-matrix) 
    (print-prom-distance-matrix $_ $_ $_))
; 


  (= 
    (print-prom-distance-matrix $Piece $Player $PieceT) 
    ( (format "Piece Player Prom_distance Matrix:~n" Nil) (or (, (piece-player-prom-distance $Piece $Player $PieceT $Dist) (format "~p: ~p -> ~p <~p>~n" (:: $Player $Piece $PieceT $Dist)) (fail)) True)))
; 




  (= 
    (ppd $P $Pl $PieceT $V) 
    (piece-player-prom-distance $P $Pl $PieceT $V))
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
    (piece-player-prom-distance $Piece $Player $PieceT $Dist) 
    ( (advice-tables $Tables) (piece-player-prom-distance $Piece $Player $PieceT $Dist $Tables)))
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
    (piece-player-prom-distance $Piece $Player $PieceT $Dist $Tables) 
    ( (player-index $Player $PlayerI) 
      (piece-index $Piece $PieceI) 
      (piece-index $PieceT $PieceTI) 
      (piece-player-list-prom-distance $PieceI $PlayerI $PieceTI $Dist $Tables)))
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
    (build-promsq-matrix $Matrix) 
    ( (distance-table $DTable) (build-promsq-matrix $DTable $Matrix)))
; 


;
; 

  (= 
    (build-promsq-matrix $DTable $Matrix) 
    (map-piece-table promsq-matrix 
      (:: $DTable) $Matrix))
; 



  (= 
    (promsq-matrix $Piece $PieceIndex $Matrix) 
    ( (distance-table $DTable) (promsq-matrix $Piece $PieceIndex $DTable $Matrix)))
; 

	
  (= 
    (promsq-matrix $Piece $PieceIndex $DTable $Matrix) 
    ( (piece-index $Piece $PieceIndex) 
      (map-square-table square-promsq 
        (:: $Piece $PieceIndex $DTable) $Matrix) 
      (tracing-anal-format tables "Built promsq table for <~p>~n" 
        (:: $Piece))))
; 




;
; 


  (= 
    (square-promsq $Sq $SqI $Piece $PieceI $DTable 
      (/ $SqTIs $Dist)) 
    ( (nearest-promotion-squares $Piece $PieceI $Player $Sq $SqI $SqTs $SqTIs $DTable $Dist) (set-det)))
; 

  (= 
    (square_promsq  $Sq $SqI $Piece $PieceI $DTable $Ignore) True)
; 





;
; 


  (= 
    (square-piece-psqs $SqI $PieceI $SqTIs $Dist $Tables) 
    ( (promsq-matrix $Tables $M) 
      (pindex-table-entry $PieceI $M $Entry) 
      (slindex-table-entries $SqI $Entry $SqTIs $Dist)))
; 



;
; 


  (= 
    (square-piece-psq $SqI $PieceI $SqTI $Dist $Tables) 
    ( (promsq-matrix $Tables $M) 
      (pindex-table-entry $PieceI $M $Entry) 
      (slindex-table-entry $SqI $Entry $SqTI $Dist)))
; 





  (= 
    (spq $S $P $SqT $D) 
    (square-piece-promsq $S $P $SqT $D))
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
    (square-piece-promsq $Sq $Piece $SqT $Dist) 
    ( (advice-tables $Tables) (square-piece-promsq $Sq $Piece $SqT $Dist $Tables)))
; 


;
; 

;
; 

;
; 

  (= 
    (square-piece-promsq $Sq $Piece $SqT $Dist $Tables) 
    ( (piece-index $Piece $PieceI) 
      (square-index $Sq $SqI) 
      (square-piece-psq $SqI $PieceI $SqTI $Dist $Tables) 
      (square-index $SqT $SqTI)))
; 





;
; 

;
; 


  (= 
    (print-promsq-matrix) 
    (print-promsq-matrix $_ $_))
; 


  (= 
    (print-promsq-matrix $Sq $Piece) 
    ( (format "Square Piece Promsq Matrix:~n" Nil) (or (, (square-piece-promsq $Sq $Piece $SqT $Dist) (format "matrix[~p][~p] = ~p (~p)~n" (:: $Sq $Piece $SqT $Dist)) (fail)) True)))
; 




;
; 


  (= 
    (nearest-promotion-squares $Piece $Player $Sq $SqTs $Dist) 
    (nearest-promotion-squares $Piece $PieceI $Player $Sq $SqI $SqTs $SqTIs $Dist))
; 


  (= 
    (nearest-promotion-squares $Piece $PieceI $Player $Sq $SqI $SqTs $SqTIs $Dist) 
    ( (distance-table $DTable) (nearest-promotion-squares $Piece $PieceI $Player $Sq $SqI $SqTs $SqTIs $DTable $Dist)))
; 


  (= 
    (nearest-promotion-squares $Piece $PieceI $Player $Sq $SqI $SqTs $SqTIs $DTable $Dist) 
    ( (piece-index $Piece $PieceI) 
      (square-index $Sq $SqI) 
      (owns $Piece $Player) 
      (prom-square-indices $Player $PromSquares) 
      (nearest-squares $PromSquares $PieceI $SqI $SqTs $SqTIs $DTable $Dist)))
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
    (nearest-squares $Squares $PieceI $SqI $SqTs $SqTIs $DTable $Dist) 
    ( (square-distances $Squares $PieceI $SqI $DTable $AllDists) 
      (closest-dists $AllDists $Dist $SqTIs) 
      (maplist square-index $SqTs $SqTIs)))
; 




  (= 
    (square-distances $Squares $PieceI $SqI $DTable $AllDists) 
    (findall 
      (- $Dist $SqTI) 
      (, 
        (member $SqTI $Squares) 
        (\== $SqTI $SqI) 
        (square-piece-sq-dist1 $SqI $PieceI $SqTI $DTable $Dist)) $AllDists))
; 
  
	   

  (= 
    (closest-dists $AllDists $V $Es) 
    ( (p-to-s-graph $AllDists $Graph) (first-connected $Graph (- $V $Es))))
; 



  (= 
    (first-connected 
      (Cons  $H $Rest) $First) 
    (first1 $H $Rest $First))
; 



  (= 
    (first1 
      (- $V $Es) $Rest $First) 
    (det-if-then-else 
      (= $Es Nil) 
      (first-connected $Rest $First) 
      (= $First 
        (- $V $Es))))
; 

 

