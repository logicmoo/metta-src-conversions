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
    (legal $M) 
    ( (control $Player) (legal-move $M $Player)))
; 


;
; 


  (= 
    (legal-move $Move $Player) 
    ( (stage $Stage) (legal-move $Move $Stage $Player)))
; 


;
; 

;
; 


  (= 
    (legal-move $M $Stage $Player) 
    ( (increment-move-count) 
      (legal-move $M $Stage $Player $StageOut $_) 
      (transfer-stage $Stage $StageOut)))
; 




  (= 
    (legal-move 
      (Cons  $M $Rest) $StageIn $PlayerIn $StageOut $PlayerOut) 
    ( (pseudo-op $StageIn $M $PlayerIn $Stage1 $Player1) (det-if-then-else (\== $PlayerIn $Player1) (, (= $Player1 $PlayerOut) (= $Stage1 $StageOut)) (legal-move $Rest $Stage1 $Player1 $StageOut $PlayerOut))))
; 




;
; 

;
; 

;
; 



  (= 
    (player_role  player) True)
; 

  (= 
    (player_role  opponent) True)
; 



  (= 
    (opposite_role  player opponent) True)
; 

  (= 
    (opposite_role  opponent player) True)
; 





  (= 
    (change $P1 $P2) 
    ( (del $P1) (add $P2)))
; 


;
; 


;
; 


  (= 
    (empty $Sq) 
    (on empty $Sq))
; 



  (= 
    (set-empty $Sq) 
    (add (on empty $Sq)))
; 


;
; 


  (= 
    (on $PieceStruct $Player $Square) 
    ( (piece-struct-owner $PieceStruct $Player) (on $PieceStruct $Square)))
; 


;
; 


  (= 
    (piece_struct_name  
      (piece  $Name $Owner) $Name) True)
; 


  (= 
    (piece_struct_owner  
      (piece  $Name $Owner) $Owner) True)
; 



  (= 
    (piece_struct  
      (piece  $Name $Owner) $Name $Owner) True)
; 



  (= 
    (owns $Piece $Owner) 
    (piece-struct-owner $Piece $Owner))
; 




  (= 
    (add-to-board $Piece $Square) 
    (place-piece $Piece $Square))
; 



  (= 
    (put-in-hand $Piece $Player) 
    (add (in-hand $Piece $Player)))
; 




  (= 
    (place-piece-from-hand $Piece $Player $Square) 
    ( (del (in-hand $Piece $Player)) (place-piece $Piece $Square)))
; 


;
; 

;
; 


  (= 
    (lift-piece $Piece $Square) 
    (change 
      (on $Piece $Square) 
      (on empty $Square)))
; 



  (= 
    (place-piece $Piece $Square) 
    (change 
      (on empty $Square) 
      (on $Piece $Square)))
; 



  (= 
    (remove-piece $Taken $TakenSq) 
    (lift-piece $Taken $TakenSq))
; 




  (= 
    (put-control $Player) 
    ( (det-if-then-else 
        (control $P) 
        (del (control $P)) True) (add (control $Player))))
; 



;
; 


  (= 
    (transfer-control $Player) 
    (transfer-control $Player $Opp))
; 


  (= 
    (transfer-control $Player $Opp) 
    ( (opposite-role $Player $Opp) (change (control $Player) (control $Opp))))
; 



;
; 

;
; 


  (= 
    (move-piece-record $Piece $SqF $SqT) 
    ( (move-piece $Piece $SqF $SqT) (add (moved-onto $Piece $SqT))))
; 



  (= 
    (place-piece-record $Piece $SqT) 
    (add (moved-onto $Piece $SqT)))
; 


;
; 


  (= 
    (move-piece $Piece $SqF $SqT) 
    ( (on $Piece $SqF) 
      (lift-piece $Piece $SqF) 
      (place-piece $Piece $SqT)))
; 




  (= 
    (set-effect $Effect $Captured) 
    (add (effects $Effect $Captured)))
; 
 


  (= 
    (del-effect $Effect $Captured) 
    (del (effects $Effect $Captured)))
; 
 


  (= 
    (set-effect $Effect) 
    (add (effect $Effect)))
; 
 


  (= 
    (del-effect $Effect) 
    (del (effect $Effect)))
; 
 


  (= 
    (transfer-stage $Stage) 
    ( (stage $OldStage) (transfer-stage $OldStage $Stage)))
; 


  (= 
    (transfer-stage $Old $New) 
    (change 
      (stage $Old) 
      (stage $New)))
; 



  (= 
    (put-stage $Stage) 
    ( (det-if-then-else 
        (stage $OldStage) 
        (del (stage $OldStage)) True) (add (stage $Stage))))
; 




  (= 
    (capture-piece $Taken $TakenSq) 
    (remove-piece $Taken $TakenSq))
; 


;
; 

;
; 


  (= 
    (replace-piece $Old $New $Sq) 
    (det-if-then-else 
      (= $Old $New) True 
      (change 
        (on $Old $Sq) 
        (on $New $Sq))))
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
    (replace-piece-record $Old $New $Sq) 
    (place-piece $New $Sq))
; 




  (= 
    (increment-move-count) 
    ( (move-count $M) 
      (is $M1 
        (+ $M 1)) 
      (change 
        (move-count $M) 
        (move-count $M1))))
; 



;
; 


  (= 
    (assignable $Square $Player) 
    ( (assignable-squares $Player $Squares) (member $Square $Squares)))
; 

	
;
; 

;
; 


  (= 
    (still-assigning) 
    ( (stage assign) (in-hand $Piece $Player)))
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
    (pseudo-op assign 
      (assign $Piece $Player $Square) $Player assign $Opponent) 
    ( (assign $Piece $Player $Square) (transfer-control $Player $Opponent)))
; 

  (= 
    (pseudo-op assign end-assign $Player move $Player) 
    (end-assign))
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
    (pseudo-op init-promote 
      (opponent-promote $Sq $OldPiece $NewPiece) $Player move $Player) 
    (opponent-promote $Sq $OldPiece $NewPiece $Player))
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
    (pseudo-op move 
      (place $Piece $Player $Square) $Player move $Opponent) 
    (place-op $Piece $Player $Square $Opponent))
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
    (pseudo-op move 
      (move $Piece $Player $SqF $SqT) $Player $Stage $Player) 
    (global-or-local-move $Piece $Player $SqF $SqT $Stage))
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
    (pseudo-op capture 
      (capture $Effect $Captured) $Player continue $Player) 
    (capture-op $Effect $Captured $Player))
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
    (pseudo-op continue $Move $Player $Stage $Player) 
    (try-continue-or-end $Move $Player $Stage))
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
    (pseudo-op promote 
      (try-promote $Square $OldPiece $NewPiece) $Player $Stage $Player2) 
    (try-promote $Square $OldPiece $NewPiece $Player $Stage $Player2))
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
    (pseudo-op promote-select 
      (promote-select $Sq $OldPiece $NewPiece) $Player $Stage $Player2) 
    (promote-select $Sq $OldPiece $NewPiece $Player $Stage $Player2))
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
    (assign $Piece $Player $Square) 
    ( (placeable $Piece $Player $Square) 
      (assignable $Square $Player) 
      (place-piece-from-hand $Piece $Player $Square)))
; 



  (= 
    (end-assign) 
    (not (in-hand $Piece $Player)))
; 



;
; 

;
; 

;
; 


  (= 
    (place-op $Piece $Player $Square $Opponent) 
    ( (placeable $Piece $Player $Square) 
      (place-piece-from-hand $Piece $Player $Square) 
      (transfer-control $Player $Opponent)))
; 




  (= 
    (placeable $Piece $Player $Square) 
    ( (bagof $Piece 
        (in-hand $Piece $Player) $AllPieces) 
      (remove-duplicates $AllPieces $Pieces) 
      (member $Piece $Pieces) 
      (empty $Square)))
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
    (global-or-local-move $Piece $Player $SqF $SqT $Stage) 
    (det-if-then-else current-game-must-capture 
      (global-prefer-capture $Piece $Player $SqF $SqT $Stage) 
      (local-move $Piece $Player $SqF $SqT $Stage)))
; 



  (= 
    (global-prefer-capture $Piece $Player $SqF $SqT $Stage) 
    ( (global-prefer-capture1 $Piece1 $Player $SqF1 $SqT1 $Stage1) 
      (= $Piece1 $Piece) 
      (= $SqF1 $SqF) 
      (= $SqT1 $SqT) 
      (= $Stage $Stage1)))
; 



  (= 
    (global-prefer-capture1 $Piece $Player $SqF $SqT $Stage) 
    (if 
      (, 
        (moveable $Piece $Player $SqF) 
        (capturing $Piece $Player $SqF $SqT)) 
      (= $Stage capture) 
      (, 
        (moveable $Piece $Player $SqF) 
        (moving $Piece $Player $SqF $SqT) 
        (= $Stage promote))))
; 

	      


  (= 
    (local-move $Piece $Player $SqF $SqT $Stage) 
    ( (moveable $Piece $Player $SqF) (local-could-move $Piece $Player $SqF $SqT $Stage)))
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
    (local-could-move $Piece $Player $SqF $SqT $Stage) 
    (det-if-then-else 
      (must-capture $Piece) 
      (local-prefer-capture $Piece $Player $SqF $SqT $Stage) 
      (general-moving $Piece $Player $SqF $SqT $Stage)))
; 



  (= 
    (local-prefer-capture $Piece $Player $SqF $SqT $Stage) 
    (if 
      (capturing $Piece $Player $SqF $Sq1) 
      (, 
        (= $Sq1 $SqT) 
        (= $Stage capture)) 
      (, 
        (moving $Piece $Player $SqF $SqT) 
        (= $Stage promote))))
; 




  (= 
    (general-moving $Piece $Player $SqF $SqT capture) 
    (capturing $Piece $Player $SqF $SqT))
; 

  (= 
    (general-moving $Piece $Player $SqF $SqT promote) 
    (moving $Piece $Player $SqF $SqT))
; 



  (= 
    (must-capture $Piece) 
    (game--piece-must $Piece))
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
    (reaches $Piece $Player $SqF $SqT) 
    (moves $Piece $Player $SqF $SqT))
; 

  (= 
    (reaches $Piece $Player $SqF $SqT) 
    (captures $Piece $Player $SqF $SqT))
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
    (moving $Piece $Player $SqF $SqT) 
    ( (game--piece-has-movement $Piece $Move) 
      (moving-movement-for-piece $Piece $SqF $SqT $Player $Move $Dir $Hop) 
      (place-piece-record $Piece $SqT)))
; 



  (= 
    (moves $Piece $Player $SqF $SqT) 
    ( (game--piece-has-movement $Piece $Move) (moving-movement-for-piece $Piece $SqF $SqT $Player $Move $Dir $Hop)))
; 
 


;
; 

;
; 

;
; 


  (= 
    (moving-movement-for-piece $Piece $SqF $SqT $Player $Movement $Dir $Hop) 
    ( (if 
        (, 
          (ride $Movement $Dir $Min $Max $Longest) 
          (longest $Longest) 
          (current-board-type planar)) 
        (longest-moving-ride $SqF $Dir $Min $Max $SqT) 
        (movement-for-piece $Piece $SqF $SqT $Player $Movement $Dir $Hop)) (valid-movement $SqT)))
; 


;
; 


  (= 
    (valid-movement $SqT) 
    (empty $SqT))
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
    (moveable $Piece $Player $SqF) 
    ( (on $Piece $Player $SqF) (lift-piece $Piece $SqF)))
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
    (movement-for-piece $Piece $SqF $SqT $Player $Movement $Dir Nil) 
    ( (leap $Movement $Dir) (connected $SqF $SqT $Dir)))
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
    (movement-for-piece $Piece $SqF $SqT $Player $Movement $Dir Nil) 
    ( (ride $Movement $Dir $Min $Max $Longest) (open-line $SqF $Dir $Min $Max empty $Squares $SqT)))
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
    (movement-for-piece $Piece $SqF $SqT $Player $Movement $Dir $Hopped) 
    ( (hop $Movement $Dir $Before $Over $After $Description) (hoplines $SqF $SqT $Dir $Before $Over $After $Description $Hopped)))
; 



  (= 
    (hoplines $SqF $SqT $Dir $Before $Over $After $Description $Hopped) 
    (det-if-then-else 
      (var $SqF) 
      (hoplines-rev $SqF $SqT $Dir $Before $Over $After $Description $Hopped) 
      (hoplines-fwd $SqF $SqT $Dir $Before $Over $After $Description $Hopped)))
; 



  (= 
    (hoplines-fwd $SqF $SqT $Dir $Before $Over $After $Description $Hopped) 
    ( (hopline $SqF $Before $Dir empty $_ $SqB) 
      (hopline $SqB $Over $Dir $Description $Hopped $SqO) 
      (hopline $SqO $After $Dir empty $_ $SqL) 
      (connected $SqL $SqT $Dir)))
; 



  (= 
    (hoplines-rev $SqF $SqT $Dir $Before $Over $After $Description $Hopped) 
    ( (connected $SqL $SqT $Dir) 
      (hopline $SqO $After $Dir empty $_ $SqL) 
      (hopline $SqB $Over $Dir $Description $Hopped $SqO) 
      (hopline $SqF $Before $Dir empty $_ $SqB)))
; 




  (= 
    (hopline $SqF $Range $Dir $Descr $Squares $SqT) 
    ( (comparative-interval $Range $Dir $MinB $MaxB) (constrained-line $SqF $Dir $MinB $MaxB $Descr $Squares $SqT)))
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
    (comparative-interval 
      (comparison $Comp $X) $Dir $N $MaxLeaps) 
    (comparative-interval $Comp $X $Dir $N $MaxLeaps))
; 


  (= 
    (comparative-interval geq $N $Dir $N $MaxLeaps) 
    (valid-max $_ $Dir $MaxLeaps))
; 

  (= 
    (comparative_interval  eq $N $_ $N $N) True)
; 

  (= 
    (comparative_interval  leq $N $_ 0 $N) True)
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
    (open-line $SqF $Dir $Min $Max $Cond $Squares $SqT) 
    (det-if-then-else 
      (var $SqF) 
      (open-line-rev $SqF $Dir $Min $Max $Cond $Squares $SqT) 
      (open-line-fwd $SqF $Dir $Min $Max $Cond $Squares $SqT)))
; 

	


  (= 
    (open-line-fwd $SqF $Dir $Min $Max $Cond $Squares $SqT) 
    ( (is $Min1 
        (- $Min 1)) 
      (is $Max1 
        (- $Max 1)) 
      (constrained-line-fwd $SqF $Dir $Min1 $Max1 $Cond $Squares $Sq1) 
      (connected $Sq1 $SqT $Dir)))
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
    (constrained-line $SqF $Dir $Min $Max $Cond $Squares $SqT) 
    (det-if-then-else 
      (var $SqF) 
      (constrained-line-rev $SqF $Dir $Min $Max $Cond $Squares $SqT) 
      (constrained-line-fwd $SqF $Dir $Min $Max $Cond $Squares $SqT)))
; 



  (= 
    (constrained-line-fwd $SqF $Dir $Min $Max $Cond $Squares $SqT) 
    (all-sat-fwd $SqF $Dir $Cond 0 $Min $Max $Squares $SqT))
; 

	

  (= 
    (all-sat-fwd $SqF $Dir $Cond $Count $Min $Max Nil $SqF) 
    ( (>= $Count $Min) (=< $Count $Max)))
; 

  (= 
    (all-sat-fwd $SqF $Dir $Cond $Count $Min $Max 
      (Cons  $Sq1 $Squares) $SqT) 
    ( (< $Count $Max) 
      (connected $SqF $Sq1 $Dir) 
      (crossable $Cond $Sq1) 
      (is $Count1 
        (+ $Count 1)) 
      (all-sat-fwd $Sq1 $Dir $Cond $Count1 $Min $Max $Squares $SqT)))
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
    (open-line-rev $SqF $Dir $Min $Max $Cond $Squares $SqT) 
    ( (is $Min1 
        (- $Min 1)) 
      (is $Max1 
        (- $Max 1)) 
      (connected $Sq1 $SqT $Dir) 
      (constrained-line-rev $SqF $Dir $Min1 $Max1 $Cond $Squares $Sq1)))
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
    (constrained-line-rev $SqF $Dir $Min $Max $Cond $Squares $SqT) 
    (all-sat-rev $SqF $Dir $Cond 0 $Min $Max $Squares $SqT))
; 

	

  (= 
    (all-sat-rev $SqF $Dir $Cond $Count $Min $Max Nil $SqF) 
    ( (>= $Count $Min) (=< $Count $Max)))
; 

  (= 
    (all-sat-rev $SqF $Dir $Cond $Count $Min $Max 
      (Cons  $SqT $Squares) $SqT) 
    ( (< $Count $Max) 
      (connected $Sq1 $SqT $Dir) 
      (crossable $Cond $SqT) 
      (is $Count1 
        (+ $Count 1)) 
      (all-sat-rev $SqF $Dir $Cond $Count1 $Min $Max $Squares $Sq1)))
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
    (crossable $Descr $Sq) 
    (det-if-then-else 
      (= $Descr empty) 
      (empty $Sq) 
      (, 
        (on $P $Sq) 
        (matches $Descr $P))))
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
    (capturing $Piece $Player $SqF $SqT) 
    ( (captures $Piece $Player $SqF $SqT $Effect $Captured) 
      (place-piece-record $Piece $SqT) 
      (set-effect $Effect $Captured)))
; 
 

;
; 

;
; 

;
; 


  (= 
    (captures $Piece $Player $SqF $SqT) 
    (captures $Piece $Player $SqF $SqT $Effect $Captured))
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
    (captures $Piece $Player $SqF $SqT $Effect $Captured) 
    ( (game--piece-has-capture $Piece $Capture) 
      (capture-has-movement $Capture $Movement) 
      (capture-effect $Capture $Effect) 
      (capturing-movement-for-piece $Piece $SqF $SqT $Player $Movement $Capture $Captured)))
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
    (capturing-movement-for-piece $Piece $SqF $SqT $Player $Movement $Capture $Captured) 
    (capturing-movement-for-piece $Piece $SqF $SqT $Player $Dir $Movement $Capture $Captured))
; 


;
; 

  (= 
    (capturing-movement-for-piece $Piece $SqF $SqT $Player $Dir $Movement $Capture $Captured) 
    (if 
      (, 
        (ride $Movement $Dir $Min $Max $Longest) 
        (longest $Longest) 
        (current-board-type planar)) 
      (longest-capturing-ride $SqF $Dir $Min $Max $SqT $Capture $Captured) 
      (, 
        (movement-for-piece $Piece $SqF $SqT $Player $Movement $Dir $Hopped) 
        (captured-pieces $SqF $SqT $Capture $Dir $Hopped $Captured))))
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
    (longest-moving-ride $SqF $Dir $Min $Max $SqT) 
    ( (> $Min $Max) 
      (set-det) 
      (fail)))
; 

  (= 
    (longest-moving-ride $SqF $Dir $Min $Max $SqT) 
    ( (open-line $SqF $Dir $Max $Max empty $Squares $SqT) 
      (empty $SqT) 
      (set-det)))
; 

  (= 
    (longest-moving-ride $SqF $Dir $Min $Max $SqT) 
    ( (is $Max1 
        (- $Max 1)) (longest-moving-ride $SqF $Dir $Min $Max1 $SqT)))
; 



;
; 



  (= 
    (longest-capturing-ride $SqF $Dir $Min $Max $SqT $Capture $_) 
    ( (> $Min $Max) 
      (set-det) 
      (fail)))
; 

  (= 
    (longest-capturing-ride $SqF $Dir $Min $Max $SqT $Capture $Captured) 
    ( (open-line $SqF $Dir $Max $Max empty $Squares $SqT) 
      (captured-pieces $SqF $SqT $Capture $Dir Nil $Captured) 
      (set-det)))
; 

  (= 
    (longest-capturing-ride $SqF $Dir $Min $Max $SqT $Capture $Captured) 
    ( (is $Max1 
        (- $Max 1)) (longest-capturing-ride $SqF $Dir $Min $Max1 $SqT $Capture $Captured)))
; 


;
; 



  (= 
    (captured-pieces $SqF $SqT $Capturing $Dir $Hopped $Captures) 
    ( (capture-type $Capturing $Type) 
      (if 
        (clobbers $SqT $Capturing $Type $Clobs) True 
        (= $Clobs Nil)) 
      (if 
        (retrieves $SqF $Dir $Capturing $Type $Retrs) True 
        (= $Retrs Nil)) 
      (det-if-then-else 
        (hops $Hopped $Capturing $Type $Hops) True 
        (= $Hops Nil)) 
      (append $Retrs $Clobs $L1) 
      (append $Hops $L1 $Captures) 
      (valid-capture $SqT $Captures)))
; 


;
; 

;
; 

;
; 


  (= 
    (valid-capture $Final $Captured) 
    ( (something-captured $Captured) (will-be-empty $Final $Captured)))
; 


;
; 


  (= 
    (something_captured  
      (Cons  $_ $_)) True)
; 


;
; 


  (= 
    (will-be-empty $Sq $Captured) 
    (det-if-then-else 
      (captured-piece $Piece $Sq $Captured) True 
      (empty $Sq)))
; 



  (= 
    (captured-piece $Victim $SqV $Captured) 
    ( (captured $Cap $Victim $SqV) (member $Cap $Captured)))
; 




  (= 
    (clobbers $Sq $Capturing $Type 
      (:: (@ $Piece $Sq))) 
    ( (capture-has-method $Capturing clobber) 
      (on $Piece $Sq) 
      (matches $Type $Piece)))
; 



  (= 
    (retrieves $SqF $Dir $Capturing $Type 
      (:: (@ $Piece $Sq1))) 
    ( (capture-has-method $Capturing retrieve) 
      (connected $Sq1 $SqF $Dir) 
      (on $Piece $Sq1) 
      (matches $Type $Piece)))
; 



  (= 
    (hops $Hopped $Capturing $Type $Hops) 
    ( (capture-has-method $Capturing hop) (matchers-on-squares $Hopped $Type $Hops)))
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
    (matchers_on_squares  () $_ ()) True)
; 

  (= 
    (matchers-on-squares 
      (Cons  $H $Hs) $Type $Caps) 
    ( (on $P $H) 
      (det-if-then-else 
        (matches $Type $P) 
        (= $Caps 
          (Cons  
            (@ $P $H) $Rest)) 
        (= $Caps $Rest)) 
      (matchers-on-squares $Hs $Type $Rest)))
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
    (capture-op $Effect $Captured $Player) 
    ( (del-effect $Effect $Captured) (effect-captures $Captured $Effect)))
; 




  (= 
    (effect_captures  () $_) True)
; 

  (= 
    (effect-captures 
      (Cons  $Cap $Caps) $Effect) 
    ( (effect-capture $Effect $Cap) (effect-captures $Caps $Effect)))
; 




  (= 
    (effect-capture remove $Cap) 
    ( (captured $Cap $Piece $Sq) (lift-piece $Piece $Sq)))
; 
 
  (= 
    (effect-capture 
      (possess $Owner) $Cap) 
    ( (captured $Cap $Piece $Sq) 
      (lift-piece $Piece $Sq) 
      (piece-struct $Piece $Type $Player) 
      (piece-struct $NewPiece $Type $Owner) 
      (put-in-hand $NewPiece $Owner)))
; 



  (= 
    (captured  
      (@  $Piece $Sq) $Piece $Sq) True)
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
    (try-continue-or-end $Move $Player $Stage) 
    (if 
      (may-continue $Piece $Sq) 
      (if 
        (must-continue $Piece) 
        (continue-captures $Piece $Player $Sq $SqT $Move $Stage) 
        (continue-or-end $Piece $Player $Sq $SqT $Move $Stage)) 
      (discontinue $Move $Stage)))
; 

	      

  (= 
    (must-continue $Piece) 
    (game--piece-must $Piece))
; 



  (= 
    (may-continue $Piece $Sq) 
    ( (moved-onto $Piece $Sq) (game--piece-continues $Piece)))
; 




  (= 
    (continue-or-end $Piece $Player $Sq $SqT 
      (move $Piece $Player $Sq $SqT) capture) 
    ( (del (moved-onto $Piece $Sq)) (capturing $Piece $Player $Sq $SqT)))
; 

  (= 
    (continue-or-end $_ $_ $_ $_ $Move $Stage) 
    (discontinue $Move $Stage))
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
    (continue-captures $Piece $Player $Sq $SqT $Move $Stage) 
    (if 
      (, 
        (= $Move 
          (move $Piece $Player $Sq $SqT)) 
        (del (moved-onto $Piece $Sq)) 
        (capturing $Piece $Player $Sq $SqT)) 
      (= $Stage capture) 
      (discontinue $Move $Stage)))
; 



  (= 
    (discontinue  end_continues promote) True)
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
    (try-promote $Sq $OldPiece $NewPiece $Player $Stage $Player2) 
    ( (moved-onto $OldPiece $Sq) 
      (verbosely (format "Checking for promotion~n" Nil)) 
      (promote-if $OldPiece $Player $Sq $NewPiece $Stage $Player2)))
; 



  (= 
    (promote-if $OldPiece $Player1 $Sq $NewPiece $Stage $Player2) 
    ( (in-promote-region $Sq $Player1) 
      (set-det) 
      (game--piece-promoting $OldPiece $Promoting) 
      (promote-if1 $OldPiece $Promoting $Player1 $Sq $NewPiece $Stage $Player2)))
; 


;
; 

;
; 

;
; 

  (= 
    (promote-if $OldPiece $Player1 $Sq Nil $Stage $Player2) 
    ( (del (moved-onto $OldPiece $Sq)) 
      (verbosely (format "~n~p is not in promotion territory for ~p~n" (:: $OldPiece $Player1))) 
      (place-piece $OldPiece $Sq) 
      (end-promote $Player1 $Stage $Player2)))
; 
	


  (= 
    (promote-if1 $OldPiece $Promoting $Player1 $Sq $NewPiece $Stage $Player2) 
    ( (simple-promote $Promoting $Player1 $NewPiece) 
      (verbosely (format "Promoting: ~p on ~p --> ~p~n" (:: $OldPiece $Sq $NewPiece))) 
      (replace-piece-record $OldPiece $NewPiece $Sq) 
      (del (moved-onto $OldPiece $Sq)) 
      (end-promote $Player1 $Stage $Player2)))
; 
	
	

  (= 
    (promote-if1 $OldPiece $Promoting $Player1 $Sq $NewPiece $Stage $Player2) 
    ( (promoter $Promoting $Player1 $Promoter) (promote-role $Promoter $Player1 $OldPiece $Sq $Stage $Player2)))
; 


;
; 

;
; 


  (= 
    (promote_role  $Player $Player $_ $_ promote_select $Player) True)
; 

  (= 
    (promote-role $Opponent $Player $OldPiece $Sq $Stage $Player2) 
    ( (opposite-role $Player $Opponent) 
      (add (opponent-promotes $OldPiece $Sq)) 
      (del (moved-onto $OldPiece $Sq)) 
      (end-promote $Player $Stage $Player2)))
; 
	



  (= 
    (simple-promote $Promoting $Player $NewPiece) 
    ( (promoting-info $Promoting 
        (promote $Type)) (piece-struct $NewPiece $Type $Player)))
; 



  (= 
    (promoter $Promoting $Player $Promoter) 
    (= $Promoting 
      (promote $Promoter $Descr)))
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
    (in-promote-region $Sq $Player) 
    ( (game--promote-rank $Rank) 
      (invert $Sq $Player $Sq1) 
      (square $Sq1 $X $Y) 
      (>= $Y $Rank)))
; 




;
; 

;
; 

;
; 



  (= 
    (promote-select $Sq $OldPiece $NewPiece $Player $Stage $Player2) 
    ( (moved-onto $OldPiece $Sq) 
      (verbosely-format "~p decides on promotion for ~p on ~p~n" 
        (:: $Player $OldPiece $Sq)) 
      (game--piece-promoting $OldPiece $Promoting) 
      (promoting-options $Promoting $Player $Descr) 
      (matches $Descr $NewPiece) 
      (replace-piece-record $OldPiece $NewPiece $Sq) 
      (del (moved-onto $OldPiece $Sq)) 
      (end-promote $Player $Stage $Player2)))
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
    (end-promote $Player $Stage $Player2) 
    (end-move $Player $Stage $Player2))
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
    (promoting-options $Promoting $Player $Descr) 
    ( (= $Promoting 
        (promote $Actor $Descr1)) (det-if-then-else (opposite-role $Player $Actor) (invert $Descr1 opponent $Descr) (= $Descr $Descr1))))
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
    (opponent-promote $Sq $OldPiece $NewPiece $Player) 
    (det-if-then-else 
      (opponent-promotes $OldPiece $Sq) 
      (, 
        (del (opponent-promotes $OldPiece $Sq)) 
        (verbosely-format "~p decides on promotion for ~p on ~p~n" 
          (:: $Player $OldPiece $Sq)) 
        (init-promote-option $OldPiece $Player $NewPiece) 
        (replace-piece-record $OldPiece $NewPiece $Sq)) 
      (, 
        (= $Sq Nil) 
        (= $OldPiece Nil) 
        (= $NewPiece Nil))))
; 


;
; 


  (= 
    (init-promote-option $OldPiece $Player $NewPiece) 
    ( (game--piece-promoting $OldPiece $Promoting) 
      (promoting-options $Promoting $Player $Descr) 
      (matches $Descr $NewPiece)))
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
    (end-move $Player init-promote $Opponent) 
    ( (verbosely-format "~p finished moving~n" 
        (:: $Player)) (transfer-control $Player $Opponent)))
; 

	
	

