;
; 

;
; 

;
; 

;
; 


;
; 

;
; 



  !(my-ensure-loaded (library piece-names))
; 

  !(my-ensure-loaded (library genstructs))
; 


;
; 

;
; 

;
; 



  (= 
    (generate-game (game $Name $Board $PieceDefs $Goal $Constraints)) 
    ( (record-seed) 
      (new-game-name $Name) 
      (generate-board $Board) 
      (write 'Generated Board') 
      (nl) 
      (generate-pieces $Board $PieceDefs) 
      (write 'Generated Pieces') 
      (nl) 
      (generate-global-constraints $Constraints) 
      (write 'Generated Global Constraints') 
      (nl) 
      (generate-goal $Board $Goal) 
      (write 'Generated Goal ... And Game.') 
      (nl)))
; 



  (= 
    (new-game-name $Name) 
    (gensym game $Name))
; 



  (= 
    (reset-game-name) 
    (reset-gensym game))
; 




;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (generate-board $B) 
    ( (board $B) 
      (board-size $B $Size) 
      (board-type $B $Type) 
      (board-inversion $B $Inversion) 
      (board-promote-rows $B $PromoteRow) 
      (board-array-rows $B $MaxRow) 
      (board-piece-types $B $UniquePieces) 
      (board-placed-pieces $B $UniquePlacedPieces) 
      (board-assignments $B $Assignments) 
      (choose-board-size $Size) 
      (size $Size $XMax $YMax) 
      (choose-board-type $Type) 
      (choose-board-inversion $Inversion) 
      (choose-board-crowding $Crowd) 
      (choose-initial-array-rows $YMax $Crowd $MaxRow) 
      (choose-promotion-regions $MaxRow $MinRow) 
      (is $PromoteRow 
        (- $YMax $MinRow)) 
      (array-squares $XMax $MaxRow $ArSquares) 
      (placeable-pieces $ArSquares $UniquePlacedPieces $PlacedPieceSet) 
      (place-pieces-if-arbitrary $ArSquares $PlacedPieceSet $Assignments) 
      (promote-only-pieces $UniquePlacedPieces $PromotePieces) 
      (append $UniquePlacedPieces $PromotePieces $UniquePieces)))
; 




;
; 


  (= 
    (array-squares $MaxCol $MaxRow $Squares) 
    ( (squares-in-rows 1 $MaxRow $MaxCol $Squares1) (reverse $Squares1 $Squares)))
; 


;
; 

;
; 



  (= 
    (squares-in-rows $Min $Max $Size $Squares) 
    ( (is $Min1 
        (- $Min 1)) (squares-in-rows $Max $Min1 $Max $Size $Squares)))
; 


  (= 
    (squares-in-rows $Min $Min $Max $Size Nil) 
    (set-det))
; 

  (= 
    (squares-in-rows $_ $_ $_ 0 Nil) 
    (set-det))
; 

  (= 
    (squares-in-rows $Row $Min $Max $Size $Squares) 
    ( (squares-in-row $Row $Size $RowSquares) 
      (is $Row1 
        (- $Row 1)) 
      (squares-in-rows $Row1 $Min $Max $Size $RestRows) 
      (append $RowSquares $RestRows $Squares)))
; 




  (= 
    (squares-in-row $Row 0 Nil) 
    (set-det))
; 

  (= 
    (squares-in-row $Row $Size 
      (Cons  $Square $Squares)) 
    ( (is $Size1 
        (- $Size 1)) 
      (square $Square $Size $Row) 
      (squares-in-row $Row $Size1 $Squares)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (place-pieces-if-arbitrary $ArSquares $PieceSet $Assignments) 
    ( (choose-placement-method $Method) (placement-for-method $Method $PieceSet $ArSquares $Assignments)))
; 



  (= 
    (placement-for-method arbitrary $PieceSet $ArSquares $Assignments) 
    ( (set-det) (assign-pieces-to-squares $PieceSet $ArSquares $Assignments)))
; 

  (= 
    (placement-for-method $Method $PieceSet $ArSquares $Assignments) 
    (decision $Assignments $Method $PieceSet $ArSquares))
; 



  (= 
    (assign-pieces-to-squares $Pieces $Squares $Assignments) 
    ( (randomly-pair $Pieces $Squares $Assignments1) (collect-placements $Assignments1 $Assignments)))
; 



  (= 
    (collect-placements $In $Out) 
    (collect-placements $In Nil $Out))
; 


  (= 
    (collect_placements  () $X $X) True)
; 

  (= 
    (collect-placements 
      (Cons  
        (= $A $Elt) $Rest) $Current $New) 
    ( (collapse 
        (= $A $Elt) $Current $New1) (collect-placements $Rest $New1 $New)))
; 



  (= 
    (collapse 
      (= $A $Elt) Nil 
      (:: (= $A (:: $Elt)))) 
    (set-det))
; 

  (= 
    (collapse 
      (= $A $Elt) 
      (Cons  
        (= $A $Elts) $Rest) 
      (Cons  
        (= $A 
          (Cons  $Elt $Elts)) $Rest)) 
    (set-det))
; 

  (= 
    (collapse 
      (= $A $Elt) 
      (Cons  $H $Rest) 
      (Cons  $H $Out)) 
    (collapse 
      (= $A $Elt) $Rest $Out))
; 

;
; 



  (= 
    (uncollect  () ()) True)
; 

  (= 
    (uncollect 
      (Cons  
        (= $A $Elts) $Rest) $Out) 
    ( (uncollapse $A $Elts $As) 
      (uncollect $Rest $R) 
      (append $As $R $Out)))
; 



  (= 
    (uncollapse  $_ () ()) True)
; 

  (= 
    (uncollapse $A 
      (Cons  $H $T) 
      (Cons  
        (= $A $H) $Ts)) 
    (uncollapse $A $T $Ts))
; 



  (= 
    (unpair  () ()) True)
; 

  (= 
    (unpair 
      (Cons  
        (= $Piece $Sq) $Rest) 
      (Cons  $Sq $Squares)) 
    (unpair $Rest $Squares))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (placeable-pieces $ArSquares $UniquePieces $PieceSet) 
    ( (length $ArSquares $Possible) 
      (unique-piece-size $Possible $UniqueNum $SetSize) 
      (tracing-gen-format pieces "Out of <~p> max possible types, chose <~p>, <~p> unique~n" 
        (:: $Possible $SetSize $UniqueNum)) 
      (n-piece-names $UniqueNum $UniquePieces) 
      (duplicate-pieces $UniquePieces $SetSize $PieceSet) 
      (tracing-gen-format pieces "Unique: ~p~n Resulting set: ~p~n" 
        (:: $UniquePieces $PieceSet))))
; 



  (= 
    (unique-piece-size $Possible $UniqueNum $SetSize) 
    ( (piece-set-size $Possible $SetSize) 
      (choose-parameter piece-variety $Variety_Factor) 
      (is $UniqueNum1 
        (integer (* $SetSize $Variety_Factor))) 
      (max $UniqueNum1 1 $UniqueNum)))
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
    (piece-set-size $Possible $SetSize) 
    ( (choose-parameter row-crowding $Crowding) 
      (is $Size 
        (integer (* $Crowding $Possible))) 
      (max $Size 1 $SetSize)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (promote-only-pieces $PlacedPieces $Pieces) 
    ( (choose-parameter promote-only-pieces $N) 
      (length $PlacedPieces $Used) 
      (is $Next 
        (+ $Used 1)) 
      (is $Last1 
        (+ $Next $N)) 
      (min $Last1 26 $Last) 
      (n-piece-names $Next $Last $Pieces)))
; 




;
; 

;
; 

;
; 


  (= 
    (duplicate-pieces $UniquePieces $SetSize $PieceSet) 
    ( (duplicate-elements $UniquePieces $SetSize $PieceSet1) (stable-sort $PieceSet1 $PieceSet)))
; 



  (= 
    (duplicate-elements $UniqueElements $SetSize $Elementset) 
    ( (length $UniqueElements $L) 
      (duplicate-elements $UniqueElements $L $SetSize $DuplicateSet) 
      (append $UniqueElements $DuplicateSet $Elementset)))
; 


  (= 
    (duplicate-elements $Types $Size $Size Nil) 
    (set-det))
; 

  (= 
    (duplicate-elements $Types $Size $Target 
      (Cons  $E $Elements)) 
    ( (random-element $Types $E) 
      (is $Size1 
        (+ $Size 1)) 
      (duplicate-elements $Types $Size1 $Target $Elements)))
; 



;
; 

;
; 

;
; 




  (= 
    (n-piece-names $N $Pieces) 
    (n-piece-names 1 $N $Pieces))
; 



  (= 
    (n-piece-names $N $Max Nil) 
    ( (> $N $Max) (set-det)))
; 

  (= 
    (n-piece-names $N $Max 
      (Cons  $P $Pieces)) 
    ( (new-piece-name $N $P) 
      (is $N1 
        (+ $N 1)) 
      (n-piece-names $N1 $Max $Pieces)))
; 





  (= 
    (new-piece-name $Index $Name) 
    (random-success (possible-index-piece $Index $Name)))
; 




  (= 
    (possible-index-piece $I $Name) 
    ( (indexed-names $I $Names) (member $Name $Names)))
; 



;
; 



  (= 
    (choose-board-size $Size) 
    ( (choose-parameter board-size $XMax) 
      (choose-parameter board-size $YMax) 
      (size $Size $XMax $YMax)))
; 



  (= 
    (choose-board-type $Type) 
    (choose-parameter board-type $Type))
; 



  (= 
    (choose-board-inversion $Type) 
    (choose-parameter board-inversion $Type))
; 




  (= 
    (choose-board-crowding $Crowd) 
    (choose-parameter board-crowding $Crowd))
; 



  (= 
    (choose-initial-array-rows $Size $Crowding $MaxRow) 
    ( (is $MaxRow1 
        (integer (/ (* $Size $Crowding) 2))) (max 1 $MaxRow1 $MaxRow)))
; 



  (= 
    (choose-promotion-regions $MaxRow $MinRow) 
    ( (choose-promotion-fraction $D) (is $MinRow (integer (* $MaxRow $D)))))
; 



  (= 
    (choose-promotion-fraction $D) 
    (choose-parameter promotion-fraction $D))
; 




  (= 
    (choose-placement-method $M) 
    (choose-parameter placement-method $M))
; 



;
; 

;
; 

;
; 



  (= 
    (generate-pieces $Board $PieceDefs) 
    ( (board-piece-types $Board $Types) 
      (generate-pieces $Types $Board $PieceDefs1) 
      (sort $PieceDefs1 $PieceDefs)))
; 



;
; 

  (= 
    (generate_pieces  () $_ ()) True)
; 

  (= 
    (generate-pieces 
      (Cons  $P $Ps) $Board 
      (Cons  $Def $Defs)) 
    ( (generate-piece $P $Board $Def) (generate-pieces $Ps $Board $Defs)))
; 




  (= 
    (generate-piece $Name $Board $Piece) 
    ( (piece $Piece) 
      (piece-name $Piece $Name) 
      (assign-movement-power $Piece $Board) 
      (assign-capture-power $Piece $Board) 
      (assign-promotion-power $Piece $Board) 
      (assign-piece-constraints $Piece $Board)))
; 



;
; 

;
; 

;
; 




  (= 
    (assign-movement-power $Piece $Board) 
    ( (create-complex-movement $Board $Movement) (piece-movement $Piece $Movement)))
; 

	

  (= 
    (create-complex-movement $Board $Movement) 
    ( (create-movement $Board $M1) (complexify-movement $Board $M1 $Movement)))
; 



  (= 
    (complexify-movement $Board $MIn 
      (Cons  $MIn $MOut)) 
    ( (choose-parameter movement-complexity) 
      (set-det) 
      (create-complex-movement $Board $MOut)))
; 

  (= 
    (complexify_movement  $_ $X 
      ($X)) True)
; 
           




  (= 
    (create-movement $Board $Movement) 
    ( (movement $Movement) 
      (choose-direction $Board $Dir) 
      (movement-dir $Movement $Dir) 
      (choose-symmetries $Syms) 
      (movement-sym $Movement $Syms) 
      (choose-movement-type $Type) 
      (constrain-movement $Type $Dir $Board $Movement)))
; 



;
; 

;
; 

;
; 


;
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
    (choose-direction $Board $Dir) 
    ( (choose-locality $L) 
      (board-size $Board $X $Y) 
      (is $XMax1 
        (integer (* (- $X 1) $L))) 
      (is $YMax1 
        (integer (* (- $Y 1) $L))) 
      (max 1 $XMax1 $XMax) 
      (max 1 $YMax1 $YMax) 
      (choose-dir $XMax $YMax $Dir)))
; 



  (= 
    (choose-dir $XMax $YMax $Dir) 
    ( (choose-delta $XMax $Dx) 
      (choose-delta $YMax $Dy) 
      (legal-dir $XMax $YMax $Dx $Dy $Dir)))
; 


;
; 

;
; 


  (= 
    (legal-dir $XMax $YMax 0 0 $Dir) 
    ( (set-det) (choose-dir $XMax $YMax $Dir)))
; 

  (= 
    (legal-dir $_ $_ $Dx $Dy $Dir) 
    (direction $Dir $Dx $Dy))
; 

	
;
; 


  (= 
    (choose-delta $Max $Delta) 
    ( (is $M 
        (+ $Max 1)) (random 0 $M $Delta)))
; 



  (= 
    (max-delta $Dir $D) 
    ( (direction $Dir $X $Y) (max $X $Y $D)))
; 


;
; 

;
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
    (choose-symmetries $Sym) 
    ( (symmetry $Sym) 
      (choose-parameter 
        (symmetry rotation) $R) 
      (choose-parameter 
        (symmetry forward) $F) 
      (choose-parameter 
        (symmetry side) $S) 
      (sym-forward $Sym $F) 
      (sym-side $Sym $S) 
      (sym-rotation $Sym $R)))
; 


;
; 

;
; 

;
; 

;
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
    (choose-locality $L) 
    (choose-parameter locality $L))
; 



  (= 
    (choose-movement-type $T) 
    (choose-parameter movement-type $T))
; 



  (= 
    (constrain-movement leaper $_ $Board $Movement) 
    ( (set-det) 
      (leaper $L) 
      (movement-type $Movement $L)))
; 

  (= 
    (constrain-movement rider $Dir $Board $Movement) 
    ( (set-det) 
      (rider $Rider) 
      (choose-must-ride $Rider) 
      (choose-min-ride $Board $Dir $Rider) 
      (choose-max-ride $Board $Dir $Rider) 
      (movement-type $Movement $Rider)))
; 

  (= 
    (constrain-movement hopper $Dir $Board $Movement) 
    ( (set-det) 
      (hopper $Hopper) 
      (choose-hopper-type $Board $R) 
      (hopper-type $Hopper $R) 
      (choose-before $Hopper $Board) 
      (choose-over $Hopper $Board) 
      (choose-after $Hopper $Board) 
      (valid-hopper $Hopper $Dir $Board $Movement)))
; 



;
; 

;
; 

;
; 



  (= 
    (choose-must-ride $Rider) 
    ( (choose-parameter must-ride $Must) (rider-must $Rider $Must)))
; 



  (= 
    (choose-min-ride $Board $Dir $Rider) 
    (rider-min $Rider 1))
; 


;
; 

;
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
    (choose-max-ride $Board $Dir $Rider) 
    ( (choose-locality $L) 
      (board-max-size $Board $Size) 
      (max-delta $Dir $D) 
      (is $Rides 
        (integer (/ (* $L $Size) $D))) 
      (set-max-rides $Rider $Rides)))
; 




  (= 
    (set-max-rides $Rider $Rides) 
    ( (> $Rides 1) 
      (set-det) 
      (rider-max $Rider $Rides)))
; 

  (= 
    (set-max-rides $Rider $_) 
    (rider-max $Rider any))
; 



;
; 

;
; 

;
; 



  (= 
    (choose-before $Hopper $Board) 
    (constrain-hopper before $Board $Hopper))
; 



  (= 
    (choose-over $Hopper $Board) 
    (constrain-hopper over $Board $Hopper))
; 



  (= 
    (choose-after $Hopper $Board) 
    (constrain-hopper after $Board $Hopper))
; 



;
; 

;
; 

;
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
    (constrain-hopper $Type $Board $Hopper) 
    (det-if-then-else 
      (choose-parameter (constrain (hopper $Type $_))) 
      (hopper-equation $Type $Board $Hopper) 
      (hopper-any $Type $Hopper)))
; 


;
; 

;
; 

;
; 


  (= 
    (hopper-equation $Type $Board $Hopper) 
    ( (board-max-size $Board $Dist) 
      (hopper-component $Type $Hopper $Comp) 
      (choose-parameter 
        (hopper $Type $Dist) $P) 
      (choose-equation $P $Comp)))
; 


;
; 


  (= 
    (hopper-any over $Hopper) 
    ( (set-det) 
      (comparison $C geq 1) 
      (hopper-component over $Hopper $C)))
; 

  (= 
    (hopper-any $Type $Hopper) 
    ( (comparison $C geq 0) (hopper-component $Type $Hopper $C)))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (choose-hopper-type $Board $C) 
    (choose-piece-description $Board $C))
; 



  (= 
    (hopper-component before $Hopper $X) 
    (hopper-before $Hopper $X))
; 

  (= 
    (hopper-component over $Hopper $X) 
    (hopper-over $Hopper $X))
; 

  (= 
    (hopper-component after $Hopper $X) 
    (hopper-after $Hopper $X))
; 




;
; 


  (= 
    (valid-hopper $Hopper $Dir $Board $Movement) 
    ( (hopper-can-move $Hopper $Dir $Board) 
      (set-det) 
      (movement-type $Movement $Hopper)))
; 

  (= 
    (valid-hopper $Hopper $Dir $Board $Movement) 
    (constrain-movement hopper $Dir $Board $Movement))
; 



;
; 

;
; 

;
; 

;
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
    (hopper-can-move $Hopper $Dir $Board) 
    ( (hopper-min-rides $Hopper $Rides) 
      (board-size $Board $BX $BY) 
      (direction $Dir $DX $DY) 
      (< 
        (* $Rides $DX) $BX) 
      (< 
        (* $Rides $DY) $BY)))
; 




  (= 
    (valid-hopper-max-dir $Dx $Dy $Max) 
    ( (current-board-size $Bx $By) 
      (is $XMax 
        (// $Bx $Dx)) 
      (is $YMax 
        (// $By $Dy)) 
      (min $XMax $YMax $Max)))
; 



;
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
    (hopper-min-rides $H $Min) 
    ( (component-min $H before $Min1) 
      (component-min $H over $Min2) 
      (component-min $H after $Min3) 
      (is $Min 
        (+ 
          (+ 
            (+ $Min1 $Min2) $Min3) 1))))
; 



  (= 
    (component-min $H $T $Min) 
    ( (hopper-component $T $H $Eq) (min-rides $Eq $Min)))
; 

	

  (= 
    (min-rides $Eq $Min) 
    ( (comparison $Eq geq $Min) (set-det)))
; 

  (= 
    (min-rides $Eq $Min) 
    ( (comparison $Eq eq $Min) (set-det)))
; 

  (= 
    (min_rides  $Eq 0) True)
; 





  (= 
    (choose-equation $Num $Eq) 
    ( (choose-parameter comparative $C) (comparison $Eq $C $Num)))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (choose-piece-description $Board $Desc) 
    ( (player-generalization $Board $Player) 
      (piece-generalization $Board $Pieces) 
      (piece-description $Desc $Player $Pieces)))
; 



  (= 
    (piece-generalization $Board $Gen) 
    ( (choose-piece-generalization-level $L) (generalize-piece $L $Board $Gen)))
; 



  (= 
    (generalize-piece specific $Board $Pieces) 
    (choose-general-piece-set $Board $Pieces))
; 

  (= 
    (generalize_piece  any $Board any_piece) True)
; 



;
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
    (choose-general-piece-set $Board $Pieces) 
    ( (board-piece-types $Board $General) (choose-set-by-param $General more-general-pieces $Pieces)))
; 


;
; 

;
; 


  (= 
    (choose-unplaced-piece-set $Board $Pieces) 
    ( (board-unplaced-pieces $Board $Unplaced) (choose-set-by-param $Unplaced more-arrival-pieces $Pieces)))
; 


;
; 

;
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
    (choose-set-by-param $Set $Param $Items) 
    ( (= $Goal 
        (choose-parameter $Param)) (choose-conditionally-from-set $Set $Goal $Items)))
; 



;
; 

;
; 

;
; 

;
; 

;
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
    (choose-conditionally-from-set $Set $Goal $Items) 
    ( (random-permute $Set $Set1) 
      (choose-conditionally-from-set1 $Set1 $Goal $Items1) 
      (sort $Items1 $Items)))
; 



  (= 
    (choose_conditionally_from_set1  () $_ ()) True)
; 

  (= 
    (choose-conditionally-from-set1 
      (Cons  $Item $Rest) $Goal 
      (Cons  $Item $Items)) 
    (det-if-then-else 
      (call $Goal) 
      (choose-conditionally-from-set $Rest $Goal $Items) 
      (= $Items Nil)))
; 



  (= 
    (player-generalization $Board $Gen) 
    ( (choose-player-generalization-level $L) (generalize-player $L $Board $Gen)))
; 


;
; 


  (= 
    (generalize-player specific $Board $Player) 
    ( (set-det) (random-player $Board $Player)))
; 

  (= 
    (generalize_player  any $Board any_player) True)
; 



  (= 
    (random-player $Board $Player) 
    ( (players $Players) (random-element $Players $Player)))
; 



  (= 
    (players  
      (player opponent)) True)
; 




  (= 
    (random-piece $Board $Piece) 
    ( (board-piece-types $Board $Types) (random-element $Types $Piece)))
; 



  (= 
    (random-square $Board $Sq) 
    ( (board-size $Board $XMax $YMax) 
      (random $XMax $X) 
      (random $YMax $Y) 
      (square $Sq $X $Y)))
; 



  (= 
    (choose-player-generalization-level $L) 
    (choose-parameter player-generalization-level $L))
; 



  (= 
    (choose-piece-generalization-level $L) 
    (choose-parameter piece-generalization-level $L))
; 


;
; 

;
; 

;
; 



  (= 
    (assign-capture-power $Piece $Board) 
    ( (create-complex-capture $Board $Capture) (piece-capture $Piece $Capture)))
; 



  (= 
    (create-complex-capture $Board $Capture) 
    ( (create-capture $Board $C1) (complexify-capture $Board $C1 $Capture)))
; 



  (= 
    (complexify-capture $Board $CIn 
      (Cons  $CIn $C1)) 
    ( (choose-parameter capture-complexity) 
      (set-det) 
      (create-complex-capture $Board $C1)))
; 

  (= 
    (complexify_capture  $_ $X 
      ($X)) True)
; 
           




  (= 
    (create-capture $Board $Capture) 
    ( (choose-capture-movements $Board $Movement) 
      (capture-movement $Capture $Movement) 
      (choose-capture-methods $Method) 
      (capture-methods $Capture $Method) 
      (choose-capture-type $Board $Restrict) 
      (capture-type $Capture $Restrict) 
      (choose-capture-effect $Effect) 
      (capture-effect $Capture $Effect)))
; 



;
; 

;
; 

;
; 

;
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
    (choose-capture-movements $Board $Movement) 
    (create-complex-movement $Board $Movement))
; 




;
; 

;
; 

;
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
    (choose-capture-methods $Methods) 
    ( (choose-parameter 
        (capture-method retrieve) $R) 
      (choose-parameter 
        (capture-method clobber) $C) 
      (choose-parameter 
        (capture-method hop) $H) 
      (method-retrieve $Methods $R) 
      (method-clobber $Methods $C) 
      (method-hop $Methods $H)))
; 




;
; 

;
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
    (choose-capture-effect $E) 
    (choose-parameter capture-effect $E))
; 



;
; 

;
; 

;
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
    (choose-capture-type $Board $C) 
    (choose-piece-description $Board $C))
; 


;
; 

;
; 

;
; 



  (= 
    (assign-promotion-power $Piece $Board) 
    ( (choose-promotion $Board $Prom) (piece-promote $Piece $Prom)))
; 



  (= 
    (choose-promotion $Board $Prom) 
    (det-if-then-else 
      (choose-parameter specific-promotion) 
      (specific-promotion $Board $Prom) 
      (promotion-decision $Board $Prom)))
; 
 


  (= 
    (specific-promotion $Board 
      (promote $Piece)) 
    (random-piece $Board $Piece))
; 



  (= 
    (promotion-decision $Board $D) 
    ( (random-player $Player) 
      (choose-piece-description $Board $Desc) 
      (decision $D) 
      (decision-chooser $D $Player) 
      (decision-options $D $Desc)))
; 



  (= 
    (random-player $P) 
    ( (board-players $_ $Players) (random-element $Players $P)))
; 



  (= 
    (board-players $B 
      (:: player opponent)) 
    (board $B))
; 



;
; 

;
; 

;
; 

;
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
    (assign-piece-constraints $Piece $Board) 
    ( (choose-piece-constraints $Board $Con) (piece-constraints $Piece $Con)))
; 

	

  (= 
    (choose-piece-constraints $Board $Con) 
    ( (constraint $Con) 
      (choose-parameter must-capture $Must) 
      (constraint-must-capture $Con $Must) 
      (choose-parameter continue-captures $Cont) 
      (constraint-continue-captures $Con $Cont)))
; 




  (= 
    (piece-must-capture $Piece $Must) 
    ( (piece-constraints $Piece $Con) (constraint-must-capture $Con $Must)))
; 



  (= 
    (piece-continue-captures $Piece $Continue) 
    ( (piece-constraints $Piece $Con) (constraint-continue-captures $Con $Continue)))
; 




;
; 

;
; 

;
; 

;
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
    (generate-global-constraints $Con) 
    (choose-game-constraints $Con))
; 

	
;
; 

;
; 


  (= 
    (choose-game-constraints $Con) 
    ( (constraint $Con) 
      (choose-parameter must-capture $Must) 
      (constraint-must-capture $Con $Must) 
      (constraint-continue-captures $Con no)))
; 




  (= 
    (game-must-capture $Game $Must) 
    ( (game-constraints $Game $Con) (constraint-must-capture $Con $Must)))
; 



  (= 
    (game-continue-captures $Game $Continue) 
    ( (game-constraints $Game $Con) (constraint-continue-captures $Con $Continue)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (generate-goal $Board $Goals) 
    ( (choose-stalemate-goal $Board $Goal1) 
      (complexify-goal $Board Nil $ComplexGoal) 
      (tracing-gen-format goals "~n** Simplifying goals ...~n" Nil) 
      (tracing-gen simplify 
        (, 
          (format "Before: ~n" Nil) 
          (ppl (Cons  $Goal1 $ComplexGoal)))) 
      (simplify-goals 
        (Cons  $Goal1 $ComplexGoal) $Board $Goals) 
      (tracing-gen-format goals "~n** Done Simplifying.~n" Nil) 
      (tracing-gen simplify 
        (, 
          (format "~nAfter: ~n" Nil) 
          (ppl $Goals)))))
; 



  (= 
    (complexify-goal $Board $GIn $G) 
    ( (choose-parameter goal-complexity) 
      (set-det) 
      (unsubsumed-goal $GIn $Board $GNew) 
      (complexify-goal $Board 
        (Cons  $GNew $GIn) $G)))
; 

  (= 
    (complexify_goal  $_ $X $X) True)
; 
           


;
; 

;
; 


  (= 
    (unsubsumed-goal $Goals $Board $NewGoal) 
    ( (create-goal $Board $Goal) (ensure-new-goal $Goals $Board $Goal $NewGoal)))
; 



  (= 
    (ensure-new-goal $Goals $Board $Goal $NewGoal) 
    ( (member $Goal $Goals) 
      (set-det) 
      (unsubsumed-goal $Goals $Board $NewGoal)))
; 

  (= 
    (ensure-new-goal $Goals $Board $Goal $NewGoal) 
    ( (subsumed-goal $Goal $Goals $Board) 
      (set-det) 
      (unsubsumed-goal $Goals $Board $NewGoal)))
; 

  (= 
    (ensure_new_goal  $Goals $Board $Goal $Goal) True)
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
    (create-goal $Board $Goal) 
    ( (choose-parameter goal-type $Type) (choose-goal-of-type $Type $Board $Goal)))
; 



  (= 
    (choose-goal-of-type arrive $B $Goal) 
    (choose-arrive-goal $B $Goal))
; 

  (= 
    (choose-goal-of-type eradicate $B $Goal) 
    (choose-eradicate-goal $B $Goal))
; 


;
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
    (choose-stalemate-goal $Board $Stale) 
    ( (random-player $Board $Player) (stalemate-goal $Stale $Player)))
; 


;
; 

;
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
    (choose-arrive-goal $Board $Goal) 
    ( (choose-parameter arrive-goal-player $Player) 
      (choose-arrive-goal-for-player $Player $Type $Squares $Board) 
      (piece-description $Desc $Player $Type) 
      (arrive-goal $Goal $Desc $Squares)))
; 

	

  (= 
    (choose-arrive-goal-for-player player $Type $Squares $Board) 
    (player-arrive-goal $Type $Squares $Board))
; 

  (= 
    (choose-arrive-goal-for-player any-player $Type $Squares $Board) 
    (player-arrive-goal $Type $Squares $Board))
; 

  (= 
    (choose-arrive-goal-for-player opponent $Type $Squares $Board) 
    (opponent-arrive-goal $Type $Squares $Board))
; 


;
; 


  (= 
    (player-arrive-goal $Type $Squares $Board) 
    ( (choose-unplaced-piece-set $Board $Type) 
      (random-square $Board $Sq) 
      (= $Squares 
        (:: $Sq))))
; 


;
; 

;
; 

;
; 


  (= 
    (opponent-arrive-goal $Type $Squares $Board) 
    ( (piece-generalization $Board $Type) 
      (opponent-random-free-square $Board $Sq) 
      (= $Squares 
        (:: $Sq))))
; 




  (= 
    (opponent-random-free-square $Board $Sq) 
    ( (board-size $Board $XMax $YMax) 
      (array-squares $XMax $YMax $BSquares) 
      (random-permute $BSquares $RSquares) 
      (board-player-assigned-squares $Board opponent $Assigned) 
      (free-different-member $RSquares $Assigned $Sq)))
; 



  (= 
    (free-different-member Nil $_ $_) 
    ( (format "Error: no free squares for opponent arrival goal!~n" Nil) 
      (set-det) 
      (fail)))
; 

  (= 
    (free-different-member 
      (Cons  $S $Sqs) $Assigned $Sq) 
    (det-if-then-else 
      (member $S $Assigned) 
      (free-different-member $Sqs $Assigned $Sq) 
      (= $Sq $S)))
; 

	  

;
; 

;
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
    (choose-eradicate-goal $Board $Goal) 
    ( (choose-parameter eradicate-goal-player $Player) 
      (choose-eradicate-goal-for-player $Player $Type $Board) 
      (piece-description $Desc $Player $Type) 
      (eradicate-goal $Goal $Desc)))
; 

	

  (= 
    (choose-eradicate-goal-for-player $Player $PieceGen $Board) 
    (eradicate-piece-generalization $Board $PieceGen))
; 

	
;
; 

;
; 

;
; 

;
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
    (eradicate-piece-generalization $Board $Gen) 
    ( (choose-eradicate-generalization-level $L) (generalize-eradicate-piece $L $Board $Gen)))
; 


;
; 

;
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
    (eradicate-piece-set $Board $SortedPieces) 
    ( (random-placed-piece $Board $Placed) 
      (board-piece-types $Board $Types) 
      (select $Placed $Types $Rest) 
      (choose-set-by-param $Rest more-eradicate-pieces $Pieces) 
      (sort 
        (Cons  $Placed $Pieces) $SortedPieces)))
; 



  (= 
    (random-placed-piece $Board $Piece) 
    ( (board-placed-pieces $Board $Pieces) (random-element $Pieces $Piece)))
; 

	

  (= 
    (choose-eradicate-generalization-level $L) 
    (choose-parameter eradicate-generalization-level $L))
; 



  (= 
    (generalize-eradicate-piece specific $Board $Pieces) 
    (eradicate-piece-set $Board $Pieces))
; 

  (= 
    (generalize_eradicate_piece  any $Board any_piece) True)
; 



;
; 

;
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
    (simplify-goals $Complex $Board $Simplified) 
    ( (remove-duplicates $Complex $Simp1) (remove-subsumed $Simp1 $Board $Simplified)))
; 



  (= 
    (remove-subsumed $Goals $Board $Simplified) 
    (remove-subsumed $Goals $Goals $Board $Simplified))
; 


;
; 

;
; 

  (= 
    (remove_subsumed  () $_ $_ ()) True)
; 

  (= 
    (remove-subsumed 
      (Cons  $Goal $Goals) $AllGoals $Board $Gs) 
    ( (subsumed-goal $Goal $AllGoals $Board) 
      (set-det) 
      (remove-subsumed $Goals $AllGoals $Board $Gs)))
; 

  (= 
    (remove-subsumed 
      (Cons  $Goal $Goals) $AllGoals $Board 
      (Cons  $Goal $Gs)) 
    (remove-subsumed $Goals $AllGoals $Board $Gs))
; 



;
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
    (subsumed-goal $Goal $Goals $Board) 
    ( (member $Sub $Goals) 
      (not (= $Sub $Goal)) 
      (subsumes $Sub $Goal $Board) 
      (tracing-gen-format subsume "~nSubsumed Goal: ~p~nSubsumed by: ~p~n" 
        (:: $Goal $Sub))))
; 


;
; 

;
; 

;
; 


  (= 
    (subsumes $Sub $Goal $Board) 
    (goal-implies $Goal $Sub $Board))
; 



  (= 
    (goal-implies $Erad1 $Erad2 $Board) 
    ( (eradicate-goal $Erad1 $Player1 $Type1) 
      (eradicate-goal $Erad2 $Player2 $Type2) 
      (erad-implies $Player1 $Type1 $Player2 $Type2)))
; 

  (= 
    (goal-implies $Arr1 $Arr2 $Board) 
    ( (arrive-goal $Arr1 $Player1 $Type1 $Sq1) 
      (arrive-goal $Arr2 $Player2 $Type2 $Sq2) 
      (arrive-implies $Player1 $Type1 $Sq1 $Player2 $Type2 $Sq2 $Board)))
; 


;
; 

;
; 

;
; 

;
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
    (erad-implies $Player1 $Type1 $Player2 $Type2) 
    (type-contains $Type1 $Type2))
; 


;
; 

;
; 

;
; 

;
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
    (arrive-implies $Player1 $Type1 $Sq1 $Player1 $Type2 $Sq2 $Board) 
    ( (set-det) (arrive-implies $Type1 $Sq1 $Type2 $Sq2)))
; 

  (= 
    (arrive-implies $Player1 $Type1 $Sq1 $Player2 $Type2 $Sq2 $Board) 
    ( (invert-board-squares $Board $Sq1 $Sq1Inv) (arrive-implies $Type1 $Sq1Inv $Type2 $Sq2)))
; 

	
  (= 
    (arrive-implies $Type1 $Sq1 $Type2 $Sq2) 
    ( (type-contains $Type2 $Type1) (squares-contains $Sq2 $Sq1)))
; 



  (= 
    (squares-contains $Sq1 $Sq2) 
    (ord-subset $Sq2 $Sq1))
; 



  (= 
    (type_contains  any_piece $_) True)
; 

  (= 
    (type-contains $Type1 $Type2) 
    (ord-subset $Type2 $Type1))
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
    (unplaced-piece-description $Board $Desc) 
    (not (placed-piece-description $Board $Desc)))
; 


;
; 


  (= 
    (placed-piece-description $Board $Desc) 
    ( (piece-description-piece $Desc $Piece) (contains-placed-piece $Piece $Board)))
; 



  (= 
    (contains-placed-piece any-piece $_) 
    (set-det))
; 

  (= 
    (contains-placed-piece $Pieces $Board) 
    ( (board-placed-pieces $Board $Placed) 
      (member $P $Pieces) 
      (member $P $Placed) 
      (set-det)))
; 


;
; 


;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  !(my-ensure-loaded (library tracing))
; 



  (= 
    (tracing-gen $Type $Call) 
    (det-if-then-else 
      (tracing (gen $Type)) 
      (call $Call) True))
; 


;
; 


  (= 
    (tracing-gen-format $Type $String $Args) 
    (det-if-then-else 
      (tracing (gen $Type)) 
      (format $String $Args) True))
; 



  (= 
    (tracing-gen-timing $Type $Call) 
    (trace-timing 
      (gen $Type) $Call))
; 



  (= 
    (set-gen-verbosity $Level $Status) 
    (set-tracing 
      (gen $Level) $Status))
; 



  (= 
    (silent-gen) 
    (all-gen off))
; 


  (= 
    (loud-gen) 
    (all-gen on))
; 



  (= 
    (all-gen $Status) 
    ( (set-gen-verbosity goals $Status) 
      (set-gen-verbosity simplify $Status) 
      (set-gen-verbosity subsume $Status) 
      (set-gen-verbosity pieces $Status)))
; 
 


  (= 
    (trace-gen-subsume) 
    (set-gen-verbosity subsume on))
; 
 

  (= 
    (trace-gen-simplify) 
    (set-gen-verbosity simplify on))
; 
 

  (= 
    (trace-gen-goals) 
    (set-gen-verbosity goals on))
; 
 

  (= 
    (trace-gen-pieces) 
    (set-gen-verbosity pieces on))
; 
 

;
; 

;
; 




