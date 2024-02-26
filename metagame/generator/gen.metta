;                                           (convert_to_metta_file  gen $_397920 metagame/generator/gen.pl metagame/generator/gen.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; gen.pl
;
; Generates new symmetric chess-like games.


  !(my-ensure-loaded (library piece-names))
  !(my-ensure-loaded (library genstructs))

;
; ================================================================================
;
; GENERATE GAME
;
; ================================================================================


  (= (generate-game (game $Name $Board $PieceDefs $Goal $Constraints))  
    (record-seed)
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
    (nl))


  (= (new-game-name $Name)  
    (gensym game $Name))


  (= (reset-game-name)  
    (reset-gensym game))



;
; ================================================================================
;
; GENERATE BOARD
;
; ================================================================================

;
; Parameters used:
;
; board_type
;
; board_size
;
; board_crowding
;
; row_crowding
;
; placement_method
;
; promotion_fraction
;
; promote_only
;
; 
;
; board_size: Avg. size of square board. Chess has 8 squares.
;
; board_crowding: Fraction of board to use for placing initial arrays.
;
; Chess uses 1/2 the board
;
; row_crowding: Fraction of each row in init array to fill. Chess fills
;
; the rows entirely.
;
; piece_variety: Of possible pieces (# used array locations), fraction which
;
; will be unique piece types.
;
; promote_only: A number of pieces-types which are generated, but not placed
;
; on the initial board, and thus can only appear by promotion.
;
; While piece_variety is a fraction constrained by the board size,
;
; this is unconstrained, and is thus a range parameter.

;
; Thus, the number of unique piece-types used in a game will be:
;
; placed_pieces: (board_size^2/2)*board_crowding*row_crowding*piece_variety
;
; promote_only: [Lower,Upper]
;
; total = placed_pieces + promote_only
;
; 
;
; Note that the total number of piece types is a major source of complexity
;
; in these games. It does not generally influence the branching factor
;
; (as new pieces only appear when old ones go away),
;
; but it increases exponentially the number of possible positions which can
;
; occur.



;
; board(Size,Type,Inversion,MaxRow,PromoteRow,_KilledSquares,
;
; UniquePieces,UniquePlacedPieces,Assignments)) :-


  (= (generate-board $B)  
    (board $B)
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
    (append $UniquePlacedPieces $PromotePieces $UniquePieces))



;
; array_squares(+XMax,+YMax,-Squares)

  (= (array-squares $MaxCol $MaxRow $Squares)  
    (squares-in-rows 1 $MaxRow $MaxCol $Squares1)
    (reverse $Squares1 $Squares))

;
; squares_in_rows(MinRow,MaxRow,MaxCol,Squares)
;
; Generates MaxCol squares in each of rows [MinRow .. MaxRow].


  (= (squares-in-rows $Min $Max $Size $Squares)  
    (is $Min1 
      (- $Min 1))
    (squares-in-rows $Max $Min1 $Max $Size $Squares))

  (= (squares-in-rows $Min $Min $Max $Size Nil)  
    (set-det))
  (= (squares-in-rows $_ $_ $_ 0 Nil)  
    (set-det))
  (= (squares-in-rows $Row $Min $Max $Size $Squares)  
    (squares-in-row $Row $Size $RowSquares)
    (is $Row1 
      (- $Row 1))
    (squares-in-rows $Row1 $Min $Max $Size $RestRows)
    (append $RowSquares $RestRows $Squares))



  (= (squares-in-row $Row 0 Nil)  
    (set-det))
  (= (squares-in-row $Row $Size (Cons  $Square $Squares))  
    (is $Size1 
      (- $Size 1))
    (square $Square $Size $Row)
    (squares-in-row $Row $Size1 $Squares))
; ; Column first, then row.


;
; PLACE_PIECES_IF_ARBITRARY(+ArSquares,+PieceSet,-Assignments)
;
; PieceSet is the set of pieces which will be assigned to
;
; squares in ArSquares (the initial array squares).
;
; This is either done arbitrarily, in which case the pairings
;
; are made now, or by some method of decision, in which
;
; case the piece set and assignable squares will be given with
;
; the corresponding method, the assignment itself to be done at
;
; game time.
;
; 
;
; Parameter: placement_method
;
; 

  (= (place-pieces-if-arbitrary $ArSquares $PieceSet $Assignments)  
    (choose-placement-method $Method)
    (placement-for-method $Method $PieceSet $ArSquares $Assignments))


  (= (placement-for-method arbitrary $PieceSet $ArSquares $Assignments)  
    (set-det)
    (assign-pieces-to-squares $PieceSet $ArSquares $Assignments))
  (= (placement-for-method $Method $PieceSet $ArSquares $Assignments)  
    (decision $Assignments $Method $PieceSet $ArSquares))


  (= (assign-pieces-to-squares $Pieces $Squares $Assignments)  
    (randomly-pair $Pieces $Squares $Assignments1)
    (collect-placements $Assignments1 $Assignments))


  (= (collect-placements $In $Out)  
    (collect-placements $In Nil $Out))

  (= (collect_placements  () $X $X)  True)
  (= (collect-placements (Cons  (= $A $Elt) $Rest) $Current $New)  
    (collapse 
      (= $A $Elt) $Current $New1)
    (collect-placements $Rest $New1 $New))


  (= (collapse (= $A $Elt) Nil (:: (= $A (:: $Elt))))  
    (set-det))
  (= (collapse (= $A $Elt) (Cons  (= $A $Elts) $Rest) (Cons  (= $A (Cons  $Elt $Elts)) $Rest))  
    (set-det))
  (= (collapse (= $A $Elt) (Cons  $H $Rest) (Cons  $H $Out))  
    (collapse 
      (= $A $Elt) $Rest $Out))
;
; collapse(X,Y,[X|Y]).


  (= (uncollect  () ())  True)
  (= (uncollect (Cons  (= $A $Elts) $Rest) $Out)  
    (uncollapse $A $Elts $As)
    (uncollect $Rest $R)
    (append $As $R $Out))


  (= (uncollapse  $_ () ())  True)
  (= (uncollapse $A (Cons  $H $T) (Cons  (= $A $H) $Ts))  
    (uncollapse $A $T $Ts))


  (= (unpair  () ())  True)
  (= (unpair (Cons  (= $Piece $Sq) $Rest) (Cons  $Sq $Squares))  
    (unpair $Rest $Squares))


;
; PLACEABLE_PIECES(+ArSquares,-UniquePieces,-PieceSet)
;
; -------------
;
; ARSQUARES: a set of squares to which pieces might be
;
; initially assigned.
;
; UNIQUEPIECES: a set of unique piece names (containing at least 1 piece)
;
; PIECESET: duplicates these unique pieces to achieve a set of the right size.
;
; 
;
; Parameters used here:
;
; row_crowding: what fraction of squares to place pieces on in initial array.
;
; piece_variety: of a possible number of pieces, what fraction should be unique.
;
; (Checkers has low variety, chess has high, shogi even higher).
;
; row_crowding * possible_set_size --> number of squares to be assigned pieces
;
; piece_variety * assigned_squares --> number of unique pieces to be generated.
;
; 

  (= (placeable-pieces $ArSquares $UniquePieces $PieceSet)  
    (length $ArSquares $Possible)
    (unique-piece-size $Possible $UniqueNum $SetSize)
    (tracing-gen-format pieces "Out of <~p> max possible types, chose <~p>, <~p> unique~n" 
      (:: $Possible $SetSize $UniqueNum))
    (n-piece-names $UniqueNum $UniquePieces)
    (duplicate-pieces $UniquePieces $SetSize $PieceSet)
    (tracing-gen-format pieces "Unique: ~p~n Resulting set: ~p~n" 
      (:: $UniquePieces $PieceSet)))


  (= (unique-piece-size $Possible $UniqueNum $SetSize)  
    (piece-set-size $Possible $SetSize)
    (choose-parameter piece-variety $Variety_Factor)
    (is $UniqueNum1 
      (integer (* $SetSize $Variety_Factor)))
    (max $UniqueNum1 1 $UniqueNum))

;
; PIECE_SET_SIZE(+Possible,-SetSize)
;
; Out of some possible maximum size, chooses some subset of
;
; squares to assign pieces to initially, based on the parameter
;
; ROW_CROWDING, the average fraction of possible squares to use.

  (= (piece-set-size $Possible $SetSize)  
    (choose-parameter row-crowding $Crowding)
    (is $Size 
      (integer (* $Crowding $Possible)))
    (max $Size 1 $SetSize))

;
; PROMOTE_ONLY_PIECES(-Pieces)
;
; Generates a number of unique piece types, which will
;
; only be seen via promotion (i.e. they are not placed on
;
; the initial board).
;
; We ensure that no more than 26 pieces will be placed in total,
;
; else we can no longer distinguish them using letters!
;
; Parameter: promote_only
;
; Note this parameter is independent of board size,
;
; and is specified as a range.
;
; 

  (= (promote-only-pieces $PlacedPieces $Pieces)  
    (choose-parameter promote-only-pieces $N)
    (length $PlacedPieces $Used)
    (is $Next 
      (+ $Used 1))
    (is $Last1 
      (+ $Next $N))
    (min $Last1 26 $Last)
    (n-piece-names $Next $Last $Pieces))



;
; DUPLICATE_PIECES
;
; continue duplicating random elements until achieve
;
; a set of the desired size. Resulting list is sorted.

  (= (duplicate-pieces $UniquePieces $SetSize $PieceSet)  
    (duplicate-elements $UniquePieces $SetSize $PieceSet1)
    (stable-sort $PieceSet1 $PieceSet))


  (= (duplicate-elements $UniqueElements $SetSize $Elementset)  
    (length $UniqueElements $L)
    (duplicate-elements $UniqueElements $L $SetSize $DuplicateSet)
    (append $UniqueElements $DuplicateSet $Elementset))

  (= (duplicate-elements $Types $Size $Size Nil)  
    (set-det))
  (= (duplicate-elements $Types $Size $Target (Cons  $E $Elements))  
    (random-element $Types $E)
    (is $Size1 
      (+ $Size 1))
    (duplicate-elements $Types $Size1 $Target $Elements))


;
; ================================================================================
;
; Generating unique piece names
;
; ================================================================================



  (= (n-piece-names $N $Pieces)  
    (n-piece-names 1 $N $Pieces))


  (= (n-piece-names $N $Max Nil)  
    (> $N $Max)
    (set-det))
  (= (n-piece-names $N $Max (Cons  $P $Pieces))  
    (new-piece-name $N $P)
    (is $N1 
      (+ $N 1))
    (n-piece-names $N1 $Max $Pieces))




  (= (new-piece-name $Index $Name)  
    (random-success (possible-index-piece $Index $Name)))



  (= (possible-index-piece $I $Name)  
    (indexed-names $I $Names)
    (member $Name $Names))


;
; ================================================================================


  (= (choose-board-size $Size)  
    (choose-parameter board-size $XMax)
    (choose-parameter board-size $YMax)
    (size $Size $XMax $YMax))


  (= (choose-board-type $Type)  
    (choose-parameter board-type $Type))


  (= (choose-board-inversion $Type)  
    (choose-parameter board-inversion $Type))



  (= (choose-board-crowding $Crowd)  
    (choose-parameter board-crowding $Crowd))


  (= (choose-initial-array-rows $Size $Crowding $MaxRow)  
    (is $MaxRow1 
      (integer (/ (* $Size $Crowding) 2)))
    (max 1 $MaxRow1 $MaxRow))


  (= (choose-promotion-regions $MaxRow $MinRow)  
    (choose-promotion-fraction $D)
    (is $MinRow 
      (integer (* $MaxRow $D))))


  (= (choose-promotion-fraction $D)  
    (choose-parameter promotion-fraction $D))



  (= (choose-placement-method $M)  
    (choose-parameter placement-method $M))


;
; ================================================================================
;
; GENERATE PIECES
;
; ================================================================================


  (= (generate-pieces $Board $PieceDefs)  
    (board-piece-types $Board $Types)
    (generate-pieces $Types $Board $PieceDefs1)
    (sort $PieceDefs1 $PieceDefs))


;
; generate_pieces(+PieceTypes,+Board,-PieceDefs)
  (= (generate_pieces  () $_ ())  True)
  (= (generate-pieces (Cons  $P $Ps) $Board (Cons  $Def $Defs))  
    (generate-piece $P $Board $Def)
    (generate-pieces $Ps $Board $Defs))



  (= (generate-piece $Name $Board $Piece)  
    (piece $Piece)
    (piece-name $Piece $Name)
    (assign-movement-power $Piece $Board)
    (assign-capture-power $Piece $Board)
    (assign-promotion-power $Piece $Board)
    (assign-piece-constraints $Piece $Board))


;
; ============================================================
;
; Constructing movements
;
; ============================================================



  (= (assign-movement-power $Piece $Board)  
    (create-complex-movement $Board $Movement)
    (piece-movement $Piece $Movement))
	

  (= (create-complex-movement $Board $Movement)  
    (create-movement $Board $M1)
    (complexify-movement $Board $M1 $Movement))


  (= (complexify-movement $Board $MIn (Cons  $MIn $MOut))  
    (choose-parameter movement-complexity)
    (set-det)
    (create-complex-movement $Board $MOut))
  (= (complexify_movement  $_ $X ($X))  True)           




  (= (create-movement $Board $Movement)  
    (movement $Movement)
    (choose-direction $Board $Dir)
    (movement-dir $Movement $Dir)
    (choose-symmetries $Syms)
    (movement-sym $Movement $Syms)
    (choose-movement-type $Type)
    (constrain-movement $Type $Dir $Board $Movement))


;
; ========================================
;
; Directions of movement
;
; ========================================

;
; Scales in both directions, s.t. a wide board
;
; is likely to have pieces which move farther in
;
; Y direction than in X.
;
; Subtracts 1 from X & Y, since piece must be on
;
; a square initially, so 1 less to move total.

  (= (choose-direction $Board $Dir)  
    (choose-locality $L)
    (board-size $Board $X $Y)
    (is $XMax1 
      (integer (* (- $X 1) $L)))
    (is $YMax1 
      (integer (* (- $Y 1) $L)))
    (max 1 $XMax1 $XMax)
    (max 1 $YMax1 $YMax)
    (choose-dir $XMax $YMax $Dir))
; ; XMax1 is (X-1)*L // 1, ; YMax1 is (Y-1)*L // 1,


  (= (choose-dir $XMax $YMax $Dir)  
    (choose-delta $XMax $Dx)
    (choose-delta $YMax $Dy)
    (legal-dir $XMax $YMax $Dx $Dy $Dir))

;
; If (0,0), choose both again.
;
; Rules out ZERO leaper (Dickens).

  (= (legal-dir $XMax $YMax 0 0 $Dir)  
    (set-det)
    (choose-dir $XMax $YMax $Dir))
  (= (legal-dir $_ $_ $Dx $Dy $Dir)  
    (direction $Dir $Dx $Dy))
	
;
; A delta is any random integer from [0,Max].

  (= (choose-delta $Max $Delta)  
    (is $M 
      (+ $Max 1))
    (random 0 $M $Delta))


  (= (max-delta $Dir $D)  
    (direction $Dir $X $Y)
    (max $X $Y $D))

;
; ========================================
;
; Symmetries
;
; ========================================
;
; Chooses the three symmetries with their respective (and independent)
;
; probabilities (which are parameters).
;
; 

  (= (choose-symmetries $Sym)  
    (symmetry $Sym)
    (choose-parameter 
      (symmetry rotation) $R)
    (choose-parameter 
      (symmetry forward) $F)
    (choose-parameter 
      (symmetry side) $S)
    (sym-forward $Sym $F)
    (sym-side $Sym $S)
    (sym-rotation $Sym $R))

;
; ========================================
;
; Constraints on movements
;
; ========================================
;
; Movement types are chosen with different exclusive probabilities,
;
; using the parameter: movement_type.
;
; Leapers are unconstrained.
;
; Riders can have a min, max, and longest restrictions (must ride).
;
; Hoppers have number before, over, after, and hopped-over piece_type restrictions.


  (= (choose-locality $L)  
    (choose-parameter locality $L))


  (= (choose-movement-type $T)  
    (choose-parameter movement-type $T))


  (= (constrain-movement leaper $_ $Board $Movement)  
    (set-det)
    (leaper $L)
    (movement-type $Movement $L))
  (= (constrain-movement rider $Dir $Board $Movement)  
    (set-det)
    (rider $Rider)
    (choose-must-ride $Rider)
    (choose-min-ride $Board $Dir $Rider)
    (choose-max-ride $Board $Dir $Rider)
    (movement-type $Movement $Rider))
  (= (constrain-movement hopper $Dir $Board $Movement)  
    (set-det)
    (hopper $Hopper)
    (choose-hopper-type $Board $R)
    (hopper-type $Hopper $R)
    (choose-before $Hopper $Board)
    (choose-over $Hopper $Board)
    (choose-after $Hopper $Board)
    (valid-hopper $Hopper $Dir $Board $Movement))


;
; ========================================
;
; Riders
;
; ========================================


  (= (choose-must-ride $Rider)  
    (choose-parameter must-ride $Must)
    (rider-must $Rider $Must))


  (= (choose-min-ride $Board $Dir $Rider)  
    (rider-min $Rider 1))

;
; CHOOSE_MAX_RIDE(+Board,+Dir,+Rider)
;
; Parameters:
;
; locality: the fraction of a board which should be traversible
;
; by an average piece movement.
;
; The max ride is then some number of rides s.t. a piece won't
;
; go beyond this locality by riding the max distance.

  (= (choose-max-ride $Board $Dir $Rider)  
    (choose-locality $L)
    (board-max-size $Board $Size)
    (max-delta $Dir $D)
    (is $Rides 
      (integer (/ (* $L $Size) $D)))
    (set-max-rides $Rider $Rides))
; ; Dist is L*Size // 1, ; Rides is Dist // D,



  (= (set-max-rides $Rider $Rides)  
    (> $Rides 1)
    (set-det)
    (rider-max $Rider $Rides))
  (= (set-max-rides $Rider $_)  
    (rider-max $Rider any))


;
; ========================================
;
; Hoppers
;
; ========================================


  (= (choose-before $Hopper $Board)  
    (constrain-hopper before $Board $Hopper))


  (= (choose-over $Hopper $Board)  
    (constrain-hopper over $Board $Hopper))


  (= (choose-after $Hopper $Board)  
    (constrain-hopper after $Board $Hopper))


;
; CONSTRAIN_HOPPER(+Type,+Board,+Hopper)
;
; Different Hop fields (before, over, after)
;
; are constrained or not with their own probabililities,
;
; defined by parameters.
;
; If they aren't to be constrained, an unconstraining value
;
; is placed in that slot (by hopper_any/2).
;
; 

  (= (constrain-hopper $Type $Board $Hopper)  
    (det-if-then-else 
      (choose-parameter (constrain (hopper $Type $_))) 
      (hopper-equation $Type $Board $Hopper) 
      (hopper-any $Type $Hopper)))

;
; HOPPER_EQUATION(Type,Board,Hopper)
;
; If a certain field (like BEFORE) is to be constrained,
;
; 

  (= (hopper-equation $Type $Board $Hopper)  
    (board-max-size $Board $Dist)
    (hopper-component $Type $Hopper $Comp)
    (choose-parameter 
      (hopper $Type $Dist) $P)
    (choose-equation $P $Comp))

;
; Must always hop over at least one piece.

  (= (hopper-any over $Hopper)  
    (set-det)
    (comparison $C geq 1)
    (hopper-component over $Hopper $C))
  (= (hopper-any $Type $Hopper)  
    (comparison $C geq 0)
    (hopper-component $Type $Hopper $C))



;
; ========================================
;
; Hopper Type
;
; ========================================
;
; Given a hopper might hop over some pieces,
;
; the legality of the hop may be
;
; restricted to only some kinds of pieces, based on who
;
; owns them, and what type they are.
;
; Doesn't distinguish between order or positions of hopped
;
; pieces (as in hop over 2 empty, then 1player,1opponent,1queen),
;
; though this would be interesting extension.


  (= (choose-hopper-type $Board $C)  
    (choose-piece-description $Board $C))


  (= (hopper-component before $Hopper $X)  
    (hopper-before $Hopper $X))
  (= (hopper-component over $Hopper $X)  
    (hopper-over $Hopper $X))
  (= (hopper-component after $Hopper $X)  
    (hopper-after $Hopper $X))



;
; Check constraints possible on board, else get new one.

  (= (valid-hopper $Hopper $Dir $Board $Movement)  
    (hopper-can-move $Hopper $Dir $Board)
    (set-det)
    (movement-type $Movement $Hopper))
  (= (valid-hopper $Hopper $Dir $Board $Movement)  
    (constrain-movement hopper $Dir $Board $Movement))


;
; HOPPER_CAN_MOVE(?Hopper,+Dir,+Board)
;
; A hopper can move if it could be on square (1,1),
;
; and make the minimum number of rides along
;
; its principal direction <Dx,Dy>, and still be on the board.
;
; 
;
; Note it is possible a hopper might be able to hop even if
;
; this is false, because of symmetries, but here we ensure
;
; it can without using symmetries.

  (= (hopper-can-move $Hopper $Dir $Board)  
    (hopper-min-rides $Hopper $Rides)
    (board-size $Board $BX $BY)
    (direction $Dir $DX $DY)
    (< 
      (* $Rides $DX) $BX)
    (< 
      (* $Rides $DY) $BY))
; ; Must be at least 1.



  (= (valid-hopper-max-dir $Dx $Dy $Max)  
    (current-board-size $Bx $By)
    (is $XMax 
      (// $Bx $Dx))
    (is $YMax 
      (// $By $Dy))
    (min $XMax $YMax $Max))


;
; hopper_min_rides(+Hopper,-Min)
;
; The Hopper requires at least MIN rides to make
;
; a valid move:
;
; Before_min + Over_min + After_min
;
; +1 (as hop finishes with a leap)

  (= (hopper-min-rides $H $Min)  
    (component-min $H before $Min1)
    (component-min $H over $Min2)
    (component-min $H after $Min3)
    (is $Min 
      (+ 
        (+ 
          (+ $Min1 $Min2) $Min3) 1)))


  (= (component-min $H $T $Min)  
    (hopper-component $T $H $Eq)
    (min-rides $Eq $Min))
	

  (= (min-rides $Eq $Min)  
    (comparison $Eq geq $Min)
    (set-det))
  (= (min-rides $Eq $Min)  
    (comparison $Eq eq $Min)
    (set-det))
  (= (min_rides  $Eq 0)  True)




  (= (choose-equation $Num $Eq)  
    (choose-parameter comparative $C)
    (comparison $Eq $C $Num))


;
; ===========================================================================
;
; Generalized Piece Descriptions
;
; ===========================================================================
;
; Piece Description has two components:
;
; Player
;
; Name
;
; Generalizing player, we can get: Player, Any_Player
;
; Generalizing Name, we can get: Piece_list, Any_Piece
;
; 
;
; | ?- board_piece_types(B,[piece1,piece2]),choose_piece_description(B,Desc).
;
; 
;
; B = board(_233,_234,_235,_236,_237,[piece1,piece2],_239,_240),
;
; Desc = piece_desc(opponent,piece2) ?


  (= (choose-piece-description $Board $Desc)  
    (player-generalization $Board $Player)
    (piece-generalization $Board $Pieces)
    (piece-description $Desc $Player $Pieces))


  (= (piece-generalization $Board $Gen)  
    (choose-piece-generalization-level $L)
    (generalize-piece $L $Board $Gen))


  (= (generalize-piece specific $Board $Pieces)  
    (choose-general-piece-set $Board $Pieces))
  (= (generalize_piece  any $Board any_piece)  True)


;
; ----------------------------------------------------------------
;
; Choosing subsets of defined pieces
;
; ----------------------------------------------------------------

;
; CHOOSE_GENERAL_PIECE_SET(Board,Pieces)
;
; Chooses a set of any defined pieces.

  (= (choose-general-piece-set $Board $Pieces)  
    (board-piece-types $Board $General)
    (choose-set-by-param $General more-general-pieces $Pieces))

;
; CHOOSE_UNPLACED_PIECE_SET(Board,Pieces)
;
; Chooses a set of unplaced (promote_only) pieces.

  (= (choose-unplaced-piece-set $Board $Pieces)  
    (board-unplaced-pieces $Board $Unplaced)
    (choose-set-by-param $Unplaced more-arrival-pieces $Pieces))

;
; =======================================
;
; CHOOSE_SET_BY_PARAM(+Set,+Param,-Items)
;
; =======================================
;
; Choose conditionally subject to the boolean parameter
;
; Param.
;
; 

  (= (choose-set-by-param $Set $Param $Items)  
    (= $Goal 
      (choose-parameter $Param))
    (choose-conditionally-from-set $Set $Goal $Items))


;
; ================================================
;
; CHOOSE_CONDITIONALLY_FROM_SET(+Set,+Goal,-Items)
;
; ================================================
;
; Choose a random Item from Set, and if Goal is
;
; satisfied, continue choosing more items.
;
; Ends when no more items to choose, or Goal fails.
;
; Goal should be random from this to be useful.
;
; Resulting list is sorted.
;
; 

  (= (choose-conditionally-from-set $Set $Goal $Items)  
    (random-permute $Set $Set1)
    (choose-conditionally-from-set1 $Set1 $Goal $Items1)
    (sort $Items1 $Items))


  (= (choose_conditionally_from_set1  () $_ ())  True)
  (= (choose-conditionally-from-set1 (Cons  $Item $Rest) $Goal (Cons  $Item $Items))  
    (det-if-then-else 
      (call $Goal) 
      (choose-conditionally-from-set $Rest $Goal $Items) 
      (= $Items Nil)))


  (= (player-generalization $Board $Gen)  
    (choose-player-generalization-level $L)
    (generalize-player $L $Board $Gen))

;
; Simplified: just either choose specific or any.

  (= (generalize-player specific $Board $Player)  
    (set-det)
    (random-player $Board $Player))
  (= (generalize_player  any $Board any_player)  True)


  (= (random-player $Board $Player)  
    (players $Players)
    (random-element $Players $Player))


  (= (players  (player opponent))  True)



  (= (random-piece $Board $Piece)  
    (board-piece-types $Board $Types)
    (random-element $Types $Piece))


  (= (random-square $Board $Sq)  
    (board-size $Board $XMax $YMax)
    (random $XMax $X)
    (random $YMax $Y)
    (square $Sq $X $Y))


  (= (choose-player-generalization-level $L)  
    (choose-parameter player-generalization-level $L))


  (= (choose-piece-generalization-level $L)  
    (choose-parameter piece-generalization-level $L))

;
; ============================================================
;
; Constructing capturing powers
;
; ============================================================


  (= (assign-capture-power $Piece $Board)  
    (create-complex-capture $Board $Capture)
    (piece-capture $Piece $Capture))


  (= (create-complex-capture $Board $Capture)  
    (create-capture $Board $C1)
    (complexify-capture $Board $C1 $Capture))


  (= (complexify-capture $Board $CIn (Cons  $CIn $C1))  
    (choose-parameter capture-complexity)
    (set-det)
    (create-complex-capture $Board $C1))
  (= (complexify_capture  $_ $X ($X))  True)           




  (= (create-capture $Board $Capture)  
    (choose-capture-movements $Board $Movement)
    (capture-movement $Capture $Movement)
    (choose-capture-methods $Method)
    (capture-methods $Capture $Method)
    (choose-capture-type $Board $Restrict)
    (capture-type $Capture $Restrict)
    (choose-capture-effect $Effect)
    (capture-effect $Capture $Effect))


;
; ========================================
;
; Capture Movements
;
; ========================================
;
; (Given a piece which moves in some ways)
;
; Find how a piece can move for purposes of capturing.
;
; 
;
; An interesting additional structure parameter could make
;
; it prefer to just use same movement as capture.


  (= (choose-capture-movements $Board $Movement)  
    (create-complex-movement $Board $Movement))



;
; ========================================
;
; Capture Methods
;
; ========================================
;
; Given a movement, how does piece capture another piece when
;
; making that movement?
;
; 
;
; Kill HOP if piece has no hopping movements when capturing?
	

  (= (choose-capture-methods $Methods)  
    (choose-parameter 
      (capture-method retrieve) $R)
    (choose-parameter 
      (capture-method clobber) $C)
    (choose-parameter 
      (capture-method hop) $H)
    (method-retrieve $Methods $R)
    (method-clobber $Methods $C)
    (method-hop $Methods $H))



;
; ========================================
;
; Capture Effect
;
; ========================================
;
; Given a movement that finds a captured piece,
;
; how is the board changed (what happens to both
;
; pieces?).
	

  (= (choose-capture-effect $E)  
    (choose-parameter capture-effect $E))


;
; ========================================
;
; Capture Type
;
; ========================================
;
; Given a movement that finds a potentially
;
; captured piece, the legality of the capture may be
;
; restricted to only some pieces, based on who
;
; owns them, and what type they are.
	

  (= (choose-capture-type $Board $C)  
    (choose-piece-description $Board $C))

;
; ============================================================
;
; Constructing promotion powers
;
; ============================================================


  (= (assign-promotion-power $Piece $Board)  
    (choose-promotion $Board $Prom)
    (piece-promote $Piece $Prom))


  (= (choose-promotion $Board $Prom)  
    (det-if-then-else 
      (choose-parameter specific-promotion) 
      (specific-promotion $Board $Prom) 
      (promotion-decision $Board $Prom))) 


  (= (specific-promotion $Board (promote $Piece))  
    (random-piece $Board $Piece))


  (= (promotion-decision $Board $D)  
    (random-player $Player)
    (choose-piece-description $Board $Desc)
    (decision $D)
    (decision-chooser $D $Player)
    (decision-options $D $Desc))


  (= (random-player $P)  
    (board-players $_ $Players)
    (random-element $Players $P))


  (= (board-players $B (:: player opponent))  
    (board $B))


;
; ============================================================
;
; Constructing Piece Move Constraints
;
; ============================================================
;
; Given a piece can execute different powers,
;
; there are constraints on whether one type of
;
; power must take priority, and whether a
;
; power can be repeated. If so there's a choice
;
; whether it MUST be repeated.


  (= (assign-piece-constraints $Piece $Board)  
    (choose-piece-constraints $Board $Con)
    (piece-constraints $Piece $Con))
	

  (= (choose-piece-constraints $Board $Con)  
    (constraint $Con)
    (choose-parameter must-capture $Must)
    (constraint-must-capture $Con $Must)
    (choose-parameter continue-captures $Cont)
    (constraint-continue-captures $Con $Cont))



  (= (piece-must-capture $Piece $Must)  
    (piece-constraints $Piece $Con)
    (constraint-must-capture $Con $Must))


  (= (piece-continue-captures $Piece $Continue)  
    (piece-constraints $Piece $Con)
    (constraint-continue-captures $Con $Continue))



;
; ============================================================
;
; Constructing Global Capture Constraints
;
; ============================================================
;
; Given a piece can execute different powers,
;
; there are constraints on whether one type of
;
; power must take priority, and whether a
;
; power can be repeated. If so there's a choice
;
; whether it MUST be repeated.


  (= (generate-global-constraints $Con)  
    (choose-game-constraints $Con))
	
;
; Games don't continue capturing, only pieces do.
;
; 

  (= (choose-game-constraints $Con)  
    (constraint $Con)
    (choose-parameter must-capture $Must)
    (constraint-must-capture $Con $Must)
    (constraint-continue-captures $Con no))
; ; choose_parameter(continue_captures,Cont),



  (= (game-must-capture $Game $Must)  
    (game-constraints $Game $Con)
    (constraint-must-capture $Con $Must))


  (= (game-continue-captures $Game $Continue)  
    (game-constraints $Game $Con)
    (constraint-continue-captures $Con $Continue))

;
; ================================================================================
;
; GENERATE GOAL
;
; ================================================================================
;
; G = [stalemate(opponent),(eradicate(piece_desc(player,t)),
;
; arrive(piece_desc(opponent,t),[square(2,2)]))] ?
;
; 
;
; First geneates a stalemate goal, to stalemate either (not both) of the players.
;
; Then, based on GOAL COMPLEXITY parameters, continues to add in
;
; arrival and eradicate goals. The more complex the goal, the easier it is to
;
; achieve, as a player wins if ANY of his goals are achieved (but not those of his opponent).
;
; 
;
; Each additional goal to be added is ensured to add a new winning condition,
;
; in the sense that it is not subsumed by any of the existing goals.
;
; 
;
; After the goal conditions are made as complex as desired,
;
; the goals are SIMPLIFIED. This proceeds as follows:
;
; 1. Remove FULLY DUPLICATE goals
;
; 2. Remove SUBSUMED goals.
;
; This is necessary for goals added which subsume earlier ones (ie were not
;
; subsumed themselves). Thus the earlier ones must be removed.
;
; 
;
; Any other simplifications require more serious theorem proving,
;
; which is beyond the scope of the generator.
;
; 
;
; Subsumed goals:
;
; ----------------
;
; When the generator produces many goals, it is very likely that several of them
;
; will be somehow redundant. The generator deals with the following types of
;
; redundancy as follows:
;
; 
;
; A goal G1 for SUBSUMES another G2 when G2 being true in a position implies G1 will
;
; also (or already) be true.
;
; a. Both goals are for PLAYER (like eradicate player's [a,b] and player's [a]).
;
; In this case the subsumption is straightforward, based on the sets of pieces
;
; mentioned in the two goals.
;
; b. One goal is for PLAYER, the other for OPPONENT. This is more tricky,
;
; and (in the case of arrival goals) involves inverting the squares mentioned in
;
; the opponent's goal.
;
; 
;
; One effect of this checking is that we will not generate pairs of goals which REQUIRE
;
; to be achieved simultaneously (like a goal to eradicate my king and your king).
;
; However, it still allows situations in which two goals HAPPEN TO BE are achieved
;
; together, and this outcome is declared to be a draw.
;
; 
;
; For example, consider the goals:
;
; stalemate(opponent)
;
; eradicate([player,any_piece]).
;
; 
;
; Here, it is possible to capture your last piece, thus achieving both goals
;
; (as it is your move but your stalemated, so you achieve the first, while I achieve
;
; the second as you have no more pieces).
;
; However, it may still be possible to stalemate you w/o eradicating all your pieces,
;
; so I could still win alone. Thus neither goal subsumes the other.
;
; 

  (= (generate-goal $Board $Goals)  
    (choose-stalemate-goal $Board $Goal1)
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
        (ppl $Goals))))


  (= (complexify-goal $Board $GIn $G)  
    (choose-parameter goal-complexity)
    (set-det)
    (unsubsumed-goal $GIn $Board $GNew)
    (complexify-goal $Board 
      (Cons  $GNew $GIn) $G))
  (= (complexify_goal  $_ $X $X)  True)           


;
; Keep creating goals until we make one which is not
;
; subsumed by the existing ones.

  (= (unsubsumed-goal $Goals $Board $NewGoal)  
    (create-goal $Board $Goal)
    (ensure-new-goal $Goals $Board $Goal $NewGoal))


  (= (ensure-new-goal $Goals $Board $Goal $NewGoal)  
    (member $Goal $Goals)
    (set-det)
    (unsubsumed-goal $Goals $Board $NewGoal))
  (= (ensure-new-goal $Goals $Board $Goal $NewGoal)  
    (subsumed-goal $Goal $Goals $Board)
    (set-det)
    (unsubsumed-goal $Goals $Board $NewGoal))
  (= (ensure_new_goal  $Goals $Board $Goal $Goal)  True)
	
;
; -------------------------
;
; CREATE_GOAL(+Board,-Goal)
;
; -------------------------
;
; Determines the type of goal based on param: GOAL_TYPE.

  (= (create-goal $Board $Goal)  
    (choose-parameter goal-type $Type)
    (choose-goal-of-type $Type $Board $Goal))


  (= (choose-goal-of-type arrive $B $Goal)  
    (choose-arrive-goal $B $Goal))
  (= (choose-goal-of-type eradicate $B $Goal)  
    (choose-eradicate-goal $B $Goal))

;
; ==================================
;
; CHOOSE_STALEMATE_GOAL(+Board,Goal)
;
; ==================================
;
; Just pick a random player to be stalemated.
;
; Could make this depend on a parameter.

  (= (choose-stalemate-goal $Board $Stale)  
    (random-player $Board $Player)
    (stalemate-goal $Stale $Player))

;
; ================================
;
; CHOOSE_ARRIVE_GOAL(+Board,-Goal)
;
; ================================
;
; Arrival goals have list of squares in the grammar.
;
; Here we generate a list with only 1 square, since the same effect
;
; can be achieved by multiple arrival goals.


  (= (choose-arrive-goal $Board $Goal)  
    (choose-parameter arrive-goal-player $Player)
    (choose-arrive-goal-for-player $Player $Type $Squares $Board)
    (piece-description $Desc $Player $Type)
    (arrive-goal $Goal $Desc $Squares))
	

  (= (choose-arrive-goal-for-player player $Type $Squares $Board)  
    (player-arrive-goal $Type $Squares $Board))
  (= (choose-arrive-goal-for-player any-player $Type $Squares $Board)  
    (player-arrive-goal $Type $Squares $Board))
  (= (choose-arrive-goal-for-player opponent $Type $Squares $Board)  
    (opponent-arrive-goal $Type $Squares $Board))

;
; Player and any_player arrival goals restricted to promote_only pieces.

  (= (player-arrive-goal $Type $Squares $Board)  
    (choose-unplaced-piece-set $Board $Type)
    (random-square $Board $Sq)
    (= $Squares 
      (:: $Sq)))

;
; Opponent arrival goals constrained not to be square
;
; to which opponent's pieces might be assigned.
;
; 

  (= (opponent-arrive-goal $Type $Squares $Board)  
    (piece-generalization $Board $Type)
    (opponent-random-free-square $Board $Sq)
    (= $Squares 
      (:: $Sq)))



  (= (opponent-random-free-square $Board $Sq)  
    (board-size $Board $XMax $YMax)
    (array-squares $XMax $YMax $BSquares)
    (random-permute $BSquares $RSquares)
    (board-player-assigned-squares $Board opponent $Assigned)
    (free-different-member $RSquares $Assigned $Sq))


  (= (free-different-member Nil $_ $_)  
    (format "Error: no free squares for opponent arrival goal!~n" Nil)
    (set-det)
    (fail))
  (= (free-different-member (Cons  $S $Sqs) $Assigned $Sq)  
    (det-if-then-else 
      (member $S $Assigned) 
      (free-different-member $Sqs $Assigned $Sq) 
      (= $Sq $S)))
	  

;
; ===================================
;
; CHOOSE_ERADICATE_GOAL(+Board,-Goal)
;
; ===================================
;
; Checks that eradicate goal contains at least 1 piece which will
;
; actually be on the board at the start of the game (after
;
; placements). Otherwise the game would always be a draw.


  (= (choose-eradicate-goal $Board $Goal)  
    (choose-parameter eradicate-goal-player $Player)
    (choose-eradicate-goal-for-player $Player $Type $Board)
    (piece-description $Desc $Player $Type)
    (eradicate-goal $Goal $Desc))
	

  (= (choose-eradicate-goal-for-player $Player $PieceGen $Board)  
    (eradicate-piece-generalization $Board $PieceGen))
	
;
; -----------------------------------------
;
; ERADICATE_PIECE_GENERALIZATION(Board,Gen)
;
; -----------------------------------------
;
; Either choose any_piece, or some specific set of pieces.
;
; Depends on params:
;
; eradicate_generalization_level (specific or any)
;
; more_eradicate_pieces (boolean)
;
; 

  (= (eradicate-piece-generalization $Board $Gen)  
    (choose-eradicate-generalization-level $L)
    (generalize-eradicate-piece $L $Board $Gen))

;
; =================================
;
; ERADICATE_PIECE_SET(Pieces,Board)
;
; =================================
;
; Choose a random piece which we are sure will be on the
;
; board. Then possibly choose any other pieces.
;
; 

  (= (eradicate-piece-set $Board $SortedPieces)  
    (random-placed-piece $Board $Placed)
    (board-piece-types $Board $Types)
    (select $Placed $Types $Rest)
    (choose-set-by-param $Rest more-eradicate-pieces $Pieces)
    (sort 
      (Cons  $Placed $Pieces) $SortedPieces))


  (= (random-placed-piece $Board $Piece)  
    (board-placed-pieces $Board $Pieces)
    (random-element $Pieces $Piece))
	

  (= (choose-eradicate-generalization-level $L)  
    (choose-parameter eradicate-generalization-level $L))


  (= (generalize-eradicate-piece specific $Board $Pieces)  
    (eradicate-piece-set $Board $Pieces))
  (= (generalize_eradicate_piece  any $Board any_piece)  True)


;
; --------------------------------------------------------------------------------
;
; Simplifying Goals
;
; --------------------------------------------------------------------------------


;
; ===========================================
;
; SIMPLIFY_GOALS(+Complex,+Board,-Simplified)
;
; ===========================================


  (= (simplify-goals $Complex $Board $Simplified)  
    (remove-duplicates $Complex $Simp1)
    (remove-subsumed $Simp1 $Board $Simplified))


  (= (remove-subsumed $Goals $Board $Simplified)  
    (remove-subsumed $Goals $Goals $Board $Simplified))

;
; Removes all the goals in arg1 which are subsumed by
;
; a goal in arg2, given current board Board, resulting in arg4.
  (= (remove_subsumed  () $_ $_ ())  True)
  (= (remove-subsumed (Cons  $Goal $Goals) $AllGoals $Board $Gs)  
    (subsumed-goal $Goal $AllGoals $Board)
    (set-det)
    (remove-subsumed $Goals $AllGoals $Board $Gs))
  (= (remove-subsumed (Cons  $Goal $Goals) $AllGoals $Board (Cons  $Goal $Gs))  
    (remove-subsumed $Goals $AllGoals $Board $Gs))


;
; ==================================
;
; SUBSUMED_GOAL(+Goal,+Goals,+Board)
;
; ==================================
;
; A goal is subsumed by a set of goals if there is some *other*
;
; goal Sub in Goals which subsumes it.

  (= (subsumed-goal $Goal $Goals $Board)  
    (member $Sub $Goals)
    (not (= $Sub $Goal))
    (subsumes $Sub $Goal $Board)
    (tracing-gen-format subsume "~nSubsumed Goal: ~p~nSubsumed by: ~p~n" 
      (:: $Goal $Sub)))

;
; -----------------------------
;
; SUBSUMES(Subsumer,Goal,Board)
;
; -----------------------------

  (= (subsumes $Sub $Goal $Board)  
    (goal-implies $Goal $Sub $Board))


  (= (goal-implies $Erad1 $Erad2 $Board)  
    (eradicate-goal $Erad1 $Player1 $Type1)
    (eradicate-goal $Erad2 $Player2 $Type2)
    (erad-implies $Player1 $Type1 $Player2 $Type2))
  (= (goal-implies $Arr1 $Arr2 $Board)  
    (arrive-goal $Arr1 $Player1 $Type1 $Sq1)
    (arrive-goal $Arr2 $Player2 $Type2 $Sq2)
    (arrive-implies $Player1 $Type1 $Sq1 $Player2 $Type2 $Sq2 $Board))

;
; ------------
;
; erad_implies
;
; ------------
;
; a. true(erad(player,any_piece)) => true(erad(player,[piece1,piece2])).
;
; true(erad(player,[piece1,piece2,piece3)) => true(erad(player,[piece1,piece2])).
;
; true(erad(player,[piece1,piece2,piece3]))
;
; => (for opponent) true(erad(opponent,[piece1,piece2])).
;
; 

  (= (erad-implies $Player1 $Type1 $Player2 $Type2)  
    (type-contains $Type1 $Type2))

;
; --------------
;
; arrive_implies
;
; --------------
;
; a. true(arrive(player,[piece1],square1)) => true(arrive(player,[piece1,piece2],square1)).
;
; true(arrive(player,[piece1],square1)) =>
;
; true(arrive(player,[piece1,piece2],[square1,square2)).
;
; b. true(arrive(player,[piece1],square1)) =>
;
; true(arrive(opponent,[piece1,piece2],square2)) {when sq2 is inverted sq1}

  (= (arrive-implies $Player1 $Type1 $Sq1 $Player1 $Type2 $Sq2 $Board)  
    (set-det)
    (arrive-implies $Type1 $Sq1 $Type2 $Sq2))
  (= (arrive-implies $Player1 $Type1 $Sq1 $Player2 $Type2 $Sq2 $Board)  
    (invert-board-squares $Board $Sq1 $Sq1Inv)
    (arrive-implies $Type1 $Sq1Inv $Type2 $Sq2))
	
  (= (arrive-implies $Type1 $Sq1 $Type2 $Sq2)  
    (type-contains $Type2 $Type1)
    (squares-contains $Sq2 $Sq1))


  (= (squares-contains $Sq1 $Sq2)  
    (ord-subset $Sq2 $Sq1))


  (= (type_contains  any_piece $_)  True)
  (= (type-contains $Type1 $Type2)  
    (ord-subset $Type2 $Type1))

;
; --------------------------------------------------------------------
;
; These are not used in the generator, but may be useful elsewhere.
;
; --------------------------------------------------------------------

;
; True if Desc *cannot* match any PLACED pieces.

  (= (unplaced-piece-description $Board $Desc)  
    (not (placed-piece-description $Board $Desc)))

;
; True if Desc *can* match some PLACED pieces.

  (= (placed-piece-description $Board $Desc)  
    (piece-description-piece $Desc $Piece)
    (contains-placed-piece $Piece $Board))


  (= (contains-placed-piece any-piece $_)  
    (set-det))
  (= (contains-placed-piece $Pieces $Board)  
    (board-placed-pieces $Board $Placed)
    (member $P $Pieces)
    (member $P $Placed)
    (set-det))

;
; --------------------------------------------------------------------

;
; ================================================================================
;
; TRACING execution of game generation routines
;
; ================================================================================

;
; The following tracing modules are used in this file:
;
; goals: info goal generation
;
; simplify: info on goal simplification
;
; subsume: info on goal redundancy checking and elimination
;
; pieces: info on piece generation.
;
; 
;
; Each module can be set on/off, using set_gen_verbosity (see below), or
;
; using trace_gen_<module>.
;
; 
;
; All can be turned off with silent_gen.


  !(my-ensure-loaded (library tracing))


  (= (tracing-gen $Type $Call)  
    (det-if-then-else 
      (tracing (gen $Type)) 
      (call $Call) True))

;
; Might cause trouble later when want to use streams also.

  (= (tracing-gen-format $Type $String $Args)  
    (det-if-then-else 
      (tracing (gen $Type)) 
      (format $String $Args) True))


  (= (tracing-gen-timing $Type $Call)  
    (trace-timing 
      (gen $Type) $Call))


  (= (set-gen-verbosity $Level $Status)  
    (set-tracing 
      (gen $Level) $Status))


  (= (silent-gen)  
    (all-gen off))

  (= (loud-gen)  
    (all-gen on))


  (= (all-gen $Status)  
    (set-gen-verbosity goals $Status)
    (set-gen-verbosity simplify $Status)
    (set-gen-verbosity subsume $Status)
    (set-gen-verbosity pieces $Status)) 


  (= (trace-gen-subsume)  
    (set-gen-verbosity subsume on)) 

  (= (trace-gen-simplify)  
    (set-gen-verbosity simplify on)) 

  (= (trace-gen-goals)  
    (set-gen-verbosity goals on)) 

  (= (trace-gen-pieces)  
    (set-gen-verbosity pieces on)) 

;
; :- trace_gen_simplify.
;
; :- silent_gen.



