;                                           (convert_to_metta_file  genstructs $_315834 metagame/generator/genstructs.pl metagame/generator/genstructs.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; genstructs.pl
;
; ;; Data Structures used in generating and manipulating game structures.


  !(my-ensure-loaded (library invert))

;
; ===========================================================================
;
; Data Structures
;
; ===========================================================================

;
; ===========================================================================
;
; GAME Data Structure
;
; ===========================================================================

  (= (game $X)  
    (functor $X game 5))

  (= (game  (game  $N $B $P $G $C) $N $B $P $G $C)  True)


  (= (game-name $G $X)  
    (arg 1 $G $X))

  (= (game-board $G $X)  
    (arg 2 $G $X))

  (= (game-pieces $G $X)  
    (arg 3 $G $X))

  (= (game-goal $G $X)  
    (arg 4 $G $X))

  (= (game-constraints $G $X)  
    (arg 5 $G $X)) 

;
; ===========================================================================
;
; BOARD Data Structure
;
; ===========================================================================


  (= (board $B $Size $Type $Inv $R $Arr $K $Types $Set $Ass)  
    (board-size $B $Size)
    (board-type $B $Type)
    (board-inversion $B $Inv)
    (board-promote-rows $B $R)
    (board-array-rows $B $Arr)
    (board-killed $B $K)
    (board-piece-types $B $Types)
    (board-placed-pieces $B $Set)
    (board-assignments $B $Ass))
  (= (board $X)  
    (functor $X board 9))


  (= (board-size $B $X)  
    (arg 1 $B $X))

  (= (board-type $B $X)  
    (arg 2 $B $X))

  (= (board-inversion $B $X)  
    (arg 3 $B $X))

  (= (board-promote-rows $B $X)  
    (arg 4 $B $X))

  (= (board-killed $B $X)  
    (arg 5 $B $X))

  (= (board-piece-types $B $X)  
    (arg 6 $B $X)) 

  (= (board-placed-pieces $B $X)  
    (arg 7 $B $X)) 

  (= (board-assignments $B $X)  
    (arg 8 $B $X))

  (= (board-array-rows $B $X)  
    (arg 9 $B $X))


  (= (board-unplaced-pieces $Board $Set)  
    (board-placed-pieces $Board $Placed)
    (board-piece-types $Board $Types)
    (ord-subtract $Types $Placed $Set))



  (= (board-size $B $X $Y)  
    (board-size $B $Size)
    (size $Size $X $Y))


  (= (board-max-size $B $M)  
    (board-size $B $X $Y)
    (max $X $Y $M))



  (= (board-assigned-squares $Board $Squares)  
    (board-assignments $Board $As)
    (assignments-use-squares $As $Squares))


  (= (board-player-assigned-squares $Board $Player $Squares)  
    (board-assigned-squares $Board $Squares1)
    (invert-board-squares-player $Player $Board $Squares1 $Squares))


  (= (assignments-use-squares $As $Squares)  
    (decision $As $Player $PieceSet $Squares)
    (set-det))
  (= (assignments-use-squares $Assigns $Squares)  
    (assigned-squares $Assigns $Squares))


  (= (assigned-squares $Assigns $Squares)  
    (uncollect $Assigns $Pairs)
    (unpair $Pairs $Squares))


  (= (invert_board_squares_player  player $Board $Squares $Squares)  True)
  (= (invert-board-squares-player opponent $Board $Squares1 $Squares)  
    (invert-board-squares $Board $Squares1 $Squares))


  (= (invert-board-squares $Board $Squares1 $Squares)  
    (board-size $Board $XN $YN)
    (board-inversion $Board $Inv)
    (invert-squares-dim $Squares1 $Inv $XN $YN $Squares))


  (= (invert_squares_dim  () $Inv $XN $YN ())  True)
  (= (invert-squares-dim (Cons  $Sq $Sqs) $Inv $XN $YN (Cons  $ISq $ISqs))  
    (invert-square-dim $Inv $XN $YN $Sq $ISq)
    (invert-squares-dim $Sqs $Inv $XN $YN $ISqs))


  (= (invert-square-on-board $Board $Sq1 $Sq2)  
    (board-size $Board $XN $YN)
    (board-inversion $Inv)
    (invert-square-dim $Inv $XN $YN $Sq1 $Sq2))

;
; ===========================================================================
;
; SIZE Data Structure
;
; ===========================================================================


  (= (size  (size  $X $Y) $X $Y)  True)

;
; ===========================================================================
;
; PIECE Data Structure
;
; ===========================================================================


  (= (piece $Def)  
    (piece-definition $Def))


  (= (piece-definition $Def)  
    (functor $Def piece 5))

  (= (piece-definition $Def $Name $Movement $Capture $Promote $Con)  
    (piece-definition $Def)
    (piece-name $Def $Name)
    (piece-movement $Def $Movement)
    (piece-capture $Def $Capture)
    (piece-promote $Def $Promote)
    (piece-constraints $Def $Con))


  (= (piece-name $Piece $X)  
    (arg 1 $Piece $X))

  (= (piece-movement $Piece $X)  
    (arg 2 $Piece $X))

  (= (piece-capture $Piece $X)  
    (arg 3 $Piece $X))

  (= (piece-promote $Piece $X)  
    (arg 4 $Piece $X))

  (= (piece-constraints $Piece $X)  
    (arg 5 $Piece $X))

;
; ===========================================================================
;
; DIRECTION Data Structure
;
; ===========================================================================


  (= (direction  (dir  $X $Y) $X $Y)  True)

;
; ===========================================================================
;
; SYMMETRY Data Structure
;
; ===========================================================================


  (= (symmetry $X)  
    (functor $X symmetry 3))

  (= (symmetry  (symmetry  $F $S $R) $F $S $R)  True)


  (= (sym-forward $Sym $F)  
    (arg 1 $Sym $F))

  (= (sym-side $Sym $S)  
    (arg 2 $Sym $S))

  (= (sym-rotation $Sym $R)  
    (arg 3 $Sym $R))


  (= (forward $Sym)  
    (sym-forward $Sym yes))

  (= (side $Sym)  
    (sym-side $Sym yes))

  (= (rotation $Sym)  
    (sym-rotation $Sym yes))



  (= (has-symmetry $Sym forward)  
    (forward $Sym))
  (= (has-symmetry $Sym side)  
    (side $Sym))
  (= (has-symmetry $Sym rotation)  
    (rotation $Sym))
	               

;
; ===========================================================================
;
; LEAPER Data Structure
;
; ===========================================================================


  (= (leaper  leaper)  True)

;
; ===========================================================================
;
; RIDER Data Structure
;
; ===========================================================================


  (= (rider  (rider  $Must $Min $Max) $Must $Min $Max)  True)
  (= (rider $R)  
    (functor $R rider 3))


  (= (rider-must $R $Must)  
    (arg 1 $R $Must))

  (= (rider-min $R $Min)  
    (arg 2 $R $Min))

  (= (rider-max $R $Max)  
    (arg 3 $R $Max))


  (= (rider-must $Rider)  
    (rider-must $Rider yes))

;
; ===========================================================================
;
; HOPPER Data Structure
;
; ===========================================================================


  (= (hopper  (hopper  $Restr $B $O $A) $Restr $B $O $A)  True)
  (= (hopper $H)  
    (functor $H hopper 4))


  (= (hopper-type $H $X)  
    (hopper $H)
    (arg 1 $H $X))

  (= (hopper-before $H $X)  
    (hopper $H)
    (arg 2 $H $X))

  (= (hopper-over $H $X)  
    (hopper $H)
    (arg 3 $H $X))

  (= (hopper-after $H $X)  
    (hopper $H)
    (arg 4 $H $X))

;
; ===========================================================================
;
; MOVEMENT Data Structure
;
; ===========================================================================


  (= (movement $M)  
    (functor $M movement 3))


  (= (movement-type $M $X)  
    (arg 1 $M $X))

  (= (movement-dir $M $X)  
    (arg 2 $M $X))

  (= (movement-sym $M $X)  
    (arg 3 $M $X))

;
; ===========================================================================
;
; COMPLEX_MOVEMENT Data Structure
;
; ===========================================================================

  (= (complex_movement  (or  $M1 $M2) $M1 $M2)  True)

;
; ===========================================================================
;
; COMPLEX_CAPTURE Data Structure
;
; ===========================================================================

  (= (complex_capture  (or  $C1 $C2) $C1 $C2)  True)

;
; ===========================================================================
;
; CAPTURE Data Structure
;
; ===========================================================================


  (= (capture  (capture  $Move $Methods $Restr $Effect) $Move $Methods $Restr $Effect)  True)

  (= (capture $X)  
    (functor $X capture 4))

;
; capture_dir(H,X) :- capture(H), arg(1,H,X).

  (= (capture-movement $H $X)  
    (capture $H)
    (arg 1 $H $X))

  (= (capture-methods $H $X)  
    (capture $H)
    (arg 2 $H $X))

  (= (capture-type $H $X)  
    (capture $H)
    (arg 3 $H $X))

  (= (capture-effect $H $X)  
    (capture $H)
    (arg 4 $H $X))



;
; ===========================================================================
;
; COMPARISON Data Structure
;
; ===========================================================================


  (= (comparison $X)  
    (functor $X comparison 2))
  (= (comparison  (comparison  $Comp $Num) $Comp $Num)  True)


  (= (comparison-comp $M $X)  
    (comparison $M)
    (arg 1 $M $X))

  (= (comparison-num $M $X)  
    (comparison $M)
    (arg 2 $M $X))
;
; comparison_sym(M,X) :- comparison(M),arg(3,M,X).

;
; ===========================================================================
;
; SQUARE Data Structure
;
; ===========================================================================


  (= (square  (square  $X $Y) $X $Y)  True)


;
; ===========================================================================
;
; METHOD Data Structure
;
; ===========================================================================


  (= (method $X)  
    (functor $X method 3))
  (= (method  (method  $C $R $H) $C $R $H)  True)


  (= (method-clobber $M $X)  
    (method $M)
    (arg 1 $M $X))

  (= (method-retrieve $M $X)  
    (method $M)
    (arg 2 $M $X))

  (= (method-hop $M $X)  
    (method $M)
    (arg 3 $M $X))

;
; ===========================================================================
;
; PIECE_DESCRIPTION Data Structure
;
; ===========================================================================


  (= (piece_description  (piece_desc  $Player $Piece) $Player $Piece)  True)
  (= (piece-description $X)  
    (functor $X piece-desc 2))


  (= (piece-description-player $M $X)  
    (piece-description $M)
    (arg 1 $M $X))

  (= (piece-description-piece $M $X)  
    (piece-description $M)
    (arg 2 $M $X))
;
; piece_description_sym(M,X) :- piece_description(M),arg(3,M,X).

;
; ===========================================================================
;
; CONSTRAINT Data Structure
;
; ===========================================================================


  (= (constraint  (constraint  $Must $Cont) $Must $Cont)  True)
  (= (constraint $X)  
    (functor $X constraint 2))


  (= (constraint-must-capture $M $X)  
    (arg 1 $M $X))

  (= (constraint-continue-captures $M $X)  
    (arg 2 $M $X))

  (= (constraint-continue-captures $M)  
    (constraint $M)
    (constraint-continue-captures $M yes))

  (= (constraint-must-capture $M)  
    (constraint $M)
    (constraint-must-capture $M yes))

;
; ===========================================================================
;
; COMPLEX_GOAL Data Structure
;
; ===========================================================================

  (= (complex_goal  (or  $M1 $M2) $M1 $M2)  True)

;
; ===========================================================================
;
; Simple GOAL Data Structures
;
; ===========================================================================

;
; ARRIVE

  (= (arrive_goal  (arrive  $Desc $Sq) $Desc $Sq)  True)

  (= (arrive-goal (arrive $Desc $Sq) $Player $Type $Sq)  
    (piece-description $Desc $Player $Type))
	
;
; ERADICATE

  (= (eradicate_goal  (eradicate  $Desc) $Desc)  True)

  (= (eradicate-goal (eradicate $Desc) $Player $Type)  
    (piece-description $Desc $Player $Type))

;
; STALEMATE

  (= (stalemate_goal  (stalemate  $Player) $Player)  True)


;
; ===========================================================================
;
; Decision data structures
;
; ===========================================================================

;
; decision(Decision,Chooser,Options,Constraints).

;
; For promote decisions:
;
; Chooser is just the player who makes this decision.
;
; [Player_Gen,Piece_Gen] is a description of the options.
;
; No constraints.

;
; For assignement decisions:
;
; Chooser is just the player who makes this decision.
;
; [Piece1,...], a set of pieces, is the Options.
;
; Constraints is a set of squares on which the pieces can be placed.


  (= (decision $X)  
    (functor $X decision 3))
  (= (decision  (decision  $C $O $Con) $C $O $Con)  True)


  (= (decision-chooser $D $C)  
    (decision $D $C $_ $_))

  (= (decision-options $D $O)  
    (decision $D $_ $O $_))

  (= (decision-constraints $D $Con)  
    (decision $D $_ $_ $Con))

