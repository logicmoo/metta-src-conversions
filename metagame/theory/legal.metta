;                                           (convert_to_metta_file  legal $_267288 metagame/theory/legal.pl metagame/theory/legal.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; legal.pl
;
; Rep. class in control and stages framework

;
; /*================================================================================
;
; Basic Move Sequence
;
; 
;
; 0. Prelims/carry-overs:
;
; 0a. If opponent_promotes(Piece)), then promote piece.
;
; 
;
; 1. Decide to place or move a piece.
;
; 1a. If more pieces to be initially assigned, make an assignment and
;
; goto 7.
;
; 1b. If a player has a piece in_hand (i.e. one he possessed earlier),
;
; he can either (i) place this and goto 7, or (ii) goto 2.
;
; 1c. Note we bypass promotion for placed pieces, and the opponent
;
; can start directly in the MOVE stage.
;
; 2. Move a piece
;
; 2a. If the game has a global MUST_CAPTURE constraint:
;
; Then: if some capturing moves are available, then make one of them.
;
; Else, (no captures available) make a non-capturing move.
;
; 2b. If a piece is moved which MUST_CAPTURE if it can, and it can
;
; make a capturing move, then these are the only moves it can do.
;
; 2c. Else, it can capture or move.
;
; 2d. If it captured, goto 3, else goto 6.
;
; 3. Remove all captured pieces from the board (before executing effects)
;
; 4. Execute capture effects:
;
; 4a. Removed pieces just disappear.
;
; 4b. Possessed pieces go to hand (and color) of possessing player.
;
; 5. If piece can CONTINUE_CAPTURING, goto 2 (but only if this really captures)
;
; 5a. Can't continue unless captured something
;
; 5b. If can continue and this piece MUST_CONTINUE, then do so,
;
; otherwise this is optional.
;
; 6. Promote if in promotion territory
;
; 6a. If PLAYER PROMOTES, decide on promoted piece.
;
; 6b. If OPPONENT PROMOTES, add this effect (so opponent will promote
;
; this piece during stage 0b. of his next turn)
;
; 7. Transfer control
;
; 
;
; ================================================================================
;
; */

;
; LEGAL(?MOVE)
;
; A move begins with one of the players being in control,
;
; after which a sequence of sub-moves is made, which ends
;
; in transfer of control to the other player.
;
; A move_count is tracked throughout the game.


  (= (legal $M)  
    (control $Player)
    (legal-move $M $Player))

;
; LEGAL_MOVE(Move,Player)

  (= (legal-move $Move $Player)  
    (stage $Stage)
    (legal-move $Move $Stage $Player))

;
; Unlike the pseudo_moves, legal finally transfers
;
; stage in the state representation.

  (= (legal-move $M $Stage $Player)  
    (increment-move-count)
    (legal-move $M $Stage $Player $StageOut $_)
    (transfer-stage $Stage $StageOut))



  (= (legal-move (Cons  $M $Rest) $StageIn $PlayerIn $StageOut $PlayerOut)  
    (pseudo-op $StageIn $M $PlayerIn $Stage1 $Player1)
    (det-if-then-else 
      (\== $PlayerIn $Player1) 
      (, 
        (= $Player1 $PlayerOut) 
        (= $Stage1 $StageOut)) 
      (legal-move $Rest $Stage1 $Player1 $StageOut $PlayerOut)))



;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Basics
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (player_role  player)  True)
  (= (player_role  opponent)  True)


  (= (opposite_role  player opponent)  True)
  (= (opposite_role  opponent player)  True)




  (= (change $P1 $P2)  
    (del $P1)
    (add $P2))

;
; Here's where we state the rel between empty and on:

;
; empty(Sq) :- \+ on(P,Sq).

  (= (empty $Sq)  
    (on empty $Sq))


  (= (set-empty $Sq)  
    (add (on empty $Sq)))

;
; Piece_struct is owned by Player and on Square.

  (= (on $PieceStruct $Player $Square)  
    (piece-struct-owner $PieceStruct $Player)
    (on $PieceStruct $Square))

;
; Kill these later, to use pieces from Gen?

  (= (piece_struct_name  (piece  $Name $Owner) $Name)  True)

  (= (piece_struct_owner  (piece  $Name $Owner) $Owner)  True)


  (= (piece_struct  (piece  $Name $Owner) $Name $Owner)  True)


  (= (owns $Piece $Owner)  
    (piece-struct-owner $Piece $Owner))



  (= (add-to-board $Piece $Square)  
    (place-piece $Piece $Square))


  (= (put-in-hand $Piece $Player)  
    (add (in-hand $Piece $Player)))



  (= (place-piece-from-hand $Piece $Player $Square)  
    (del (in-hand $Piece $Player))
    (place-piece $Piece $Square))

;
; These two are opposites, naturally.
;
; Removed the empty changes from both.

  (= (lift-piece $Piece $Square)  
    (change 
      (on $Piece $Square) 
      (on empty $Square)))


  (= (place-piece $Piece $Square)  
    (change 
      (on empty $Square) 
      (on $Piece $Square)))


  (= (remove-piece $Taken $TakenSq)  
    (lift-piece $Taken $TakenSq))



  (= (put-control $Player)  
    (det-if-then-else 
      (control $P) 
      (del (control $P)) True)
    (add (control $Player)))


;
; Gives away control from Player to his Opponent.

  (= (transfer-control $Player)  
    (transfer-control $Player $Opp))

  (= (transfer-control $Player $Opp)  
    (opposite-role $Player $Opp)
    (change 
      (control $Player) 
      (control $Opp)))


;
; Effects of moving a piece.
;
; Records that this piece moved.

  (= (move-piece-record $Piece $SqF $SqT)  
    (move-piece $Piece $SqF $SqT)
    (add (moved-onto $Piece $SqT)))


  (= (place-piece-record $Piece $SqT)  
    (add (moved-onto $Piece $SqT)))
; ; place_piece(Piece,SqT),

;
; This isn't used by anybody right now.

  (= (move-piece $Piece $SqF $SqT)  
    (on $Piece $SqF)
    (lift-piece $Piece $SqF)
    (place-piece $Piece $SqT))
; ; new line



  (= (set-effect $Effect $Captured)  
    (add (effects $Effect $Captured))) 


  (= (del-effect $Effect $Captured)  
    (del (effects $Effect $Captured))) 


  (= (set-effect $Effect)  
    (add (effect $Effect))) 


  (= (del-effect $Effect)  
    (del (effect $Effect))) 


  (= (transfer-stage $Stage)  
    (stage $OldStage)
    (transfer-stage $OldStage $Stage))

  (= (transfer-stage $Old $New)  
    (change 
      (stage $Old) 
      (stage $New)))


  (= (put-stage $Stage)  
    (det-if-then-else 
      (stage $OldStage) 
      (del (stage $OldStage)) True)
    (add (stage $Stage)))



  (= (capture-piece $Taken $TakenSq)  
    (remove-piece $Taken $TakenSq))

;
; replace_piece(Old,New,Sq) :-
;
; change(on(Old,Sq),(on(New,Sq)).

  (= (replace-piece $Old $New $Sq)  
    (det-if-then-else 
      (= $Old $New) True 
      (change 
        (on $Old $Sq) 
        (on $New $Sq))))

;
; Here we lifted the piece long ago, and now we decide
;
; to place it down, possibly promoted, somewhere.
;
; Thus we don't need to actually remove the old piece, as it
;
; was removed earlier.

  (= (replace-piece-record $Old $New $Sq)  
    (place-piece $New $Sq))



  (= (increment-move-count)  
    (move-count $M)
    (is $M1 
      (+ $M 1))
    (change 
      (move-count $M) 
      (move-count $M1)))


;
; Could peval to speed up.

  (= (assignable $Square $Player)  
    (assignable-squares $Player $Squares)
    (member $Square $Squares))
	
;
; True if at least one of the players is
;
; still assigning pieces in the initial stage.

  (= (still-assigning)  
    (stage assign)
    (in-hand $Piece $Player))




;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Pseudo-Operators
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; ========================================
;
; ;; Ops during ASSIGN stage:
;
; Assigning pieces at start of game
;
; Game interpreter places initial pieces in the
;
; correct player's hands before this.
;
; 
;
; ASSIGN: Place a piece on an assignable square.
;
; Opponent will start next turn still in assign stage.
;
; 
;
; END_ASSIGN: Nothing to place, so proceed to move stage.
;
; ========================================


  (= (pseudo-op assign (assign $Piece $Player $Square) $Player assign $Opponent)  
    (assign $Piece $Player $Square)
    (transfer-control $Player $Opponent))
  (= (pseudo-op assign end-assign $Player move $Player)  
    (end-assign))
	

;
; ========================================
;
; Stage init_promote
;
; Op: opponent_promote(Sq,OldPiece,NewPiece)
;
; Stage out: move
;
; When a piece, owned by a player, has finished a move by
;
; moving into that player's promotion region, the player designated in the
;
; piece's definition gets to promote it to some piece matching the defined
;
; description.
;
; ========================================

  (= (pseudo-op init-promote (opponent-promote $Sq $OldPiece $NewPiece) $Player move $Player)  
    (opponent-promote $Sq $OldPiece $NewPiece $Player))


;
; ;; Ops during MOVE stage:

;
; ========================================
;
; PLACE Operator
;
; Stage In: Move
;
; Stage Out: Move
;
; ========================================
  (= (pseudo-op move (place $Piece $Player $Square) $Player move $Opponent)  
    (place-op $Piece $Player $Square $Opponent))


;
; ========================================
;
; MOVE Operator:
;
; Piece must be moveable.
;
; If must and can capture, then do so.
;
; Otherwise, either capture or move.
;
; Piece will not be placed after this movement,
;
; nor will captured pieces be removed.
;
; However, we will note that this piece has moved, by place_piece_record(Piece,SqT).
;
; Also we will note the capture effects (in capture code).
;
; ========================================

  (= (pseudo-op move (move $Piece $Player $SqF $SqT) $Player $Stage $Player)  
    (global-or-local-move $Piece $Player $SqF $SqT $Stage))

;
; ========================================
;
; Operator during the CAPTURE stage:
;
; 
;
; CAPTURE(Effect,Captured_Pieces)
;
; Effect will be either:
;
; a. REMOVE
;
; b. POSSESS(Owner)
;
; Captured will be a list of piece@square pairs.
;
; 
;
; New Stage will be: continue, when piece might try continuing
;
; if it can.
;
; ========================================

  (= (pseudo-op capture (capture $Effect $Captured) $Player continue $Player)  
    (capture-op $Effect $Captured $Player))

;
; ========================================
;
; Contining or ending CAPTURE EFFECTS:
;
; Ends processing the captured pieces, for both
;
; possess and remove powers.
;
; 
;
; Stages
;
; In: continue
;
; Out: capture/promote
;
; ========================================

  (= (pseudo-op continue $Move $Player $Stage $Player)  
    (try-continue-or-end $Move $Player $Stage))
	
;
; ========================================
;
; PROMOTION
;
; Op: try_promote(Square,OldPiece,NewPiece)
;
; Stages:
;
; In: promote
;
; Out: a. promote_select (If some promoting options), else
;
; b. end_move (i.e. transfer control).
;
; ========================================
;
; 
;
; When a piece, owned by a player, has moved into that player's
;
; promotion region, the player designated in the piece's definition
;
; gets to promote it to some piece matching the defined description.

  (= (pseudo-op promote (try-promote $Square $OldPiece $NewPiece) $Player $Stage $Player2)  
    (try-promote $Square $OldPiece $NewPiece $Player $Stage $Player2))

;
; ========================================
;
; Stage: promote_select
;
; Op: promote_select(Sq,OldPiece,NewPiece)
;
; This always ends a player's turn.
;
; Stage Out:
;
; If promotion was done by player, opponent's turn starts in MOVE stage.
;
; If promotion to be done by opponent, stage -> init_promote.
;
; ========================================

  (= (pseudo-op promote-select (promote-select $Sq $OldPiece $NewPiece) $Player $Stage $Player2)  
    (promote-select $Sq $OldPiece $NewPiece $Player $Stage $Player2))


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Operator Definitions
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Assigning and placing Pieces
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; ASSIGN: Place a piece on an assignable square.
;
; Opponent will start next turn still in assign stage.
;
; 
;
; END_ASSIGN: Nothing to place, so proceed to move stage.

;
; Assign

  (= (assign $Piece $Player $Square)  
    (placeable $Piece $Player $Square)
    (assignable $Square $Player)
    (place-piece-from-hand $Piece $Player $Square))


  (= (end-assign)  
    (not (in-hand $Piece $Player)))


;
; PLACING: Unlike the assignment stage,
;
; a player can PLACE a piece in his possession
;
; on any empty square. This ends his move.

  (= (place-op $Piece $Player $Square $Opponent)  
    (placeable $Piece $Player $Square)
    (place-piece-from-hand $Piece $Player $Square)
    (transfer-control $Player $Opponent))



  (= (placeable $Piece $Player $Square)  
    (bagof $Piece 
      (in-hand $Piece $Player) $AllPieces)
    (remove-duplicates $AllPieces $Pieces)
    (member $Piece $Pieces)
    (empty $Square))
; /* placeable(Piece,Player,Square) :- in_hand(Piece,Player), empty(Square). */
; ; Computes just once for each TYPE of piece in our hand. ; Doing for each token is redundant as we can only place ; one type. ;





;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Moving Pieces
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; MOVE Operator:
;
; Piece must be moveable.
;
; If must and can capture, then do so.
;
; Otherwise, either capture or move.
;
; Piece will not be placed after this movement,
;
; nor will captured pieces be removed.
;
; However, we will note that this piece has moved, by place_piece_record(Piece,SqT).
;
; Also we will note the capture effects (in capture code).


  (= (global-or-local-move $Piece $Player $SqF $SqT $Stage)  
    (det-if-then-else current-game-must-capture 
      (global-prefer-capture $Piece $Player $SqF $SqT $Stage) 
      (local-move $Piece $Player $SqF $SqT $Stage)))


  (= (global-prefer-capture $Piece $Player $SqF $SqT $Stage)  
    (global-prefer-capture1 $Piece1 $Player $SqF1 $SqT1 $Stage1)
    (= $Piece1 $Piece)
    (= $SqF1 $SqF)
    (= $SqT1 $SqT)
    (= $Stage $Stage1))


  (= (global-prefer-capture1 $Piece $Player $SqF $SqT $Stage)  
    (if 
      (, 
        (moveable $Piece $Player $SqF) 
        (capturing $Piece $Player $SqF $SqT)) 
      (= $Stage capture) 
      (, 
        (moveable $Piece $Player $SqF) 
        (moving $Piece $Player $SqF $SqT) 
        (= $Stage promote))))
	      


  (= (local-move $Piece $Player $SqF $SqT $Stage)  
    (moveable $Piece $Player $SqF)
    (local-could-move $Piece $Player $SqF $SqT $Stage))

;
; This can be used to check a piece movement without actually
;
; having that piece on the square to start with
;
; That is: IF SqF is empty, this will tell you which SqT it could
;
; legally move to.

  (= (local-could-move $Piece $Player $SqF $SqT $Stage)  
    (det-if-then-else 
      (must-capture $Piece) 
      (local-prefer-capture $Piece $Player $SqF $SqT $Stage) 
      (general-moving $Piece $Player $SqF $SqT $Stage)))


  (= (local-prefer-capture $Piece $Player $SqF $SqT $Stage)  
    (if 
      (capturing $Piece $Player $SqF $Sq1) 
      (, 
        (= $Sq1 $SqT) 
        (= $Stage capture)) 
      (, 
        (moving $Piece $Player $SqF $SqT) 
        (= $Stage promote))))



  (= (general-moving $Piece $Player $SqF $SqT capture)  
    (capturing $Piece $Player $SqF $SqT))
  (= (general-moving $Piece $Player $SqF $SqT promote)  
    (moving $Piece $Player $SqF $SqT))


  (= (must-capture $Piece)  
    (game--piece-must $Piece))	     


;
; REACHES(Piece,Player,SqF,SqT)
;
; True if Player's Piece could reach SqT from SqF,
;
; with the board exactly as it currently is.
;
; No side effects.

  (= (reaches $Piece $Player $SqF $SqT)  
    (moves $Piece $Player $SqF $SqT))
  (= (reaches $Piece $Player $SqF $SqT)  
    (captures $Piece $Player $SqF $SqT))



;
; MOVING
;
; Piece is a piece structure piece(name,player), not just the name.
;
; moveable should already have been checked.
;
; Thus By now piece has already been lifted off its start square.

  (= (moving $Piece $Player $SqF $SqT)  
    (game--piece-has-movement $Piece $Move)
    (moving-movement-for-piece $Piece $SqF $SqT $Player $Move $Dir $Hop)
    (place-piece-record $Piece $SqT))
; ; moveable(Piece,Player,SqF),


  (= (moves $Piece $Player $SqF $SqT)  
    (game--piece-has-movement $Piece $Move)
    (moving-movement-for-piece $Piece $SqF $SqT $Player $Move $Dir $Hop)) 


;
; If LONGEST and on a planar board, constrained to longest movement.
;
; Otherwise, any movement is ok.
;
; Game-implicit

  (= (moving-movement-for-piece $Piece $SqF $SqT $Player $Movement $Dir $Hop)  
    (if 
      (, 
        (ride $Movement $Dir $Min $Max $Longest) 
        (longest $Longest) 
        (current-board-type planar)) 
      (longest-moving-ride $SqF $Dir $Min $Max $SqT) 
      (movement-for-piece $Piece $SqF $SqT $Player $Movement $Dir $Hop))
    (valid-movement $SqT))

;
; The movement is valid if the final square is empty.

  (= (valid-movement $SqT)  
    (empty $SqT))


;
; Added lift_piece, which means: now we lift the piece up
;
; while considering where to move it. This is nec. for
;
; cylinder boards.
;
; But also dangerous, as makes a change which requires real
;
; backtracking.
;
; Shouldn't be too expensive, as only do 1 time per moving each
;
; piece.

  (= (moveable $Piece $Player $SqF)  
    (on $Piece $Player $SqF)
    (lift-piece $Piece $SqF))


;
; ================================================================================
;
; Piece Movements
;
; ================================================================================

;
; Leaping move
;
; A leap is equivalent to a ride of distance 1 (min=max=1).

  (= (movement-for-piece $Piece $SqF $SqT $Player $Movement $Dir Nil)  
    (leap $Movement $Dir)
    (connected $SqF $SqT $Dir))
; ; already computed syms

;
; The RIDE movement: We traverse 0 or more empties from our current square,
;
; then leap one further to the final square (which need not be empty).
;
; 
;
; Here we don't check the longest restriction (checked elsewhere already).
  (= (movement-for-piece $Piece $SqF $SqT $Player $Movement $Dir Nil)  
    (ride $Movement $Dir $Min $Max $Longest)
    (open-line $SqF $Dir $Min $Max empty $Squares $SqT))

;
; Hopping move
;
; 
;
; We can traverse B empties, then O occupied, then A empties,
;
; then leap one further to a final square (which need not be empty).
;
; 
;
; leap through B empty squares, BMIN<=B<=BMAX
;
; ending on init + BMAX emptysquares
;
; (thus, init if B=0, else an empty square)
;
; hop over O pieces [OMIN,OMAX]
;
; ending on last OMAXth piece
;
; (thus, init if O=0, else a piece)
;
; leap through A empty squares [AMIN,AMAX]
;
; end on AMAXth empty square
;
; (thus, init if A=0, else an empty square)
;
; final leap to last square
;
; end on last square
;
; (thus, can be empty or not, whatever,
;
; but makes at least 1 total leap).
;
; 
;
; Collects squares with hopped over pieces.
;
; Row hopped over (the O pieces) must be squares such that the piece on them
;
; matches the description for the hopper.

  (= (movement-for-piece $Piece $SqF $SqT $Player $Movement $Dir $Hopped)  
    (hop $Movement $Dir $Before $Over $After $Description)
    (hoplines $SqF $SqT $Dir $Before $Over $After $Description $Hopped))


  (= (hoplines $SqF $SqT $Dir $Before $Over $After $Description $Hopped)  
    (det-if-then-else 
      (var $SqF) 
      (hoplines-rev $SqF $SqT $Dir $Before $Over $After $Description $Hopped) 
      (hoplines-fwd $SqF $SqT $Dir $Before $Over $After $Description $Hopped)))


  (= (hoplines-fwd $SqF $SqT $Dir $Before $Over $After $Description $Hopped)  
    (hopline $SqF $Before $Dir empty $_ $SqB)
    (hopline $SqB $Over $Dir $Description $Hopped $SqO)
    (hopline $SqO $After $Dir empty $_ $SqL)
    (connected $SqL $SqT $Dir))


  (= (hoplines-rev $SqF $SqT $Dir $Before $Over $After $Description $Hopped)  
    (connected $SqL $SqT $Dir)
    (hopline $SqO $After $Dir empty $_ $SqL)
    (hopline $SqB $Over $Dir $Description $Hopped $SqO)
    (hopline $SqF $Before $Dir empty $_ $SqB))



  (= (hopline $SqF $Range $Dir $Descr $Squares $SqT)  
    (comparative-interval $Range $Dir $MinB $MaxB)
    (constrained-line $SqF $Dir $MinB $MaxB $Descr $Squares $SqT))

;
; comparative_interval(comparison(geq,N),Dir,N,MaxLeaps) :-
;
; valid_max(_,Dir,MaxLeaps).
;
; comparative_interval(comparison(eq,N),_,N,N).
;
; comparative_interval(comparison(leq,N),_,0,N).
	
;
; Unfolding here to eliminate choicepoints.

  (= (comparative-interval (comparison $Comp $X) $Dir $N $MaxLeaps)  
    (comparative-interval $Comp $X $Dir $N $MaxLeaps))

  (= (comparative-interval geq $N $Dir $N $MaxLeaps)  
    (valid-max $_ $Dir $MaxLeaps))
  (= (comparative_interval  eq $N $_ $N $N)  True)
  (= (comparative_interval  leq $N $_ 0 $N)  True)

;
; ================================================================================
;
; Riding and Hopping LINES
;
; ================================================================================


;
; OPEN_LINE(SqF,Dir,Min,Max,Cond,Squares,SqT)
;
; 
;
; Traverse between Min-1 and Max-1 leaps along Dir,
;
; starting after SqF, where each such square traversed
;
; satisfies COND (either 'empty' or a piece description).
;
; Then take one more leap, which brings us to (not nec. empty) SqT.
;
; 
;
; Difference betwn open and constrained line (below): open finishes with a
;
; step, so we first decrement the min and max counters, as we know
;
; we will take one leap at the end.


  (= (open-line $SqF $Dir $Min $Max $Cond $Squares $SqT)  
    (det-if-then-else 
      (var $SqF) 
      (open-line-rev $SqF $Dir $Min $Max $Cond $Squares $SqT) 
      (open-line-fwd $SqF $Dir $Min $Max $Cond $Squares $SqT)))
	


  (= (open-line-fwd $SqF $Dir $Min $Max $Cond $Squares $SqT)  
    (is $Min1 
      (- $Min 1))
    (is $Max1 
      (- $Max 1))
    (constrained-line-fwd $SqF $Dir $Min1 $Max1 $Cond $Squares $Sq1)
    (connected $Sq1 $SqT $Dir))


;
; CONSTRAINED_LINE_fwd(SqF,Dir,Min,Max,Cond,Squares,SqT)
;
; N Squares, starting with square after SqF, satisfy Cond,
;
; where Min <= N <= Max.
;
; 
;
; If SqF is S_0, and conn(S_i,Dir) = S_i+1, then
;
; true if: S_1, ..., S_i, S_i+1, ..., S_N
;
; all satisfy COND,
;
; where SqT is S_N.
;
; 
;
; Note that for i=0, SqF = SqT.
;
; 


  (= (constrained-line $SqF $Dir $Min $Max $Cond $Squares $SqT)  
    (det-if-then-else 
      (var $SqF) 
      (constrained-line-rev $SqF $Dir $Min $Max $Cond $Squares $SqT) 
      (constrained-line-fwd $SqF $Dir $Min $Max $Cond $Squares $SqT)))


  (= (constrained-line-fwd $SqF $Dir $Min $Max $Cond $Squares $SqT)  
    (all-sat-fwd $SqF $Dir $Cond 0 $Min $Max $Squares $SqT))
	

  (= (all-sat-fwd $SqF $Dir $Cond $Count $Min $Max Nil $SqF)  
    (>= $Count $Min)
    (=< $Count $Max))
  (= (all-sat-fwd $SqF $Dir $Cond $Count $Min $Max (Cons  $Sq1 $Squares) $SqT)  
    (< $Count $Max)
    (connected $SqF $Sq1 $Dir)
    (crossable $Cond $Sq1)
    (is $Count1 
      (+ $Count 1))
    (all-sat-fwd $Sq1 $Dir $Cond $Count1 $Min $Max $Squares $SqT))

;
; ================================================================================
;
; Riding and Hopping LINES : Reverse
;
; ================================================================================

;
; OPEN_LINE_REV(SqF,Dir,Min,Max,Cond,Squares,SqT)
;
; 
;
; Traverse between Min-1 and Max-1 leaps along Dir,
;
; starting after SqF, where each such square traversed
;
; satisfies COND (either 'empty' or a piece description).
;
; Then take one more leap, which brings us to (not nec. empty) SqT.
;
; 
;
; Difference betwn open and constrained line (below): open finishes with a
;
; step, so we first decrement the min and max counters, as we know
;
; we will take one leap at the end.


  (= (open-line-rev $SqF $Dir $Min $Max $Cond $Squares $SqT)  
    (is $Min1 
      (- $Min 1))
    (is $Max1 
      (- $Max 1))
    (connected $Sq1 $SqT $Dir)
    (constrained-line-rev $SqF $Dir $Min1 $Max1 $Cond $Squares $Sq1))


;
; CONSTRAINED_LINE_REV(SqF,Dir,Min,Max,Cond,Squares,SqT)
;
; N Squares, starting with square after SqF, satisfy Cond,
;
; where Min <= N <= Max.
;
; 
;
; If SqF is S_0, and conn(S_i,Dir) = S_i+1, then
;
; true if: S_1, ..., S_i, S_i+1, ..., S_N
;
; all satisfy COND,
;
; where SqT is S_N.
;
; 
;
; Note that for i=0, SqF = SqT.
;
; 


  (= (constrained-line-rev $SqF $Dir $Min $Max $Cond $Squares $SqT)  
    (all-sat-rev $SqF $Dir $Cond 0 $Min $Max $Squares $SqT))
	

  (= (all-sat-rev $SqF $Dir $Cond $Count $Min $Max Nil $SqF)  
    (>= $Count $Min)
    (=< $Count $Max))
  (= (all-sat-rev $SqF $Dir $Cond $Count $Min $Max (Cons  $SqT $Squares) $SqT)  
    (< $Count $Max)
    (connected $Sq1 $SqT $Dir)
    (crossable $Cond $SqT)
    (is $Count1 
      (+ $Count 1))
    (all-sat-rev $SqF $Dir $Cond $Count1 $Min $Max $Squares $Sq1))


;
; crossable(+Descr,?Sq)
;
; If Descr is Empty, then true if square is empty.
;
; Or a piece description, then true if piece on square matches.
;
; 
;
; crossable(empty,Sq) :- empty(Sq), !.
;
; crossable(Descr,Sq) :- on(P,Sq), matches(Descr,P).


  (= (crossable $Descr $Sq)  
    (det-if-then-else 
      (= $Descr empty) 
      (empty $Sq) 
      (, 
        (on $P $Sq) 
        (matches $Descr $P))))


;
; ======================================================================



;
; CAPTURING
;
; Now collects hopped squares.
;
; Only legal if something is captured.
;
; Capturing using a particular capture power, and a particular movement within this power.
;
; Records the capture effect corresponding to this use of power, so we'll know what to change later.
;
; Don't need to track captures list anymore, as effects taken care of.

;
; Finds way for Player to use Piece to capture, from SqF, to SqT.
;
; Move set_effect below captures. Else, when succ. 2 ways, if clear
;
; between don't get this effect.
;
; By now piece has been lifted (when checked moveable), so now just note that
;
; it should be on a new square at end, don't actually place it.
;
; Sets the capture effect information, saying which pieces were captured and what
;
; the effect will be.

  (= (capturing $Piece $Player $SqF $SqT)  
    (captures $Piece $Player $SqF $SqT $Effect $Captured)
    (place-piece-record $Piece $SqT)
    (set-effect $Effect $Captured)) 

;
; CAPTURES(Piece,Player,SqF,SqT)
;
; Like capturing, but determines if the piece could move
;
; without actually changing state.

  (= (captures $Piece $Player $SqF $SqT)  
    (captures $Piece $Player $SqF $SqT $Effect $Captured))

;
; CAPTURES(Piece,Player,SqF,SqT,Effect,Captured)
;
; True when Piece, from SqF, could use a capture power to
;
; move to SqT, making the set of captures in Captured,
;
; with capture effect Effect.
;
; This is non-side-effecting.
  (= (captures $Piece $Player $SqF $SqT $Effect $Captured)  
    (game--piece-has-capture $Piece $Capture)
    (capture-has-movement $Capture $Movement)
    (capture-effect $Capture $Effect)
    (capturing-movement-for-piece $Piece $SqF $SqT $Player $Movement $Capture $Captured))



;
; CAPTURING_MOVEMENT_FOR_PIECE(+Piece,+SqF,SqT,+Player,?Movement,+Capture,-Captured)
;
; Longest riders must make the longest capturing ride.
;
; Otherwise, makes a movement (same way as normal move),
;
; but then exercises the capture effects, then legal if something got
;
; captured and the final square is empty.
;
; 
;
; One way to think of the rel. between normal movements and captures:
;
; A moving_movement:
;
; finds a SqT s.t. the path <SqF,SqT> is appropriate for the movement def
;
; this is legal if SqT is empty.
;
; A capturing_movement:
;
; finds a SqT s.t. the path <SqF,SqT> is appropriate for the capturing_movement def.
;
; executes the capture effects, based on this path.
;
; this is legal if something was captured, and after this SqT is empty.
;
; 


  (= (capturing-movement-for-piece $Piece $SqF $SqT $Player $Movement $Capture $Captured)  
    (capturing-movement-for-piece $Piece $SqF $SqT $Player $Dir $Movement $Capture $Captured))

;
; Like above, but has direction of capture as explicit argument.
  (= (capturing-movement-for-piece $Piece $SqF $SqT $Player $Dir $Movement $Capture $Captured)  
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
; ========================================
;
; Longest rides
;
; ========================================
;
; Try riding the max distance.
;
; If can't, 1 ride less, and so on.
;
; If already failed at the minimum, give up.
;
; 
;
; LONGEST MOVING


  (= (longest-moving-ride $SqF $Dir $Min $Max $SqT)  
    (> $Min $Max)
    (set-det)
    (fail))
  (= (longest-moving-ride $SqF $Dir $Min $Max $SqT)  
    (open-line $SqF $Dir $Max $Max empty $Squares $SqT)
    (empty $SqT)
    (set-det))
  (= (longest-moving-ride $SqF $Dir $Min $Max $SqT)  
    (is $Max1 
      (- $Max 1))
    (longest-moving-ride $SqF $Dir $Min $Max1 $SqT))


;
; LONGEST CAPTURING


  (= (longest-capturing-ride $SqF $Dir $Min $Max $SqT $Capture $_)  
    (> $Min $Max)
    (set-det)
    (fail))
  (= (longest-capturing-ride $SqF $Dir $Min $Max $SqT $Capture $Captured)  
    (open-line $SqF $Dir $Max $Max empty $Squares $SqT)
    (captured-pieces $SqF $SqT $Capture $Dir Nil $Captured)
    (set-det))
  (= (longest-capturing-ride $SqF $Dir $Min $Max $SqT $Capture $Captured)  
    (is $Max1 
      (- $Max 1))
    (longest-capturing-ride $SqF $Dir $Min $Max1 $SqT $Capture $Captured))

;
; ----------------------------------------


  (= (captured-pieces $SqF $SqT $Capturing $Dir $Hopped $Captures)  
    (capture-type $Capturing $Type)
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
    (valid-capture $SqT $Captures))
; /* CAPTURING --------- o consider a capture definition, and a movement within this. o compute a path given this movement, saving significant squares. o for the given capture methods and types, see which significant squares should be captured. o ensure that something would get captured. o ensure that the final square would be empty - empty already, or one of the list to be captured o execute effects: remove pieces which should be captured (after remove-dups). */
; ; Deletes effect, as this is the last place we need it. ;
; ; CAPTURED PIECES ; Captures will be the set of Piece@Square which will be captured, ; given the effects of this capturing power, and the descriptions ; of pieces which it can capture. ; Could simplify by getting method and type first, call method routine ; like clobbers to check if matches, then capture piece if nec. ; ; Made if's instead of local cuts for bidirectionality. ; Don't need one for hoppers, since this already done in hopper code.

;
; VALID_CAPTURE(+FinalSquare,+CapturedList)
;
; In order to be a valid capture, something must actually get captured,
;
; and the final square must become empty.

  (= (valid-capture $Final $Captured)  
    (something-captured $Captured)
    (will-be-empty $Final $Captured))

;
; Something will be captured if the capture list is not empty.

  (= (something_captured  (Cons  $_ $_))  True)

;
; A square will be empty if it is already empty, or if it will be captured.

  (= (will-be-empty $Sq $Captured)  
    (det-if-then-else 
      (captured-piece $Piece $Sq $Captured) True 
      (empty $Sq)))


  (= (captured-piece $Victim $SqV $Captured)  
    (captured $Cap $Victim $SqV)
    (member $Cap $Captured))



  (= (clobbers $Sq $Capturing $Type (:: (@ $Piece $Sq)))  
    (capture-has-method $Capturing clobber)
    (on $Piece $Sq)
    (matches $Type $Piece))


  (= (retrieves $SqF $Dir $Capturing $Type (:: (@ $Piece $Sq1)))  
    (capture-has-method $Capturing retrieve)
    (connected $Sq1 $SqF $Dir)
    (on $Piece $Sq1)
    (matches $Type $Piece))


  (= (hops $Hopped $Capturing $Type $Hops)  
    (capture-has-method $Capturing hop)
    (matchers-on-squares $Hopped $Type $Hops))

;
; MATCHERS_ON_SQUARES(+Hopped,+Type,-Matchers).
;
; Matchers contains those Hopped-over pieces which match Type,
;
; and should thus be captured.
;
; Each elt of matchers is of the form: Piece@Square.

  (= (matchers_on_squares  () $_ ())  True)
  (= (matchers-on-squares (Cons  $H $Hs) $Type $Caps)  
    (on $P $H)
    (det-if-then-else 
      (matches $Type $P) 
      (= $Caps 
        (Cons  
          (@ $P $H) $Rest)) 
      (= $Caps $Rest))
    (matchers-on-squares $Hs $Type $Rest))



;
; Operator during the CAPTURE stage:
;
; 
;
; CAPTURE(Effect,Captured_Pieces)
;
; Effect will be either:
;
; a. REMOVE
;
; b. POSSESS(Owner)
;
; Captured will be a list of piece@square pairs.
;
; 
;
; 
;
; New Stage will be either: continue (if more captures), or promote.


  (= (capture-op $Effect $Captured $Player)  
    (del-effect $Effect $Captured)
    (effect-captures $Captured $Effect))
; ; effect(Effect,Captured),



  (= (effect_captures  () $_)  True)
  (= (effect-captures (Cons  $Cap $Caps) $Effect)  
    (effect-capture $Effect $Cap)
    (effect-captures $Caps $Effect))



  (= (effect-capture remove $Cap)  
    (captured $Cap $Piece $Sq)
    (lift-piece $Piece $Sq)) 
  (= (effect-capture (possess $Owner) $Cap)  
    (captured $Cap $Piece $Sq)
    (lift-piece $Piece $Sq)
    (piece-struct $Piece $Type $Player)
    (piece-struct $NewPiece $Type $Owner)
    (put-in-hand $NewPiece $Owner))


  (= (captured  (@  $Piece $Sq) $Piece $Sq)  True)



;
; =========================================
;
; Continuing captures
;
; =========================================

;
; After processed capture effects, may continue capturing in some cases.
;
; If so, then those which must continue must do so (**),
;
; and the rest decide whether to try continuing or to end.
;
; If continuing not an option, just end the move right away.
;
; 
;
; Keep on continuing, until done, then goto PROMOTE stage.


  (= (try-continue-or-end $Move $Player $Stage)  
    (if 
      (may-continue $Piece $Sq) 
      (if 
        (must-continue $Piece) 
        (continue-captures $Piece $Player $Sq $SqT $Move $Stage) 
        (continue-or-end $Piece $Player $Sq $SqT $Move $Stage)) 
      (discontinue $Move $Stage)))
	      

  (= (must-continue $Piece)  
    (game--piece-must $Piece))


  (= (may-continue $Piece $Sq)  
    (moved-onto $Piece $Sq)
    (game--piece-continues $Piece))



  (= (continue-or-end $Piece $Player $Sq $SqT (move $Piece $Player $Sq $SqT) capture)  
    (del (moved-onto $Piece $Sq))
    (capturing $Piece $Player $Sq $SqT))
; ; lift_piece(Piece,Sq),
  (= (continue-or-end $_ $_ $_ $_ $Move $Stage)  
    (discontinue $Move $Stage))

;
; Here we're committed to capturing if we can.
;
; Otherwise we end movements here.
;
; Since not checking moveable here, must lift the piece up
;
; again, otherwise it will still be there when capturing is
;
; checked.
;
; 
;
; Not any more, now not placing the piece down officially
;
; until the very end of the move (promotion stage).
;
; But must delete moved_onto.

  (= (continue-captures $Piece $Player $Sq $SqT $Move $Stage)  
    (if 
      (, 
        (= $Move 
          (move $Piece $Player $Sq $SqT)) 
        (del (moved-onto $Piece $Sq)) 
        (capturing $Piece $Player $Sq $SqT)) 
      (= $Stage capture) 
      (discontinue $Move $Stage)))
; ; lift_piece(Piece,Sq),


  (= (discontinue  end_continues promote)  True)

;
; ================================================================================
;
; Promoting
;
; ================================================================================

;
; ========================================
;
; Stages:
;
; In: promote
;
; Out: promote_select (If some promoting options)
;
; Else end_move (i.e. transfer control).
;
; ========================================
;
; 
;
; When a piece, owned by a player, has moved into that player's
;
; promotion region, the player designated in the piece's definition
;
; gets to promote it to some piece matching the defined description.



  (= (try-promote $Sq $OldPiece $NewPiece $Player $Stage $Player2)  
    (moved-onto $OldPiece $Sq)
    (verbosely (format "Checking for promotion~n" Nil))
    (promote-if $OldPiece $Player $Sq $NewPiece $Stage $Player2))
; ; dynamic pred


  (= (promote-if $OldPiece $Player1 $Sq $NewPiece $Stage $Player2)  
    (in-promote-region $Sq $Player1)
    (set-det)
    (game--piece-promoting $OldPiece $Promoting)
    (promote-if1 $OldPiece $Promoting $Player1 $Sq $NewPiece $Stage $Player2))

;
; If we don't promote the piece, then we have to put the old piece
;
; down on the final square (before we had just noted that it should have
;
; moved_onto to that square, but didn't really place it.
  (= (promote-if $OldPiece $Player1 $Sq Nil $Stage $Player2)  
    (del (moved-onto $OldPiece $Sq))
    (verbosely (format "~n~p is not in promotion territory for ~p~n" (:: $OldPiece $Player1)))
    (place-piece $OldPiece $Sq)
    (end-promote $Player1 $Stage $Player2))
; ; \+ in_promote_region(Sq,Player1)	


  (= (promote-if1 $OldPiece $Promoting $Player1 $Sq $NewPiece $Stage $Player2)  
    (simple-promote $Promoting $Player1 $NewPiece)
    (verbosely (format "Promoting: ~p on ~p --> ~p~n" (:: $OldPiece $Sq $NewPiece)))
    (replace-piece-record $OldPiece $NewPiece $Sq)
    (del (moved-onto $OldPiece $Sq))
    (end-promote $Player1 $Stage $Player2))
; ; replace_piece(OldPiece,NewPiece,Sq),	
	

  (= (promote-if1 $OldPiece $Promoting $Player1 $Sq $NewPiece $Stage $Player2)  
    (promoter $Promoting $Player1 $Promoter)
    (promote-role $Promoter $Player1 $OldPiece $Sq $Stage $Player2))

;
; If player promotes, transfer to promote_select stage.
;
; If opponent does, then add this fact, and end the move.

  (= (promote_role  $Player $Player $_ $_ promote_select $Player)  True)
  (= (promote-role $Opponent $Player $OldPiece $Sq $Stage $Player2)  
    (opposite-role $Player $Opponent)
    (add (opponent-promotes $OldPiece $Sq))
    (del (moved-onto $OldPiece $Sq))
    (end-promote $Player $Stage $Player2))	



  (= (simple-promote $Promoting $Player $NewPiece)  
    (promoting-info $Promoting 
      (promote $Type))
    (piece-struct $NewPiece $Type $Player))


  (= (promoter $Promoting $Player $Promoter)  
    (= $Promoting 
      (promote $Promoter $Descr)))


;
; IN_PROMOTE_REGION(Sq,Player)
;
; Square is in the promotion region for Player, according to the
;
; rules of Game.
;
; 
;
; Careful, this assumes that the promote rank is not
;
; already inverted. As invert only inverts squares,
;
; not numbers, we are ok.
;
; If it did invert promote rank, then don't invert square here (*)
;
; *** Could clean up by invert promote ranks automatically.
;
; !! Took out game argument.

  (= (in-promote-region $Sq $Player)  
    (game--promote-rank $Rank)
    (invert $Sq $Player $Sq1)
    (square $Sq1 $X $Y)
    (>= $Y $Rank))



;
; ========================================
;
; Stage: promote_select
;
; ========================================


  (= (promote-select $Sq $OldPiece $NewPiece $Player $Stage $Player2)  
    (moved-onto $OldPiece $Sq)
    (verbosely-format "~p decides on promotion for ~p on ~p~n" 
      (:: $Player $OldPiece $Sq))
    (game--piece-promoting $OldPiece $Promoting)
    (promoting-options $Promoting $Player $Descr)
    (matches $Descr $NewPiece)
    (replace-piece-record $OldPiece $NewPiece $Sq)
    (del (moved-onto $OldPiece $Sq))
    (end-promote $Player $Stage $Player2))
; ; dynamic pred
; ; Careful here: Must not invert perspective twice (*)
; ; replace_piece(OldPiece,NewPiece,Sq),


;
; ========================================
;
; END_PROMOTE
;
; After player promotes, end_move.
;
; ========================================

  (= (end-promote $Player $Stage $Player2)  
    (end-move $Player $Stage $Player2))


;
; Since opponent_promotes option puts opponent in contol,
;
; he will have inverted the promoting definition. So, check for this
;
; case (when actor has opposite role of player), then
;
; invert description back again if so.
;
; 
;
; Cleaner clause here (like invert to perspective?).

  (= (promoting-options $Promoting $Player $Descr)  
    (= $Promoting 
      (promote $Actor $Descr1))
    (det-if-then-else 
      (opposite-role $Player $Actor) 
      (invert $Descr1 opponent $Descr) 
      (= $Descr $Descr1)))

;
; ================================================================================
;
; Opponent_Promoting
;
; ================================================================================

;
; ========================================
;
; Stages
;
; In: init_promote
;
; Out: move
;
; ========================================

;
; When a piece, owned by a player, has finished a move by moving into that player's
;
; promotion region, the player designated in the piece's definition
;
; gets to promote it to some piece matching the defined description.



  (= (opponent-promote $Sq $OldPiece $NewPiece $Player)  
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
; ; replace_piece(OldPiece,NewPiece,Sq)

;
; Careful here: Don't want to invert perspective again (*).

  (= (init-promote-option $OldPiece $Player $NewPiece)  
    (game--piece-promoting $OldPiece $Promoting)
    (promoting-options $Promoting $Player $Descr)
    (matches $Descr $NewPiece)) 


;
; ========================================
;
; END_MOVE
;
; ========================================
;
; After possibly promoting a piece, the move
;
; ends here, and control is transferred to the
;
; other player.
;
; The next stage will be INIT_PROMOTE for opponent.
;
; 

  (= (end-move $Player init-promote $Opponent)  
    (verbosely-format "~p finished moving~n" 
      (:: $Player))
    (transfer-control $Player $Opponent))
	
	

