;                                           (convert-to-metta-file  parse2 $_313224 metagame/theory/parse2.pl metagame/theory/parse2.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; parse2.pl
;
; ;; Taking some preds out of parse, as not state-dependent.



  (= (current-game-for-player player $Game)  
    (player-current-game $Game))
  (= (current-game-for-player opponent $Game)  
    (opponent-current-game $Game))



  (= (current-game-name $GameName)  
    (player-current-game $G)
    (game-name $G $GameName)) 



  (= (current-game-piece-name $Name)  
    (player-current-game $Game)
    (game-piece-def $Game $Name $_)) 



  (= (game-piece-name $Game $Name)  
    (game-piece-def $Game $Name $_)) 

;
; Finds def. corresponding to the name of the piece in piecestruct.

  (= (game-piece-struct-def $Game $PieceStruct $Def)  
    (piece-struct-name $PieceStruct $Name)
    (game-piece-def $Game $Name $Def))

;
; Finds the definition corresponding to a particular NAME of piece.

  (= (game-piece-def $Game $Name $Def)  
    (game-piece-defs $Game $Defs)
    (member $Def $Defs)
    (piece-name $Def $Name))


  (= (game-piece-defs $G $P)  
    (game-pieces $G $P))


;
; ; Goals

  (= (game-has-goal $Game $Goal)  
    (game-goal $Game $CompGoal)
    (goal-component $CompGoal $Goal))


  (= (goal-component $CompGoal $Goal)  
    (member $Goal $CompGoal))

;
; Constraints


  (= (game-must-capture $Game)  
    (constraint-must-capture $Con)
    (game-constraints $Game $Con))
	

  (= (game-continues $Game)  
    (constraint-continue-captures $Con)
    (game-constraints $Game $Con))


  (= current-game-must-capture  
    (player-current-game $Game)
    (game-must-capture $Game))





;
; Constraints on pieces


  (= (game-piece-has-constraint $Piece $Constraint $Game)  
    (game-piece-struct-def $Game $Piece $Def)
    (piece-constraints $Def $Constraint))


;
; Movement


  (= (game-piece-has-movement $Piece $Movement $Game)  
    (game-piece-struct-def $Game $Piece $Def)
    (piece-movement $Def $CompMovement)
    (movement-component $CompMovement $Movement))


  (= (movement-component $CompMovement $Movement)  
    (member $Movement $CompMovement))

;
; Backtracking gives all the dirs that the piece
;
; can leap to, based on its symmetries.
;
; (not so happy having this in parse file!).


  (= (leap $M $Dir)  
    (movement-type $M $L)
    (leaper $L)
    (movement-sym-dir $M $Dir))
	
;
; Valid_min and valid_max are defined in boards.pl
;
; Valid_max computes the greatest number of leaps
;
; which need to be considered before the piece would
;
; be off the board or wrapped back to the original square.

  (= (ride $M $Dir $Min $Max $Longest)  
    (movement-type $M $R)
    (rider $R $Longest $Min1 $Max1)
    (movement-sym-dir $M $Dir)
    (valid-min $Min1 $Min)
    (valid-max $Max1 $Dir $Max))

;
; When unified with the longest component, succeeds if it should.

  (= (longest  yes)  True)



  (= (hop $M $Dir $Before $Over $After $Description)  
    (movement-type $M $H)
    (hopper $H $Description $Before $Over $After)
    (movement-sym-dir $M $Dir))


  (= (movement-sym-dir $M $Dir)  
    (movement-dir $M $D)
    (movement-syms $M $Syms)
    (sym-dir $D $Syms $Dir))

;
; Uses sym_set from grammar.pl.
;
; This is absurdly inefficient!


  (= (movement-syms $M $Syms)  
    (movement-sym $M $S)
    (sym-set $S $Syms Nil))
	

;
; Capturing

  (= (game-piece-has-capture $Piece $Capture $Game)  
    (game-piece-struct-def $Game $Piece $Def)
    (piece-capture $Def $CompCapture)
    (capture-component $CompCapture $Capture))


  (= (capture-has-movement $Capture $M)  
    (capture-movement $Capture $Ms)
    (member $M $Ms))


  (= (capture-component $CompCapture $Capture)  
    (member $Capture $CompCapture))



  (= (capture-has-method $Capture $Method)  
    (capture-methods $Capture $Methods)
    (component-of-method $Method $Methods))


  (= (component-of-method clobber $M)  
    (method-clobber $M yes))
  (= (component-of-method retrieve $M)  
    (method-retrieve $M yes))
  (= (component-of-method hop $M)  
    (method-hop $M yes))



  (= (capture-methods-list $Capturing $Meths)  
    (bagof $Meth 
      (capture-has-method $Capturing $Meth) $Meths))


;
; Promoting
;
; Could simplify by using same structure produced by generator,
;
; instead of translating.
;
; Must handle simple case also (promote to fish).


  (= (game-piece-has-promoting $Piece $Promoting $Game)  
    (game-piece-struct-def $Game $Piece $Def)
    (piece-promote $Def $Promoting))


  (= (game-piece-promoting $Piece $Promoting $Game)  
    (game-piece-has-promoting $Piece $Promote $Game)
    (promoting-info $Promote $Promoting))

;
; Either a decision, or a simple type.

  (= (promoting-info  (promote  $Promote) (promote  $Promote))  True)
  (= (promoting-info $Promote $Promoting)  
    (decision-chooser $Promote $Player)
    (decision-options $Promote $Descr)
    (= $Promoting 
      (promote $Player $Descr)))



  (= (game-promote-rank $Game $Rank)  
    (game-board $Game $Board)
    (board-promote-rows $Board $Rank))



