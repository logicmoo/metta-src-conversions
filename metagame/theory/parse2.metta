;
; 

;
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
    (current-game-for-player player $Game) 
    (player-current-game $Game))
; 

  (= 
    (current-game-for-player opponent $Game) 
    (opponent-current-game $Game))
; 




  (= 
    (current-game-name $GameName) 
    ( (player-current-game $G) (game-name $G $GameName)))
; 
 



  (= 
    (current-game-piece-name $Name) 
    ( (player-current-game $Game) (game-piece-def $Game $Name $_)))
; 
 



  (= 
    (game-piece-name $Game $Name) 
    (game-piece-def $Game $Name $_))
; 
 

;
; 


  (= 
    (game-piece-struct-def $Game $PieceStruct $Def) 
    ( (piece-struct-name $PieceStruct $Name) (game-piece-def $Game $Name $Def)))
; 


;
; 


  (= 
    (game-piece-def $Game $Name $Def) 
    ( (game-piece-defs $Game $Defs) 
      (member $Def $Defs) 
      (piece-name $Def $Name)))
; 



  (= 
    (game-piece-defs $G $P) 
    (game-pieces $G $P))
; 



;
; 


  (= 
    (game-has-goal $Game $Goal) 
    ( (game-goal $Game $CompGoal) (goal-component $CompGoal $Goal)))
; 



  (= 
    (goal-component $CompGoal $Goal) 
    (member $Goal $CompGoal))
; 


;
; 



  (= 
    (game-must-capture $Game) 
    ( (constraint-must-capture $Con) (game-constraints $Game $Con)))
; 

	

  (= 
    (game-continues $Game) 
    ( (constraint-continue-captures $Con) (game-constraints $Game $Con)))
; 



  (= 
    (current-game-must-capture) 
    ( (player-current-game $Game) (game-must-capture $Game)))
; 






;
; 



  (= 
    (game-piece-has-constraint $Piece $Constraint $Game) 
    ( (game-piece-struct-def $Game $Piece $Def) (piece-constraints $Def $Constraint)))
; 



;
; 



  (= 
    (game-piece-has-movement $Piece $Movement $Game) 
    ( (game-piece-struct-def $Game $Piece $Def) 
      (piece-movement $Def $CompMovement) 
      (movement-component $CompMovement $Movement)))
; 



  (= 
    (movement-component $CompMovement $Movement) 
    (member $Movement $CompMovement))
; 


;
; 

;
; 

;
; 



  (= 
    (leap $M $Dir) 
    ( (movement-type $M $L) 
      (leaper $L) 
      (movement-sym-dir $M $Dir)))
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
    (ride $M $Dir $Min $Max $Longest) 
    ( (movement-type $M $R) 
      (rider $R $Longest $Min1 $Max1) 
      (movement-sym-dir $M $Dir) 
      (valid-min $Min1 $Min) 
      (valid-max $Max1 $Dir $Max)))
; 


;
; 


  (= 
    (longest  yes) True)
; 




  (= 
    (hop $M $Dir $Before $Over $After $Description) 
    ( (movement-type $M $H) 
      (hopper $H $Description $Before $Over $After) 
      (movement-sym-dir $M $Dir)))
; 



  (= 
    (movement-sym-dir $M $Dir) 
    ( (movement-dir $M $D) 
      (movement-syms $M $Syms) 
      (sym-dir $D $Syms $Dir)))
; 


;
; 

;
; 



  (= 
    (movement-syms $M $Syms) 
    ( (movement-sym $M $S) (sym-set $S $Syms Nil)))
; 

	

;
; 


  (= 
    (game-piece-has-capture $Piece $Capture $Game) 
    ( (game-piece-struct-def $Game $Piece $Def) 
      (piece-capture $Def $CompCapture) 
      (capture-component $CompCapture $Capture)))
; 



  (= 
    (capture-has-movement $Capture $M) 
    ( (capture-movement $Capture $Ms) (member $M $Ms)))
; 



  (= 
    (capture-component $CompCapture $Capture) 
    (member $Capture $CompCapture))
; 




  (= 
    (capture-has-method $Capture $Method) 
    ( (capture-methods $Capture $Methods) (component-of-method $Method $Methods)))
; 



  (= 
    (component-of-method clobber $M) 
    (method-clobber $M yes))
; 

  (= 
    (component-of-method retrieve $M) 
    (method-retrieve $M yes))
; 

  (= 
    (component-of-method hop $M) 
    (method-hop $M yes))
; 




  (= 
    (capture-methods-list $Capturing $Meths) 
    (bagof $Meth 
      (capture-has-method $Capturing $Meth) $Meths))
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
    (game-piece-has-promoting $Piece $Promoting $Game) 
    ( (game-piece-struct-def $Game $Piece $Def) (piece-promote $Def $Promoting)))
; 



  (= 
    (game-piece-promoting $Piece $Promoting $Game) 
    ( (game-piece-has-promoting $Piece $Promote $Game) (promoting-info $Promote $Promoting)))
; 


;
; 


  (= 
    (promoting_info  
      (promote  $Promote) 
      (promote  $Promote)) True)
; 

  (= 
    (promoting-info $Promote $Promoting) 
    ( (decision-chooser $Promote $Player) 
      (decision-options $Promote $Descr) 
      (= $Promoting 
        (promote $Player $Descr))))
; 




  (= 
    (game-promote-rank $Game $Rank) 
    ( (game-board $Game $Board) (board-promote-rows $Board $Rank)))
; 




