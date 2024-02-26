;
; 

;
; 

;
; 

;
; 


;
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (current-game $Game) 
    (det-if-then-else 
      (control $Player) 
      (current-game-for-player $Player $Game) 
      (player-current-game $Game)))
; 


;
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
    (game-player-has-goal $Game $Player $Goal) 
    ( (current-game-for-player $Player $Game) (game-has-goal $Game $Goal)))
; 



;
; 



  (= 
    (current-board $B) 
    ( (current-game $G) (game-board $G $B)))
; 



  (= 
    (current-board-size $X $Y) 
    ( (player-current-game $G) 
      (game-board $G $B) 
      (board-size $B $X $Y)))
; 



  (= 
    (current-board-type $Type) 
    ( (player-current-game $G) 
      (game-board $G $B) 
      (board-type $B $Type)))
; 



  (= 
    (current-board-inversion $Type) 
    ( (player-current-game $G) 
      (game-board $G $B) 
      (board-inversion $B $Type)))
; 




;
; 

;
; 


  (= 
    (game-assignments $Game $Assign) 
    ( (player-current-game $Game) 
      (game-board $Game $Board) 
      (board-assignments $Board $Assign)))
; 



;
; 

;
; 

;
; 

;
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
    (game-has-assignments $Game $Assign) 
    ( (game-assignments $Game $Assign1) (full-assignment-if-random $Assign1 $Assign)))
; 



  (= 
    (full-assignment-if-random $As $Assignments) 
    ( (random-assignment-decision $As) 
      (set-det) 
      (random-assignment $Assignments)))
; 

  (= 
    (full_assignment_if_random  $As $As) True)
; 




;
; 

;
; 


  (= 
    (current-random-setup-game) 
    ( (player-current-game $Game) (random-setup-game $Game)))
; 



;
; 

;
; 


  (= 
    (random-setup-game $Game) 
    ( (game-assignments $Game $Assign) (random-assignment-decision $Assign)))
; 



  (= 
    (random-assignment-decision $AssignmentDef) 
    ( (assignment-decision $AssignmentDef $Assigner $PieceNames $Squares) (= $Assigner random)))
; 

	


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 




;
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
    (current-game-piece-struct-def $PieceStruct $Def) 
    ( (piece-struct-name $PieceStruct $Name) (current-game-piece-def $Game $Name $Def)))
; 



  (= 
    (current-game-piece-def $Name $Def) 
    ( (current-game $Game) (game-piece-def $Game $Name $Def)))
; 



  (= 
    (game--piece-has-movement $Piece $Movement) 
    ( (current-game $Game) (game-piece-has-movement $Piece $Movement $Game)))
; 



  (= 
    (game--piece-has-capture $Piece $Capture) 
    ( (current-game $Game) (game-piece-has-capture $Piece $Capture $Game)))
; 



  (= 
    (game--piece-has-constraint $Piece $Constraint) 
    ( (player-current-game $Game) (game-piece-has-constraint $Piece $Constraint $Game)))
; 


;
; 


  (= 
    (game--piece-continues $Piece) 
    ( (constraint-continue-captures $Con) (game--piece-has-constraint $Piece $Con)))
; 



  (= 
    (game--piece-must-capture $Piece) 
    ( (constraint-must-capture $Con) (game--piece-has-constraint $Piece $Con)))
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
    (game--piece-must $Piece) 
    (det-if-then-else current-game-must-capture True 
      (game--piece-must-capture $Piece)))
; 


;
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
    (game--piece-has-promoting $Piece $Promoting) 
    ( (current-game $Game) (game-piece-has-promoting $Piece $Promoting $Game)))
; 



  (= 
    (game--piece-promoting $Piece $Promoting) 
    ( (current-game $Game) (game-piece-promoting $Piece $Promoting $Game)))
; 



  (= 
    (game--promote-rank $Rank) 
    ( (current-game $Game) (game-promote-rank $Game $Rank)))
; 




