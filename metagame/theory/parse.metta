;                                           (convert-to-metta-file  parse $_92602 metagame/theory/parse.pl metagame/theory/parse.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; parse.pl



;
; For now, avoid explicit inversion in the legal rules by producing an
;
; entire inverted game from each player's perspective, used when
;
; they are in control.
;
; Then interpreter can always assume PLAYER is to move!
;
; 
;
; Changed to eliminate choice points.
;
; current_game(Game) :-
;
; control(opponent), !,
;
; opponent_current_game(Game).
;
; current_game(Game) :-
;
; player_current_game(Game).


  (= (current-game $Game)  
    (det-if-then-else 
      (control $Player) 
      (current-game-for-player $Player $Game) 
      (player-current-game $Game)))

;
; ; Goals
		
;
; GAME_PLAYER_HAS_GOAL(?Game,+Player,-Goal) :-
;
; If Player is in control, then already using a properly inverted game
;
; so don't need to invert it to check his goals.
;
; Otherwise, we're not using it from his perspective, so invert it.


  (= (game-player-has-goal $Game $Player $Goal)  
    (current-game-for-player $Player $Game)
    (game-has-goal $Game $Goal))


;
; ; Initial Setup


  (= (current-board $B)  
    (current-game $G)
    (game-board $G $B))


  (= (current-board-size $X $Y)  
    (player-current-game $G)
    (game-board $G $B)
    (board-size $B $X $Y))


  (= (current-board-type $Type)  
    (player-current-game $G)
    (game-board $G $B)
    (board-type $B $Type))


  (= (current-board-inversion $Type)  
    (player-current-game $G)
    (game-board $G $B)
    (board-inversion $B $Type))



;
; GAME_ASSIGNMENTS(+Game,-Assign)
;
; The game has the initial piece assignments.

  (= (game-assignments $Game $Assign)  
    (player-current-game $Game)
    (game-board $Game $Board)
    (board-assignments $Board $Assign))


;
; GAME_HAS_ASSIGNMENTS(+Game,-Assign)
;
; The game has the initial piece assignments.
;
; If the game is defined to have a random setup,
;
; the setup used for this contest must already have been determined,
;
; and stored as:
;
; current_random_assignments(Assignments).
;
; 
;
; Otherwise, just use the fully-specified assignments as generated.


  (= (game-has-assignments $Game $Assign)  
    (game-assignments $Game $Assign1)
    (full-assignment-if-random $Assign1 $Assign))


  (= (full-assignment-if-random $As $Assignments)  
    (random-assignment-decision $As)
    (set-det)
    (random-assignment $Assignments))
  (= (full-assignment-if-random  $As $As)  True)



;
; CURRENT_RANDOM_SETUP_GAME(+Game)
;
; True if current game starts with a random setup.

  (= current-random-setup-game  
    (player-current-game $Game)
    (random-setup-game $Game))


;
; RANDOM_SETUP_GAME(+Game)
;
; True if game starts with a random setup.

  (= (random-setup-game $Game)  
    (game-assignments $Game $Assign)
    (random-assignment-decision $Assign))


  (= (random-assignment-decision $AssignmentDef)  
    (assignment-decision $AssignmentDef $Assigner $PieceNames $Squares)
    (= $Assigner random))
	


;
; ;; random_assignment(Assignments)
;
; ;; If this is a random game, the assignments for this contest
;
; ;; must already have been determined externally,
;
; ;; such that this call succeeds.
;
; ;; The result must be an assignment list,
;
; ;; the result of parsing the arbitrary_assignment part of the
;
; ;; game definition in grammar.pl, of the following form:
;
; ;; Assignments = [piece1=[square1,..],...].
;
; ;; For example:
;
; ;; [piece1=[square(2,1)],piece2=[square(1,2),square(3,1)]]



;
; ; Pieces

;
; Moved to parse2.pl, use player_current_game instead!
;
; current_game_piece_name(Name) :-
;
; current_game(Game),
;
; game_piece_def(Game,Name,_).


  (= (current-game-piece-struct-def $PieceStruct $Def)  
    (piece-struct-name $PieceStruct $Name)
    (current-game-piece-def $Game $Name $Def))


  (= (current-game-piece-def $Name $Def)  
    (current-game $Game)
    (game-piece-def $Game $Name $Def))


  (= (game--piece-has-movement $Piece $Movement)  
    (current-game $Game)
    (game-piece-has-movement $Piece $Movement $Game))


  (= (game--piece-has-capture $Piece $Capture)  
    (current-game $Game)
    (game-piece-has-capture $Piece $Capture $Game))


  (= (game--piece-has-constraint $Piece $Constraint)  
    (player-current-game $Game)
    (game-piece-has-constraint $Piece $Constraint $Game))

;
; Whether a piece continues capturing.

  (= (game--piece-continues $Piece)  
    (constraint-continue-captures $Con)
    (game--piece-has-constraint $Piece $Con))


  (= (game--piece-must-capture $Piece)  
    (constraint-must-capture $Con)
    (game--piece-has-constraint $Piece $Con))

;
; Whether a piece must capture.
;
; It must capture either if the game has this constraint,
;
; or the piece itself does.
;
; Note this assumes Piece is bound, won't nec. generate them.

  (= (game--piece-must $Piece)  
    (det-if-then-else current-game-must-capture True 
      (game--piece-must-capture $Piece)))

;
; ---------------------------------------------

;
; Promoting
;
; Could simplify by using same structure produced by generator,
;
; instead of translating.
;
; Must handle simple case also (promote to fish).


  (= (game--piece-has-promoting $Piece $Promoting)  
    (current-game $Game)
    (game-piece-has-promoting $Piece $Promoting $Game))


  (= (game--piece-promoting $Piece $Promoting)  
    (current-game $Game)
    (game-piece-promoting $Piece $Promoting $Game))


  (= (game--promote-rank $Rank)  
    (current-game $Game)
    (game-promote-rank $Game $Rank))



