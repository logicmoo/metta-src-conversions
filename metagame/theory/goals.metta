;                                           (convert-to-metta-file  goals $_66008 metagame/theory/goals.pl metagame/theory/goals.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; goals.pl

;
; Representation of goal achievement and complete legal moves
;
; for symmetric chess-like games.
;
; These routines form the skeleton of the game definition,
;
; providing three components:
;
; 
;
; 1. legal(Move,Player,StateIn,StateOut)
;
; True when Move is legal for Player in StateIn, and Produces StateOut.
;
; 
;
; 2. game_over(State,_)
;
; True when the game in StateIn has ended. (Second var is dummy).
;
; 
;
; 3. game_outcome(FinalState,Outcome)
;
; Outcome is the final outcome of Game, which ends in FinalState.
;
; Outcome is either PLAYER, OPPONENT, or DRAW.
;
; 
;
; Any 2-player game must provide exactly these three procedures,
;
; and an interface which relies on these three is thus fully
;
; general to cover 2-player games.
;
; 

;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Goals
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; The GAME IS OVER when ANY of the following is true:
;
; 1. some player has achieved a goal
;
; 2. the maximum number of moves have been played.
;
; 3. the position has repeated a certain number of times.
;
; (REPETITION IS NOT IMPLEMENTED YET, maybe never!).

  (= game-over  
    (goal-achieved $Player $Game))
  (= game-over  
    (exceeded-move-limit $Game))
  (= game-over  
    (too-many-repetitions $Game))


;
; ========================================
;
; K-Move Rule
;
; ========================================


  (= (exceeded-move-limit $Game)  
    (game-move-limit $Game $L)
    (move-count $L))

;
; GAME_MOVE_LIMIT(+Game,Number).
;
; Could be generated differently with each game,
;
; or set arbitrarily.
;
; Here we just set it for all games.

  (= (game-move-limit  $Game 200)  True)

;
; MOVE_LIMIT_OUTCOME(+Game,-Outcome).
;
; If the game ended by exceeding the move limit,
;
; each game could decide what the outcome is (who wins).
;
; Here we say it is a draw for all games.

  (= (move-limit-outcome  $Game draw)  True)


;
; ========================================
;
; Repetition Rule
;
; ========================================
;
; THIS IS NOT IMPLEMENTED! Hooks are placed here in case we
;
; do implement it sometime.

;
; GAME_REPETITIONS(+Game,Number).
;
; Could be generated differently with each game, or set arbitrarily.
;
; Here we just say a game ends if a position repeats 3 times.

  (= (game-repetitions  $Game 3)  True)

;
; REPETITION_OUTCOME(+Game,-Outcome).
;
; If the game ended by too many repetitions,
;
; each game could decide what the outcome is (who wins).
;
; Here we say it is a draw for all games.

  (= (repetition-outcome  $Game draw)  True)

;
; TOO_MANY_REPETITIONS
;
; There are too many repetitions when the present position
;
; repeats a previous one a number of times.
;
; 

  (= (too-many-repetitions  $Game)  
    (empty))


;
; GAME_OUTCOME(?Outcome)
;
; There is a game outcome only when the game is over.
;
; Determines the outcome based on which players have achieved one of their goals,
;
; or on the game-specific outcome in case of exceeding the move limit or
;
; excess-repetition.

  (= (game-outcome $Outcome)  
    (too-many-repetitions $Game)
    (set-det)
    (repetition-outcome $Game $Outcome))
  (= (game-outcome $Outcome)  
    (exceeded-move-limit $Game)
    (set-det)
    (move-limit-outcome $Game $Outcome))
  (= (game-outcome $Outcome)  
    (player-outcome player $WinP)
    (player-outcome opponent $WinO)
    (outcome $WinP $WinO $Outcome))


;
; PLAYER_OUTCOME(+Player,?Yes/No)

  (= (player-outcome $Player $Outcome)  
    (det-if-then-else 
      (goal-achieved $Player $Game) 
      (= $Outcome yes) 
      (= $Outcome no)))


;
; OUTCOME(Player1_Outcome,Player2_Outcome,Outcome).
;
; Outcome is the player who achieved the goal,
;
; or draw if both.

  (= (outcome  yes yes draw)  True)
  (= (outcome  yes no player)  True)
  (= (outcome  no yes opponent)  True)


;
; GOAL_ACHIEVED(Player,Game)
;
; Player is one of the players, and has achieved one of his goals in Game.

  (= (goal-achieved $Player $Game)  
    (game-player-has-goal $Game $Player $Goal)
    (goal-true $Goal)
    (verbosely-format "Goal achieved: ~p achieved goal ~p~n" 
      (:: $Player $Goal)))


;
; An ARRIVAL goal has been achieved when a piece matching Description
;
; has arrived on one of the Squares.

  (= (goal-true (arrive $Descr $Squares))  
    (member $Sq $Squares)
    (on $Piece $Sq)
    (matches $Descr $Piece))

;
; An ERADICATE goal has been achieved when there are no pieces
;
; *on the board* matching a description, when players are no longer assigning pieces.
;
; (Else these goals would be satisfied immediately and trivially).
;
; still_assigning defined in legal.pl
  (= (goal-true (eradicate $Descr))  
    (not still-assigning)
    (not (exists $Descr)))

;
; A player is STALEMATED when it is his turn to move,
;
; but he can't make any legal moves.
  (= (goal-true (stalemate $Player))  
    (control $Player)
    (not (legal-move $M $Player)))

;
; A Description exists when a piece on a square matches it.
;
; There is an efficiency tradeoff here.
;
; 1. We could examine each piece on the board, to see if it matches the
;
; description.
;
; 2. We could consider any piece which could match the description,
;
; and see if it is on the board.
;
; 
;
; The second case is much better if we have indexing to see if
;
; specific pieces are on the board.


  (= (exists $Descr)  
    (on $Piece $Sq)
    (matches $Descr $Piece))
	


