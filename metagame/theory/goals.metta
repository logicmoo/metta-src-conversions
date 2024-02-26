;
; 

;
; 

;
; 

;
; 


;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 


;
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
    (game-over) 
    (goal-achieved $Player $Game))
; 

  (= 
    (game-over) 
    (exceeded-move-limit $Game))
; 

  (= 
    (game-over) 
    (too-many-repetitions $Game))
; 



;
; 

;
; 

;
; 



  (= 
    (exceeded-move-limit $Game) 
    ( (game-move-limit $Game $L) (move-count $L)))
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
    (game_move_limit  $Game 200) True)
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
    (move_limit_outcome  $Game draw) True)
; 



;
; 

;
; 

;
; 

;
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
    (game_repetitions  $Game 3) True)
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
    (repetition_outcome  $Game draw) True)
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
    (too_many_repetitions  $Game) 
    (empty))
; 



;
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
    (game-outcome $Outcome) 
    ( (too-many-repetitions $Game) 
      (set-det) 
      (repetition-outcome $Game $Outcome)))
; 

  (= 
    (game-outcome $Outcome) 
    ( (exceeded-move-limit $Game) 
      (set-det) 
      (move-limit-outcome $Game $Outcome)))
; 

  (= 
    (game-outcome $Outcome) 
    ( (player-outcome player $WinP) 
      (player-outcome opponent $WinO) 
      (outcome $WinP $WinO $Outcome)))
; 



;
; 


  (= 
    (player-outcome $Player $Outcome) 
    (det-if-then-else 
      (goal-achieved $Player $Game) 
      (= $Outcome yes) 
      (= $Outcome no)))
; 



;
; 

;
; 

;
; 


  (= 
    (outcome  yes yes draw) True)
; 

  (= 
    (outcome  yes no player) True)
; 

  (= 
    (outcome  no yes opponent) True)
; 



;
; 

;
; 


  (= 
    (goal-achieved $Player $Game) 
    ( (game-player-has-goal $Game $Player $Goal) 
      (goal-true $Goal) 
      (verbosely-format "Goal achieved: ~p achieved goal ~p~n" 
        (:: $Player $Goal))))
; 



;
; 

;
; 


  (= 
    (goal-true (arrive $Descr $Squares)) 
    ( (member $Sq $Squares) 
      (on $Piece $Sq) 
      (matches $Descr $Piece)))
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
    (goal-true (eradicate $Descr)) 
    ( (not still-assigning) (not (exists $Descr))))
; 


;
; 

;
; 

  (= 
    (goal-true (stalemate $Player)) 
    ( (control $Player) (not (legal-move $M $Player))))
; 


;
; 

;
; 

;
; 

;
; 

;
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
    (exists $Descr) 
    ( (on $Piece $Sq) (matches $Descr $Piece)))
; 

	


