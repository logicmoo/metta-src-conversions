;                                           (convert-to-metta-file  arrive $_198764 metagame/learning/arrive.pl metagame/learning/arrive.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; arrive.pl

;
; ARRIVAL Strategy
;
; This strategy for a player favors positions in which that player
;
; owns pieces which are closer to the destination for his arrival goals.
;
; 
;
; We check that the piece is owned by the player
;
; who has the goal, as this strategy will not in general help us
;
; get our opponent's pieces to square they don't want to be on.
;
; 
;
; The value returned by this strategy is a probability, + or -,
;
; that the player will achieve a given goal (on backtracking,
;
; gives value for each of his goals).


  (= (arrive-value $Piece $Sq $SqT $Goal $Value $Pos $Tables)  
    (owns $Piece $Player)
    (game-player-has-goal $_ $Player $Goal)
    (arrive-goal $Goal $Player $Type $Sqs)
    (arrive-goal $Goal $Descr $Squares)
    (arrive-distance $Piece $Sq $SqT $Descr $Squares $SqDist $Tables)
    (clear-path-cost $Piece $Player $Sq $SqT $SqDist $PathCost $Pos)
    (arrive-likelihood $PathCost $Prob)
    (reasonable-likelihood $Prob)
    (arrive-goal-value $Piece $IValue)
    (expected-value $Prob $IValue $AbsVal)
    (negate-for-player $Player $AbsVal $Value))


  (= (arrive-distance $Piece $Sq $SqT (arrive $Descr $Squares) $Dist $Tables)  
    (arrive-distance $Piece $Sq $SqT $Descr $Squares $Dist $Tables))

;
; We approximate the distance by using the static piece distance,
;
; instead of checking the current board dynamically.

  (= (arrive-distance $Piece $Sq $SqT $Descr $Squares $Dist $Tables)  
    (matches $Descr $Piece)
    (member $SqT $Squares)
    (approx-path-distance $Sq $Piece $SqT $Dist $Tables))


;
; How much do we value achieving arrival goals? Can't make it
;
; infinite, as this will then dominate all other considerations.
;
; It may be a function of different pieces.
;
; For now we'll set it to 1, and use a parameter to control it.

  (= (arrive-goal-value  $Piece 1)  True)

;
; The likelihood of getting a piece to a square decreases
;
; exponentially with the number of moves required to
;
; achieve it. This uses the same decay rate as is used
;
; in eventual mobility determination, and for promotions.

  (= (arrive-likelihood $Distance $Prob)  
    (distance-value $Distance $Prob))

;
; DISTANCE_VALUE(Distance,Value)
;
; Distance is a positive integer, usually an abstract number of
;
; moves. The likelihood of realizing a goal decreases
;
; by some decreasing function of the number of moves required to
;
; achieve it.

  (= (distance-value $Distance $Value)  
    (parameter discount $D)
    (discount-fn $D $Distance $Value))


  (= (discount-fn inverse $Distance $Value)  
    (is $Value 
      (/ 1 
        (+ 1 $Distance))))
  (= (discount-fn exponent $Distance $Value)  
    (is $Value 
      (/ 1 
        (<< 1 $Distance))))


;
; If there is some distance, use it.
;
; Otherwise assume it is very far away (make this a parameter also?)
;
; This at least gives some points for having the right type of
;
; piece on the board.

  (= (approx-path-distance $Sq $Piece $SqT $Dist $Tables)  
    (square-piece-distance $Sq $Piece $SqT $Dist $Tables)
    (set-det))
  (= (approx-path-distance  $Sq $Piece $SqT 10 $Tables)  True)


;
; The cost (in moves) of clearing the path to a square
;
; and then moving the piece to it.
;
; Could take into account whether piece could capture occupier
;
; and thus give a discount as nobody else need do so.
;
; Now adding 1 move penalty if player is not in control in
;
; the current position, as this would be a handicap in a race
;
; to get somewhere.

  (= (clear-path-cost $Piece $Player $Sq $SqT $SqDist $PathCost $Pos)  
    (clear-square-cost $SqT $Player $SqClearCost $Pos)
    (control-cost $Player $ControlCost $Pos)
    (is $PathCost 
      (+ 
        (+ $ControlCost $SqDist) $SqClearCost)))


  (= (clear-square-cost $SqT $Player $SqClearCost $Pos)  
    (on $Occupier $SqT $Pos)
    (clear-occupier $Occupier $Player $SqClearCost))
	
;
; Rather arbitrary value for cost of clearing opponent
;
; from the square!

  (= (clear-occupier  empty $_ 0)  True)
  (= (clear-occupier (piece $Type $Player) $Player 1)  
    (set-det))
  (= (clear-occupier  (piece  $Type $Player) $Opponent 5)  True)



  (= (reasonable-likelihood $Prob)  
    (> $Prob 0))


  (= (expected-value $Prob $IVal $Value)  
    (is $Value 
      (* $Prob $IVal)))

	


  (= (test $Sq $SqT $Value)  
    (arrive-value 
      (piece slug player) $Sq $SqT 
      (piece-desc any-player any-piece) 
      (:: $SqT) $Value))




