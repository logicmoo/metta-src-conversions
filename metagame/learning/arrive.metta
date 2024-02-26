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
    (arrive-value $Piece $Sq $SqT $Goal $Value $Pos $Tables) 
    ( (owns $Piece $Player) 
      (game-player-has-goal $_ $Player $Goal) 
      (arrive-goal $Goal $Player $Type $Sqs) 
      (arrive-goal $Goal $Descr $Squares) 
      (arrive-distance $Piece $Sq $SqT $Descr $Squares $SqDist $Tables) 
      (clear-path-cost $Piece $Player $Sq $SqT $SqDist $PathCost $Pos) 
      (arrive-likelihood $PathCost $Prob) 
      (reasonable-likelihood $Prob) 
      (arrive-goal-value $Piece $IValue) 
      (expected-value $Prob $IValue $AbsVal) 
      (negate-for-player $Player $AbsVal $Value)))
; 



  (= 
    (arrive-distance $Piece $Sq $SqT 
      (arrive $Descr $Squares) $Dist $Tables) 
    (arrive-distance $Piece $Sq $SqT $Descr $Squares $Dist $Tables))
; 


;
; 

;
; 


  (= 
    (arrive-distance $Piece $Sq $SqT $Descr $Squares $Dist $Tables) 
    ( (matches $Descr $Piece) 
      (member $SqT $Squares) 
      (approx-path-distance $Sq $Piece $SqT $Dist $Tables)))
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
    (arrive_goal_value  $Piece 1) True)
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
    (arrive-likelihood $Distance $Prob) 
    (distance-value $Distance $Prob))
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
    (distance-value $Distance $Value) 
    ( (parameter discount $D) (discount-fn $D $Distance $Value)))
; 



  (= 
    (discount-fn inverse $Distance $Value) 
    (is $Value 
      (/ 1 
        (+ 1 $Distance))))
; 

  (= 
    (discount-fn exponent $Distance $Value) 
    (is $Value 
      (/ 1 
        (<< 1 $Distance))))
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
    (approx-path-distance $Sq $Piece $SqT $Dist $Tables) 
    ( (square-piece-distance $Sq $Piece $SqT $Dist $Tables) (set-det)))
; 

  (= 
    (approx_path_distance  $Sq $Piece $SqT 10 $Tables) True)
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
    (clear-path-cost $Piece $Player $Sq $SqT $SqDist $PathCost $Pos) 
    ( (clear-square-cost $SqT $Player $SqClearCost $Pos) 
      (control-cost $Player $ControlCost $Pos) 
      (is $PathCost 
        (+ 
          (+ $ControlCost $SqDist) $SqClearCost))))
; 



  (= 
    (clear-square-cost $SqT $Player $SqClearCost $Pos) 
    ( (on $Occupier $SqT $Pos) (clear-occupier $Occupier $Player $SqClearCost)))
; 

	
;
; 

;
; 


  (= 
    (clear_occupier  empty $_ 0) True)
; 

  (= 
    (clear-occupier 
      (piece $Type $Player) $Player 1) 
    (set-det))
; 

  (= 
    (clear_occupier  
      (piece  $Type $Player) $Opponent 5) True)
; 




  (= 
    (reasonable-likelihood $Prob) 
    (> $Prob 0))
; 



  (= 
    (expected-value $Prob $IVal $Value) 
    (is $Value 
      (* $Prob $IVal)))
; 


	


  (= 
    (test $Sq $SqT $Value) 
    (arrive-value 
      (piece slug player) $Sq $SqT 
      (piece-desc any-player any-piece) 
      (:: $SqT) $Value))
; 





