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
    (path-dist $Piece $Player $SqF $SqT 0 0) 
    (set-det))
; 

  (= 
    (path-dist $Piece $Player $SqF $SqT $Max $N) 
    ( (reaches $Piece $Player $SqF $SqT $In) (path-dist $Piece $Player $SqF $SqF 0 $S)))
; 

  (= 
    (path-dist $Piece $Player $SqF $SqT $N $S) 
    ( (reaches $Piece $Player $SqF $Sq1 $S) 
      (path-dist $Piece $Player $Sq1 $SqT $N1 $S) 
      (is $N 
        (+ $N1 1))))
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
    (path-dist $Type $Player $SqF $SqT $Dist $Max $S0) 
    ( (det-if-then-else 
        (piece-struct $Type $T $Player) 
        (= $Piece $Type) 
        (piece-struct $Piece $Type $Player)) 
      (put-control $Player $S0 $S) 
      (path-dist0 $Piece $Player $SqF $SqT $Dist $Max $S) 
      (tracing-path-format squares "<~p>: ~p -> ~p in ~p moves~n" 
        (:: $Piece $SqF $SqT $Dist))))
; 

	

  (= 
    (path-dist0 $Piece $Player $SqF $SqT $N $Max $S) 
    ( (path-dist1 $Piece $Player $SqF $SqT $N $Max $S) (> $N 0)))
; 



  (= 
    (path_dist1  $Piece $Player $SqF $SqF 0 $Left $S) True)
; 

  (= 
    (path-dist1 $Piece $Player $SqF $SqT $N $Max $S) 
    ( (> $Max 0) 
      (\== $SqF $SqT) 
      (reaches $Piece $Player $SqF $Sq1 $S) 
      (is $Max1 
        (- $Max 1)) 
      (path-dist1 $Piece $Player $Sq1 $SqT $N1 $Max1 $S) 
      (is $N 
        (+ $N1 1))))
; 




  (= 
    (new-empty-state $S) 
    ( (new-state $S1) (make-empty-board $S1 $S)))
; 



  (= 
    (new-state-of-type empty $S) 
    (new-empty-state $S))
; 

  (= 
    (new-state-of-type any $S) 
    (new-state $S))
; 



  (= 
    (piece-moves-empty $Piece $Sq $SqT) 
    ( (new-empty-state $State) (piece-moves $Piece $Sq $SqT $State)))
; 



  (= 
    (piece-moves-any $Piece $Sq $SqT) 
    ( (new-state $State) (piece-moves $Piece $Sq $SqT $State)))
; 



  (= 
    (piece-moves $Piece $Sq $SqT $State) 
    ( (board-square $Sq) 
      (piece-index $Piece $_) 
      (moves $Piece $Player $Sq $SqT $State) 
      (tracing-path-format moves "<~p>: ~p -> ~p~n" 
        (:: $Piece $Sq $SqT))))
; 



  (= 
    (piece-move $Piece $Sq $SqT $MoveType $StateType) 
    ( (new-state-of-type $StateType $S) (piece-move-for-type $MoveType $Piece $Sq $SqT $S)))
; 


;
; 

;
; 


  (= 
    (piece-move-for-type $MoveType $Piece $Sq $SqT $S) 
    ( (board-square $Sq) 
      (piece-index $Piece $_) 
      (owns $Piece $Player) 
      (put-control $Player $S $S1) 
      (move-for-type $MoveType $Piece $Sq $SqT $S1)))
; 

	

  (= 
    (move-for-type moving $Piece $Sq $SqT $State) 
    ( (moves $Piece $Player $Sq $SqT $State) (tracing-path-format moves "<~p>: ~p -> ~p~n" (:: $Piece $Sq $SqT))))
; 

  (= 
    (move-for-type capturing $Piece $Sq $SqT $State) 
    ( (captures $Piece $Player $Sq $SqT $State) (tracing-path-format moves "<~p>: ~p -> ~p~n" (:: $Piece $Sq $SqT))))
; 

  (= 
    (move-for-type capturing-specific $Piece $Sq $SqT $State) 
    ( (captures $Piece $Player $Sq $SqT $Effect $Captured $State) (tracing-path-format moves "<~p>: ~p -> ~p x ~p (~p)~n" (:: $Piece $Sq $SqT $Captured $Effect))))
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



  !(my-ensure-loaded (library tracing))
; 



  (= 
    (tracing-path $Type $Call) 
    (det-if-then-else 
      (tracing (path $Type)) 
      (call $Call) True))
; 


;
; 


  (= 
    (tracing-path-format $Type $String $Args) 
    (det-if-then-else 
      (tracing (path $Type)) 
      (format $String $Args) True))
; 



  (= 
    (tracing-path-timing $Type $Call) 
    (trace-timing 
      (path $Type) $Call))
; 



  (= 
    (set-path-verbosity $Level $Status) 
    (set-tracing 
      (path $Level) $Status))
; 



  (= 
    (silent-path) 
    ( (set-path-verbosity ordering off) 
      (set-path-verbosity value off) 
      (set-path-verbosity resources off) 
      (set-path-verbosity timing off) 
      (set-path-verbosity iteration off)))
; 



  (= 
    (trace-path-squares) 
    (set-path-verbosity squares on))
; 
 

  (= 
    (trace-path-ordering) 
    (set-path-verbosity ordering on))
; 
 


  !(silent-path *)
; 

;
; 



; 

