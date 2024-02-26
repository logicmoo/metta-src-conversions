;                                           (convert_to_metta_file  paths $_17564 metagame/learning/paths.pl metagame/learning/paths.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; paths.pl



  (= (path-dist $Piece $Player $SqF $SqT 0 0)  
    (set-det))
  (= (path-dist $Piece $Player $SqF $SqT $Max $N)  
    (reaches $Piece $Player $SqF $SqT $In)
    (path-dist $Piece $Player $SqF $SqF 0 $S))
  (= (path-dist $Piece $Player $SqF $SqT $N $S)  
    (reaches $Piece $Player $SqF $Sq1 $S)
    (path-dist $Piece $Player $Sq1 $SqT $N1 $S)
    (is $N 
      (+ $N1 1)))


;
; ; PATH_DIST(Type,Player,SqF,SqT,Dist,Max,S0)
;
; ; True if, in state <S0>, <Player>'s piece of <Type> could reach SqT from SqF on
;
; ; current board in <Dist> moves, each of which uses either a moving or
;
; ; capturing power, where <Dist> <= <Max>.
;
; ; <Type> either a piece_type or piece_struct [king or
;
; ; piece(king,player)].

  (= (path-dist $Type $Player $SqF $SqT $Dist $Max $S0)  
    (det-if-then-else 
      (piece-struct $Type $T $Player) 
      (= $Piece $Type) 
      (piece-struct $Piece $Type $Player))
    (put-control $Player $S0 $S)
    (path-dist0 $Piece $Player $SqF $SqT $Dist $Max $S)
    (tracing-path-format squares "<~p>: ~p -> ~p in ~p moves~n" 
      (:: $Piece $SqF $SqT $Dist)))
	

  (= (path-dist0 $Piece $Player $SqF $SqT $N $Max $S)  
    (path-dist1 $Piece $Player $SqF $SqT $N $Max $S)
    (> $N 0))


  (= (path_dist1  $Piece $Player $SqF $SqF 0 $Left $S)  True)
  (= (path-dist1 $Piece $Player $SqF $SqT $N $Max $S)  
    (> $Max 0)
    (\== $SqF $SqT)
    (reaches $Piece $Player $SqF $Sq1 $S)
    (is $Max1 
      (- $Max 1))
    (path-dist1 $Piece $Player $Sq1 $SqT $N1 $Max1 $S)
    (is $N 
      (+ $N1 1)))



  (= (new-empty-state $S)  
    (new-state $S1)
    (make-empty-board $S1 $S))


  (= (new-state-of-type empty $S)  
    (new-empty-state $S))
  (= (new-state-of-type any $S)  
    (new-state $S))


  (= (piece-moves-empty $Piece $Sq $SqT)  
    (new-empty-state $State)
    (piece-moves $Piece $Sq $SqT $State))


  (= (piece-moves-any $Piece $Sq $SqT)  
    (new-state $State)
    (piece-moves $Piece $Sq $SqT $State))


  (= (piece-moves $Piece $Sq $SqT $State)  
    (board-square $Sq)
    (piece-index $Piece $_)
    (moves $Piece $Player $Sq $SqT $State)
    (tracing-path-format moves "<~p>: ~p -> ~p~n" 
      (:: $Piece $Sq $SqT)))


  (= (piece-move $Piece $Sq $SqT $MoveType $StateType)  
    (new-state-of-type $StateType $S)
    (piece-move-for-type $MoveType $Piece $Sq $SqT $S))

;
; Hacked the put_control to get the right definitions.
;
; This should be fixed in legal.

  (= (piece-move-for-type $MoveType $Piece $Sq $SqT $S)  
    (board-square $Sq)
    (piece-index $Piece $_)
    (owns $Piece $Player)
    (put-control $Player $S $S1)
    (move-for-type $MoveType $Piece $Sq $SqT $S1))
	

  (= (move-for-type moving $Piece $Sq $SqT $State)  
    (moves $Piece $Player $Sq $SqT $State)
    (tracing-path-format moves "<~p>: ~p -> ~p~n" 
      (:: $Piece $Sq $SqT)))
  (= (move-for-type capturing $Piece $Sq $SqT $State)  
    (captures $Piece $Player $Sq $SqT $State)
    (tracing-path-format moves "<~p>: ~p -> ~p~n" 
      (:: $Piece $Sq $SqT)))
  (= (move-for-type capturing-specific $Piece $Sq $SqT $State)  
    (captures $Piece $Player $Sq $SqT $Effect $Captured $State)
    (tracing-path-format moves "<~p>: ~p -> ~p x ~p (~p)~n" 
      (:: $Piece $Sq $SqT $Captured $Effect)))
	


;
; ================================================================================
;
; tracing execution of path-distance routines
;
; ================================================================================

;
; The following tracing modules are used in this file:
;
; squares: info on squares evaluation
;
; mobility: info on piece mobility
;
; Each module can be set on/off, using set_path_verbosity (see below), or
;
; using trace_path_<module>.
;
; 
;
; All can be turned off with silent_path.


  !(my-ensure-loaded (library tracing))


  (= (tracing-path $Type $Call)  
    (det-if-then-else 
      (tracing (path $Type)) 
      (call $Call) True))

;
; Might cause trouble later when want to use streams also.

  (= (tracing-path-format $Type $String $Args)  
    (det-if-then-else 
      (tracing (path $Type)) 
      (format $String $Args) True))


  (= (tracing-path-timing $Type $Call)  
    (trace-timing 
      (path $Type) $Call))


  (= (set-path-verbosity $Level $Status)  
    (set-tracing 
      (path $Level) $Status))


  (= (silent-path)  
    (set-path-verbosity ordering off)
    (set-path-verbosity value off)
    (set-path-verbosity resources off)
    (set-path-verbosity timing off)
    (set-path-verbosity iteration off))


  (= (trace-path-squares)  
    (set-path-verbosity squares on)) 

  (= (trace-path-ordering)  
    (set-path-verbosity ordering on)) 


  !(silent-path *)
;
; :- trace_path_squares.


; /*========================================================================= In^Out^( on(King,square(5,1),In), moves(King,player,square(2,4),SqT,In), print(SqT), fail ). In^Out^( on(King,square(5,1),In), moves(King,player,square(2,8),SqT,In), print(SqT), fail ). In^Out^( on(King,square(5,1),In), captures(King,player,square(2,8),SqT,In), print(SqT), fail ). In^Out^( on(King,square(5,1),In), reaches(King,player,square(2,8),SqT,In), print(SqT), fail ). In^Out^( on(King,square(5,1),In), reaches(King,player,square(2,7),SqT,In), print(SqT), fail ). In^Out^( on(King,square(5,1),In), reaches(King,player,square(2,7),Sq1,In), reaches(King,player,Sq1,SqT,In), print(SqT), fail ). In^Out^( on(King,square(5,1),In), reaches(King,player,square(2,7),Sq1,In), reaches(King,player,Sq1,Sq2,In), reaches(King,player,Sq1,Sq3,In), on(Piece,square(4,8),In), on(Piece,Sq3,In), print(Sq3), fail ). In^Out^( on(Piece,player,Sq,In), reaches(Piece,player,Sq,Sq1,In), reaches(Piece,player,Sq1,Sq2,In), reaches(Piece,player,Sq2,Sq3,In), on(PieceK,Sq3,In), piece_struct_name(PieceK,king), print(Sq3), write(:), print(Piece), print(from), print(Sq), nl, fail ). In^Out^( on(Piece,player,Sq,In), reaches(Piece,player,Sq,SqT,In), format("<~p>: ~p -> ~p in ~p moves~n",[Piece,Sq,SqT,1]), fail ). In^Out^( on(Piece,player,Sq,In), moves(Piece,player,Sq,SqT,In), format("<~p>: ~p -> ~p in ~p moves~n",[Piece,Sq,SqT,1]), fail ). In^Out^( on(Piece,player,Sq,In), captures(Piece,player,Sq,SqT,In), format("<~p>: ~p -> ~p in ~p moves~n",[Piece,Sq,SqT,1]), fail ). In^Out^( path_dist(queen,player,square(4,1),SqT,Dist,2,In), fail ). In^Out^( on(piece(king,opponent),SqT,In), on(Piece,player,Square,In), path_dist(Piece,player,Square,SqT,Dist,3,In) ). In^Out^( on(Piece,player,Sq,In), reaches(Piece,player,Sq,Sq1,In), reaches(Piece,player,Sq1,Sq2,In), reaches(Piece,player,Sq2,Sq3,In), on(PieceK,Sq3,In), piece_struct_name(PieceK,king), print(Sq3), write(:), print(Piece), print(from), print(Sq), nl, fail ). checkpoint(e3,In), on(piece(king,opponent),SqT,In), on(Piece,player,Square,In), path_dist(Piece,player,Square,SqT,Dist,3,In). ==================================================*/
