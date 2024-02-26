;                                           (convert-to-metta-file  tables $_480960 metagame/learning/tables.pl metagame/learning/tables.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================



;
; =============================================================================
;
; Converting square_tables to weighted digraph matrices.
;
; =============================================================================
;
; At top level, certainly good to use a fixed structure,
;
; as always have entries for those squares.
;
; At second level, don't always have entries.
;
; Could actually decide on best representation given
;
; number of args to be mapped, where if small use list
;
; and if large use array.
;
; Accessing routines would have to check which is used,
;
; but that's not hard.


  (= (matrix-to-square-table Nil $A)  
    (set-det)
    (new-square-table $A))
  (= (matrix-to-square-table (Cons  (- $V $E) $D) $A)  
    (matrix-to-square-table $D $A)
    (pairs-to-square-table $E $A2)
    (arg $V $A $A2))


  (= (pairs-to-square-table Nil $A)  
    (set-det)
    (new-square-table $A))
  (= (pairs-to-square-table (Cons  (- $V $W) $D) $A)  
    (pairs-to-square-table $D $A)
    (arg $V $A $W))


  (= (square-table-to-matrix $A $D)  
    (square-table-to-list $A $List)
    (square-tables-to-matrix $List $D))


  (= (square-tables-to-matrix  () ())  True)
  (= (square-tables-to-matrix (Cons  (- $I $A) $ARest) (Cons  (- $I $D) $DRest))  
    (square-table-to-list $A $D)
    (square-tables-to-matrix $ARest $DRest))

;
; =============================================================================
;
; Converting weighted digraphs to logarithmic arrays (not used now)
;
; =============================================================================


  (= (d-to-array Nil $A)  
    (set-det)
    (new-array $A))
  (= (d-to-array (Cons  (- $V $E) $D) $A)  
    (d-to-array $D $A1)
    (pairs-to-array $E $A2)
    (aset $V $A1 $A2 $A))


  (= (pairs-to-array Nil $A)  
    (set-det)
    (new-array $A))
  (= (pairs-to-array (Cons  (- $V $W) $D) $A)  
    (pairs-to-array $D $A1)
    (aset $V $A1 $W $A))


  (= (array-to-d $A $D)  
    (alist $A $List)
    (arrays-to-d $List $D))


  (= (arrays-to-d  () ())  True)
  (= (arrays-to-d (Cons  (- $I $A) $ARest) (Cons  (- $I $D) $DRest))  
    (alist $A $D)
    (arrays-to-d $ARest $DRest))


;
; ================================================================================
;
; make_basic_tables
;
; ================================================================================

;
; Do 'trace savetables' to keep these tables around,
;
; otherwise they get cleaned up.
;
; We use the temp file here to avoid several processes
;
; writing to the same one at once.

  (= compile-basic-tables  
    (with-temp-file basic $File 
      (, 
        (compile-basic-tables-to-file $File) 
        (compile $File))))



  (= (compile-basic-tables-to-file $File)  
    ( (format "~nCompiling Basic Tables to file: ~w~n" 
        (:: $File)) make-basic-tables 
      (with-output-file $File write list-basic-tables)))


  (= list-basic-tables  
    (whenever 
      (basic-table-pred $Pred) 
      (listing $Pred)))



  (= make-basic-tables  
    (assert-square-indices assert-piece-indices assert-prom-square-indices))


  (= (basic-table-pred  (/  opponent_prom_sq 1))  True)
  (= (basic-table-pred  (/  player_prom_sq 1))  True)
  (= (basic-table-pred  (/  prom_square_indices 2))  True)

  (= (basic-table-pred  (/  piece_type_index 2))  True)
  (= (basic-table-pred  (/  piece_type_count 1))  True)
  (= (basic-table-pred  (/  index_to_piece 2))  True)
  (= (basic-table-pred  (/  total_piece_count 1))  True)

  (= (basic-table-pred  (/  index_to_square 2))  True)
  (= (basic-table-pred  (/  total_square_count 1))  True)
  (= (basic-table-pred  (/  board_dim 1))  True)




;
; ================================================================================
;
; SQUARE_TABLE data structure
;
; ================================================================================


  (= (new-square-table $A)  
    (square-table $A))


  (= (square-table-distance $Item1 $Item2 $Table $Distance)  
    (arg $Item1 $Table $Sub)
    (arg $Item2 $Sub $Distance1)
    (interpret-distance $Distance1 $Distance))


  (= (square-matrix-distance $Item1 $Item2 $Table $Distance)  
    (member1-pair 
      (- $Item1 $Sub) $Table)
    (member1-pair 
      (- $Item2 $Distance1) $Sub)
    (interpret-distance $Distance1 $Distance))



;
; Could make clause for default distance when non-reachable.

  (= (interpret-distance $Distance $Distance)  
    (nonvar $Distance))



  (= (square-table-to-list $A $List)  
    (functor $A square-table $N)
    (sqtl 1 $N $A $List))


  (= (sqtl $N1 $N $A Nil)  
    (> $N1 $N)
    (set-det))
  (= (sqtl $N $Max $A (Cons  (- $N $I) $In))  
    (arg $N $A $I)
    (is $N1 
      (+ $N 1))
    (sqtl $N1 $Max $A $In))
	


  (= (square-table $Table)  
    (total-square-count $Count)
    (functor $Table square-table $Count))


  (= (sindex-table-entry $Index $Table $Entry)  
    (arg $Index $Table $Entry))

;
; An sl_table is like a square table where each
;
; element is a structure: List/Data, instead of a normal value.

  (= (slindex-table-entries $Index $Table $List $Data)  
    (arg $Index $Table $Val)
    (nonvar $Val)
    (= $Val 
      (/ $List $Data)))
	
;
; Like the above, but here backtrack over elements of that list.

  (= (slindex-table-entry $Index $Table $Entry $Data)  
    (slindex-table-entries $Index $Table $List $Data)
    (member $Entry $List))
	





  (= (square-table-entry $Square $Index $Table $Entry)  
    (square-index $Square $Index)
    (sindex-table-entry $Index $Table $Entry))
	

  (= (square-indices $Is)  
    (setof $I 
      (^ $P 
        (square-index $P $I)) $Is))


	

;
; ================================================================================
;
; Mapping square tables
;
; ================================================================================

;
; Example:
;
; map_square_table(transition_table,Square,SIndex,[Matrix],Table)
;
; Will call the predicate:
;
; transition_table(Square,SIndex,Matrix,Entry)
;
; With each Entry being the corresponding slot of the Square'th
;
; entry in the final Table.
;
; Here we're saying args must be insensitive to side-effects.
;
; That is, we are using the same copy of the args each time.


  (= (map-square-table $Pred $Table)  
    (map-square-table $Pred Nil $Table))

  (= (map-square-table $Pred $Args $Table)  
    (square-table $Table)
    (square-indices $Indices)
    (map-for-squares $Indices $Pred $Args $Table))


  (= (map-for-squares  () $_ $_ $_)  True)
  (= (map-for-squares (Cons  $S $Ss) $Pred $Args $Table)  
    (square-table-entry $Square $S $Table $Entry)
    (append 
      (Cons  $Pred 
        (Cons  $Square 
          (Cons  $S $Args))) 
      (:: $Entry) $GoalList)
    (=.. $Goal $GoalList)
    (call $Goal)
    (map-for-squares $Ss $Pred $Args $Table))
	



;
; Maps a goal across to corresponding square tables.
;
; maps_square_table(transition_table,Square,PIndex,[Matrix],Entry)
;
; transition_table(Square,PIndex,Matrix,Entry)


  (= (maps-square-table $Pred $Table1 $Table2)  
    (maps-square-table $Pred Nil $Table1 $Table2))

  (= (maps-square-table $Pred $Args $Table1 $Table2)  
    (square-table $Table1)
    (square-table $Table2)
    (square-indices $Indices)
    (maps-for-squares $Indices $Pred $Args $Table1 $Table2))


  (= (maps-for-squares  () $_ $_ $_ $_)  True)
  (= (maps-for-squares (Cons  $S $Ss) $Pred $Args $Table1 $Table2)  
    (square-table-entry $Square $S $Table1 $Entry1)
    (square-table-entry $Square $S $Table2 $Entry2)
    (append 
      (Cons  $Pred 
        (Cons  $Square 
          (Cons  $S $Args))) 
      (:: $Entry1 $Entry2) $GoalList)
    (=.. $Goal $GoalList)
    (call $Goal)
    (maps-for-squares $Ss $Pred $Args $Table1 $Table2))
	

;
; Counts the number of square indices for
;
; which Goal is true.
;
; This is often an easy way to count some function
;
; across the whole board.

  (= (count-bagof-squares $Sq $Goal $Squares)  
    (count-bagof $Sq 
      (^ 
        (, $X $Y) 
        (, 
          (square-to-index $X $Y $Sq) $Goal)) $Squares))



  (= add-portray-square-table  
    ( (new-square-table $T) (add-atom  &self (:-  (portray  $T) (portray-square-table  $T)))))


  (= (portray-square-table $T)  
    (format "<Square Table>" Nil))



;
; ================================================================================
;
; PIECE_TABLE data structure
;
; ================================================================================


  (= (new-piece-table $A)  
    (piece-table $A))



  (= (piece-table $Table)  
    (total-piece-count $Count)
    (functor $Table piece-table $Count))


  (= (pindex-table-entry $Index $Table $Entry)  
    (arg $Index $Table $Entry))


  (= (piece-table-entry $Piece $Index $Table $Entry)  
    (piece-index $Piece $Index)
    (pindex-table-entry $Index $Table $Entry))
	

  (= (piece-matrix-distance $Item1 $Item2 $Table $Distance)  
    (member1-pair 
      (- $Item1 $Sub) $Table)
    (member1-pair 
      (- $Item2 $Distance1) $Sub)
    (interpret-distance $Distance1 $Distance))





  (= (piece-indices $Is)  
    (setof $I 
      (^ $P 
        (piece-index $P $I)) $Is))


  (= add-portray-piece-table  
    ( (new-piece-table $T) (add-atom  &self (:-  (portray  $T) (portray-piece-table  $T)))))


  (= (portray-piece-table $T)  
    (format "<Piece Table>" Nil))


  (= portray-tables  
    (add-portray-piece-table add-portray-square-table))



;
; ================================================================================
;
; Mapping piece tables
;
; ================================================================================

;
; Example:
;
; map_piece_table(transition_table,Piece,PIndex,[Matrix],Table)
;
; Will call the predicate:
;
; transition_table(Piece,PIndex,Matrix,Entry)
;
; With each Entry being the corresponding slot of the Piece'th
;
; entry in the final Table.
;
; Here we're saying args must be insensitive to side-effects.
;
; That is, we are using the same copy of the args each time.


  (= (map-piece-table $Pred $Table)  
    (map-piece-table $Pred Nil $Table))

  (= (map-piece-table $Pred $Args $Table)  
    (piece-table $Table)
    (piece-indices $Indices)
    (map-for-pieces $Indices $Pred $Args $Table))


  (= (map-for-pieces  () $_ $_ $_)  True)
  (= (map-for-pieces (Cons  $P $Ps) $Pred $Args $Table)  
    (piece-table-entry $Piece $P $Table $Entry)
    (append 
      (Cons  $Pred 
        (Cons  $Piece 
          (Cons  $P $Args))) 
      (:: $Entry) $GoalList)
    (=.. $Goal $GoalList)
    (call $Goal)
    (map-for-pieces $Ps $Pred $Args $Table))
	



;
; Maps a goal across to corresponding piece tables.
;
; maps_piece_table(transition_table,Piece,PIndex,[Matrix],Entry)
;
; transition_table(Piece,PIndex,Matrix,Entry)


  (= (maps-piece-table $Pred $Table1 $Table2)  
    (maps-piece-table $Pred Nil $Table1 $Table2))

  (= (maps-piece-table $Pred $Args $Table1 $Table2)  
    (piece-table $Table1)
    (piece-table $Table2)
    (piece-indices $Indices)
    (maps-for-pieces $Indices $Pred $Args $Table1 $Table2))


  (= (maps-for-pieces  () $_ $_ $_ $_)  True)
  (= (maps-for-pieces (Cons  $P $Ps) $Pred $Args $Table1 $Table2)  
    (piece-table-entry $Piece $P $Table1 $Entry1)
    (piece-table-entry $Piece $P $Table2 $Entry2)
    (append 
      (Cons  $Pred 
        (Cons  $Piece 
          (Cons  $P $Args))) 
      (:: $Entry1 $Entry2) $GoalList)
    (=.. $Goal $GoalList)
    (call $Goal)
    (maps-for-pieces $Ps $Pred $Args $Table1 $Table2))

;
; ==============================================================================
;
; Making Square Index Table
;
; ==============================================================================

;
; SQUARE_INDEX(?Square,?Index)
;
; True when square-struct SQUARE has the Index in the table.
;
; This is Bidirectional, and gives indexing for both arguments.
;
; If both args are unbound, will generate all squares and their indices
;
; for the current game.
;
; Square should be a square_struct: square(type,player).
;
; This should only be used after the table has been created with
;
; ASSERT_SQUARE_INDICES.
;
; 

  (= (square-index $Square $Index)  
    (det-if-then-else 
      (var $Index) 
      (square-to-index $Square $Index) 
      (index-to-square $Index $Square)))



  (= (current-board-dim $Dim)  
    (current-board-size $Dim $Y))
; ; current_board_size(_X,Dim).

;
; Makes available the pred: BOARD_DIM/1.

  (= set-board-dim  
    ( (abolish (/ board-dim 1)) 
      (current-board-dim $Dim) 
      (add-atom  &self 
        (board-dim  $Dim))))
	
;
; Makes available the pred: TOTAL_SQUARE_COUNT/1.

  (= set-square-count  
    (set-board-dim 
      (abolish (/ total-square-count 1)) 
      (current-board-size $X $Y) 
      (is $Total 
        (* $X $Y)) 
      (add-atom  &self 
        (total-square-count  $Total))))


;
; Makes a table mapping each index into a different square struct.

  (= assert-square-indices  
    ( (abolish (/ index-to-square 2)) set-square-count 
      (whenever 
        (square-to-index $Square $Key) 
        (assert-square-index $Square $Key)) 
      (tracing-anal index print-square-indices)))


  (= (assert-square-index $Square $Index)  
    (add-atom  &self 
      (index-to-square  $Index $Square)))


  (= print-square-indices  
    (format "Square Index Table:~n" Nil)
    (or 
      (, 
        (index-to-square $I $P) 
        (format "~p --> ~p~n" 
          (:: $I $P)) fail) True))


  (= (square-to-index (square $X $Y) $Index)  
    (board-square $X $Y)
    (board-dim $Dim)
    (is $Index 
      (+ 
        (* $Dim 
          (- $Y 1)) $X)))

  (= (square-to-index $X $Y $Index)  
    (board-square $X $Y)
    (board-dim $Dim)
    (is $Index 
      (+ 
        (* $Dim 
          (- $Y 1)) $X)))


  (= (board-square (square $X $Y))  
    (board-square $X $Y))

;
; Need board_square predicate.
  (= (board-square $X $Y)  
    (current-board-size $XMax $YMax)
    (between 1 $YMax $Y)
    (between 1 $XMax $X))



  (= (piece-index $Piece $Index)  
    (det-if-then-else 
      (var $Index) 
      (piece-to-index $Piece $Index) 
      (index-to-piece $Index $Piece)))
; /* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Piece-Type Index table looks like this: 1 --> firefly 2 --> slug 3 --> termite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Piece Index table looks like this: 1 --> white firefly 2 --> white slug 3 --> white termite 4 --> black firefly 5 --> black slug 6 --> black termite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; */
; ; PIECE_INDEX(?Piece,?Index) ; True when piece-struct PIECE has the Index in the table. ; This is Bidirectional, and gives indexing for both arguments. ; If both args are unbound, will generate all pieces and their indices ; for the current game. ; Piece should be a piece_struct: piece(type,player). ; This should only be used after the table has been created with ; ASSERT_PIECE_INDICES. ;


  (= (current-game-piece-count $Count)  
    (player-current-game $Game)
    (game-piece-count $Count $Game))
	

  (= (game-piece-count $Count $Game)  
    (game-piece-names $Game $Names)
    (length $Names $Count))


  (= (nth-piece-name $N $P)  
    (player-current-game $G)
    (game-piece-names $G $Names)
    (nth $N $Names $P))

;
; Makes a table mapping each index into a different piece struct.

  (= assert-piece-indices  
    (assert-piece-type-indices 
      (abolish (/ index-to-piece 2)) 
      (whenever 
        (piece-to-index $Piece $Key) 
        (assert-piece-index $Piece $Key)) 
      (tracing-anal index print-piece-indices)))


  (= assert-piece-type-indices  
    ( (abolish (/ piece-type-index 2)) set-piece-type-count 
      (whenever 
        (nth-piece-name $N $P) 
        (assert-piece-type-index $P $N))))
		    
;
; Makes available the pred: piece_type_count/1.
;
; Also pred: total_piece_count/1 (just double the above).

  (= set-piece-type-count  
    ( (abolish (/ piece-type-count 1)) 
      (abolish (/ total-piece-count 1)) 
      (current-game-piece-count $Count) 
      (add-atom  &self 
        (piece-type-count  $Count)) 
      (is $Total 
        (* $Count 2)) 
      (add-atom  &self 
        (total-piece-count  $Total))))

	

  (= (assert-piece-type-index $Piece $Index)  
    (add-atom  &self 
      (piece-type-index  $Piece $Index)))
; ; dir_key(Dir,Key),


  (= (assert-piece-index $Piece $Index)  
    (add-atom  &self 
      (index-to-piece  $Index $Piece)))
; ; piece_key(Piece,Index),


  (= print-piece-type-indices  
    (or 
      (, 
        (piece-type-index $P $I) 
        (format "~p --> ~p~n" 
          (:: $I $P)) fail) True))


  (= print-piece-indices  
    (format "Piece Index Table:~n" Nil)
    (or 
      (, 
        (index-to-piece $I $P) 
        (format "~p --> ~p~n" 
          (:: $I $P)) fail) True))



  (= (piece-to-index (piece $Name $Player) $Key)  
    (piece-player-mult $Player $Mult)
    (piece-type-index $Name $Y)
    (piece-type-count $Count)
    (is $Key 
      (+ 
        (* $Count $Mult) $Y)))
	

  (= (piece-player-mult  player 0)  True)
  (= (piece-player-mult  opponent 1)  True)

	

;
; ================================================================================
;
; PLAYER_TABLE data structure
;
; ================================================================================


  (= (total-player-count  2)  True)


  (= (player-index  player 1)  True)
  (= (player-index  opponent 2)  True)


  (= (player-table $Table)  
    (total-player-count $Count)
    (functor $Table player-table $Count))


  (= (player-table-entry $Player $Index $Table $Entry)  
    (player-index $Player $Index)
    (pindex-table-entry $Index $Table $Entry))
	

  (= (player-indices $Is)  
    (setof $I 
      (^ $P 
        (player-index $P $I)) $Is))


;
; ================================================================================
;
; Mapping player tables
;
; ================================================================================

;
; Example:
;
; map_player_table(transition_table,Player,PIndex,[Matrix],Table)
;
; Will call the predicate:
;
; transition_table(Player,PIndex,Matrix,Entry)
;
; With each Entry being the corresponding slot of the Player'th
;
; entry in the final Table.
;
; Here we're saying args must be insensitive to side-effects.
;
; That is, we are using the same copy of the args each time.


  (= (map-player-table $Pred $Table)  
    (map-player-table $Pred Nil $Table))

  (= (map-player-table $Pred $Args $Table)  
    (player-table $Table)
    (player-indices $Indices)
    (map-for-players $Indices $Pred $Args $Table))


  (= (map-for-players  () $_ $_ $_)  True)
  (= (map-for-players (Cons  $P $Ps) $Pred $Args $Table)  
    (player-table-entry $Player $P $Table $Entry)
    (append 
      (Cons  $Pred 
        (Cons  $Player 
          (Cons  $P $Args))) 
      (:: $Entry) $GoalList)
    (=.. $Goal $GoalList)
    (call $Goal)
    (map-for-players $Ps $Pred $Args $Table))
	



;
; Maps a goal across to corresponding player tables.
;
; maps_player_table(transition_table,Player,PIndex,[Matrix],Entry)
;
; transition_table(Player,PIndex,Matrix,Entry)


  (= (maps-player-table $Pred $Table1 $Table2)  
    (maps-player-table $Pred Nil $Table1 $Table2))

  (= (maps-player-table $Pred $Args $Table1 $Table2)  
    (player-table $Table1)
    (player-table $Table2)
    (player-indices $Indices)
    (maps-for-players $Indices $Pred $Args $Table1 $Table2))


  (= (maps-for-players  () $_ $_ $_ $_)  True)
  (= (maps-for-players (Cons  $P $Ps) $Pred $Args $Table1 $Table2)  
    (player-table-entry $Player $P $Table1 $Entry1)
    (player-table-entry $Player $P $Table2 $Entry2)
    (append 
      (Cons  $Pred 
        (Cons  $Player 
          (Cons  $P $Args))) 
      (:: $Entry1 $Entry2) $GoalList)
    (=.. $Goal $GoalList)
    (call $Goal)
    (maps-for-players $Ps $Pred $Args $Table1 $Table2))


;
; ==============================================================================
;
; Making Promotion Square Index Table
;
; ==============================================================================


  (= assert-prom-square-indices  
    (abolish (/ prom-square-indices 2))
    (assert-prom-square-indices player)
    (assert-prom-square-indices opponent))
	
  (= (assert-prom-square-indices $Player)  
    ( (find-prom-square-indices $Player $SqIs) 
      (add-atom  &self 
        (prom-square-indices  $Player $SqIs)) 
      (assert-player-prom-sqs $Player $SqIs)))


  (= (assert-player-prom-sqs player $SqIs)  
    (abolish (/ player-prom-sq 1))
    (whenever 
      (member $Sq $SqIs) 
      (add-atom  &self 
        (player-prom-sq  $Sq))))
  (= (assert-player-prom-sqs opponent $SqIs)  
    (abolish (/ opponent-prom-sq 1))
    (whenever 
      (member $Sq $SqIs) 
      (add-atom  &self 
        (opponent-prom-sq  $Sq))))


  (= (player-prom-pred  player $Sq (player-prom-sq  $Sq))  True)
  (= (player-prom-pred  opponent $Sq (opponent-prom-sq  $Sq))  True)
	


  (= (find-prom-square-indices $Player $SqIs)  
    (prom-sqs $Player $_ $SqIs))

;
; PLAYER_PROMOTION_SQUARE(?Player,?PlI,?Sq,?SqI)
;
; True if Sq is a square in promotion region for Player,
;
; both with their respective indices.
;
; This uses the constructed table and is thus very efficient.

  (= (player-promotion-square $Player $PlI $Sq $SqI)  
    (player-index $Player $PlI)
    (square-index $Sq $SqI)
    (prom-square-for-player $Player $SqI))

  (= (player-promotion-square $Player $Sq)  
    (player-promotion-square $Player $PlI $Sq $SqI))


  (= (prom-square-for-player player $Sq)  
    (player-prom-sq $Sq))
  (= (prom-square-for-player opponent $Sq)  
    (opponent-prom-sq $Sq))
	

  (= (in-promote-region $Sq $Player)  
    (player-promotion-square $Player $Sq))


;
; Could make faster by tabulating.

  (= (prom-sqs $Player $Sqs $SqIs)  
    (setof 
      (- $Sq $SqI) 
      (prom-sq $Player $Sq $SqI) $Pairs)
    (pair-list $Sqs $SqIs $Pairs))
	 


  (= (prom-sq $Player $Sq $SqI)  
    (square-index $Sq $SqI)
    (prom-sq $Player $Sq))

  (= (prom-sq $Player $Sq)  
    (current-game-for-player $Player $Game)
    (game-promote-rank $Game $Rank)
    (invert $Sq $Player $Sq1)
    (square $Sq1 $X $Y)
    (>= $Y $Rank))
	



; /* ; Random: Time is 0.367 sec. in_promote_region(Sq,Player,_) :- in_promote_region(Sq,Player). */
