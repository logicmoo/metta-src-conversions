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
    (build-transition-matrix) 
    ( (new-empty-state $State) (build-transition-matrix $Matrix $State)))
; 



  (= 
    (build-transition-matrix $Matrix $State) 
    (map-piece-table transition-matrix 
      (:: $State) $Matrix))
; 



  (= 
    (print-transition-matrix) 
    (print-transition-matrix $Sq $Piece $SqT))
; 


  (= 
    (print-transition-matrix $Sq $Piece $SqT) 
    ( (format "Piece Square Transition Matrix:~n" Nil) (or (, (square-piece-transition $Sq $Piece $SqT) (format "~p: ~p -> ~p~n" (:: $Piece $Sq $SqT)) (fail)) True)))
; 




  (= 
    (spt $S $P $V) 
    (square-piece-transition $S $P $V))
; 


;
; 

;
; 

;
; 


  (= 
    (square-piece-transition $Sq $Piece $SqT) 
    ( (advice-tables $Tables) (square-piece-transition $Sq $Piece $SqT $Tables)))
; 


;
; 

;
; 

;
; 

  (= 
    (square-piece-transition $Sq $Piece $SqT $Tables) 
    (square-piece-transition $Sq $SqI $Piece $PieceI $SqT $SqTI $Tables))
; 


  (= 
    (square-piece-transition $Sq $SqI $Piece $PieceI $SqT $SqTI $Tables) 
    ( (piece-index $Piece $PieceI) 
      (square-index $Sq $SqI) 
      (square-piece-sq $SqI $PieceI $SqTI $Tables) 
      (square-index $SqT $SqTI)))
; 



  (= 
    (square-piece-sq $SqI $PieceI $SqTI $Tables) 
    ( (transition-matrix $Tables $M) (square-piece-sq1 $SqI $PieceI $SqTI $M)))
; 



  (= 
    (square-piece-sq1 $SqI $PieceI $SqTI $M) 
    ( (piece-table-entry $_ $PieceI $M $Entry) 
      (member1-pair 
        (- $SqI $Ts) $Entry) 
      (member $SqTI $Ts)))
; 



  (= 
    (square-piece-sqs $SqI $PieceI $TIs) 
    ( (transition-matrix $M) (square-piece-sqs $SqI $PieceI $TIs $M)))
; 


  (= 
    (square-piece-sqs $SqI $PieceI $TIs $M) 
    ( (piece-table-entry $_ $PieceI $M $Entry) (member1-pair (- $SqI $TIs) $Entry)))
; 



;
; 

;
; 


  (= 
    (transition-matrix $Piece $PieceIndex $State $Matrix) 
    ( (piece-index $Piece $PieceIndex) 
      (setof 
        (- $SqIndex $SqTIndices) 
        (^ $Sq 
          (safe-transitions-type $Piece $PieceIndex $Sq $SqIndex moving $SqTIndices $State)) $Matrix) 
      (tracing-anal-format tables "Built transition matrix for <~p>~n" 
        (:: $Piece))))
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
    (build-mobility-matrix $Matrix) 
    ( (transition-matrix $Trans) (build-mobility-matrix $Trans $Matrix)))
; 


;
; 

  (= 
    (build-mobility-matrix $Trans $Matrix) 
    (map-piece-table mobility-matrix 
      (:: $Trans) $Matrix))
; 



  (= 
    (mobility-matrix $Piece $PieceIndex $Matrix) 
    ( (transition-matrix $Trans) (mobility-matrix $Piece $PieceIndex $Trans $Matrix)))
; 

	
  (= 
    (mobility-matrix $Piece $PieceIndex $Trans $Matrix) 
    ( (piece-index $Piece $PieceIndex) 
      (map-square-table square-p-mob 
        (:: $Piece $PieceIndex $Trans) $Matrix) 
      (tracing-anal-format tables "Built mobility table for <~p>~n" 
        (:: $Piece))))
; 



  (= 
    (square-p-mob $Sq $SqIndex $Piece $PieceIndex $Trans $Value) 
    (piece-square-mob $Piece $PieceIndex $Sq $SqIndex $Trans $Value))
; 



  (= 
    (square-piece-mob $SqI $PieceI $Val) 
    ( (advice-tables $Tables) (square-piece-mob $SqI $PieceI $Val $Tables)))
; 


  (= 
    (square-piece-mob $SqI $PieceI $Val $Tables) 
    ( (mobility-matrix $Tables $M) 
      (pindex-table-entry $PieceI $M $Entry) 
      (sindex-table-entry $SqI $Entry $Val)))
; 



  (= 
    (spm $S $P $V) 
    (square-piece-mobility $S $P $V))
; 


;
; 

;
; 

;
; 


  (= 
    (square-piece-mobility $Sq $Piece $Value) 
    ( (advice-tables $Tables) (square-piece-mobility $Sq $Piece $Value $Tables)))
; 


;
; 

;
; 

;
; 

  (= 
    (square-piece-mobility $Sq $Piece $Value $Tables) 
    ( (piece-index $Piece $PieceI) 
      (square-index $Sq $SqI) 
      (square-piece-mob $SqI $PieceI $Value $Tables)))
; 




  (= 
    (piece-square-mob $Piece $Sq $Value) 
    ( (transition-matrix $TransMatrix) (piece-square-mob $Piece $Sq $TransMatrix $Value)))
; 


  (= 
    (piece-square-mob $Piece $Sq $TransMatrix $Value) 
    (piece-square-mob $Piece $PieceIndex $Sq $SquareIndex $TransMatrix $Value))
; 

	  
  (= 
    (piece-square-mob $Piece $PieceIndex $Sq $SquareIndex $TransMatrix $Value) 
    ( (piece-index $Piece $PieceIndex) 
      (trace-timing 
        (anal mob-count) 
        (mob-count $PieceIndex $SquareIndex $TransMatrix $Value)) 
      (tracing-anal-format detailed "~p: ~p -> ~p~n" 
        (:: $Piece $Sq $Value))))
; 



  (= 
    (mob-count $Piece $Sq $Trans $Value) 
    ( (square-piece-sqs $Sq $Piece $Moves $Trans) (length $Moves $Value)))
; 


;
; 

;
; 


  (= 
    (print-mobility-matrix) 
    (print-mobility-matrix $_ $_))
; 


  (= 
    (print-mobility-matrix $Sq $Piece) 
    ( (format "Square Piece Mobility Matrix:~n" Nil) (or (, (square-piece-mobility $Sq $Piece $Value) (format "matrix[~p][~p] = ~p~n" (:: $Sq $Piece $Value)) (fail)) True)))
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
    (build-eventual-matrix $Matrix) 
    ( (distance-matrix $Dist) (build-eventual-matrix $Dist $Matrix)))
; 


;
; 

  (= 
    (build-eventual-matrix $Dist $Matrix) 
    (map-piece-table eventual-matrix 
      (:: $Dist) $Matrix))
; 



  (= 
    (eventual-matrix $Piece $PieceIndex $Matrix) 
    ( (distance-matrix $Dist) (eventual-matrix $Piece $PieceIndex $Dist $Matrix)))
; 

	
;
; 

  (= 
    (eventual-matrix $Piece $PieceIndex $DistMatrix $Matrix) 
    ( (= $Distance 4) 
      (piece-indist-matrix $Piece $PieceIndex $Distance $DistMatrix $Matrix) 
      (tracing-anal-format tables "Built eventual table for <~p>~n" 
        (:: $Piece))))
; 



;
; 

;
; 

;
; 


  (= 
    (square-piece-reachability $Sq $Piece $Value) 
    ( (advice-tables $Tables) (square-piece-reachability $Sq $Piece $Value $Tables)))
; 


;
; 

;
; 

;
; 

  (= 
    (square-piece-reachability $Sq $Piece $Value $Tables) 
    ( (piece-index $Piece $PieceI) 
      (square-index $Sq $SqI) 
      (square-piece-reach $SqI $PieceI $Value $Tables)))
; 




  (= 
    (square-piece-reach $SqI $PieceI $Val $Tables) 
    ( (eventual-matrix $Tables $M) 
      (pindex-table-entry $PieceI $M $Entry) 
      (sindex-table-entry $SqI $Entry $Val)))
; 



;
; 

;
; 


  (= 
    (print-eventual-matrix) 
    (print-eventual-matrix $_ $_))
; 


  (= 
    (print-eventual-matrix $Sq $Piece) 
    ( (format "Square Piece Eventual Matrix:~n" Nil) (or (, (square-piece-reachability $Sq $Piece $Value) (format "matrix[~p][~p] = ~p~n" (:: $Sq $Piece $Value)) (fail)) True)))
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
    (build-distance-matrix $Trans $Matrix) 
    (map-piece-table distance-matrix 
      (:: $Trans) $Matrix))
; 




  (= 
    (build-distance-table) 
    ( (distance-matrix $M) (build-distance-table $M $_)))
; 



;
; 

  (= 
    (build-distance-table $Trans $Matrix) 
    (map-piece-table distance-table 
      (:: $Trans) $Matrix))
; 




  (= 
    (distance-matrix $Piece $PieceIndex $Matrix) 
    ( (transition-matrix $Trans) (distance-matrix $Piece $PieceIndex $Trans $Matrix)))
; 


  (= 
    (distance-matrix $Piece $PieceIndex $Trans $Matrix) 
    ( (piece-table-entry $Piece $PieceIndex $Trans $Matrix1) 
      (s-floyd $Matrix1 $Matrix) 
      (tracing-anal-format tables "Built distance matrix for <~p>~n" 
        (:: $Piece))))
; 




  (= 
    (distance-table $Piece $PieceIndex $Table) 
    ( (distance-matrix $Matrix) (distance-table $Piece $PieceIndex $Matrix $Table)))
; 


  (= 
    (distance-table $Piece $PieceIndex $Dist $Table) 
    ( (piece-table-entry $Piece $PieceIndex $Dist $Matrix) 
      (matrix-to-square-table $Matrix $Table) 
      (tracing-anal-format tables "Built distance table for <~p>~n" 
        (:: $Piece))))
; 





  (= 
    (square-piece-sq-dist $SqI $PieceI $SqTI $Dist $Tables) 
    ( (distance-table $Tables $Table) (square-piece-sq-dist1 $SqI $PieceI $SqTI $Table $Dist)))
; 

	
;
; 


  (= 
    (square-piece-sq-dist1 $SqI $PieceI $SqTI $Table $Dist) 
    ( (piece-table-entry $_ $PieceI $Table $Entry) 
      (square-table-distance $SqI $SqTI $Entry $Dist) 
      (set-det)))
; 
	

;
; 


  (= 
    (square-piece-sq-dist-max $SqI $PieceI $SqTI $DTable $Dist) 
    (det-if-then-else 
      (square-piece-sq-dist1 $SqI $PieceI $SqTI $DTable $Dist) True 
      (= $Dist 10000)))
; 



  (= 
    (square-piece-list-dist $SqI $PieceI $SqTI $Dist) 
    ( (distance-matrix $Matrix) (square-piece-list-dist $SqI $PieceI $SqTI $Matrix $Dist)))
; 

	
  (= 
    (square-piece-list-dist $SqI $PieceI $SqTI $Table $Dist) 
    ( (piece-table-entry $_ $PieceI $Table $Entry) (square-matrix-distance $SqI $SqTI $Entry $Dist)))
; 
	


  (= 
    (square-piece-list-distance $SqI $PieceI $SqTI $Dist) 
    ( (distance-matrix $Matrix) (square-piece-list-distance $SqI $PieceI $SqTI $Matrix $Dist)))
; 

	
  (= 
    (square-piece-list-distance $SqI $PieceI $SqTI $Table $Dist) 
    ( (piece-table-entry $_ $PieceI $Table $Entry) (square-matrix-distance $SqI $SqTI $Entry $Dist)))
; 
	




  (= 
    (print-distance-matrix) 
    (print-distance-matrix $_ $_ $_))
; 



  (= 
    (print-distance-matrix $Sq $Piece $SqT) 
    ( (format "Square Piece Distance Matrix:~n" Nil) (or (, (square-piece-list-distance $Sq $Piece $SqT $Dist) (format "~p: ~p -> ~p <~p>~n" (:: $Piece $Sq $SqT $Dist)) (fail)) True)))
; 




  (= 
    (print-distance-table) 
    (print-distance-table $_ $_ $_))
; 



  (= 
    (print-distance-table $Sq $Piece $SqT) 
    ( (format "Square Piece Distance Table:~n" Nil) (or (, (square-piece-distance $Sq $Piece $SqT $Dist) (format "~p: ~p -> ~p <~p>~n" (:: $Piece $Sq $SqT $Dist)) (fail)) True)))
; 




  (= 
    (piece-distance-table $Piece $Table) 
    ( (distance-table $D) (piece-table-entry $Piece $_ $D $Table)))
; 




  (= 
    (spd $S $P $SqT $V) 
    (square-piece-distance $S $P $SqT $V))
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
    (square-piece-distance $Sq $Piece $SqT $Dist) 
    ( (advice-tables $Tables) (square-piece-distance $Sq $Piece $SqT $Dist $Tables)))
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
    (square-piece-distance $Sq $Piece $SqT $Dist $Tables) 
    (square-piece-distance $Sq $SqI $Piece $PieceI $SqT $SqTI $Dist $Tables))
; 


  (= 
    (square-piece-distance $Sq $SqI $Piece $PieceI $SqT $SqTI $Dist $Tables) 
    ( (piece-index $Piece $PieceI) 
      (square-index $Sq $SqI) 
      (square-index $SqT $SqTI) 
      (square-piece-sq-dist $SqI $PieceI $SqTI $Dist $Tables)))
; 




;
; 

;
; 

;
; 



  (= 
    (square-piece-reaches $Sq $Piece $Squares) 
    ( (transition-matrix $Trans) 
      (piece-table-entry $Piece $_ $Trans $PTrans) 
      (square-piece-reaches $Sq $SqIndex $PTrans $Squares)))
; 


  (= 
    (square-piece-reaches $Sq $SqIndex $Trans $Squares) 
    ( (square-index $Sq $SqIndex) (sq-piece-reaches $SqIndex $Trans $Squares)))
; 



  (= 
    (sq-piece-reaches $Sq $Trans $Squares) 
    (reachable $Sq $Trans $Squares))
; 




  (= 
    (indist-set $Piece $Sq $SqT $Dist $Set) 
    (setof $SqT 
      (indist $Piece $Sq $SqT $Dist) $Set))
; 



  (= 
    (indist-set2 $Piece $Sq $SqT $Dist $Set) 
    (setof $SqT 
      (indist $Piece $Sq $SqT $Dist) $Set))
; 




  (= 
    (indist $Piece $Sq $SqT $Dist) 
    ( (distance-table $Table) (piece-square-within-distance $Piece $_ $Sq $SqI $SqT $SqTI $Dist $Table)))
; 

	


  (= 
    (piece-square-at-distance $Piece $PieceI $Sq $SqI $SqT $SqTI $Dist $Table) 
    ( (piece-table-entry $Piece $PieceI $Table $T) 
      (square-index $Sq $SqI) 
      (square-index $SqT $SqTI) 
      (square-table-distance $SqI $SqTI $T $D) 
      (=< $D $Dist)))
; 

	
;
; 


  (= 
    (piece-dist-squares-matrix $P $PI $Sq $SqI $Squares) 
    ( (distance-matrix $D) (piece-dist-squares-matrix $P $PI $Sq $SqI $D $Squares)))
; 



  (= 
    (piece-dist-squares-matrix $P $PI $Sq $SqI $DistMatrix $Squares) 
    ( (piece-table-entry $P $PI $DistMatrix $Matrix) 
      (member1-pair 
        (- $SqI $Dist) $Matrix) 
      (p-transpose $Dist $New) 
      (p-to-s-graph $New $Squares)))
; 



  (= 
    (piece-dist-squares $P $PI $Sq $SqI $Distance $Squares) 
    ( (distance-matrix $DistMatrix) (piece-dist-squares $P $PI $Sq $SqI $Distance $DistMatrix $Squares)))
; 


  (= 
    (piece-dist-squares $P $PI $Sq $SqI $Distance $DistMatrix $Squares) 
    ( (piece-dist-squares-matrix $P $PI $Sq $SqI $DistMatrix $Matrix) (member1-pair (- $Distance $Squares) $Matrix)))
; 



  (= 
    (piece-dist-count $P $Sq $Distance $Count) 
    ( (piece-index $P $PI) 
      (square-index $Sq $SqI) 
      (piece-dist-count $P $PI $Sq $SqI $Distance $Count)))
; 


  (= 
    (piece-dist-count $P $PI $Sq $SqI $Distance $Count) 
    ( (distance-matrix $DistMatrix) (piece-dist-count $P $PI $Sq $SqI $Distance $DistMatrix $Count)))
; 


  (= 
    (piece-dist-count $P $PI $Sq $SqI $Distance $DistMatrix $Count) 
    ( (piece-dist-squares $P $PI $Sq $SqI $Distance $DistMatrix $Squares) (length $Squares $Count)))
; 




  (= 
    (piece-indist-sum $Piece $PieceI $Sq $SqI $Distance $Count) 
    ( (distance-matrix $DistMatrix) (piece-indist-sum $Piece $PieceI $Sq $SqI $Distance $DistMatrix $Count)))
; 


  (= 
    (piece-indist-sum $Piece $PieceI $Sq $SqI $Distance $DistMatrix $Count) 
    ( (bagof $Count1 
        (piece-indist-count $Piece $PieceI $Sq $SqI $Distance $DistMatrix $Count1) $Counts) 
      (square-index $Sq $SqI) 
      (sumlist $Counts $Count)))
; 

	    

  (= 
    (piece-indist-count $Piece $PieceI $Sq $SqI $Distance $Count) 
    ( (distance-matrix $DistMatrix) (piece-indist-count $Piece $PieceI $Sq $SqI $Distance $DistMatrix $Count)))
; 


  (= 
    (piece-indist-count $Piece $PieceI $Sq $SqI $Distance $DistMatrix $Count) 
    ( (piece-dist-count $Piece $PieceI $Sq $SqI $Dist1 $DistMatrix $Count) (=< $Dist1 $Distance)))
; 




  (= 
    (piece-indist-crunchsum $P $PI $Sq $SqI $Distance $Count) 
    ( (distance-matrix $DistMatrix) (piece-indist-crunchsum $P $PI $Sq $SqI $Distance $DistMatrix $Count)))
; 


  (= 
    (piece-indist-crunchsum $P $PI $Sq $SqI $Distance $DistMatrix $Count) 
    ( (piece-dist-squares-matrix $P $PI $Sq $SqI $DistMatrix $Squares) 
      (crunch-ds $Squares $S) 
      (count-less $S $Distance $Count)))
; 





  (= 
    (piece-discounted-sum $P $PI $Sq $SqI $Distance $Count) 
    ( (distance-matrix $DistMatrix) (piece-discounted-sum $P $PI $Sq $SqI $Distance $DistMatrix $Count)))
; 


  (= 
    (piece-discounted-sum $P $PI $Sq $SqI $Distance $DistMatrix $Count) 
    ( (piece-dist-squares-matrix $P $PI $Sq $SqI $DistMatrix $Squares) 
      (crunch-ds $Squares $Series) 
      (discounted-sum $Series $Distance $Count)))
; 





  (= 
    (piece-indist-matrix $P $PI $Distance $Matrix) 
    ( (distance-matrix $DistMatrix) (piece-indist-matrix $P $PI $Distance $DistMatrix $Matrix)))
; 


  (= 
    (piece-indist-matrix $P $PI $Distance $DistMatrix $Matrix) 
    ( (piece-index $P $PI) (map-square-table sq-piece-indist-crunchsum (:: $P $PI $Distance $DistMatrix) $Matrix)))
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
    (sq-piece-indist-crunchsum $Sq $SqI $P $PI $Distance $DistMatrix $Count) 
    (piece-discounted-sum $P $PI $_ $SqI $Distance $DistMatrix $Count))
; 






  (= 
    (crunch-invert $SqDs $DSqs) 
    ( (p-transpose $SqDs $New) (p-to-s-graph $New $DSqs)))
; 




  (= 
    (crunchtop  () ()) True)
; 

  (= 
    (crunchtop 
      (Cons  
        (- $Sq $Sqs) $G) 
      (Cons  
        (- $Sq $Ds) $GRest)) 
    ( (crunch-invert $Sqs $DSqs) 
      (crunch-ds $DSqs $Ds) 
      (crunchtop $G $GRest)))
; 


;
; 

;
; 

;
; 


  (= 
    (crunch_ds  () ()) True)
; 

  (= 
    (crunch-ds 
      (Cons  
        (- $D $Sqs) $G) 
      (Cons  
        (- $D $Count) $Rest)) 
    ( (length $Sqs $Count) (crunch-ds $G $Rest)))
; 



  (= 
    (count_less  () $_ 0) True)
; 

  (= 
    (count-less 
      (Cons  
        (- $Dist $_) $_) $Max 0) 
    ( (< $Max $Dist) (set-det)))
; 

  (= 
    (count-less 
      (Cons  
        (- $Dist $C1) $As) $Max $Count) 
    ( (count-less $As $Max $CRest) (is $Count (+ $C1 $CRest))))
; 


;
; 


  (= 
    (discounted-sum $Series $Discount $Sum) 
    (discounted-sum $Series $Discount 0 $Sum))
; 


  (= 
    (discounted_sum  () $_ $Count $Count) True)
; 

  (= 
    (discounted-sum 
      (Cons  
        (- $Dist $C1) $Rest) $Discount $Sum1 $Sum) 
    ( (discount-value $Dist $C1 $Discount $Val) 
      (is $Sum2 
        (+ $Sum1 $Val)) 
      (discounted-sum $Rest $Discount $Sum2 $Sum)))
; 



  (= 
    (discount-value $Distance $Count $Discount $Val) 
    ( (distance-value $Distance $V) (is $Val (* $Count $V))))
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
    (rev-transition-matrix $Piece $PieceIndex $Matrix $State) 
    ( (transition-matrix $Piece $PieceIndex $Matrix1 $State) (s-transpose $Matrix1 $Matrix)))
; 



  (= 
    (rev-distance-matrix $Piece $PieceIndex $Matrix $State) 
    ( (rev-transition-matrix $Piece $PieceIndex $Matrix1 $State) (s-floyd $Matrix1 $Matrix)))
; 



  (= 
    (rev-distance-table $Piece $PieceIndex $Table $State) 
    ( (rev-distance-matrix $Piece $PieceIndex $Matrix $State) (d-to-array $Matrix $Table)))
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



  !(my-ensure-loaded (library tracing))
; 



  (= 
    (tracing-anal $Type $Call) 
    (det-if-then-else 
      (tracing (anal $Type)) 
      (call $Call) True))
; 


;
; 


  (= 
    (tracing-anal-format $Type $String $Args) 
    (det-if-then-else 
      (tracing (anal $Type)) 
      (format $String $Args) True))
; 



  (= 
    (tracing-anal-timing $Type $Call) 
    (trace-timing 
      (anal $Type) $Call))
; 



  (= 
    (set-anal-verbosity $Level $Status) 
    (set-tracing 
      (anal $Level) $Status))
; 



  (= 
    (silent-anal) 
    (all-anal off))
; 


  (= 
    (loud-anal) 
    (all-anal on))
; 



  (= 
    (all-anal $Status) 
    ( (set-anal-verbosity index $Status) 
      (set-anal-verbosity simplify $Status) 
      (set-anal-verbosity subsume $Status) 
      (set-anal-verbosity pieces $Status)))
; 
 


  (= 
    (trace-anal-tables) 
    (set-anal-verbosity tables on))
; 
 

  (= 
    (trace-anal-index) 
    (set-anal-verbosity index on))
; 
 

  (= 
    (trace-anal-subsume) 
    (set-anal-verbosity subsume on))
; 
 

  (= 
    (trace-anal-simplify) 
    (set-anal-verbosity simplify on))
; 
 

  (= 
    (trace-anal-pieces) 
    (set-anal-verbosity pieces on))
; 
 


  !(trace-anal-tables *)
; 

;
; 






