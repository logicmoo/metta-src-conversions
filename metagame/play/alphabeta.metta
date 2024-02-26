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

;
; 

;
; 

;
; 



  (= 
    (alpha-beta-choose $Player $Move $SIn $SOut) 
    (toggle-weights-choose alpha-beta-move $Player $Move $SIn $SOut))
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
    (iterate-choose $Player $Move $SIn $SOut) 
    (toggle-weights-choose iterate-move $Player $Move $SIn $SOut))
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
    (iterate-random-choose $Player $Move $SIn $SOut) 
    (toggle-weights-choose iterate-random-move $Player $Move $SIn $SOut))
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
    (iterate-fixed-choose $Player $Move $SIn $SOut) 
    (toggle-weights-choose iterate-fixed-move $Player $Move $SIn $SOut))
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
    (toggle-weights-choose $Type $Player $Move $SIn $SOut) 
    ( (=.. $Goal 
        (:: $Type $Move $SIn $SOut)) (det-if-then-else (toggle-alpha-beta-weights $Player $Old) (det-if-then-else (timing $Goal) (, (print-choice $Move $SIn $SOut) (set-parameter weights $Old)) (, (set-parameter weights $Old) (fail))) (det-if-then (timing $Goal) (print-choice $Move $SIn $SOut)))))
; 



;
; 

;
; 

;
; 



  (= 
    (ab-depth $D) 
    (parameter depth $D))
; 


;
; 


  (= 
    (bound $B) 
    (parameter bound $B))
; 



  (= 
    (move-time-limit $T) 
    (parameter move-time-limit $T))
; 



  (= 
    (move-horizon $T) 
    (parameter move-horizon $T))
; 


;
; 


  (= 
    (approx-window $A $B) 
    ( (value-of-outcome player $B) (value-of-outcome opponent $A)))
; 



  (= 
    (reset-alpha-params) 
    ( (set-parameter depth 1) 
      (set-parameter move-time-limit 10000) 
      (set-parameter move-horizon 1) 
      (set-parameter ordering random)))
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
    (search-timeout-time $StartTime $SIn $EndTime) 
    ( (control $P $SIn) 
      (game-time-left $P $GameLeft) 
      (move-time-limit $MoveLimit) 
      (estimated-moves-remaining $MovesLeft $SIn) 
      (search-timeout-time $StartTime $GameLeft $MovesLeft $MoveLimit $EndTime)))
; 


  (= 
    (search-timeout-time $StartTime $GameLeft $MovesLeft $MoveLimit $EndTime) 
    ( (is $Avg 
        (/ $GameLeft $MovesLeft)) 
      (is $TimeAvail 
        (min $MoveLimit $Avg)) 
      (is $EndTime 
        (+ $StartTime $TimeAvail))))
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
    (estimated-moves-remaining $Horizon $_) 
    (move-horizon $Horizon))
; 



;
; 

;
; 


  (= 
    (timeout-for-node $Node $End) 
    ( (tracing-ab timing 
        (print-timeout-check $Node)) 
      (number $End) 
      (current-time $Time) 
      (>= $Time $End) 
      (is $Diff 
        (- $Time $End)) 
      (print-timeout-message $Diff)))
; 




;
; 

;
; 

  (= 
    (timeout-for-node 
      (Cons  $Node $Rest) $BestNode $End) 
    (timeout-for-node $Node $End))
; 




  (= 
    (print-timeout-check $Node) 
    ( (format "Testing for timeout~n" Nil) 
      (node-move $Node $Move) 
      (print-move $Move)))
; 




  (= 
    (print-timeout-message $Diff) 
    (tracing-ab-format timing "Out of Time by <~p> msec~n" 
      (:: $Diff)))
; 
 



  (= 
    (current-time $T0) 
    (statistics runtime 
      (Cons  $T0 $_)))
; 
 

;
; 


  (= 
    (search-timeout-time $EndTime) 
    ( (move-time-limit $Limit) 
      (current-time $T0) 
      (is $EndTime 
        (+ $T0 $Limit))))
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
    (iterate-fixed-move $Move $SIn $SOut) 
    ( (change-parameter ordering $Ord fixed) 
      (iterate-move $Move $SIn $SOut) 
      (change-parameter ordering $_ $Ord)))
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
    (iterate-random-move $Move $SIn $SOut) 
    ( (change-parameter ordering $Ord random) 
      (iterate-move $Move $SIn $SOut) 
      (change-parameter ordering $_ $Ord)))
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
    (iterate-move $Move $SIn $SOut) 
    ( (current-time $StartTime) 
      (search-timeout-time $StartTime $SIn $EndTime) 
      (initialized-start-node $SIn $NodeIn) 
      (instant-move $M $SIn $SFirst) 
      (det-if-then-else 
        (forced-move $M $SIn) 
        (accept-forced-move $M $SFirst $Move $SOut) 
        (det-if-then-else 
          (, 
            (timing (iterate 1 $StartTime $EndTime $NodeIn $NodeOut)) 
            (nonvar $NodeOut)) 
          (initialized-choice-node $SOut $NodeOut $Move) 
          (accept-rushed-move $M $SFirst $Move $SOut)))))
; 


;
; 

;
; 


  (= 
    (accept-forced-move $M $S $M $S) 
    (print-forced-notice))
; 
   ;
; 



  (= 
    (accept-rushed-move $M $S $M $S) 
    (print-rushed-notice))
; 
   ;
; 


;
; 


  (= 
    (forced-move $Move $SIn) 
    (not (, (legal $Move2 $SIn $S2) (not (= $Move $Move2)))))
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
    (iterate $Depth $StartTime $EndTime $NodeIn $NodeOut) 
    ( (find-advice-tables $Tables) (iterate $Depth $StartTime $EndTime $NodeIn $NodeOut $Tables)))
; 



  (= 
    (iterate $Depth $StartTime $EndTime $NodeIn $NodeOut $Tables) 
    ( (approx-window $Alpha $Beta) 
      (tracing-ab-timing iteration 
        (alphabeta $Depth $Counts $EndTime $NodeIn $Alpha $Beta $Node1 $Val $_ $Tables)) 
      (tracing-ab-format value "Searching at depth <~p>: ~n" 
        (:: $Depth)) 
      (tracing-ab-format value "The best move found has a value of: ~p~n" 
        (:: $Val)) 
      (tracing-ab resources 
        (print-resource-consumption $Counts)) 
      (tracing-ab value 
        (print-pc-info $NodeIn)) 
      (iterate-deeper $Depth $StartTime $EndTime $Val $NodeIn $Node1 $NodeOut $Tables)))
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
    (iterate-deeper $Depth $StartTime $EndTime $Val $NodeIn $Node1 $Node1 $Tables) 
    ( (win-detected $Val $Depth) (set-det)))
; 

  (= 
    (iterate-deeper $Depth $StartTime $EndTime $_ $NodeIn $Node1 $Node1 $Tables) 
    ( (not-enough-time $Depth $StartTime $EndTime) (set-det)))
; 

  (= 
    (iterate-deeper $Depth $StartTime $EndTime $_ $NodeIn $_ $NodeOut $Tables) 
    ( (node-cont $NodeIn $PC) 
      (node-state $NodeIn $SIn) 
      (pc-moves $PC $Moves) 
      (initialized-start-node $SIn $Node1) 
      (node-pc $Node1 $Moves) 
      (is $Depth1 
        (+ $Depth 1)) 
      (iterate $Depth1 $StartTime $EndTime $Node1 $NodeOut $Tables)))
; 



  (= 
    (win-detected $Val $Depth) 
    ( (var $Val) 
      (set-det) 
      (tracing-ab-format value "No useful info this iteration.~n" Nil)))
; 
 
  (= 
    (win-detected $Val $Depth) 
    ( (player-role $Player) 
      (value-of-outcome $Player $Val) 
      (set-det) 
      (tracing-ab-format value "A win is detected for <~p> in <~p> ply!~n" 
        (:: $Player $Depth))))
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
    (not-enough-time $Depth $StartTime $EndTime) 
    ( (current-time $Now) (>= $Now $EndTime)))
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
    (alpha-beta-move $Move $SIn $SOut) 
    ( (ab-depth $Depth) (alpha-beta-move $Depth $Move $SIn $SOut)))
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
    (alpha-beta-move $Depth $Move $SIn $SOut) 
    ( (current-time $StartTime) 
      (search-timeout-time $StartTime $SIn $EndTime) 
      (initialized-start-node $SIn $NodeIn) 
      (instant-move $M $SIn $SFirst) 
      (det-if-then-else 
        (forced-move $M $SIn) 
        (accept-forced-move $M $SFirst $Move $SOut) 
        (det-if-then-else 
          (, 
            (timing (alpha-beta-iterate $Depth $StartTime $EndTime $NodeIn $NodeOut)) 
            (nonvar $NodeOut)) 
          (initialized-choice-node $SOut $NodeOut $Move) 
          (accept-rushed-move $M $SFirst $Move $SOut)))))
; 




  (= 
    (alpha-beta-iterate $Depth $StartTime $EndTime $NodeIn $NodeOut) 
    ( (approx-window $Alpha $Beta) 
      (tracing-ab-timing iteration 
        (alphabeta $Depth $Counts $EndTime $NodeIn $Alpha $Beta $NodeOut $Val $_)) 
      (tracing-ab-format value "Searching at depth <~p>: ~n" 
        (:: $Depth)) 
      (tracing-ab-format value "The best move found has a value of: ~p~n" 
        (:: $Val)) 
      (tracing-ab resources 
        (print-resource-consumption $Counts)) 
      (tracing-ab value 
        (print-pc-info $NodeIn))))
; 



  (= 
    (pc-moves Nil Nil) 
    (set-det))
; 

  (= 
    (pc-moves $Node 
      (Cons  $Move $RestMoves)) 
    ( (node-move $Node $Move) 
      (node-cont $Node $Rest) 
      (pc-moves $Rest $RestMoves)))
; 

	
;
; 

;
; 

;
; 



  (= 
    (print-pc-info $Node) 
    ( (node-cont $Node $PC) 
      (pc-moves $PC $Moves) 
      (format "The principal continuation here is: ~n" Nil) 
      (print-moves $Moves) 
      (nl)))
; 



  (= 
    (print_moves  ()) True)
; 

  (= 
    (print-moves (Cons  $M $Moves)) 
    ( (print-move $M) (print-moves $Moves)))
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
    (alphabeta $MaxDepth $Counts $EndTime $Node $Alpha $Beta $GoodNode $Val $Complete) 
    ( (find-advice-tables $Tables) (alphabeta $MaxDepth $Counts $EndTime $Node $Alpha $Beta $GoodNode $Val $Complete $Tables)))
; 


  (= 
    (alphabeta $MaxDepth $Counts $EndTime $Node $Alpha $Beta $GoodNode $Val $Complete $Tables) 
    (alphabeta 0 $MaxDepth $Counts $EndTime $Node $Alpha $Beta $GoodNode $Val $Complete $Tables))
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
    (alphabeta $_ $_ $Counts $End $Node $Alpha $Beta $GoodNode $Val no $_) 
    ( (timeout-for-node $Node $End) 
      (set-det) 
      (node-complete $Node no) 
      (zero-counts $Counts)))
; 


  (= 
    (alphabeta $_ $_ $Counts $End $Node $Alpha $Beta $GoodNode $Val yes $Tables) 
    ( (terminal-node $Node $Val $Tables) 
      (set-det) 
      (node-complete $Node yes) 
      (node-cont $Node Nil) 
      (terminal-counts $Counts)))
; 

	
  (= 
    (alphabeta $Max $Max $Counts $End $Node $Alpha $Beta $GoodNode $Val yes $Tables) 
    ( (set-det) 
      (eval-node $Node $Val $Tables) 
      (node-complete $Node yes) 
      (static-counts $Counts)))
; 


;
; 

;
; 

;
; 

  (= 
    (alphabeta $Depth $MaxDepth $Counts $EndTime $Node $Alpha $Beta $GoodNode $Val $Complete $Tables) 
    ( (< $Depth $MaxDepth) 
      (is $Depth1 
        (+ $Depth 1)) 
      (expand-node $Node $NodeList $Tables) 
      (node-cont $Node $GoodNode) 
      (boundedbest $Depth1 $MaxDepth $Counts1 $EndTime $NodeList $Alpha $Beta $GoodNode $Val $Complete $Tables) 
      (close-node $Node $GoodNode $Val $Complete) 
      (node-complete $Node $Complete) 
      (add-expansion-counts $Counts1 $Counts)))
; 




;
; 

;
; 

;
; 


  (= 
    (boundedbest $Depth $Max $Counts $EndTime 
      (Cons  $Node $NodeList) $Alpha $Beta $GoodNode $GoodVal $Complete $Tables) 
    ( (alphabeta $Depth $Max $Counts1 $EndTime $Node $Alpha $Beta $_ $Val $Comp1 $Tables) 
      (goodenough $Depth $Max $Counts2 $EndTime $NodeList $Alpha $Beta $Node $Val $GoodNode $GoodVal $Comp1 $Complete $Tables) 
      (sum-resource-counts $Counts1 $Counts2 $Counts)))
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
    (goodenough $_ $_ $Counts $End $NodeList $Alpha $Beta $Node $Val $GoodNode $GoodVal $Comp $Comp $Tables) 
    ( (seen-enough $NodeList $Alpha $Beta $Node $Val $GoodNode $GoodVal $Comp) 
      (set-det) 
      (zero-counts $Counts)))
; 

  (= 
    (goodenough $Depth $Max $Counts $End $NodeList $Alpha $Beta $Node $Val $GoodNode $GoodVal yes $Complete $Tables) 
    ( (newbounds-node $Alpha $Beta $Node $Val $NewAlpha $NewBeta) 
      (boundedbest $Depth $Max $Counts $End $NodeList $NewAlpha $NewBeta $Node1 $Val1 $Complete $Tables) 
      (betterof-node $Complete $Depth $Max $Node $Val $Node1 $Val1 $GoodNode $GoodVal)))
; 



;
; 

;
; 

;
; 


  (= 
    (seen-enough Nil $_ $_ $Node $Val $Node $Val $Comp) 
    (set-det))
; 
    ;
; 

  (= 
    (seen-enough $NodeList $_ $_ $Node $Val $Node $Val no) 
    (set-det))
; 
 ;
; 

  (= 
    (seen-enough $NodeList $Alpha $Beta $Node $Val $Node $Val yes) 
    ( (or 
        (, 
          (min-to-move-node $Node) 
          (>= $Val $Beta)) 
        (, 
          (max-to-move-node $Node) 
          (=< $Val $Alpha))) (set-det)))
; 




  (= 
    (newbounds-node $Alpha $Beta $Node $Val $NewAlpha $NewBeta) 
    ( (node-state $Node $Pos) (newbounds $Alpha $Beta $Pos $Val $NewAlpha $NewBeta)))
; 


;
; 

;
; 


  (= 
    (newbounds $Alpha $Beta $Pos $Val $Val $Beta) 
    ( (min-to-move $Pos) 
      (> $Val $Alpha) 
      (set-det)))
; 
   ;
; 

  (= 
    (newbounds $Alpha $Beta $Pos $Val $Alpha $Val) 
    ( (max-to-move $Pos) 
      (< $Val $Beta) 
      (set-det)))
; 
    ;
; 

  (= 
    (newbounds  $Alpha $Beta $_ $_ $Alpha $Beta) True)
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
    (betterof-node no $Depth $Max $Node $Val $Node1 $Val1 $Node $Val) 
    ( (node-complete $Node1 no) (set-det)))
; 

  (= 
    (betterof-node $Complete $Depth $Max $Node $Val $Node1 $Val1 $NodeB $ValB) 
    (betterof-node $Node $Val $Node1 $Val1 $NodeB $ValB))
; 


  (= 
    (betterof-node $Node $Val $Node1 $Val1 $Node $Val) 
    (or 
      (, 
        (min-to-move-node $Node) 
        (>= $Val $Val1) 
        (set-det)) 
      (, 
        (max-to-move-node $Node) 
        (=< $Val $Val1) 
        (set-det))))
; 

  (= 
    (betterof_node  $_ $_ $Node1 $Val1 $Node1 $Val1) True)
; 



  (= 
    (max-to-move-node $Node) 
    ( (node-state $Node $Pos) (max-to-move $Pos)))
; 


  (= 
    (min-to-move-node $Node) 
    ( (node-state $Node $Pos) (min-to-move $Pos)))
; 



  (= 
    (max-to-move $Pos) 
    (control player $Pos))
; 


  (= 
    (min-to-move $Pos) 
    (control opponent $Pos))
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
    (expand-node $Node $NodeList $Tables) 
    ( (tracing-ab expand 
        (, 
          (format "<Expanding> node from move: " Nil) 
          (node-move $Node $Move) 
          (print-move $Move))) (ordered-moves $Node $NodeList $Tables)))
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
    (ordered-moves $Node $NodeList $Tables) 
    ( (successor-nodes $Node $NodeList1 $Tables) 
      (node-pc $Node $PC) 
      (det-if-then-else 
        (, 
          (nonvar $PC) 
          (= $PC 
            (Cons  $M $Moves))) 
        (, 
          (tracing-ab ordering 
            (, 
              (format "Ordering PC highest~n" Nil) 
              (print-moves $PC))) 
          (search-node $PCNode) 
          (node-pc $PCNode $Moves) 
          (node-move $PCNode $M) 
          (select $PCNode $NodeList1 $NodeList2) 
          (= $NodeList 
            (Cons  $PCNode $NodeList2))) 
        (= $NodeList $NodeList1))))
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
    (successor-nodes $Node $Nodes $Tables) 
    (det-if-then-else 
      (parameter ordering random) 
      (random-findall $Node2 
        (successor-node $Node $Node2 $Tables) $Nodes) 
      (det-if-then-else 
        (parameter ordering fixed) 
        (findall $Node2 
          (successor-node $Node $Node2 $Tables) $Nodes) 
        (det-if-then otherwise 
          (, 
            (format "Error in successor_nodes/3: Invalid ordering parameter!!~n") 
            (fail))))))
; 



  (= 
    (successor-node $Node $Node2 $Tables) 
    ( (node-state $Node $State) 
      (successor-pos $Move $State $State2 $Tables) 
      (search-node $Node2) 
      (node-state $Node2 $State2) 
      (node-move $Node2 $Move) 
      (node-parent $Node2 $Node)))
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
    (moves $State $States) 
    (bagof $State2 
      (^ $Move 
        (legal $Move $State $State2)) $States))
; 

	


  (= 
    (close-node $Node $GoodNode $Val $Complete) 
    (tracing-ab expand 
      (, 
        (format "<Closing> node from move: " Nil) 
        (node-move $Node $Move) 
        (print-move $Move) 
        (format "Resulting (~p-complete) value <~p> after move:~n" 
          (:: $Complete $Val)) 
        (node-move $GoodNode $GoodMove) 
        (print-move $GoodMove) 
        (nl))))
; 


;
; 

;
; 

;
; 



  (= 
    (terminal-node $Node $Val $Tables) 
    ( (node-state $Node $Pos) 
      (terminal-pos-value $Pos $Val $Tables) 
      (tracing-ab eval 
        (print-eval-info $Node $Val))))
; 





  (= 
    (eval-node $Node $Val $Tables) 
    ( (node-state $Node $Pos) 
      (staticval $Pos $Val $Tables) 
      (tracing-ab eval 
        (print-eval-info $Node $Val)) 
      (tracing-ab state 
        (print-state $Pos)) 
      (tracing-ab advice 
        (show-advices $Pos $Tables))))
; 

	 


  (= 
    (print-eval-info $Node $Val) 
    ( (node-move $Node $Move) 
      (format "Evaluation <~p> for move: " 
        (:: $Val)) 
      (print-move $Move) 
      (nl)))
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
    (value_of_outcome  draw 0) True)
; 

  (= 
    (value_of_outcome  player 100000) True)
; 

  (= 
    (value_of_outcome  opponent -100000) True)
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
    (primary-choice-node $Node) 
    ( (node-parent $Node $Parent) (node-parent $Parent Nil)))
; 



  (= 
    (initialized-search-nodes $Move $SIn $NodeIn $SOut $NodeOut) 
    ( (search-node $NodeIn) 
      (node-state $NodeIn $SIn) 
      (node-parent $NodeIn Nil) 
      (search-node $NodeOut) 
      (node-state $NodeOut $SOut) 
      (node-move $NodeOut $Move)))
; 



  (= 
    (initialized-start-node $SIn $NodeIn) 
    ( (search-node $NodeIn) 
      (node-state $NodeIn $SIn) 
      (node-parent $NodeIn Nil)))
; 
 ;
; 



  (= 
    (initialized-choice-node $SOut $NodeOut $Move) 
    ( (search-node $NodeOut) 
      (node-state $NodeOut $SOut) 
      (node-move $NodeOut $Move)))
; 




  (= 
    (portray-node (node $State $Parent $Move $Cont $PC)) 
    ( (format "<Node from parent: ~p to state: ~p" 
        (:: '' $State)) 
      (format "by move: " Nil) 
      (print-move $Move) 
      (format "end node>~n" Nil)))
; 


;
; 



;
; 



  (= 
    (search_node  
      (node  $State $Parent $Move $Cont $PC $Comp) $State $Parent $Move $Cont $PC $Comp) True)
; 

  (= 
    (search-node $N) 
    (functor $N node 6))
; 


  (= 
    (node-state $N $A) 
    (arg 1 $N $A))
; 


  (= 
    (node-parent $N $A) 
    (arg 2 $N $A))
; 


  (= 
    (node-move $N $A) 
    (arg 3 $N $A))
; 


  (= 
    (node-cont $N $A) 
    (arg 4 $N $A))
; 


  (= 
    (node-pc $N $A) 
    (arg 5 $N $A))
; 


  (= 
    (node-complete $N $A) 
    (arg 6 $N $A))
; 



;
; 

;
; 

;
; 



  (= 
    (search-resource $N) 
    (functor $N resource 3))
; 


  (= 
    (resource-expansions $N $A) 
    (arg 1 $N $A))
; 


  (= 
    (resource-statics $N $A) 
    (arg 2 $N $A))
; 


  (= 
    (resource-terminals $N $A) 
    (arg 3 $N $A))
; 




  (= 
    (zero-counts $Counts) 
    ( (search-resource $Counts) 
      (resource-expansions $Counts 0) 
      (resource-statics $Counts 0) 
      (resource-terminals $Counts 0)))
; 



  (= 
    (terminal-counts $Counts) 
    ( (search-resource $Counts) 
      (resource-expansions $Counts 0) 
      (resource-statics $Counts 0) 
      (resource-terminals $Counts 1)))
; 



  (= 
    (static-counts $Counts) 
    ( (search-resource $Counts) 
      (resource-expansions $Counts 0) 
      (resource-statics $Counts 1) 
      (resource-terminals $Counts 0)))
; 



  (= 
    (expansion-counts $Counts) 
    ( (search-resource $Counts) 
      (resource-expansions $Counts 1) 
      (resource-statics $Counts 0) 
      (resource-terminals $Counts 0)))
; 



  (= 
    (add-expansion-counts $Counts1 $Counts) 
    ( (expansion-counts $Counts0) (sum-resource-counts $Counts0 $Counts1 $Counts)))
; 

	

  (= 
    (sum-resource-counts $Counts1 $Counts2 $Counts) 
    ( (resource-expansions $Counts1 $Exp1) 
      (resource-statics $Counts1 $Stat1) 
      (resource-terminals $Counts1 $Term1) 
      (resource-expansions $Counts2 $Exp2) 
      (resource-statics $Counts2 $Stat2) 
      (resource-terminals $Counts2 $Term2) 
      (is $Exp 
        (+ $Exp1 $Exp2)) 
      (is $Stat 
        (+ $Stat1 $Stat2)) 
      (is $Term 
        (+ $Term1 $Term2)) 
      (search-resource $Counts) 
      (resource-expansions $Counts $Exp) 
      (resource-statics $Counts $Stat) 
      (resource-terminals $Counts $Term)))
; 



  (= 
    (print-resource-consumption $Counts) 
    ( (resource-expansions $Counts $Exp) 
      (resource-statics $Counts $Stat) 
      (resource-terminals $Counts $Term) 
      (is $Total 
        (+ 
          (+ $Exp $Stat) $Term)) 
      (format "Number of nodes expanded: <~p>~n" 
        (:: $Exp)) 
      (format "Number of nodes statically evaluated: <~p>~n" 
        (:: $Stat)) 
      (format "Number of terminal nodes encountered: <~p>~n" 
        (:: $Term)) 
      (format "Number of terminal node tests:  <~p>~n" 
        (:: $Total))))
; 



;
; 

;
; 

;
; 



  (= 
    (search-limit $N) 
    (functor $N limit 4))
; 


  (= 
    (limit-depth $N $A) 
    ( (search-limit $N) (arg 1 $N $A)))
; 


  (= 
    (limit-count $N $A) 
    ( (search-limit $N) (arg 2 $N $A)))
; 


  (= 
    (limit-timeused $N $A) 
    ( (search-limit $N) (arg 3 $N $A)))
; 


  (= 
    (limit-timeleft $N $A) 
    ( (search-limit $N) (arg 4 $N $A)))
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



  !(my-ensure-loaded (library tracing))
; 



  (= 
    (tracing-ab $Type $Call) 
    (det-if-then-else 
      (tracing (ab $Type)) 
      (call $Call) True))
; 


;
; 


  (= 
    (tracing-ab-format $Type $String $Args) 
    (det-if-then-else 
      (tracing (ab $Type)) 
      (format $String $Args) True))
; 



  (= 
    (tracing-ab-timing $Type $Call) 
    (trace-timing 
      (ab $Type) $Call))
; 



  (= 
    (set-ab-verbosity $Level $Status) 
    (set-tracing 
      (ab $Level) $Status))
; 



  (= 
    (silent-ab) 
    (all-ab off))
; 


  (= 
    (loud-ab) 
    (all-ab on))
; 



  (= 
    (all-ab $Status) 
    ( (set-ab-verbosity ordering $Status) 
      (set-ab-verbosity value $Status) 
      (set-ab-verbosity eval $Status) 
      (set-ab-verbosity expand $Status) 
      (set-ab-verbosity resources $Status) 
      (set-ab-verbosity timing $Status) 
      (set-ab-verbosity iteration $Status)))
; 




  (= 
    (trace-ab-expand) 
    (set-ab-verbosity expand on))
; 
 

  (= 
    (trace-ab-eval) 
    (set-ab-verbosity eval on))
; 
 

  (= 
    (trace-ab-value) 
    (set-ab-verbosity value on))
; 
 

  (= 
    (trace-ab-ordering) 
    (set-ab-verbosity ordering on))
; 
 

  (= 
    (trace-ab-resources) 
    (set-ab-verbosity resources on))
; 
 

  (= 
    (trace-ab-timing) 
    (set-ab-verbosity timing on))
; 
 

  (= 
    (trace-ab-iterations) 
    (set-ab-verbosity iteration on))
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
    (alpha-beta-com $Move $SIn $SOut) 
    ( (timing (alpha-beta-move $Move $SIn $SOut)) (select-move $Move $SIn $SOut)))
; 

	
  (= 
    (alpha-beta-com $Move $SIn $SOut $Depth) 
    ( (timing (alpha-beta-move $Depth $Move $SIn $SOut)) (select-move $Move $SIn $SOut)))
; 



  (= 
    (iterate-com $Move $SIn $SOut) 
    ( (timing (iterate-move $Move $SIn $SOut)) (select-move $Move $SIn $SOut)))
; 



  (= 
    (iterate-random-com $Move $SIn $SOut) 
    ( (timing (iterate-random-move $Move $SIn $SOut)) (select-move $Move $SIn $SOut)))
; 



  (= 
    (iterate-fixed-com $Move $SIn $SOut) 
    ( (timing (iterate-fixed-move $Move $SIn $SOut)) (select-move $Move $SIn $SOut)))
; 



