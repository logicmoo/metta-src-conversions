;                                           (convert_to_metta_file  alphabeta $_356316 metagame/play/alphabeta.pl metagame/play/alphabeta.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; alphabeta.pl

;
; This is the hottest version yet:
;
; 1. alpha-beta
;
; 2. counts nodes evaluated
;
; 3. time cutoffs, chooses best move up to cutoff.
;
; 4. iterative deepening
;
; 5. principal continuation heuristic
;
; 6. plays forced moves immediately
;
; 7. either default or random candidate move-ordering
;
; 8. stops when forced win for a player (should do draws also)
;
; - outputs win in N-ply message!
;
; 
;
; Thus, there are no more (well, few) magic parameters here, and program
;
; can now play lots of games very nicely!

;
; ================================================================================
;
; IMPLEMENTED PLAYERS
;
; ================================================================================
;
; The following players (x_choose) are selectable from the
;
; interface as players in their own right (from the top-level menu).
;
; They can also be called as move selectors (x_com) to assist a human in making
;
; a move, from the move-menu.
;
; The core routines supporting these procedures are called (x_move).
;
; Full documentation is made with these core routines.

;
; ========================================
;
; ALPHA_BETA_CHOOSE(Player,Move,SIn,SOut)
;
; ========================================
;
; Plays move with highest minimaxed
;
; value, using the current evaluation function.
;
; Searches to a fixed depth determined by the parameter: DEPTH.
;
; 
;
; Uses whatever move ordering parameter ORDERING is set to.


  (= (alpha-beta-choose $Player $Move $SIn $SOut)  
    (toggle-weights-choose alpha-beta-move $Player $Move $SIn $SOut))

;
; ========================================
;
; ITERATE_CHOOSE(Player,Move,SIn,SOut)
;
; ========================================
;
; Plays move with highest iterated minimaxed
;
; value, using the current evaluation function.
;
; Uses whatever move ordering parameter is set to.


  (= (iterate-choose $Player $Move $SIn $SOut)  
    (toggle-weights-choose iterate-move $Player $Move $SIn $SOut))

;
; ========================================
;
; ITERATE_RANDOM_CHOOSE(Player,Move,SIn,SOut)
;
; ========================================
;
; Plays move with highest iterated minimaxed
;
; value, using the current evaluation function.
;
; Uses random move ordering heuristic.


  (= (iterate-random-choose $Player $Move $SIn $SOut)  
    (toggle-weights-choose iterate-random-move $Player $Move $SIn $SOut))

;
; ========================================
;
; ITERATE_FIXED_CHOOSE(Player,Move,SIn,SOut)
;
; ========================================
;
; Plays move with highest iterated minimaxed
;
; value, using the current evaluation function.
;
; Uses fixed move ordering heuristic (use order from generator).


  (= (iterate-fixed-choose $Player $Move $SIn $SOut)  
    (toggle-weights-choose iterate-fixed-move $Player $Move $SIn $SOut))


;
; =================================================
;
; TOGGLE_WEIGHTS_CHOOSE(Type,Player,Move,SIn,SOut)
;
; =================================================
;
; Sets alpha-beta parameters for Player (if any),
;
; then calls the Type of search method with these weights,
;
; finally (whether successful or not) sets them back to their original values.
;
; 
;
; This is *not* a player in itself, just a support function for
;
; players based on alpha-beta which use these weights.
;
; 

  (= (toggle-weights-choose $Type $Player $Move $SIn $SOut)  
    (=.. $Goal 
      (:: $Type $Move $SIn $SOut))
    (det-if-then-else 
      (toggle-alpha-beta-weights $Player $Old) 
      (det-if-then-else 
        (timing $Goal) 
        (, 
          (print-choice $Move $SIn $SOut) 
          (set-parameter weights $Old)) 
        (, 
          (set-parameter weights $Old) 
          (fail))) 
      (det-if-then 
        (timing $Goal) 
        (print-choice $Move $SIn $SOut))))


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Parameters
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (ab-depth $D)  
    (parameter depth $D))

;
; Not used.

  (= (bound $B)  
    (parameter bound $B))


  (= (move-time-limit $T)  
    (parameter move-time-limit $T))


  (= (move-horizon $T)  
    (parameter move-horizon $T))

;
; Within what ranges we want our values.

  (= (approx-window $A $B)  
    (value-of-outcome player $B)
    (value-of-outcome opponent $A))


  (= (reset-alpha-params)  
    (set-parameter depth 1)
    (set-parameter move-time-limit 10000)
    (set-parameter move-horizon 1)
    (set-parameter ordering random))
;
; initialize_weights.


;
; ================================================================================
;
; Time Management
;
; ================================================================================

;
; ==============================================
;
; SEARCH_TIMEOUT_TIME(+StartTime,State,-EndTime)
;
; StartTime is the time this search started.
;
; State is the current position (in which P is the player to move)
;
; EndTime is the time we will stop the current search.
;
; ==============================================
;
; 
;
; GAME_TIME_LEFT: The total amount of time remaining to play all our moves.
;
; MOVE_TIME_LIMIT: The max amount of time we can spend on this move.
;
; Can be infinite if no move-time-limit, or can give a suggested limit,
;
; or an enforced limit if these are the rules of the tournament.
;
; ESTIMATED_MOVES_REMAINING: An estimate of the number of moves we will
;
; still need to play this game. This can be determined in several ways:
;
; 1. If we are near the N-move-rule for ending the game, we can use this
;
; number.
;
; 2. We can assume there is always some K moves left to play (like 30 moves).
;
; This leads to spending more time in the early moves and speeding up
;
; later.
;
; 3. This can be learned from past games, possibly context-dependent.
;
; a. Just observe total game lengths, use that number for all positions.
;
; b. Learn to estimate based on features of a position.
;
; 
;
; The result of the whole function is as follows:
;
; Allocates for this move the avg. amount of time needed
;
; to survive the number of moves we will need to play this game.
;
; If this time exceeds the move_time_limit, just uses that.
;
; 

  (= (search-timeout-time $StartTime $SIn $EndTime)  
    (control $P $SIn)
    (game-time-left $P $GameLeft)
    (move-time-limit $MoveLimit)
    (estimated-moves-remaining $MovesLeft $SIn)
    (search-timeout-time $StartTime $GameLeft $MovesLeft $MoveLimit $EndTime))

  (= (search-timeout-time $StartTime $GameLeft $MovesLeft $MoveLimit $EndTime)  
    (is $Avg 
      (/ $GameLeft $MovesLeft))
    (is $TimeAvail 
      (min $MoveLimit $Avg))
    (is $EndTime 
      (+ $StartTime $TimeAvail)))

;
; Could be a more complicated procedure.
;
; For now use parameter: move_horizon.
;
; 
;
; estimated_moves_remaining(30,_).

  (= (estimated-moves-remaining $Horizon $_)  
    (move-horizon $Horizon))


;
; Ensures End is a number. If not, time will not be checked.
;
; This is a way to disable the time checking!

  (= (timeout-for-node $Node $End)  
    (tracing-ab timing 
      (print-timeout-check $Node))
    (number $End)
    (current-time $Time)
    (>= $Time $End)
    (is $Diff 
      (- $Time $End))
    (print-timeout-message $Diff))
; ; primary_choice_node(Node),



;
; timeout_for_node([],Node,_) :-
;
; tracing_ab_format("Called to check timeout, but no other nodes~n",[]).
  (= (timeout-for-node (Cons  $Node $Rest) $BestNode $End)  
    (timeout-for-node $Node $End))



  (= (print-timeout-check $Node)  
    (format "Testing for timeout~n" Nil)
    (node-move $Node $Move)
    (print-move $Move))



  (= (print-timeout-message $Diff)  
    (tracing-ab-format timing "Out of Time by <~p> msec~n" 
      (:: $Diff))) 



  (= (current-time $T0)  
    (statistics runtime 
      (Cons  $T0 $_))) 

;
; An old function which ignores the limit on total game time.

  (= (search-timeout-time $EndTime)  
    (move-time-limit $Limit)
    (current-time $T0)
    (is $EndTime 
      (+ $T0 $Limit)))

;
; ================================================================================

;
; ==================================
;
; ITERATE_FIXED_MOVE(Move,SIn,SOut)
;
; ==================================
;
; Uses fixed move ordering as provided by move generator.

  (= (iterate-fixed-move $Move $SIn $SOut)  
    (change-parameter ordering $Ord fixed)
    (iterate-move $Move $SIn $SOut)
    (change-parameter ordering $_ $Ord))

;
; ==================================
;
; ITERATE_RANDOM_MOVE(Move,SIn,SOut)
;
; ==================================
;
; Uses the random_ordering heuristic with iterate_move.
;
; 

  (= (iterate-random-move $Move $SIn $SOut)  
    (change-parameter ordering $Ord random)
    (iterate-move $Move $SIn $SOut)
    (change-parameter ordering $_ $Ord))

;
; ===========================
;
; ITERATE_MOVE(Move,SIn,SOut)
;
; ===========================
;
; If there is only one legal move, plays it immediately.
;
; Else, does iterative deepening alpha-beta search until out of
;
; time. Then plays the move selected, or the first move available
;
; if the search had no time to find anything at all.
;
; 
;
; Uses whatever move ordering is currently set (parameter: ordering).

  (= (iterate-move $Move $SIn $SOut)  
    (current-time $StartTime)
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
        (accept-rushed-move $M $SFirst $Move $SOut))))
; ; in advisors.pl

;
; Accepting a forced or rushed move means printing a notice to this effect,
;
; and equating the input and output moves and states.

  (= (accept-forced-move $M $S $M $S)  
    (print-forced-notice))   ;
; in advisors.pl


  (= (accept-rushed-move $M $S $M $S)  
    (print-rushed-notice))   ;
; in advisors.pl

;
; A move is forced if all legal moves are identical to it.

  (= (forced-move $Move $SIn)  
    (not (, (legal $Move2 $SIn $S2) (not (= $Move $Move2)))))


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Iterative-Deepening Alpha-Beta algorithm.
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; ================================================
;
; ITERATE(Depth,StartTime,EndTime,NodeIn,NodeOut)
;
; ================================================
;
; Calls alpha beta with the current max depth, then
;
; considers whether to iterate deeper or return the
;
; best move found so far.


  (= (iterate $Depth $StartTime $EndTime $NodeIn $NodeOut)  
    (find-advice-tables $Tables)
    (iterate $Depth $StartTime $EndTime $NodeIn $NodeOut $Tables))


  (= (iterate $Depth $StartTime $EndTime $NodeIn $NodeOut $Tables)  
    (approx-window $Alpha $Beta)
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
    (iterate-deeper $Depth $StartTime $EndTime $Val $NodeIn $Node1 $NodeOut $Tables))
; ; timing(alphabeta(Depth,Counts,EndTime,NodeIn,Alpha,Beta,Node1,Val,_)),


;
; =================================================================
;
; ITERATE_DEEPER(Depth,StartTime,EndTime,Val,NodeIn,Node1,NodeOut)
;
; =================================================================
;
; If one player has a forced win, don't look any further.
;
; If not enough time for another iteration, just use the
;
; best move we've found on the prev. iteration.
;
; If still enough time, use pc to start anouther depth.
;
; 
;
; Would be nice to have a clause for draws, but can't tell if a draw
;
; or even on an evaluation fn.

  (= (iterate-deeper $Depth $StartTime $EndTime $Val $NodeIn $Node1 $Node1 $Tables)  
    (win-detected $Val $Depth)
    (set-det))
  (= (iterate-deeper $Depth $StartTime $EndTime $_ $NodeIn $Node1 $Node1 $Tables)  
    (not-enough-time $Depth $StartTime $EndTime)
    (set-det))
  (= (iterate-deeper $Depth $StartTime $EndTime $_ $NodeIn $_ $NodeOut $Tables)  
    (node-cont $NodeIn $PC)
    (node-state $NodeIn $SIn)
    (pc-moves $PC $Moves)
    (initialized-start-node $SIn $Node1)
    (node-pc $Node1 $Moves)
    (is $Depth1 
      (+ $Depth 1))
    (iterate $Depth1 $StartTime $EndTime $Node1 $NodeOut $Tables))


  (= (win-detected $Val $Depth)  
    (var $Val)
    (set-det)
    (tracing-ab-format value "No useful info this iteration.~n" Nil)) 
  (= (win-detected $Val $Depth)  
    (player-role $Player)
    (value-of-outcome $Player $Val)
    (set-det)
    (tracing-ab-format value "A win is detected for <~p> in <~p> ply!~n" 
      (:: $Player $Depth)))


;
; Could use a more sophisticated version below, to avoid
;
; even starting when there's clearly not enough time to
;
; do a full ply. But with the PC heuristic, why not just
;
; keep on searching to see if we might have been wrong?

  (= (not-enough-time $Depth $StartTime $EndTime)  
    (current-time $Now)
    (>= $Now $EndTime))
;
; not_enough_time(_Depth,StartTime,EndTime) :-
;
; current_time(Now),
;
; TimeUsed is Now - StartTime,
;
; TimeLeft is EndTime - Now,
;
; TimeUsed > TimeLeft / 2.



;
; ======================================
;
; ALPHA_BETA_MOVE(Move,SIn,SOut)
;
; ======================================
;
; Searches to a fixed depth, based on the parameter: AB_DEPTH.


  (= (alpha-beta-move $Move $SIn $SOut)  
    (ab-depth $Depth)
    (alpha-beta-move $Depth $Move $SIn $SOut))

;
; ======================================
;
; ALPHA_BETA_MOVE(Depth,Move,SIn,SOut)
;
; ======================================
;
; 
;
; Plays move with highest alpha_beta minimaxed
;
; value, using the current evaluation function.
;
; Searches according to the following parameters:
;
; 
;
; BOUND (approx_window): least optimistic value s.t. we take any move
;
; which is better than this for the player to move.
;
; (This is currently not used as a parameter, instead the bound is
;
; just win and loss values).
;
; 
;
; DEPTH: depth of tree at which we evaluate statically.
;
; 
;
; If there is only one legal move, plays it immediately.
;
; Else, does alpha-beta search until reaches DEPTH or out of time.
;
; Then plays the move selected, or the first move available
;
; if the search had no time to find anything at all.
;
; 
;
; Uses whatever move ordering is currently set (parameter: ordering).
;
; 
  (= (alpha-beta-move $Depth $Move $SIn $SOut)  
    (current-time $StartTime)
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
        (accept-rushed-move $M $SFirst $Move $SOut))))
; ; in advisors.pl



  (= (alpha-beta-iterate $Depth $StartTime $EndTime $NodeIn $NodeOut)  
    (approx-window $Alpha $Beta)
    (tracing-ab-timing iteration 
      (alphabeta $Depth $Counts $EndTime $NodeIn $Alpha $Beta $NodeOut $Val $_))
    (tracing-ab-format value "Searching at depth <~p>: ~n" 
      (:: $Depth))
    (tracing-ab-format value "The best move found has a value of: ~p~n" 
      (:: $Val))
    (tracing-ab resources 
      (print-resource-consumption $Counts))
    (tracing-ab value 
      (print-pc-info $NodeIn)))


  (= (pc-moves Nil Nil)  
    (set-det))
; /* alpha_beta_move(Depth,Move,SIn,SOut) :- current_time(StartTime), search_timeout_time(StartTime,SIn,EndTime), approx_window(Alpha,Beta), initialized_search_nodes(Move,SIn,NodeIn,SOut,NodeOut), tracing_ab_timing(iteration, alphabeta(Depth,Counts,EndTime,NodeIn,Alpha,Beta,NodeOut,Val,_)), tracing_ab_format(value,"Searching at depth <~p>: ~n",[Depth]), tracing_ab_format(value,"The best move found has a value of: ~p~n",[Val]), tracing_ab(resources,print_resource_consumption(Counts)), tracing_ab(value,print_pc_info(NodeIn)). */
  (= (pc-moves $Node (Cons  $Move $RestMoves))  
    (node-move $Node $Move)
    (node-cont $Node $Rest)
    (pc-moves $Rest $RestMoves))
	
;
; If want to print continuation INCLUDING this node, delete first line.
;
; PC is thus a sequence of positions which follow down to DEPTH,
;
; or just the current move with its value if this is a terminal position.


  (= (print-pc-info $Node)  
    (node-cont $Node $PC)
    (pc-moves $PC $Moves)
    (format "The principal continuation here is: ~n" Nil)
    (print-moves $Moves)
    (nl))


  (= (print_moves  ())  True)
  (= (print-moves (Cons  $M $Moves))  
    (print-move $M)
    (print-moves $Moves))


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Core Alpha-Beta algorithm.
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 
;
; Based on algorithm in Bratko, p. 366.

;
; Start the search at depth 0, as the top position is the root.
;
; 
;
; Uses the advice tables constructed already, as information for the evaluation
;
; function. If players using different info are both using this shell,
;
; their respective tables should be passed in to the procedure which uses
;
; tables explicitly.

  (= (alphabeta $MaxDepth $Counts $EndTime $Node $Alpha $Beta $GoodNode $Val $Complete)  
    (find-advice-tables $Tables)
    (alphabeta $MaxDepth $Counts $EndTime $Node $Alpha $Beta $GoodNode $Val $Complete $Tables))

  (= (alphabeta $MaxDepth $Counts $EndTime $Node $Alpha $Beta $GoodNode $Val $Complete $Tables)  
    (alphabeta 0 $MaxDepth $Counts $EndTime $Node $Alpha $Beta $GoodNode $Val $Complete $Tables))


;
; alphabeta(Depth,MaxDepth,Counts,EndTime,Node, Alpha, Beta, GoodNode, Val,Complete,Tables)
;
; Depth is the current depth in the search (where the root position depth=0).
;
; MaxDepth is the maximum depth to search down to (where static eval positions).
;
; Node is a search tree node representing a position.
;
; Best move from Node leads to a node GoodNode, with minimax
;
; value Val.
;
; Complete is yes if Node was complete, no otherwise.

;
; Search stops when one of the following is true:
;
; 1. The node is a terminal position.
;
; 2. We have gone down Depth ply, so the current depth counter is 0.
;
; 3. We are out of time.

;
; When we evaluate, we'll here do so always from perspective of
;
; PLAYER. Thus, player likes positions where this is maximized,
;
; opponent likes these minimized.
;
; 
;
; Note that when we check min_to_move(Pos), this tells us
;
; that the MAX player is the parent, who is thus making the
;
; choice. So this could be rewritten: choice_for_max(Pos).
;
; 
;
; From now on, a POS is a NODE data structure, which contains
;
; a state and more info. So all the procedures which here
;
; operator on POS's will now operate on these structures.
;
; 
;
; If run out of time before can find a value for this node,
;
; GoodNode=_Var, Val=_Var, Complete=no.
;
; And node_complete(Node,Complete).

  (= (alphabeta $_ $_ $Counts $End $Node $Alpha $Beta $GoodNode $Val no $_)  
    (timeout-for-node $Node $End)
    (set-det)
    (node-complete $Node no)
    (zero-counts $Counts))
; ; ran out of time.

  (= (alphabeta $_ $_ $Counts $End $Node $Alpha $Beta $GoodNode $Val yes $Tables)  
    (terminal-node $Node $Val $Tables)
    (set-det)
    (node-complete $Node yes)
    (node-cont $Node Nil)
    (terminal-counts $Counts))
	
  (= (alphabeta $Max $Max $Counts $End $Node $Alpha $Beta $GoodNode $Val yes $Tables)  
    (set-det)
    (eval-node $Node $Val $Tables)
    (node-complete $Node yes)
    (static-counts $Counts))
; ; At max depth, evaluate

;
; So here we are not at a terminal Node.
;
; And not out of time.
;
; Find the best of the available moves at the next depth.
  (= (alphabeta $Depth $MaxDepth $Counts $EndTime $Node $Alpha $Beta $GoodNode $Val $Complete $Tables)  
    (< $Depth $MaxDepth)
    (is $Depth1 
      (+ $Depth 1))
    (expand-node $Node $NodeList $Tables)
    (node-cont $Node $GoodNode)
    (boundedbest $Depth1 $MaxDepth $Counts1 $EndTime $NodeList $Alpha $Beta $GoodNode $Val $Complete $Tables)
    (close-node $Node $GoodNode $Val $Complete)
    (node-complete $Node $Complete)
    (add-expansion-counts $Counts1 $Counts))
; ; Use heuristics to find ordered moves
; ; format("Calling bbest from ab~n",[]),



;
; ============
;
; BOUNDEDBEST
;
; ============

  (= (boundedbest $Depth $Max $Counts $EndTime (Cons  $Node $NodeList) $Alpha $Beta $GoodNode $GoodVal $Complete $Tables)  
    (alphabeta $Depth $Max $Counts1 $EndTime $Node $Alpha $Beta $_ $Val $Comp1 $Tables)
    (goodenough $Depth $Max $Counts2 $EndTime $NodeList $Alpha $Beta $Node $Val $GoodNode $GoodVal $Comp1 $Complete $Tables)
    (sum-resource-counts $Counts1 $Counts2 $Counts))
; ; format("Callng ab from bbest~n",[]),
; ; format("About to call goodenough from bbest~n",[]),


;
; ===========
;
; GOODENOUGH
;
; ===========
;
; Finds a good enough position out of Poslist, in order
;
; to approximate the value of the parent.
;
; If the best value we've seen is already outside the alpha-beta
;
; window, we don't need consider any of these moves further,
;
; as they certainly will not be on the principal continuation.
;
; Otherwise, consider the moves successively (so long as we are
;
; still in the window), revise the window as necessary, and
;
; choose the best move.
;
; 
;
; I think the timeout check really has to come first here.
;
; Otherwise we might mistakenly use an incomplete value.


  (= (goodenough $_ $_ $Counts $End $NodeList $Alpha $Beta $Node $Val $GoodNode $GoodVal $Comp $Comp $Tables)  
    (seen-enough $NodeList $Alpha $Beta $Node $Val $GoodNode $GoodVal $Comp)
    (set-det)
    (zero-counts $Counts))
  (= (goodenough $Depth $Max $Counts $End $NodeList $Alpha $Beta $Node $Val $GoodNode $GoodVal yes $Complete $Tables)  
    (newbounds-node $Alpha $Beta $Node $Val $NewAlpha $NewBeta)
    (boundedbest $Depth $Max $Counts $End $NodeList $NewAlpha $NewBeta $Node1 $Val1 $Complete $Tables)
    (betterof-node $Complete $Depth $Max $Node $Val $Node1 $Val1 $GoodNode $GoodVal))
; ; refine bounds ; format("Calling bbest from goodenough~n",[]),


;
; SEEN_ENOUGH
;
; We've seen enough if there is nothing left to see, we're out of time,
;
; or the moves definitely not on the principal continuation.

  (= (seen-enough Nil $_ $_ $Node $Val $Node $Val $Comp)  
    (set-det))    ;
; no other candidate
  (= (seen-enough $NodeList $_ $_ $Node $Val $Node $Val no)  
    (set-det)) ;
; out of time
  (= (seen-enough $NodeList $Alpha $Beta $Node $Val $Node $Val yes)  
    (or 
      (, 
        (min-to-move-node $Node) 
        (>= $Val $Beta)) 
      (, 
        (max-to-move-node $Node) 
        (=< $Val $Alpha)))
    (set-det))
; ; maximizer attained upper bound
; ; minimizer attained lower bound



  (= (newbounds-node $Alpha $Beta $Node $Val $NewAlpha $NewBeta)  
    (node-state $Node $Pos)
    (newbounds $Alpha $Beta $Pos $Val $NewAlpha $NewBeta))

;
; Could modify here to store those moves whose val was eq
;
; to alpha or beta, to get set of moves of = val.

  (= (newbounds $Alpha $Beta $Pos $Val $Val $Beta)  
    (min-to-move $Pos)
    (> $Val $Alpha)
    (set-det))   ;
; maximizer increased lower bound
  (= (newbounds $Alpha $Beta $Pos $Val $Alpha $Val)  
    (max-to-move $Pos)
    (< $Val $Beta)
    (set-det))    ;
; minimizer decreased upper bound
  (= (newbounds  $Alpha $Beta $_ $_ $Alpha $Beta)  True)



;
; BETTEROF_NODE(Complete,Depth,Max,Node,Val,Node1,Val1,NodeB,ValB)
;
; ================================================================
;
; If we didn't get a useful estimate for the second node,
;
; throw it away and just return the first node immediately.
;
; To be useful, all the node's children must have been completed.
;
; This was a new change, and finally got rid of all the bugs
;
; resulting from misusing or notusing incomplete searches.
;
; 

  (= (betterof-node no $Depth $Max $Node $Val $Node1 $Val1 $Node $Val)  
    (node-complete $Node1 no)
    (set-det))
  (= (betterof-node $Complete $Depth $Max $Node $Val $Node1 $Val1 $NodeB $ValB)  
    (betterof-node $Node $Val $Node1 $Val1 $NodeB $ValB))

  (= (betterof-node $Node $Val $Node1 $Val1 $Node $Val)  
    (or 
      (, 
        (min-to-move-node $Node) 
        (>= $Val $Val1) 
        (set-det)) 
      (, 
        (max-to-move-node $Node) 
        (=< $Val $Val1) 
        (set-det))))
  (= (betterof_node  $_ $_ $Node1 $Val1 $Node1 $Val1)  True)


  (= (max-to-move-node $Node)  
    (node-state $Node $Pos)
    (max-to-move $Pos))

  (= (min-to-move-node $Node)  
    (node-state $Node $Pos)
    (min-to-move $Pos))


  (= (max-to-move $Pos)  
    (control player $Pos))

  (= (min-to-move $Pos)  
    (control opponent $Pos))


;
; ================================================================================
;
; Expanding and Closing Search Nodes
;
; ================================================================================

;
; Use heuristics to find ordered moves

  (= (expand-node $Node $NodeList $Tables)  
    (tracing-ab expand 
      (, 
        (format "<Expanding> node from move: " Nil) 
        (node-move $Node $Move) 
        (print-move $Move)))
    (ordered-moves $Node $NodeList $Tables)) 

;
; Instead of returning a NodeList, could attach the pc to
;
; the Node which will here get ordered first. Then won't
;
; have to look in successor positions to see if they're on the
;
; pc.
;
; Now doing this: Take the successor node who's move is the move
;
; on the PC, if any, give it the rest of the PC and order it first.


  (= (ordered-moves $Node $NodeList $Tables)  
    (successor-nodes $Node $NodeList1 $Tables)
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
      (= $NodeList $NodeList1)))
; ; legal moves in Node produce Nodelist


;
; ===================================
;
; SUCCESSOR_NODES(Node,Nodes,Tables)
;
; ===================================
;
; Nodes is a list of legal successor nodes (containing possible moves).
;
; The order is based on the parameter: ORDERING, as follows:
;
; a. random ==> randomly permute the list order
;
; b. fixed ==> use the list as initially generated.
;
; 

  (= (successor-nodes $Node $Nodes $Tables)  
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


  (= (successor-node $Node $Node2 $Tables)  
    (node-state $Node $State)
    (successor-pos $Move $State $State2 $Tables)
    (search-node $Node2)
    (node-state $Node2 $State2)
    (node-move $Node2 $Move)
    (node-parent $Node2 $Node))


;
; ----------------------------------------
;
; SUCCESSOR_POS(Move,State,State2,Tables)
;
; ----------------------------------------
;
; External predicate, backtracks over all successor
;
; STATE2 reachable from STATE.
;
; Uses evaluation tables.
;
; Move should be a unique name of this transition.



;
; Not used here anymore.

  (= (moves $State $States)  
    (bagof $State2 
      (^ $Move 
        (legal $Move $State $State2)) $States))
	


  (= (close-node $Node $GoodNode $Val $Complete)  
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
; ================================================================================
;
; Static evaluation and terminal position detection
;
; ================================================================================


  (= (terminal-node $Node $Val $Tables)  
    (node-state $Node $Pos)
    (terminal-pos-value $Pos $Val $Tables)
    (tracing-ab eval 
      (print-eval-info $Node $Val)))




  (= (eval-node $Node $Val $Tables)  
    (node-state $Node $Pos)
    (staticval $Pos $Val $Tables)
    (tracing-ab eval 
      (print-eval-info $Node $Val))
    (tracing-ab state 
      (print-state $Pos))
    (tracing-ab advice 
      (show-advices $Pos $Tables)))
	 


  (= (print-eval-info $Node $Val)  
    (node-move $Node $Move)
    (format "Evaluation <~p> for move: " 
      (:: $Val))
    (print-move $Move)
    (nl))



;
; -----------------------------------
;
; TERMINAL_POS_VALUE(Pos,Val,Tables)
;
; -----------------------------------
;
; External predicate. Returns a Val for a Pos if it
;
; is terminal.

;
; This defined in value.pl
;
; terminal_game_outcome(Pos,Val) :-
;
; game_outcome(Outcome,Pos),
;
; value_of_outcome(Outcome,Val), !.


;
; VALUE_OF_OUTCOME(Winner,Value)
;
; 

  (= (value_of_outcome  draw 0)  True)
  (= (value_of_outcome  player 100000)  True)
  (= (value_of_outcome  opponent -100000)  True)


;
; -------------------------
;
; STATICVAL(Pos,Val,Tables)
;
; -------------------------
;
; Must be defined by external file, determines what
;
; evaluation procedure will be used!




;
; ================================================================================
;
; NODE data structure
;
; ================================================================================


;
; ========================================
;
; Managing Search Nodes
;
; ========================================


  (= (primary-choice-node $Node)  
    (node-parent $Node $Parent)
    (node-parent $Parent Nil))


  (= (initialized-search-nodes $Move $SIn $NodeIn $SOut $NodeOut)  
    (search-node $NodeIn)
    (node-state $NodeIn $SIn)
    (node-parent $NodeIn Nil)
    (search-node $NodeOut)
    (node-state $NodeOut $SOut)
    (node-move $NodeOut $Move))
; ; Set to be root node of tree


  (= (initialized-start-node $SIn $NodeIn)  
    (search-node $NodeIn)
    (node-state $NodeIn $SIn)
    (node-parent $NodeIn Nil)) ;
; Set to be root node of tree


  (= (initialized-choice-node $SOut $NodeOut $Move)  
    (search-node $NodeOut)
    (node-state $NodeOut $SOut)
    (node-move $NodeOut $Move))



  (= (portray-node (node $State $Parent $Move $Cont $PC))  
    (format "<Node from parent: ~p to state: ~p" 
      (:: '' $State))
    (format "by move: " Nil)
    (print-move $Move)
    (format "end node>~n" Nil))

;
; :- add_portray(portray_node).


;
; Low-level node implementation.


  (= (search_node  (node  $State $Parent $Move $Cont $PC $Comp) $State $Parent $Move $Cont $PC $Comp)  True)
  (= (search-node $N)  
    (functor $N node 6))

  (= (node-state $N $A)  
    (arg 1 $N $A))

  (= (node-parent $N $A)  
    (arg 2 $N $A))

  (= (node-move $N $A)  
    (arg 3 $N $A))

  (= (node-cont $N $A)  
    (arg 4 $N $A))

  (= (node-pc $N $A)  
    (arg 5 $N $A))

  (= (node-complete $N $A)  
    (arg 6 $N $A))


;
; ================================================================================
;
; RESOURCE data structure
;
; ================================================================================


  (= (search-resource $N)  
    (functor $N resource 3))

  (= (resource-expansions $N $A)  
    (arg 1 $N $A))

  (= (resource-statics $N $A)  
    (arg 2 $N $A))

  (= (resource-terminals $N $A)  
    (arg 3 $N $A))



  (= (zero-counts $Counts)  
    (search-resource $Counts)
    (resource-expansions $Counts 0)
    (resource-statics $Counts 0)
    (resource-terminals $Counts 0))


  (= (terminal-counts $Counts)  
    (search-resource $Counts)
    (resource-expansions $Counts 0)
    (resource-statics $Counts 0)
    (resource-terminals $Counts 1))


  (= (static-counts $Counts)  
    (search-resource $Counts)
    (resource-expansions $Counts 0)
    (resource-statics $Counts 1)
    (resource-terminals $Counts 0))


  (= (expansion-counts $Counts)  
    (search-resource $Counts)
    (resource-expansions $Counts 1)
    (resource-statics $Counts 0)
    (resource-terminals $Counts 0))


  (= (add-expansion-counts $Counts1 $Counts)  
    (expansion-counts $Counts0)
    (sum-resource-counts $Counts0 $Counts1 $Counts))
	

  (= (sum-resource-counts $Counts1 $Counts2 $Counts)  
    (resource-expansions $Counts1 $Exp1)
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
    (resource-terminals $Counts $Term))


  (= (print-resource-consumption $Counts)  
    (resource-expansions $Counts $Exp)
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
      (:: $Total)))


;
; ================================================================================
;
; LIMIT data structure
;
; ================================================================================


  (= (search-limit $N)  
    (functor $N limit 4))

  (= (limit-depth $N $A)  
    (search-limit $N)
    (arg 1 $N $A))

  (= (limit-count $N $A)  
    (search-limit $N)
    (arg 2 $N $A))

  (= (limit-timeused $N $A)  
    (search-limit $N)
    (arg 3 $N $A))

  (= (limit-timeleft $N $A)  
    (search-limit $N)
    (arg 4 $N $A))


;
; ================================================================================
;
; tracing execution of alphabeta routines
;
; ================================================================================

;
; The following tracing modules are used in this file:
;
; ordering: info regarding move ordering heuristics
;
; value: info regarding value of moves found during search
;
; also traces principal continuations
;
; resources: info regarding resource consumption during search
;
; timing: info on timeout checks during search
;
; iteration: info on each iteration of the search (currently just how long they took)
;
; 
;
; Each module can be set on/off, using set_ab_verbosity (see below), or
;
; using trace_ab_<module>.
;
; 
;
; All can be turned off with silent_ab.


  !(my-ensure-loaded (library tracing))


  (= (tracing-ab $Type $Call)  
    (det-if-then-else 
      (tracing (ab $Type)) 
      (call $Call) True))

;
; Might cause trouble later when want to use streams also.

  (= (tracing-ab-format $Type $String $Args)  
    (det-if-then-else 
      (tracing (ab $Type)) 
      (format $String $Args) True))


  (= (tracing-ab-timing $Type $Call)  
    (trace-timing 
      (ab $Type) $Call))


  (= (set-ab-verbosity $Level $Status)  
    (set-tracing 
      (ab $Level) $Status))


  (= (silent-ab)  
    (all-ab off))

  (= (loud-ab)  
    (all-ab on))


  (= (all-ab $Status)  
    (set-ab-verbosity ordering $Status)
    (set-ab-verbosity value $Status)
    (set-ab-verbosity eval $Status)
    (set-ab-verbosity expand $Status)
    (set-ab-verbosity resources $Status)
    (set-ab-verbosity timing $Status)
    (set-ab-verbosity iteration $Status))



  (= (trace-ab-expand)  
    (set-ab-verbosity expand on)) 

  (= (trace-ab-eval)  
    (set-ab-verbosity eval on)) 

  (= (trace-ab-value)  
    (set-ab-verbosity value on)) 

  (= (trace-ab-ordering)  
    (set-ab-verbosity ordering on)) 

  (= (trace-ab-resources)  
    (set-ab-verbosity resources on)) 

  (= (trace-ab-timing)  
    (set-ab-verbosity timing on)) 

  (= (trace-ab-iterations)  
    (set-ab-verbosity iteration on))

;
; :- trace_ab_value.
;
; :- silent_ab.

;
; ================================================================================
;
; Interface
;
; ================================================================================


  (= (alpha-beta-com $Move $SIn $SOut)  
    (timing (alpha-beta-move $Move $SIn $SOut))
    (select-move $Move $SIn $SOut))
	
  (= (alpha-beta-com $Move $SIn $SOut $Depth)  
    (timing (alpha-beta-move $Depth $Move $SIn $SOut))
    (select-move $Move $SIn $SOut))


  (= (iterate-com $Move $SIn $SOut)  
    (timing (iterate-move $Move $SIn $SOut))
    (select-move $Move $SIn $SOut))


  (= (iterate-random-com $Move $SIn $SOut)  
    (timing (iterate-random-move $Move $SIn $SOut))
    (select-move $Move $SIn $SOut))


  (= (iterate-fixed-com $Move $SIn $SOut)  
    (timing (iterate-fixed-move $Move $SIn $SOut))
    (select-move $Move $SIn $SOut))


