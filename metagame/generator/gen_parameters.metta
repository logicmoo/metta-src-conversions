;                                           (convert-to-metta-file  gen_parameters $_135208 metagame/generator/gen_parameters.pl metagame/generator/gen_parameters.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; gen_parameters.pl
;
; Defines the parameters used in the chess-like game generator.


;
; board_size: Avg. size of square board. Chess has 8 squares.
;
; board_crowding: Fraction of board to use for placing initial arrays.
;
; Chess uses 1/2 the board
;
; row_crowding: Fraction of each row in init array to fill. Chess fills
;
; the rows entirely.
;
; piece_variety: Of possible pieces (# used array locations), fraction which
;
; will be unique piece types.
;
; 
;
; Thus, the number of unique piece-types used in a game will be:
;
; (board_size^2/2)*board_crowding*row_crowding*piece_variety


  !(dynamic (/ forced-gen-parameter 2))
  !(dynamic (/ gen-parameter 2))


  (= (gen-parameter  board_size (range  5 6))  True)

;
; board_crowding: Fraction of board to use for placing initial arrays.
;
; Chess uses 1/2 the board (4/8 rows)
;
; Checkers uses 3/4 the board (6/8 rows)
;
; Correct to mean distribution later.
  (= (gen-parameter  board_crowding (distribution  ((= 0.5  0.7) (= 0.3  0.15) (= 0.7  0.15))))  True)

;
; row_crowding: what fraction of squares to place pieces on in initial array.
;
; Correct to mean distribution eventually.
  (= (gen-parameter  row_crowding (distribution  ((= 1.0  0.7) (= 0.7  0.3))))  True)


;
; Fraction of initial array rows used for promotion,
;
; where smaller fraction means farther distance to promotion.
  (= (gen-parameter  promotion_fraction (distribution  ((= 1.0  0.3) (= 0.8  0.1) (= 0.6  0.1) (= 0.4  0.2) (= 0.2  0.2) (= 0  0.1))))  True)

;
; Of a possible number of pieces, what fraction should be unique.
;
; (Checkers has low variety, chess has high, shogi even higher).
  (= (gen-parameter  piece_variety (distribution  ((= 1.0  0.4) (= 0.8  0.2) (= 0.6  0.2) (= 0.4  0.2) (= 0.2  0.0))))  True)


;
; Number of unique piece types, which will
;
; only be seen via promotion (i.e. they are not placed on
;
; the initial board.
;
; Note this gen_parameter is independent of board size.
;
; 
  (= (gen-parameter  promote_only_pieces (range  1 3))  True)



;
; placement_method: How the init. config is determined.
;
; Random --> randomly determined before each game.
;
; Arbitrary --> randomly determined before first game, then fixed.
;
; Player --> each player alternates deciding his placement.
;
; Opponent --> each player alternates deciding opponent's placement.
;
; 
;
; So arbitrary is most pre-structured, random is least, decision
;
; is midway.

  (= (gen-parameter  placement_method (distribution  ((= random  0.2) (= arbitrary  0.6) (= player  0.1) (= opponent  0.1))))  True)


;
; Probability to choose planar vs. cylinder boards.
  (= (gen-parameter  board_type (distribution  ((= planar  0.9) (= vertical_cylinder  0.1))))  True)

;
; Probability to choose forward vs. diagonal board inversion.
  (= (gen-parameter  board_inversion (distribution  ((= forward  0.5) (= diagonal  0.5))))  True)




;
; Prob. of choosing each kind of symmetry (independent).
  (= (gen-parameter  (symmetry  rotation) 0.9)  True)
  (= (gen-parameter  (symmetry  forward) 0.9)  True)
  (= (gen-parameter  (symmetry  side) 0.9)  True)

;
; Complexity of pieces

;
; Upper bound on fraction of board leapers should traverse
;
; each step.
  (= (gen-parameter  locality (range  0.1 0.8))  True)



;
; What fraction of movements should be leapers, riders, and hoppers.
  (= (gen-parameter  movement_type (distribution  ((= leaper  0.4) (= rider  0.4) (= hopper  0.2))))  True)

;
; Whether riders must make the longest ride (continue riding as long as possible).
  (= (gen-parameter  must_ride 0.2)  True)

;
; Whether to constrain part of the description of a hopper.
;
; (Each component determined independently)
  (= (gen-parameter  (constrain  (hopper  before $_)) 0.5)  True)
  (= (gen-parameter  (constrain  (hopper  over $_)) 0.5)  True)
  (= (gen-parameter  (constrain  (hopper  after $_)) 0.5)  True)

;
; A hopper can hop 0..Max squares before a piece.
  (= (gen-parameter  (hopper  before $Max) (range  0 $Max))  True)

;
; A hopper can hop over 1..Max pieces.
  (= (gen-parameter  (hopper  over $Max) (range  1 $Max))  True)

;
; A hopper can hop 1..Max squares after the last piece.
  (= (gen-parameter  (hopper  after $Max) (range  1 $Max))  True)


;
; When need a comparison term, which one to use.
  (= (gen-parameter  comparative (distribution  ((= eq  0.5) (= geq  0.2) (= leq  0.3))))  True)



;
; Prob. of continuing to add piece_definitions to a piece_type.
;
; So X; chance of piece having >1 method of moving/capturing.
  (= (gen-parameter  movement_complexity 0.2)  True)
  (= (gen-parameter  capture_complexity 0.2)  True)
  (= (gen-parameter  goal_complexity 0.6)  True)  


;
; Independent Prob. of assigning each type of capturing method to
;
; a piece (they can have multiple powers).
;
; Hopping power can only be attached when the capture_movement
;
; is hopping already.
 
  (= (gen-parameter  (capture-method  retrieve) 0.2)  True)
  (= (gen-parameter  (capture-method  clobber) 0.9)  True) 
  (= (gen-parameter  (capture-method  hop) 0.5)  True)


  (= (gen-parameter  capture_effect (distribution  ((= remove  0.5) (= (possess  player)  0.3) (= (possess  opponent)  0.2))))  True)


;
; Whether to use any_player or a particular
;
; player in general piece descriptions (ie. for capturing and hopping).

  (= (gen-parameter  player_generalization_level (distribution  ((= any  0.2) (= specific  0.8))))  True)

;
; Whether to use any_piece or a particular
;
; set of pieces in piece descriptions.
;
; The more general, the less constrained is the game,
;
; as all interactions apply to more objects.

  (= (gen-parameter  piece_generalization_level (distribution  ((= any  0.5) (= specific  0.5))))  True)

;
; If true, adds some pieces to set in a description.
;
; Thus, a high setting will have more genereral piece
;
; descriptions (when the specific option is selected
;
; by piece_generalization_level above).

  (= (gen-parameter  more_pieces 0.8)  True)

  (= (gen-parameter  more_general_pieces 0.8)  True)



;
; Whether a piece must capture if it can.

  (= (gen-parameter  must_capture 0.3)  True)

;
; Whether you can continue capturing with a piece once you've captured
;
; something.

  (= (gen-parameter  continue_captures 0.1)  True)


;
; For additional goals besides stalemate, the probability of adding
;
; eradicate or arrive goals.

  (= (gen-parameter  goal_type (distribution  ((= eradicate  0.5) (= arrive  0.5))))  True)

;
; Whether an arrival goal is to arrive player's piece,
;
; opponent's piece, or either player's piece on a square.
;
; In goals, having any_player too often makes the game a draw,
;
; as arrival or eradicate goals predicated on any_player
;
; are always achieved for both players together.
;
; Thus these any_player options have now been removed.

  (= (gen-parameter  arrive_goal_player (distribution  ((= player  0.5) (= opponent  0.5))))  True)

;
; If true, adds more pieces to those which are in the
;
; arrival goal for a player.
;
; Thus, a high setting will make a given arrival goal
;
; easier to achieve.

  (= (gen-parameter  more_arrival_pieces 0.8)  True)


;
; Whether an eradicate goal is to eradicate player's piece
;
; or opponent's piece.
  (= (gen-parameter  eradicate_goal_player (distribution  ((= player  0.2) (= opponent  0.8))))  True)

;
; If true, adds more pieces to those which are in the
;
; eradicate goal for a player.
;
; Thus, a high setting will make a given eradicate goal
;
; *harder* to achieve.

  (= (gen-parameter  more_eradicate_pieces 0.8)  True)


;
; Whether to use any_piece or a particular
;
; set of pieces in a given eradicate goal.
;
; The more general, the more pieces will need to be
;
; eradicated, and thus the goal becomes harder.

  (= (gen-parameter  eradicate_generalization_level (distribution  ((= any  0.2) (= specific  0.8))))  True)



;
; The probability of having a piece promote to exactly 1 type of piece,
;
; or having a piece promote as a decision of one of the players.

  (= (gen-parameter  specific_promotion 0.4)  True)


;
; The method of promoting a given piece, if it is not arbitrary.

  (= (gen-parameter  promotion_method (distribution  ((= arbitrary  0.3) (= player  0.5) (= opponent  0.1))))  True)


;
; ================================================================================
;
; CHOOSE_PARAMETER(+Name,-Value)
;
; Still must include option for Mean,Std.

  (= (choose-parameter $Name $Value)  
    (forced-gen-parameter $Name 
      (distribution $Dist))
    (set-det)
    (sample-from-distribution 
      (distribution $Dist) $Value))
  (= (choose-parameter $Name $Value)  
    (gen-parameter $Name 
      (distribution $Dist))
    (set-det)
    (sample-from-distribution 
      (distribution $Dist) $Value))
  (= (choose-parameter $Name $Value)  
    (gen-parameter $Name 
      (range $Min $Max))
    (set-det)
    (sample-from-range 
      (range $Min $Max) $Value))
  (= (choose-parameter $Name $Value)  
    (gen-parameter $Name $Int)
    (integer $Int)
    (set-det)
    (adjust-mean $Int $Value))
  (= (choose-parameter $Name $Value)  
    (gen-parameter $Name $Prob)
    (sample-from-distribution 
      (distribution (:: (= yes $Prob) (= no 1))) $Value))


  (= (choose-parameter $Name)  
    (choose-parameter $Name yes))



  (= (block-parameter $Name $Items)  
    ( (block-distribution $Name $Items $Dist) (add-atom  &self (forced-gen-parameter  $Name (distribution  $Dist)))))


  (= (unblock-parameter $Name)  
    (remove-atom  &self 
      (forced-gen-parameter  $Name 
        (distribution  $Dist))))


  (= reset-gen-parameters  
    (remove-all-atoms  &self 
      (forced-gen-parameter  $_ $_)))



  (= (set-gen-parameter $P $V)  
    (det-if-then-else 
      (remove-atom  &self 
        (gen-parameter  $P $_)) 
      (add-atom  &self 
        (gen-parameter  $P $V)) 
      (det-if-then otherwise 
        (trace-output 'Unknown generator parameter ~p!~n' 
          (:: $P)))))



;
; Removes the possible events in ITEMS from
;
; appearing in distribution for gen_parameter NAME,
;
; renormalizing, and saving as a blocked distribution.

  (= (block-distribution $Name $Items $Dist)  
    (gen-parameter $Name 
      (distribution $Dist0))
    (set-det)
    (remove-items $Items $Dist0 $Prob $Dist1)
    (is $P1 
      (/ 1 
        (- 1 $Prob)))
    (renormalize $Dist1 $P1 $Dist))


  (= (remove-items  () $D 0 $D)  True)
  (= (remove-items (Cons  $I $Is) $Dist1 $Prob $Dist)  
    (remove-item $I $Dist1 $Prob1 $Dist2)
    (remove-items $Is $Dist2 $Prob2 $Dist)
    (is $Prob 
      (+ $Prob1 $Prob2)))


  (= (remove-item  $I ([|]  (= $I  $P) $Dist) $P $Dist)  True)
  (= (remove-item $I (Cons  $H $T) $P (Cons  $H $Dist))  
    (remove-item $I $T $P $Dist))


  (= (renormalize  () $_ ())  True)
  (= (renormalize (Cons  (= $H $P) $Rest) $Ratio (Cons  (= $H $P1) $Rest1))  
    (is $P1 
      (* $P $Ratio))
    (renormalize $Rest $Ratio $Rest1))


;
; For now, just return it.

  (= (adjust-mean  $Int $Int)  True)


;
; ==============================================================================
;
; Printing generator parameters
;
; ==============================================================================


  (= show-gen-parameters  
    (whenever 
      (gen-parameter $Name $Val) 
      (, 
        (portray-param (gen-parameter $Name $Val)) nl))
    (getrand $R)
    (format '~nrandom seed = ~p~n' 
      (:: $R)))


  (= (portray-param (gen-parameter $Name $Val))  
    (format "<~p> --> ~p~n" 
      (:: $Name $Val)))


  (= (portray-range (range $Min $Max))  
    (format "[~p .. ~p]" 
      (:: $Min $Max)))


  (= (portray-dist (distribution $Pairs))  
    (portray-pairs $Pairs))


  (= (portray-pairs  ())  True)
  (= (portray-pairs (Cons  $Pair $Pairs))  
    (portray-pair $Pair)
    (portray-pairs $Pairs))


  (= (portray-pair (= $Event $Val))  
    (format "\n     ~p: ~p" 
      (:: $Event $Val)))


  !(add-portrayals (:: portray-param portray-range portray-dist))

;
; ==============================================================================
;
; Interface for changing generator parameters
;
; ==============================================================================


  (= (change-gen-param $Name)  
    (read-new-gen-value $Name $New)
    (set-gen-parameter $Name $New))


  (= (read-new-gen-value $Name $New)  
    (gen-parameter $Name $Value)
    (read-gen-value $Name $Value $New))


  (= (read-gen-value $Name $Value $New)  
    (format "Enter new settings for parameter <~p>:~n" 
      (:: $Name))
    (format "Old parameter setting: ~p~n" 
      (:: $Value))
    (read-gen-value $Value $New)
    (format "New parameter setting: ~p~n" 
      (:: $New)))


  (= (read-gen-value (distribution $Dist) (distribution $New))  
    (set-det)
    (read-dist $Dist $New))
  (= (read-gen-value (range $Min1 $Max1) (range $Min $Max))  
    (set-det)
    (read-range $Min1 $Max1 $Min $Max))
  (= (read-gen-value $Old $New)  
    (read-new-val new $Old $New))



  (= (read-dist  () ())  True)
  (= (read-dist (Cons  (= $Event $Prob1) $Rest1) (Cons  (= $Event $Prob2) $Rest2))  
    (read-event $Event $Prob1 $Prob2)
    (read-dist $Rest1 $Rest2))


  (= (read-event $Event $Old $New)  
    (format "Prob for <~p> (~p): " 
      (:: $Event $Old))
    (read $New1)
    (new-event-val $Event $Old $New1 $New))


  (= (new-event-val $Event $Old $New1 $New)  
    (new-val $Old $New1 $New))
	

  (= (new-val $Old $New1 $New)  
    (det-if-then-else 
      (= $New1 z) 
      (= $New $Old) 
      (= $New $New1)))



  (= (read-range $Min1 $Max1 $Min $Max)  
    (read-new-val min $Min1 $Min)
    (read-new-val max $Max1 $Max))


  (= (read-new-val $Name $Old $New)  
    (format "<~p> value (~p): " 
      (:: $Name $Old))
    (read $New1)
    (new-val $Old $New1 $New))





