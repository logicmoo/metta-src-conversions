;                                           (convert-to-metta-file  boards $_318114 metagame/theory/boards.pl metagame/theory/boards.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ; boards.pl

;
; ================================================================================
;
; Starting Game
;
; ================================================================================
;
; Initialize the board (create all the empty squares),
;
; set the initial stage to be assign, which will terminate
;
; if there is no assignment stage for the game.
;
; The game starts with PLAYER in control, and MOVE_COUNT = 0.
;
; The initial setup is determined, possibly using a random
;
; setup as returned from the hook: random_assignments(Assignments).
;
; (See parse.pl)
 

;
; ----------
;
; START_GAME
;
; ----------
;
; Passes a dummy variable to start_game/1, as for now the current
;
; game is passed through the asserted procedures:
;
; player_current_game/1
;
; opponent_current_game/1
;
; 
;
; This is usually achieved earlier by calling the predicate:
;
; file_make_test_game/1
;
; with a file in which a game is stored.
;
; A cleaner verion would pass the game as a variable too, and the dummy
;
; variable reminds us of this!


  (= start-game  
    (start-game $Game))

  (= (start-game $Game)  
    (initialize-state-properties (create-board-and-setup $Game)))



  (= (default-init-move-count  0)  True)

  (= (default-init-stage  assign)  True)

  (= (default-init-control  player)  True)

;
; initialize_state_properties :-
;
; put_stage(assign),
;
; put_control(player),
;
; set_initial_move_count.


  (= initialize-state-properties  
    (default-init-stage $Stage)
    (default-init-control $Player)
    (default-init-move-count $Count)
    (initialize-state-properties $Stage $Player $Count))

  (= (initialize-state-properties $Stage $Player $Count)  
    (put-stage $Stage)
    (put-control $Player)
    (add (move-count $Count)))



  (= (create-board-and-setup $Game)  
    (initialize-board $Game)
    (create-initial-setup $Game))



  (= (create-initial-setup $Game)  
    (game-has-assignments $Game $Ass)
    (do-assignments $Ass))


;
; ================================================================================
;
; Initial Generation of Board
;
; ================================================================================
;
; A board is a set of squares, initially empty, and a connectivity
;
; relation between them.


  (= (initialize-board $Game)  make-empty-board)


  (= make-empty-board  
    (current-board $B)
    (board-size $B $X $Y)
    (array-squares $X $Y $Squares)
    (make-empty $Squares))


  (= (make-empty  ())  True)
  (= (make-empty (Cons  $Sq $Rest))  
    (set-empty $Sq)
    (make-empty $Rest))

;
; ================================================================================
;
; Determining initial setup:
;
; ================================================================================
;
; a. arbitrary, put the pieces on the squares.
;
; b. (player) assigns: Put the pieces in player's hands,
;
; and assert that player can assign to the specified squares.
;
; (c. random: by this point, the initial assignment is already determined by
;
; an external source. It will thus look like an arbitrary assignment,
;
; so we need no special case.)

  (= (do-assignments $As)  
    (arbitrary-assignment $As)
    (set-det)
    (do-arbitrary-assignment $As))
  (= (do-assignments $As)  
    (assignment-decision $As $Assigner $PieceNames $Squares)
    (player $Assigner)
    (set-det)
    (invert 
      (= $Assigner $Squares) opponent 
      (= $OppAssigner $OppSquares))
    (do-assignments-for-player player $Assigner $PieceNames $Squares)
    (do-assignments-for-player opponent $OppAssigner $PieceNames $OppSquares))


  (= (assignment-decision $As $Assigner $PieceNames $Squares)  
    (decision $As $Assigner $PieceNames $Squares))



  (= (do-assignments-for-player $Owner $Placer $PieceNames $Squares)  
    (make-assignable-squares $Placer $Squares)
    (place-pieces-in-hand $PieceNames $Owner $Placer))


  (= (make-assignable-squares $Player $Squares)  
    ( (det-if-then-else 
        (remove-atom  &self 
          (assignable-squares  $Player $_)) True True) (add-atom  &self (assignable-squares  $Player $Squares))))

	

;
; Arbitary can be:
;
; 1. a list of assignments of pieces to squares.
;
; This is the general case for symmetric opening games.
;
; 2. a structure assign(A1,A2), where A1 is an assignment
;
; for player, and A2 for opponent.
;
; This is only used for interface setup, at the moment.
;
; (Actually, we don't use this at all yet).

  (= (arbitrary-assignment  ([|]  $_ $_))  True)
;
; arbitrary_assignment(assign(A1,A2)).


  (= (do-arbitrary-assignment $As)  
    (place-pieces-on-squares $As))


  (= (place-pieces-on-squares $Assign)  
    (uncollect $Assign $As)
    (place-pieces-on-squares $As player)
    (invert $As opponent $IAs)
    (place-pieces-on-squares $IAs opponent))
; ; from generator/gen.pl

  (= (place-pieces-on-squares  () $_)  True)
  (= (place-pieces-on-squares (Cons  $A $As) $Player)  
    (assign-piece-to-square $A $Player)
    (place-pieces-on-squares $As $Player))


  (= (assign-piece-to-square (= $P $Sq) $Player)  
    (piece-struct $Piece $P $Player)
    (place-piece $Piece $Sq))


;
; place_pieces_in_hand(P,Player,Hand)
;
; Player owns the pieces.
;
; Hand will assign them.


  (= (place-pieces-in-hand  () $Player $Hand)  True)
  (= (place-pieces-in-hand (Cons  $P $Ps) $Player $Hand)  
    (place-piece-in-hand $P $Player $Hand)
    (place-pieces-in-hand $Ps $Player $Hand))


  (= (place-piece-in-hand $P $Player $Hand)  
    (piece-struct-name $Piece $P)
    (owns $Piece $Player)
    (put-in-hand $Piece $Hand))

;
; ================================================================================
;
; Board Connectivity
;
; ================================================================================
;
; Convert squares and directions as generated to simple ordered pairs.
;
; Really need Game as an arg here, to decide vertical and board size.

  (= (connected $S1 $S2 $Dir)  
    (square $S1 $Xf $Yf)
    (square $S2 $Xt $Yt)
    (direction $Dir $DX $DY)
    (current-board-type $T)
    (conn-for-type $T 
      (, $Xf $Yf) 
      (, $Xt $Yt) 
      (, $DX $DY)))


  (= (conn-for-type planar $From $To $Dir)  
    (conn $From $To $Dir))
  (= (conn-for-type vertical-cylinder $From $To $Dir)  
    (conn-cyl $From $To $Dir))


;
; The structure of the board is represented by the connected relation that
;
; defines how each location (Xf,Yf) on the board is connected to each
;
; other location (Xt,Yt) via a vector (Dx,Dy) defined as:
;
; Xt = Xf + Dx,
;
; Yt = Yf + Dy
;
; Note: this is intended to be an ``operational'' predicate:
;
; This predicate is used for all generated games.
;
; (Thanks to Nick Flann for this predicate).

  (= (conn (, $Xf $Yf) (, $Xt $Yt) (, $Dx $Dy))  
    (| 
      (det-if-then 
        (, 
          (var $Xf) 
          (var $Yf)) 
        (, 
          (is $Xf 
            (- $Xt $Dx)) 
          (is $Yf 
            (- $Yt $Dy)) 
          (legal-location (, $Xf $Yf)))) 
      (| 
        (det-if-then 
          (, 
            (var $Xt) 
            (var $Yt)) 
          (, 
            (is $Xt 
              (+ $Xf $Dx)) 
            (is $Yt 
              (+ $Yf $Dy)) 
            (legal-location (, $Xt $Yt)))) 
        (| 
          (det-if-then 
            (, 
              (var $Dx) 
              (var $Dy)) 
            (, 
              (is $Dx 
                (- $Xt $Xf)) 
              (is $Dy 
                (- $Yt $Yf)))) 
          (det-if-then otherwise 
            (, 
              (is $Xf 
                (- $Xt $Dx)) 
              (is $Yf 
                (- $Yt $Dy))))))))


;
; This pred. is instantiated for the board dimensions chosen
;
; in the game.
;
; Make max and min specific constants.

  (= (legal-location $Sq)  
    (on-board $Sq))


  (= (on-board (, $X $Y))  
    (current-board-size $XMax $YMax)
    (>= $X 1)
    (=< $X $XMax)
    (>= $Y 1)
    (=< $Y $YMax))


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Cylindrical Boards
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  (= (conn-cyl (, $Xf $Yf) (, $Xt $Yt) (, $Dx $Dy))  
    (| 
      (det-if-then 
        (, 
          (var $Xf) 
          (var $Yf)) 
        (, 
          (is $Xf1 
            (- $Xt $Dx)) 
          (is $Yf1 
            (- $Yt $Dy)) 
          (legal-location-cyl 
            (, $Xf1 $Yf1) 
            (, $Xf $Yf)))) 
      (| 
        (det-if-then 
          (, 
            (var $Xt) 
            (var $Yt)) 
          (, 
            (is $Xt1 
              (+ $Xf $Dx)) 
            (is $Yt1 
              (+ $Yf $Dy)) 
            (legal-location-cyl 
              (, $Xt1 $Yt1) 
              (, $Xt $Yt)))) 
        (| 
          (det-if-then 
            (, 
              (var $Dx) 
              (var $Dy)) 
            (, 
              (is $Dx 
                (- $Xt $Xf)) 
              (is $Dy 
                (- $Yt $Yf)))) 
          (det-if-then otherwise 
            (, 
              (is $Xf1 
                (- $Xt $Dx)) 
              (is $Yf1 
                (- $Yt $Dy)) 
              (legal-location-cyl 
                (, $Xf1 $Yf1) 
                (, $Xf $Yf))))))))



;
; This pred. is instantiated for the board dimensions chosen
;
; in the game.
;
; This won't work backwards to generate a from square.
;
; Add an XN to cover 1 negative wrap, as mod undef for X<0.

  (= (legal-location-cyl (, $X1 $Y1) (, $X $Y))  
    (current-board-size $XN $YN)
    (is $X 
      (+ 
        (mod 
          (- 
            (+ $X1 $XN) 1) $XN) 1))
    (= $Y $Y1)
    (on-board (, $X $Y)))




;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Scaling # leaps to until it is off the board or wraps
;
; back to original square (for vertical cylinder boards).
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (valid-min $N $N)  
    (number $N)
    (set-det))
  (= (valid-min  $_ 1)  True)


;
; VALID_MAX(Constraint,Dir,Max)
;
; If Constraint already a number, return it.
;
; Otherwise, find the constraint based on the
;
; direction and the board.

  (= (valid-max $N $_ $N)  
    (number $N)
    (set-det))
  (= (valid-max $_ $Dir $Max)  
    (direction $Dir $Dx $Dy)
    (is $XMag 
      (abs $Dx))
    (is $YMag 
      (abs $Dy))
    (valid-max-dir $XMag $YMag $Max))

;
; VALID_MAX_DIR(+XMag,+YMag,-Max)
;
; Given a magnitude for X and Y offsets
;
; of each leap, MAX is the number of
;
; leaps which need be considered on the board.
;
; 
;
; If both are 0, only 1 leap is necessary, max!
;
; If X is 0, it's just the number of leaps to
;
; cross the board to last rank.
;
; If Y is 0, use the max leaps to cross the
;
; X axis, which varies on the type of board.
;
; If neither 0 but on a cylinder board, the Y axis
;
; is the constraint.
;
; Else, the max is the min of the maxes on both axes.

  (= (valid-max-dir 0 0 1)  
    (set-det))
  (= (valid-max-dir 0 $Dy $Max)  
    (set-det)
    (current-board-size $XMax $YMax)
    (is $Max 
      (// $YMax $Dy)))
  (= (valid-max-dir $Dx 0 $Max)  
    (set-det)
    (current-board-type $Type)
    (current-board-size $XMax $YMax)
    (max-leaps $Type $XMax $Dx $Max))
  (= (valid-max-dir $Dx $Dy $Max)  
    (current-board-type vertical-cylinder)
    (set-det)
    (current-board-size $XMax $YMax)
    (is $Max 
      (// $YMax $Dy)))
  (= (valid-max-dir $Dx $Dy $Max)  
    (current-board-size $Bx $By)
    (is $XMax 
      (// $Bx $Dx))
    (is $YMax 
      (// $By $Dy))
    (min $XMax $YMax $Max))



  (= (max-leaps planar $BMax $Delta $Max)  
    (set-det)
    (is $Max 
      (// $BMax $Delta)))
  (= (max-leaps vertical-cylinder $BMax $Delta $Max)  
    (wrap-leaps $BMax $Delta $Max))
	



  (= (wl $A $B $C)  
    (wrap-leaps $A $B $C))


  (= (wrap-leaps $Board $D $Max)  
    (gcf $Board $D $F)
    (max $Board $D $M)
    (is $Max 
      (// $M $F)))

;
; gcf(HighestCurrentFactor,CurrentRemainder,GCF).
;
; This is the Euclidan algorithm.

  (= (gcf $A 0 $A)  
    (set-det))
  (= (gcf $A $B $F)  
    (is $M 
      (mod $A $B))
    (gcf $B $M $F))


