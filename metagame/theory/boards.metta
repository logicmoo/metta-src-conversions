;
; 

;
; 

;
; 

;
; 


;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (start-game) 
    (start-game $Game))
; 


  (= 
    (start-game $Game) 
    ( (initialize-state-properties) (create-board-and-setup $Game)))
; 




  (= 
    (default_init_move_count  0) True)
; 


  (= 
    (default_init_stage  assign) True)
; 


  (= 
    (default_init_control  player) True)
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
    (initialize-state-properties) 
    ( (default-init-stage $Stage) 
      (default-init-control $Player) 
      (default-init-move-count $Count) 
      (initialize-state-properties $Stage $Player $Count)))
; 


  (= 
    (initialize-state-properties $Stage $Player $Count) 
    ( (put-stage $Stage) 
      (put-control $Player) 
      (add (move-count $Count))))
; 




  (= 
    (create-board-and-setup $Game) 
    ( (initialize-board $Game) (create-initial-setup $Game)))
; 




  (= 
    (create-initial-setup $Game) 
    ( (game-has-assignments $Game $Ass) (do-assignments $Ass)))
; 



;
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
    (initialize-board $Game) 
    (make-empty-board))
; 



  (= 
    (make-empty-board) 
    ( (current-board $B) 
      (board-size $B $X $Y) 
      (array-squares $X $Y $Squares) 
      (make-empty $Squares)))
; 



  (= 
    (make_empty  ()) True)
; 

  (= 
    (make-empty (Cons  $Sq $Rest)) 
    ( (set-empty $Sq) (make-empty $Rest)))
; 


;
; 

;
; 

;
; 

;
; 

;
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
    (do-assignments $As) 
    ( (arbitrary-assignment $As) 
      (set-det) 
      (do-arbitrary-assignment $As)))
; 

  (= 
    (do-assignments $As) 
    ( (assignment-decision $As $Assigner $PieceNames $Squares) 
      (player $Assigner) 
      (set-det) 
      (invert 
        (= $Assigner $Squares) opponent 
        (= $OppAssigner $OppSquares)) 
      (do-assignments-for-player player $Assigner $PieceNames $Squares) 
      (do-assignments-for-player opponent $OppAssigner $PieceNames $OppSquares)))
; 



  (= 
    (assignment-decision $As $Assigner $PieceNames $Squares) 
    (decision $As $Assigner $PieceNames $Squares))
; 




  (= 
    (do-assignments-for-player $Owner $Placer $PieceNames $Squares) 
    ( (make-assignable-squares $Placer $Squares) (place-pieces-in-hand $PieceNames $Owner $Placer)))
; 



  (= 
    (make-assignable-squares $Player $Squares) 
    ( (det-if-then-else 
        (remove-symbol  &self 
          (assignable_squares  $Player $_)) True True) (add-symbol  &self (assignable_squares  $Player $Squares))))
; 


	

;
; 

;
; 

;
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
    (arbitrary_assignment  
      (Cons  $_ $_)) True)
; 

;
; 



  (= 
    (do-arbitrary-assignment $As) 
    (place-pieces-on-squares $As))
; 



  (= 
    (place-pieces-on-squares $Assign) 
    ( (uncollect $Assign $As) 
      (place-pieces-on-squares $As player) 
      (invert $As opponent $IAs) 
      (place-pieces-on-squares $IAs opponent)))
; 


  (= 
    (place_pieces_on_squares  () $_) True)
; 

  (= 
    (place-pieces-on-squares 
      (Cons  $A $As) $Player) 
    ( (assign-piece-to-square $A $Player) (place-pieces-on-squares $As $Player)))
; 



  (= 
    (assign-piece-to-square 
      (= $P $Sq) $Player) 
    ( (piece-struct $Piece $P $Player) (place-piece $Piece $Sq)))
; 



;
; 

;
; 

;
; 



  (= 
    (place_pieces_in_hand  () $Player $Hand) True)
; 

  (= 
    (place-pieces-in-hand 
      (Cons  $P $Ps) $Player $Hand) 
    ( (place-piece-in-hand $P $Player $Hand) (place-pieces-in-hand $Ps $Player $Hand)))
; 



  (= 
    (place-piece-in-hand $P $Player $Hand) 
    ( (piece-struct-name $Piece $P) 
      (owns $Piece $Player) 
      (put-in-hand $Piece $Hand)))
; 


;
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
    (connected $S1 $S2 $Dir) 
    ( (square $S1 $Xf $Yf) 
      (square $S2 $Xt $Yt) 
      (direction $Dir $DX $DY) 
      (current-board-type $T) 
      (conn-for-type $T 
        (, $Xf $Yf) 
        (, $Xt $Yt) 
        (, $DX $DY))))
; 



  (= 
    (conn-for-type planar $From $To $Dir) 
    (conn $From $To $Dir))
; 

  (= 
    (conn-for-type vertical-cylinder $From $To $Dir) 
    (conn-cyl $From $To $Dir))
; 



;
; 

;
; 

;
; 

;
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
    (conn 
      (, $Xf $Yf) 
      (, $Xt $Yt) 
      (, $Dx $Dy)) 
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



;
; 

;
; 

;
; 


  (= 
    (legal-location $Sq) 
    (on-board $Sq))
; 



  (= 
    (on-board (, $X $Y)) 
    ( (current-board-size $XMax $YMax) 
      (>= $X 1) 
      (=< $X $XMax) 
      (>= $Y 1) 
      (=< $Y $YMax)))
; 



;
; 

;
; 

;
; 




  (= 
    (conn-cyl 
      (, $Xf $Yf) 
      (, $Xt $Yt) 
      (, $Dx $Dy)) 
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




;
; 

;
; 

;
; 

;
; 


  (= 
    (legal-location-cyl 
      (, $X1 $Y1) 
      (, $X $Y)) 
    ( (current-board-size $XN $YN) 
      (is $X 
        (+ 
          (mod 
            (- 
              (+ $X1 $XN) 1) $XN) 1)) 
      (= $Y $Y1) 
      (on-board (, $X $Y))))
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
    (valid-min $N $N) 
    ( (number $N) (set-det)))
; 

  (= 
    (valid_min  $_ 1) True)
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
    (valid-max $N $_ $N) 
    ( (number $N) (set-det)))
; 

  (= 
    (valid-max $_ $Dir $Max) 
    ( (direction $Dir $Dx $Dy) 
      (is $XMag 
        (abs $Dx)) 
      (is $YMag 
        (abs $Dy)) 
      (valid-max-dir $XMag $YMag $Max)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (valid-max-dir 0 0 1) 
    (set-det))
; 

  (= 
    (valid-max-dir 0 $Dy $Max) 
    ( (set-det) 
      (current-board-size $XMax $YMax) 
      (is $Max 
        (// $YMax $Dy))))
; 

  (= 
    (valid-max-dir $Dx 0 $Max) 
    ( (set-det) 
      (current-board-type $Type) 
      (current-board-size $XMax $YMax) 
      (max-leaps $Type $XMax $Dx $Max)))
; 

  (= 
    (valid-max-dir $Dx $Dy $Max) 
    ( (current-board-type vertical-cylinder) 
      (set-det) 
      (current-board-size $XMax $YMax) 
      (is $Max 
        (// $YMax $Dy))))
; 

  (= 
    (valid-max-dir $Dx $Dy $Max) 
    ( (current-board-size $Bx $By) 
      (is $XMax 
        (// $Bx $Dx)) 
      (is $YMax 
        (// $By $Dy)) 
      (min $XMax $YMax $Max)))
; 




  (= 
    (max-leaps planar $BMax $Delta $Max) 
    ( (set-det) (is $Max (// $BMax $Delta))))
; 

  (= 
    (max-leaps vertical-cylinder $BMax $Delta $Max) 
    (wrap-leaps $BMax $Delta $Max))
; 

	



  (= 
    (wl $A $B $C) 
    (wrap-leaps $A $B $C))
; 



  (= 
    (wrap-leaps $Board $D $Max) 
    ( (gcf $Board $D $F) 
      (max $Board $D $M) 
      (is $Max 
        (// $M $F))))
; 


;
; 

;
; 


  (= 
    (gcf $A 0 $A) 
    (set-det))
; 

  (= 
    (gcf $A $B $F) 
    ( (is $M 
        (mod $A $B)) (gcf $B $M $F)))
; 



