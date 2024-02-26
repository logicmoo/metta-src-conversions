;
; 

;
; 

;
; 

;
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
    (print-state) 
    ( (print-board) 
      (move-count $Count) 
      (control $P) 
      (nl) 
      (write-pieces-in-hand player) 
      (write-pieces-in-hand opponent) 
      (format "~nMove Number: ~d~nControl: ~p~n" 
        (:: $Count $P)) 
      (print-stage) 
      (print-opponent-promotes) 
      (print-effect) 
      (print-movement)))
; 



  (= 
    (print-stage) 
    (det-if-then-else 
      (stage $S) 
      (verbosely-format "Stage: ~p~n" 
        (:: $S)) True))
; 


;
; 

;
; 

;
; 


  (= 
    (print-opponent-promotes) 
    (det-if-then-else 
      (opponent-promotes $OldPiece $Sq) 
      (, 
        (control $O) 
        (format "~p must promote: ~p on ~p~n" 
          (:: $O $OldPiece $Sq))) True))
; 



  (= 
    (print-movement) 
    (det-if-then-else 
      (moved-onto $Piece $Square) 
      (format "~p moved onto ~p~n" 
        (:: $Piece $Square)) True))
; 



  (= 
    (print-effect) 
    (det-if-then-else 
      (effects $Effect $Captured) 
      (, 
        (real-capture 
          (:: (captured $Effect $Captured)) Nil $String Nil) 
        (print-tokens $String)) True))
; 

	  


  (= 
    (print-captured) 
    (det-if-then-else 
      (setof 
        (@ $C $Sq) 
        (captured $C $Sq) $Caps) 
      (format "Captured: ~p~n" 
        (:: $Caps)) True))
; 




  (= 
    (pieces-in-hand $Player $Pieces) 
    (bagof $Piece 
      (in-hand $Piece $Player) $Pieces))
; 



  (= 
    (write-pieces-in-hand $Player) 
    (det-if-then-else 
      (pieces-in-hand $Player $Pieces) 
      (format "Pieces in ~p's hand: ~p~n" 
        (:: $Player $Pieces)) True))
; 



  (= 
    (print-board) 
    ( (current-board-size $X $Y) 
      (nl) 
      (print-squares-in-rows 1 $Y $X)))
; 




  (= 
    (print-hline 0) 
    ( (format "+~n" Nil) (set-det)))
; 

  (= 
    (print-hline $N) 
    ( (format "+---" Nil) 
      (is $N1 
        (- $N 1)) 
      (print-hline $N1)))
; 



  (= 
    (print-column-labels $Size) 
    (print-column-labels 1 $Size))
; 


  (= 
    (print-column-labels $X $Max) 
    ( (> $X $Max) 
      (set-det) 
      (nl)))
; 

  (= 
    (print-column-labels $C $Max) 
    ( (write   ) 
      (print-column-label $C) 
      (write ' ') 
      (is $C1 
        (+ $C 1)) 
      (print-column-labels $C1 $Max)))
; 



  (= 
    (print-column-label $C) 
    ( (nth-letter $C $Letter) (write $Letter)))
; 

	



;
; 

;
; 



  (= 
    (print-squares-in-rows $Min $Max $Size) 
    ( (is $Min1 
        (- $Min 1)) 
      (print-hline $Size) 
      (print-squares-in-rows $Max $Min1 $Max $Size)))
; 


  (= 
    (print-squares-in-rows $Min $Min $Max $Size) 
    ( (set-det) (print-column-labels $Size)))
; 

  (= 
    (print-squares-in-rows $_ $_ $_ 0) 
    (set-det))
; 

  (= 
    (print-squares-in-rows $Row $Min $Max $Size) 
    ( (print-squares-in-row $Row $Size) 
      (nl) 
      (print-hline $Size) 
      (is $Row1 
        (- $Row 1)) 
      (print-squares-in-rows $Row1 $Min $Max $Size)))
; 



  (= 
    (print-squares-in-row $Row $Size) 
    (print-squares-in-row $Row 1 $Size))
; 





  (= 
    (print-end-row $Row) 
    (format "| ~p" 
      (:: $Row)))
; 



  (= 
    (print-squares-in-row $Row $Current $Max) 
    ( (> $Current $Max) 
      (set-det) 
      (print-end-row $Row)))
; 

  (= 
    (print-squares-in-row $Row $Cur $Max) 
    ( (is $Cur1 
        (+ $Cur 1)) 
      (print-square $Cur $Row) 
      (print-squares-in-row $Row $Cur1 $Max)))
; 





  (= 
    (print-square $X $Y) 
    ( (write |) 
      (square $Square $X $Y) 
      (print-piece-on-square $Square)))
; 



;
; 

;
; 


  (= 
    (print-piece-on-square $Square) 
    ( (on $Piece $Square) 
      (ground $Piece) 
      (set-det) 
      (print-piece-or-empty $Piece $Square)))
; 

  (= 
    (print-piece-on-square $Square) 
    ( (question-marker $Square $Mark) (write $Mark)))
; 





  (= 
    (print-piece-or-empty $Piece $Square) 
    ( (piece-struct-name $Piece $Name) 
      (set-det) 
      (piece-struct-owner $Piece $Player) 
      (print-player-piece $Player $Name)))
; 

  (= 
    (print-piece-or-empty $Piece $Square) 
    (print-empty-square $Square))
; 



  (= 
    (print-empty-square $Square) 
    ( (square $Square $X $Y) 
      (parity $X $Y $Par) 
      (parity-marker $Par $Mark) 
      (write $Mark)))
; 




  (= 
    (parity $X $Y $Total) 
    (is $Total 
      (mod 
        (+ $X $Y) 2)))
; 


;
; 

;
; 



  (= 
    (parity_marker  0 ...) True)
; 

  (= 
    (parity_marker  1    ) True)
; 



  (= 
    (question_marker  $_  ? ) True)
; 




  (= 
    (print-colored player $Name) 
    (format " ~p " 
      (:: $Name)))
; 

  (= 
    (print-colored opponent $Name) 
    (format "*~p*" 
      (:: $Name)))
; 




  (= 
    (print-player-piece $Player $Piece) 
    ( (player-piece-print-name $Player $Piece $Name) (print-colored $Player $Name)))
; 



  (= 
    (player-piece-print-name $Player $P $Name) 
    ( (piece-print-name $P $PName) (name-for-player $Player $PName $Name)))
; 


;
; 

;
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
    (piece-print-name $P $PieceNum) 
    ( (bi-concat piece $PieceNum $P) 
      (number-chars $PieceNum $NumChars) 
      (set-det)))
; 

  (= 
    (piece-print-name $P $PieceNum) 
    ( (name $P 
        (Cons  $InitChar $Rest)) 
      (name $Letter 
        (:: $InitChar)) 
      (nth-letter $PieceNum $Letter)))
; 

	
;
; 

;
; 

;
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
    (name-for-player $Player $PieceNum $NewName) 
    ( (player-offset $Player $Off) 
      (is $OrdNum 
        (- 
          (+ $Off $PieceNum) 1)) 
      (name $NewName 
        (:: $OrdNum))))
; 

	



  (= 
    (player-offset player $O) 
    (name A 
      (:: $O)))
; 

  (= 
    (player-offset opponent $O) 
    (name A 
      (:: $O)))
; 


;
; 





