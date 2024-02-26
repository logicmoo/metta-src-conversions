;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (possess-value $Piece $Player 
      (advice possess $Sq $Value) $Position $Tables) 
    ( (total-square-count $Total) 
      (empty $Sq $Position) 
      (local-evaluation $Piece $Sq $LVal $Position $Tables) 
      (is $Value 
        (/ $LVal $Total))))
; 




  (= 
    (initprom-value 
      (advice initprom 
        (best $OldPiece $NewPiece) $Value) $Position $Tables) 
    ( (opponent-promotes $OldPiece $Sq $Position) 
      (control $Player $Position) 
      (best-choice $OldPiece $Player $Sq $NewPiece $Value $Position $Tables)))
; 



  (= 
    (best-choice $OldPiece $Player $Sq $NewPiece $Value $Position $Tables) 
    ( (findall 
        (- $NewVal $NewPiece) 
        (initprom-value $OldPiece $Sq $Player $NewPiece $NewVal $Position $Tables) $Pairs) (best-player-choice $Player $Pairs $NewPiece $Value)))
; 
 


  (= 
    (best-player-choice $Player $Pairs $NewPiece $Value) 
    ( (keysort-for-player $Player $Pairs $Ordered) (= $Ordered (Cons  (- $Value $NewPiece) $Rest))))
; 
 


;
; 

;
; 

;
; 


  (= 
    (keysort-for-player $Player $Pairs $Ordered) 
    ( (keysort $Pairs $Sorted) (reverse-for-player $Player $Sorted $Ordered)))
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
    (reverse_for_player  opponent $Sorted $Sorted) True)
; 

  (= 
    (reverse-for-player player $Sorted $Ordered) 
    (reverse $Sorted $Ordered))
; 
 


;
; 

;
; 


  (= 
    (initprom-value $OldPiece $Sq $Player $NewPiece $NewVal $Position $Tables) 
    ( (init-promote-option $OldPiece $Player $NewPiece $Position) (local-evaluation $NewPiece $Sq $NewVal $Position $Tables)))
; 



;
; 

;
; 

;
; 

;
; 


;
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
    (possess-offset $N) 
    (parameter possess-offset $N))
; 
 



  (= 
    (favor-possess $Player $Val1 $Value $Position) 
    ( (possess-offset $Offset) (is $Value (* $Val1 $Offset))))
; 
 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (estimate-possess-value $Possessor $Piece $LVal $EVal $Position $Tables) 
    (det-if-then-else 
      (owns $Piece $Possessor) 
      (= $EVal $LVal) 
      (is $EVal 
        (- $LVal))))
; 
 
	

