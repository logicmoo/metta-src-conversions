;                                           (convert-to-metta-file  possess $_315286 metagame/learning/possess.pl metagame/learning/possess.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; possess.pl
;
; 
;
; A possessed piece gets some amount of value based on
;
; the value of each square it could possibly be placed
;
; on.
;
; Here we divide the potential value by the size of the board,
;
; to roughly average the contributions from each square.
;
; One consequence of this is that having a piece in hand is never
;
; better than having it on the board on its best square.
;
; This is incorrect, but a start anyway.

  (= (possess-value $Piece $Player (advice possess $Sq $Value) $Position $Tables)  
    (total-square-count $Total)
    (empty $Sq $Position)
    (local-evaluation $Piece $Sq $LVal $Position $Tables)
    (is $Value 
      (/ $LVal $Total)))



  (= (initprom-value (advice initprom (best $OldPiece $NewPiece) $Value) $Position $Tables)  
    (opponent-promotes $OldPiece $Sq $Position)
    (control $Player $Position)
    (best-choice $OldPiece $Player $Sq $NewPiece $Value $Position $Tables))


  (= (best-choice $OldPiece $Player $Sq $NewPiece $Value $Position $Tables)  
    (findall 
      (- $NewVal $NewPiece) 
      (initprom-value $OldPiece $Sq $Player $NewPiece $NewVal $Position $Tables) $Pairs)
    (best-player-choice $Player $Pairs $NewPiece $Value)) 


  (= (best-player-choice $Player $Pairs $NewPiece $Value)  
    (keysort-for-player $Player $Pairs $Ordered)
    (= $Ordered 
      (Cons  
        (- $Value $NewPiece) $Rest))) 


;
; Pairs is some list [v1-item1,...]. Ordered is a sorted
;
; version, such that first new v1 is best choice for Player
;
; (min if black, max if white).

  (= (keysort-for-player $Player $Pairs $Ordered)  
    (keysort $Pairs $Sorted)
    (reverse-for-player $Player $Sorted $Ordered))

;
; The sorted list gives the minimum node first.
;
; We assume the black player is trying to minimize the evaluation,
;
; so he just chooses this first (lowest) choice.
;
; The white player is maximizing, so he will choose the last (highest) choice.

  (= (reverse-for-player  opponent $Sorted $Sorted)  True)
  (= (reverse-for-player player $Sorted $Ordered)  
    (reverse $Sorted $Ordered)) 


;
; Init_promote_option is defined as part of the theory, legal.pl
;
; 

  (= (initprom-value $OldPiece $Sq $Player $NewPiece $NewVal $Position $Tables)  
    (init-promote-option $OldPiece $Player $NewPiece $Position)
    (local-evaluation $NewPiece $Sq $NewVal $Position $Tables))


;
; ================================================================================
;
; Routines used to estimate effect of possessing a piece when
;
; thinking about threats.
;
; ================================================================================

;
; Predict a piece is N times as valuable possessed as it would be
;
; on the board.
;
; possess_offset(2).
;
; Actually there is more to it than we use here. If we possess-capture
;
; opponent's piece, it is

  (= (possess-offset $N)  
    (parameter possess-offset $N)) 



  (= (favor-possess $Player $Val1 $Value $Position)  
    (possess-offset $Offset)
    (is $Value 
      (* $Val1 $Offset))) 


;
; ESTIMATE_POSSESS_VALUE(+Possessor,+Piece,+LVal,-EVal,+Position,+Tables).
;
; EValue is the estimated value accrued to Possessor if he possesses
;
; Piece, given that in Position, Piece was worth LVal.
;
; We would like to use possess_value above, but that is too expensive
;
; to use every time we need this estimate.
;
; 
;
; Thus: if Possessor already owns piece, then say it is just worth the
;
; original value it had on the board.
;
; If not, there will be a change of possession, so say it is worth the
;
; opposite of its current value.
;
; We could use static value, value of special arrival squares, etc.
;
; 

  (= (estimate-possess-value $Possessor $Piece $LVal $EVal $Position $Tables)  
    (det-if-then-else 
      (owns $Piece $Possessor) 
      (= $EVal $LVal) 
      (is $EVal 
        (- $LVal)))) 
	

