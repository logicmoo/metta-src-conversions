;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
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
    (pthreat $Player $Value $Pos $Tables) 
    ( (control $Player $Pos) 
      (potent-threats $Player $Evaled $Pos $Tables) 
      (pair-list $Vals $Threats $Evaled) 
      (max-for-player $Player $Vals $Value)))
; 



  (= 
    (potent-threats $Player $Evaled $Pos $Tables) 
    (findall 
      (- $Val $Threat) 
      (, 
        (potent-threat-mover $Player $Piece $Square $SqT $Victim $SqV $Effect $Val $Pos $Tables) 
        (= $Threat 
          (potent $Piece $Square $SqT $Victim $SqV $Effect $Val))) $Evaled))
; 


;
; 

;
; 

;
; 



;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
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
    (potent-threat-mover $Piece $Square $PieceV $SqV $Value $Pos $Tables) 
    ( (blank-state-if $Pos) 
      (find-advice-tables-if $Tables) 
      (add-capturing-table-if $Pos $Tables) 
      (control $Player $Pos) 
      (potent-threat-mover $Player $Piece $Square $SqT $PieceV $SqV $Effect $Value $Pos $Tables)))
; 


;
; 

;
; 

  (= 
    (potent-threat-mover $Player $Piece $Square $SqT $Victim $SqV $Effect $Value $S $Tables) 
    ( (capture-threat $Piece $Player $Square $SqT $Victim $SqV $Effect $Tables) 
      (effect-threat-evaluation $Effect $Player $Victim $SqV $Val $S $Tables) 
      (potency-value $Player $Piece $Square $SqT $Victim $SqV $Val $Value $S $Tables) 
      (tracing-anal-format pthreat "<~p>: ~p -> ~p x ~p ~p (~p) [~p]~n" 
        (:: $Piece $Square $SqT $Victim $SqV $Effect $Value))))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (potency-value $Player $Piece $Square $SqT $Victim $SqV $ValV $Value $S $Tables) 
    (det-if-then-else 
      (, 
        (defended $Player $Piece $Square $SqT $Victim $SqV $Effect $S $Tables) 
        (\== $Effect 
          (possess $Player))) 
      (, 
        (local-threat-evaluation $Piece $Square $ValA $S $Tables) 
        (is $Net 
          (- $ValV $ValA)) 
        (min-for-player $Player 
          (:: $ValV $Net) $Value) 
        (favorable-to-owner $Player $Value)) 
      (= $Value $ValV)))
; 

	


;
; 

;
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
    (defended $Player $PieceA $SqA $SqT $PieceV $SqV $Effect $S $Tables) 
    ( (opposite-role $Player $Opp) 
      (do-capture $PieceA $SqA $SqT $PieceV $SqV $S $PosOut) 
      (on $PieceD $Opp $SqD $PosOut) 
      (target-capture $PieceD $SqD $PieceA $SqT $Capture $Movement $Dir $Effect $PosOut $Tables)))
; 



;
; 

;
; 

;
; 

;
; 

;
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
    (target-capture $PieceA $SqA $PieceV $SqV $Capture $Movement $Dir $Effect $Pos $Tables) 
    ( (owns $PieceA $Player) 
      (potential-capture $PieceA $SqA $PieceV $SqV $Capture $Movement $Dir $Effect $Pos $Tables) 
      (capturing-movement-for-piece $PieceA $SqA $SqT $Player $Dir $Movement $Capture $Captured $Pos) 
      (captured-piece $PieceV $SqV $Captured)))
; 



;
; 

;
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
    (potential-capture $PieceA $SqA $PieceT $SqT $Effect $Pos $Tables) 
    (potential-capture $PieceA $SqA $PieceT $SqT $Capture $Movement $Dir $Effect $Pos $Tables))
; 


  (= 
    (potential-capture $PieceA $SqA $PieceT $SqT $Capture $Movement $Dir $Effect $Pos $Tables) 
    ( (threat-piece-victim $PieceA $Player $PieceT $VPlayer $Effect $Capture) 
      (opposite-role $Player $VPlayer) 
      (capture-has-movement $Capture $Movement) 
      (movement-sym-dir $Movement $Dir) 
      (capture-aligned $Capture $Movement $SqA $SqT $Dir)))
; 



  (= 
    (capture-aligned $Capture $Movement $SqA $SqT $Dir) 
    ( (capture-has-method $Capture $Method) (det-if-then (method-aligned $Method $Movement $SqA $SqT $Dir) True)))
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
    (method-aligned clobber $Movement $SqA $SqT $Dir) 
    (aligned $SqA $SqT $Dir))
; 

  (= 
    (method-aligned hop $Movement $SqA $SqT $Dir) 
    (aligned $SqA $SqT $Dir))
; 

  (= 
    (method-aligned retrieve $Movement $SqA $SqT $Dir) 
    (connected $SqT $SqA $Dir))
; 


	
;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (aligned $S1 $S2 $Dir) 
    ( (square $S1 $Xf $Yf) 
      (square $S2 $Xt $Yt) 
      (direction $Dir $DX $DY) 
      (current-board-type $T) 
      (x-given-y-leaps $T $Yf $Yt $DY $Xf $Xt $DX)))
; 



  (= 
    (x-given-y-leaps $T $Yf $Yt $DY $Xf $Xt $DX) 
    ( (is $DiffY 
        (- $Yt $Yf)) 
      (same-sign $DiffY $DY) 
      (det-if-then-else 
        (\== $DY 0) 
        (, 
          (is 0 
            (mod $DiffY 
              (abs $DY))) 
          (is $Leaps 
            (// $DiffY $DY)) 
          (is $XNew 
            (+ $Xf 
              (* $Leaps $DX))) 
          (align-for-type $T $XNew $Xt)) 
        (det-if-then-else 
          (\== $T vertical-cylinder) 
          (, 
            (is $DiffX 
              (- $Xt $Xf)) 
            (same-sign $DiffX $DX) 
            (is 0 
              (mod 
                (- $Xt $Xf) 
                (abs $DX)))) 
          (, 
            (valid-max-dir $DX 0 $Max1) 
            (is $Max 
              (abs $Max1)) 
            (some-reaches $Max $T $DX $Xf $Xt))))))
; 



  (= 
    (some-reaches $Leaps $T $DX $Xf $Xt) 
    ( (> $Leaps 0) 
      (is $XNew 
        (+ $Xf 
          (* $Leaps $DX))) 
      (align-for-type $T $XNew $Xt)))
; 

  (= 
    (some-reaches $Leaps $T $DX $Xf $Xt) 
    ( (> $Leaps 1) 
      (is $L1 
        (- $Leaps 1)) 
      (some-reaches $L1 $T $DX $Xf $Xt)))
; 



	



  (= 
    (y-leaps $Yf $Yt $DY $Leaps) 
    ( (is $DiffY 
        (- $Yt $Yf)) 
      (same-sign $DiffY $DY) 
      (is 0 
        (mod $DiffY 
          (abs $DY))) 
      (is $Leaps 
        (// $DiffY $DY))))
; 
 



  (= 
    (align_for_type  planar $X $X) True)
; 

  (= 
    (align-for-type vertical-cylinder $X1 $X) 
    ( (current-board-size $XN $YN) (is $X (+ (mod (- (+ $X1 $XN) 1) $XN) 1))))
; 





  (= 
    (sign $X $Sign) 
    (det-if-then-else 
      (< $X 0) 
      (= $Sign -1) 
      (det-if-then-else 
        (> $X 0) 
        (= $Sign 1) 
        (det-if-then otherwise 
          (= $Sign 0)))))
; 




  (= 
    (same-sign $X1 $X2) 
    ( (sign $X1 $S) (sign $X2 $S)))
; 




  (= 
    (test-potential-capture $PieceA $SqA $PieceT $SqT $Capture $Movement $Dir $Effect $S $Tables) 
    ( (checkpoint init $S) 
      (on $PieceA $_ $SqA $S) 
      (on $PieceT $_ $SqT $S) 
      (potential-capture $PieceA $SqA $PieceT $SqT $Capture $Movement $Dir $Effect $Pos $Tables)))
; 



  (= 
    (test-target-capture $PieceA $SqA $PieceV $SqV $Capture $Movement $Dir $Effect $S $Tables) 
    ( (on $PieceA $_ $SqA $S) 
      (on $PieceV $_ $SqV $S) 
      (target-capture $PieceA $SqA $PieceV $SqV $Capture $Movement $Dir $Effect $S $Tables)))
; 



