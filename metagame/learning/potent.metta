;                                           (convert-to-metta-file  potent $_430796 metagame/learning/potent.pl metagame/learning/potent.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ================================================================================
;
; Global Potent Threats
;
; ================================================================================

;
; The global value of the player to move having potent threats is the
;
; maximum of the value of all potent threats he has.
;
; We could add value for the other player, but not nearly as clear how
;
; to determine this.


  (= (pthreat $Player $Value $Pos $Tables)  
    (control $Player $Pos)
    (potent-threats $Player $Evaled $Pos $Tables)
    (pair-list $Vals $Threats $Evaled)
    (max-for-player $Player $Vals $Value))


  (= (potent-threats $Player $Evaled $Pos $Tables)  
    (findall 
      (- $Val $Threat) 
      (, 
        (potent-threat-mover $Player $Piece $Square $SqT $Victim $SqV $Effect $Val $Pos $Tables) 
        (= $Threat 
          (potent $Piece $Square $SqT $Victim $SqV $Effect $Val))) $Evaled))

;
; ================================================================================
;
; Local Potent Threats
;
; ================================================================================


;
; The value of having a potent threat on a piece is the amount which would be increased
;
; for the threatener if that piece disappeared (or 0 if that is negative).
;
; Perhaps discounted by an offset based on whether the threat can be executed now or
;
; if the victim gets a chance to defend.

;
; Threat is potent for mover if:
;
; 1. Capturing target (under the effect) has favorable value (V) for mover, and
;
; 2. Target undefended by non-mover (then V is net threat value) or
;
; 3. Target defended, but TargV (V) - AttackV (on current square) > 0.
;
; (then this diff is net value).
;
; We are at present ignoring the effects (besides removal) that happen
;
; if opponent has a defense against our attacker.


  (= (potent-threat-mover $Piece $Square $PieceV $SqV $Value $Pos $Tables)  
    (blank-state-if $Pos)
    (find-advice-tables-if $Tables)
    (add-capturing-table-if $Pos $Tables)
    (control $Player $Pos)
    (potent-threat-mover $Player $Piece $Square $SqT $PieceV $SqV $Effect $Value $Pos $Tables))
; ; ensures mover has threat ; on(Piece,Player,Square,Pos),

;
; Weigh-by-effect ensures favorable to capture.
;
; Assumes Player is in control.
  (= (potent-threat-mover $Player $Piece $Square $SqT $Victim $SqV $Effect $Value $S $Tables)  
    (capture-threat $Piece $Player $Square $SqT $Victim $SqV $Effect $Tables)
    (effect-threat-evaluation $Effect $Player $Victim $SqV $Val $S $Tables)
    (potency-value $Player $Piece $Square $SqT $Victim $SqV $Val $Value $S $Tables)
    (tracing-anal-format pthreat "<~p>: ~p -> ~p x ~p ~p (~p) [~p]~n" 
      (:: $Piece $Square $SqT $Victim $SqV $Effect $Value)))


;
; POTENCY_VALUE(Player,Piece,Square,SqT,Victim,SqV,ValV,Value,S,Tables)
;
; 
;
; ValV is the value for capturing the victim if undefended.
;
; If it is defended, player gets the net difference between the value
;
; of the capture, and losing his own piece. If he wants to lose his piece
;
; however, it is as if the piece was undefended anyway!
;
; 
;
; Should really use effect_threat_evaluation, since opponent gets value for capturing
;
; us back based on the effect of his recapture, given where our piece will be now.
;
; For example, suppose we can remove-capture his bishop with our night, but he
;
; defends it with a possess-capture piece. Then it would presently look like
;
; we have a real threat (bishop-night value), but actually we don't, as this is
;
; just a small increment for us, while he winds up possessing a night
;
; after the transaction, putting him at least a piece up!
;
; 
;
; For the moment, just assume he wants to do it as long as the effect isn't to
;
; give it back to us.
;
; 
;
; If use this, don't need this min-for-player
;
; business, as it won't even be seen as a defense unless the enemy wins
;
; something for recapturing us.
;
; 

  (= (potency-value $Player $Piece $Square $SqT $Victim $SqV $ValV $Value $S $Tables)  
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
; DEFENDED(Player,PieceA,SqA,SqT,PieceV,SqV,Effect,S,_Tables)
;
; General routine to check if some considered attack is defended.
;
; The attack PieceA @ SqA -> SqT x PieceV @ SqV must be known applicable already.
;
; This threat is defended if there is some defender which could capture the
;
; attacker if this considered capture were executed.
;
; 


  (= (defended $Player $PieceA $SqA $SqT $PieceV $SqV $Effect $S $Tables)  
    (opposite-role $Player $Opp)
    (do-capture $PieceA $SqA $SqT $PieceV $SqV $S $PosOut)
    (on $PieceD $Opp $SqD $PosOut)
    (target-capture $PieceD $SqD $PieceA $SqT $Capture $Movement $Dir $Effect $PosOut $Tables))


;
; TARGET_CAPTURE(PieceA,SqA,PieceV,SqV,Capture,Movement,Dir,Effect,Pos,_Tables)
;
; ==============
;
; 
;
; When the target piece is known, it is easier to check whether an attacker can
;
; actually capture it in the present position. First check if it is even potentially
;
; possible (when the target is the right piece type, and in the right capture line).
;
; If so, check that that capturing movement in that direction really works to create a capture.
;
; 
;
; This is a general routine, and can be very useful.

  (= (target-capture $PieceA $SqA $PieceV $SqV $Capture $Movement $Dir $Effect $Pos $Tables)  
    (owns $PieceA $Player)
    (potential-capture $PieceA $SqA $PieceV $SqV $Capture $Movement $Dir $Effect $Pos $Tables)
    (capturing-movement-for-piece $PieceA $SqA $SqT $Player $Dir $Movement $Capture $Captured $Pos)
    (captured-piece $PieceV $SqV $Captured))


;
; We have a potential capture if:
;
; The piece can capture victims of that type with some capture power.
;
; The effect is not to give it right back to oppenent.
;
; That capture power has some movement which is aligned with the target.
;
; 
;
; Could take account of factors like min-ride, max-ride, hoppers, etc.

  (= (potential-capture $PieceA $SqA $PieceT $SqT $Effect $Pos $Tables)  
    (potential-capture $PieceA $SqA $PieceT $SqT $Capture $Movement $Dir $Effect $Pos $Tables))

  (= (potential-capture $PieceA $SqA $PieceT $SqT $Capture $Movement $Dir $Effect $Pos $Tables)  
    (threat-piece-victim $PieceA $Player $PieceT $VPlayer $Effect $Capture)
    (opposite-role $Player $VPlayer)
    (capture-has-movement $Capture $Movement)
    (movement-sym-dir $Movement $Dir)
    (capture-aligned $Capture $Movement $SqA $SqT $Dir))


  (= (capture-aligned $Capture $Movement $SqA $SqT $Dir)  
    (capture-has-method $Capture $Method)
    (det-if-then 
      (method-aligned $Method $Movement $SqA $SqT $Dir) True)) 

;
; For retrieve method, the attacker can move in a dir
;
; away from the target if the target is one leap behind.
;
; For now ignore Movement, but could make use of movement
;
; constraints as well.

  (= (method-aligned clobber $Movement $SqA $SqT $Dir)  
    (aligned $SqA $SqT $Dir))
  (= (method-aligned hop $Movement $SqA $SqT $Dir)  
    (aligned $SqA $SqT $Dir))
  (= (method-aligned retrieve $Movement $SqA $SqT $Dir)  
    (connected $SqT $SqA $Dir))

	
;
; ALIGNED(S1,S2,Dir)
;
; 
;
; True if S2 is on the direction vector DIR from S1, given the
;
; current board size and types.
;
; This would be more complicated if both axes could wraparound, but since we
;
; know Y doesn't we just find the # leaps there, then check that moving that
;
; many leaps with wrapping along X brings us back to X square.
;
; This of course checks that there is an integer, not fractional, number of leaps.
;
; For example:
;
; On vertical cylinder 5x6 board:
;
; aligned(square(2,2),square(1,1),dir(-1,-1)).
;
; aligned(square(1,5),square(1,3),dir(-5,-2)).
;
; aligned(square(1,2),square(5,1),dir(-1,-1)).
;
; But not:
;
; * aligned(square(2,2),square(3,3),dir(-1,-1)).
;
; 

  (= (aligned $S1 $S2 $Dir)  
    (square $S1 $Xf $Yf)
    (square $S2 $Xt $Yt)
    (direction $Dir $DX $DY)
    (current-board-type $T)
    (x-given-y-leaps $T $Yf $Yt $DY $Xf $Xt $DX))


  (= (x-given-y-leaps $T $Yf $Yt $DY $Xf $Xt $DX)  
    (is $DiffY 
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
          (some-reaches $Max $T $DX $Xf $Xt)))))


  (= (some-reaches $Leaps $T $DX $Xf $Xt)  
    (> $Leaps 0)
    (is $XNew 
      (+ $Xf 
        (* $Leaps $DX)))
    (align-for-type $T $XNew $Xt))
  (= (some-reaches $Leaps $T $DX $Xf $Xt)  
    (> $Leaps 1)
    (is $L1 
      (- $Leaps 1))
    (some-reaches $L1 $T $DX $Xf $Xt))


	



  (= (y-leaps $Yf $Yt $DY $Leaps)  
    (is $DiffY 
      (- $Yt $Yf))
    (same-sign $DiffY $DY)
    (is 0 
      (mod $DiffY 
        (abs $DY)))
    (is $Leaps 
      (// $DiffY $DY))) 



  (= (align-for-type  planar $X $X)  True)
  (= (align-for-type vertical-cylinder $X1 $X)  
    (current-board-size $XN $YN)
    (is $X 
      (+ 
        (mod 
          (- 
            (+ $X1 $XN) 1) $XN) 1)))




  (= (sign $X $Sign)  
    (det-if-then-else 
      (< $X 0) 
      (= $Sign -1) 
      (det-if-then-else 
        (> $X 0) 
        (= $Sign 1) 
        (det-if-then otherwise 
          (= $Sign 0)))))



  (= (same-sign $X1 $X2)  
    (sign $X1 $S)
    (sign $X2 $S))



  (= (test-potential-capture $PieceA $SqA $PieceT $SqT $Capture $Movement $Dir $Effect $S $Tables)  
    (checkpoint init $S)
    (on $PieceA $_ $SqA $S)
    (on $PieceT $_ $SqT $S)
    (potential-capture $PieceA $SqA $PieceT $SqT $Capture $Movement $Dir $Effect $Pos $Tables))


  (= (test-target-capture $PieceA $SqA $PieceV $SqV $Capture $Movement $Dir $Effect $S $Tables)  
    (on $PieceA $_ $SqA $S)
    (on $PieceV $_ $SqV $S)
    (target-capture $PieceA $SqA $PieceV $SqV $Capture $Movement $Dir $Effect $S $Tables))
; ; checkpoint(init,S),


