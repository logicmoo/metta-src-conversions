;                                           (convert-to-metta-file  mobility $_466180 metagame/play/mobility.pl metagame/play/mobility.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; mobility.pl
;
; ;; Defines a basic evaluation used by some players
;
; ;; (like alpha_beta or evaluating player).
;
; ;; Features are mobility and material (piece_count).

;
; ========================================
;
; EVALUATING_CHOOSE(Player,Move,SIn,SOut)
;
; ========================================
;
; Wins immediately if possible, else plays a random move
;
; among those with highest value.
;
; Note doesn't search any more, so this might leave it
;
; open for a loss. Also semantics not clear if this move
;
; ends the game for either player.


  (= (evaluating-choose $Player $Move $SIn $SOut)  
    (timing (evaluating-move $Move $SIn $SOut))
    (set-det)
    (print-choice $Move $SIn $SOut))



  (= (evaluating-move $Move $SIn $SOut)  
    (victor-move $Move $SIn $SOut)
    (set-det))
  (= (evaluating-move $Move $SIn $SOut)  
    (max-value-move $Move $SIn $SOut))

;
; Evaluates position relative to player who is to move.
;
; First choices have lower value, therefore WORSE for player to
;
; move.
;
; Thus, a player evaluating all possible successors will choose the
;
; lowest valued one, as this is the worst one for his opponent.


  (= (evaluated-move $Move $Val $SIn)  
    (legal $Move $SIn $S1)
    (evaluate $S1 $Val))
	


  (= (max-value-move $Move $SIn $SOut)  
    (setof 
      (- $Val $Move) 
      (evaluated-move $Move $Val $SIn) $Pairs)
    (random-best 
      (- $Val $Move) $Pairs)
    (format "The best move found has a value of: ~p~n" 
      (:: $Val))
    (legal $Move $SIn $SOut))
	

  (= (random-best $Best $L)  
    (collect-init $L $BestFew)
    (random-element $BestFew $Best))


  (= (collect-init (Cons  (- $V $M) $Rest) (Cons  (- $V $M) $BestFew))  
    (collect-init $Rest $V $BestFew))

  (= (collect-init  () $V1 ())  True)
  (= (collect-init (Cons  (- $V2 $_) $_) $V1 Nil)  
    (< $V1 $V2)
    (set-det))
  (= (collect-init (Cons  $Pair $Rest) $Threshold (Cons  $Pair $R1))  
    (collect-init $Rest $Threshold $R1))




  (= (evaluate-com $Move $SIn $_)  
    (timing (evaluate $SIn $Value))
    (format "Position's value for player to move:  ~p~n" 
      (:: $Value)))



  (= (evaluate $S $Value)  
    (control $P $S)
    (opposite-role $O $P)
    (evaluate $P $S $PVal)
    (evaluate $O $S $OVal)
    (is $Value 
      (- $PVal $OVal)))


  (= (evaluate $Player $S $Value)  
    (weight-vector $Weights)
    (material-weight $Weights $MatW)
    (mobility-weight $Weights $MobW)
    (weighted-mobility $MobW $Player $S $WMob)
    (weighted-material $MatW $Player $S $WMat)
    (is $Value 
      (+ $WMob $WMat)))
; /* evaluate(Player,S,Value) :- weight_vector(Weights), material_weight(Weights,WMat), mobility_weight(Weights,WMob), mobility(Player,S,Mob), material(Player,S,Mat), Value is Mob*WMob + Mat*WMat. */


  (= (weighted-mobility 0 $_ $_ 0)  
    (set-det))
  (= (weighted-mobility $MobW $Player $S $WMob)  
    (mobility $Player $S $Mob)
    (is $WMob 
      (* $MobW $Mob)))



  (= (weighted-material 0 $_ $_ 0)  
    (set-det))
  (= (weighted-material $MatW $Player $S $WMat)  
    (material $Player $S $Mat)
    (is $WMat 
      (* $MatW $Mat)))




  (= (mobility $Player $S $M)  
    (put-control $Player $S $S1)
    (count-bagof $Move 
      (^ $S2 
        (legal-move $Move $Player $S1 $S2)) $M))
	


  (= (material $Player $S $M)  
    (count-bagof $Piece 
      (^ $Sq 
        (on 
          (piece $Piece $Player) $Sq $S)) $M))



;
; EVALUATE2
;
; Only considers opponent's mobility, and player's material,
;
; in a future position. Not very good, or even that much faster.

  (= (evaluate2 $S $Value)  
    (control $P $S)
    (opposite-role $O $P)
    (evaluate2 $P $O $S $Value))

  (= (evaluate2 $Player $Opponent $S $Value)  
    (mobility $Opponent $S $Mob)
    (material $Player $S $Mat)
    (weight material $WMat)
    (weight mobility $WMob)
    (is $Value 
      (- 
        (* $Mat $WMat) 
        (* $Mob $WMob))))


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Manipulating weights
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (toggle-alpha-beta-weights $Player $Old)  
    (weight-vector $Player $Weights)
    (change-parameter weights $Old $Weights))
	


  (= (weight-vector $Player $Weights)  
    (parameter 
      (weights $Player) $Weights))

  (= (weight-vector $Weights)  
    (parameter weights $Weights))



  (= initialize-weights  
    (default-alpha-weights $W)
    (add-parameter weights $W))


  (= (default-alpha-weights (weights $Mat $Mob))  
    (default-weight material $Mat)
    (default-weight mobility $Mob))

;
; General function, preferring mobility and material.

  (= (default-weight  material 4)  True)
  (= (default-weight  mobility 1)  True)

;
; Anti-material function, perferring mobility but not material.
;
; default_weight(material,-10).
;
; default_weight(mobility,1).




;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Interface
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (material-weight  (weights  $Mat $Mob) $Mat)  True)

  (= (mobility-weight  (weights  $Mat $Mob) $Mob)  True)


  (= (set-player-weights $Player $Mat $Mob)  
    (material-weight $W $Mat)
    (mobility-weight $W $Mob)
    (det-if-then-else 
      (= $Player default) 
      (add-parameter weights $W) 
      (add-parameter 
        (weights $Player) $W)))



  (= show-player-weights  
    (show-player-weights player)
    (show-player-weights opponent))

  (= (show-player-weights $Player)  
    (det-if-then-else 
      (weight-vector $Player $W) 
      (= $Default '') 
      (, 
        (weight-vector $W) 
        (= $Default '(by default)')))
    (material-weight $W $Mat)
    (mobility-weight $W $Mob)
    (format "\n<~p>'s weights are ~w:\n       material:  ~p\n       mobility:  ~p\n" 
      (:: $Player $Default $Mat $Mob)))	


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Menus
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; WEIGHT: Allows command: "weight {player, opponent, default} Mat Mob"

  (= (weight-com $_ $_ $_ $Player $Mat $Mob)  
    (set-player-weights $Player $Mat $Mob))


  (= (weight-top $Player $Mat $Mob)  
    (set-player-weights $Player $Mat $Mob))


  (= (show-com $_ $_ $_ weights $Player)  
    (show-player-weights $Player))

  (= (show-com $_ $_ $_ weights)  show-player-weights)


  (= (show-top weights $Player)  
    (show-player-weights $Player))

  (= (show-top weights)  show-player-weights)

;
; toggle player ==> sets default weights to be those of player.

  (= (toggle-com $_ $_ $_ $Player)  
    (toggle-alpha-beta-weights $Player $Old))



