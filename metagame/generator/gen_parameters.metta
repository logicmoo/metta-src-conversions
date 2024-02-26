;
; 

;
; 

;
; 

;
; 


;
; 

;
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  !(dynamic (/ forced-gen-parameter 2))
; 

  !(dynamic (/ gen-parameter 2))
; 



  (= 
    (gen_parameter  board_size 
      (range  5 6)) True)
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
    (gen_parameter  board_crowding 
      (distribution  
        ( (= 0.5 0.7) 
          (= 0.3 0.15) 
          (= 0.7 0.15)))) True)
; 


;
; 

;
; 

  (= 
    (gen_parameter  row_crowding 
      (distribution  
        ( (= 1.0 0.7) (= 0.7 0.3)))) True)
; 



;
; 

;
; 

  (= 
    (gen_parameter  promotion_fraction 
      (distribution  
        ( (= 1.0 0.3) 
          (= 0.8 0.1) 
          (= 0.6 0.1) 
          (= 0.4 0.2) 
          (= 0.2 0.2) 
          (= 0 0.1)))) True)
; 


;
; 

;
; 

  (= 
    (gen_parameter  piece_variety 
      (distribution  
        ( (= 1.0 0.4) 
          (= 0.8 0.2) 
          (= 0.6 0.2) 
          (= 0.4 0.2) 
          (= 0.2 0.0)))) True)
; 



;
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
    (gen_parameter  promote_only_pieces 
      (range  1 3)) True)
; 




;
; 

;
; 

;
; 

;
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
    (gen_parameter  placement_method 
      (distribution  
        ( (= random 0.2) 
          (= arbitrary 0.6) 
          (= player 0.1) 
          (= opponent 0.1)))) True)
; 



;
; 

  (= 
    (gen_parameter  board_type 
      (distribution  
        ( (= planar 0.9) (= vertical_cylinder 0.1)))) True)
; 


;
; 

  (= 
    (gen_parameter  board_inversion 
      (distribution  
        ( (= forward 0.5) (= diagonal 0.5)))) True)
; 





;
; 

  (= 
    (gen_parameter  
      (symmetry  rotation) 0.9) True)
; 

  (= 
    (gen_parameter  
      (symmetry  forward) 0.9) True)
; 

  (= 
    (gen_parameter  
      (symmetry  side) 0.9) True)
; 


;
; 


;
; 

;
; 

  (= 
    (gen_parameter  locality 
      (range  0.1 0.8)) True)
; 




;
; 

  (= 
    (gen_parameter  movement_type 
      (distribution  
        ( (= leaper 0.4) 
          (= rider 0.4) 
          (= hopper 0.2)))) True)
; 


;
; 

  (= 
    (gen_parameter  must_ride 0.2) True)
; 


;
; 

;
; 

  (= 
    (gen_parameter  
      (constrain  
        (hopper  before $_)) 0.5) True)
; 

  (= 
    (gen_parameter  
      (constrain  
        (hopper  over $_)) 0.5) True)
; 

  (= 
    (gen_parameter  
      (constrain  
        (hopper  after $_)) 0.5) True)
; 


;
; 

  (= 
    (gen_parameter  
      (hopper  before $Max) 
      (range  0 $Max)) True)
; 


;
; 

  (= 
    (gen_parameter  
      (hopper  over $Max) 
      (range  1 $Max)) True)
; 


;
; 

  (= 
    (gen_parameter  
      (hopper  after $Max) 
      (range  1 $Max)) True)
; 



;
; 

  (= 
    (gen_parameter  comparative 
      (distribution  
        ( (= eq 0.5) 
          (= geq 0.2) 
          (= leq 0.3)))) True)
; 




;
; 

;
; 

  (= 
    (gen_parameter  movement_complexity 0.2) True)
; 

  (= 
    (gen_parameter  capture_complexity 0.2) True)
; 

  (= 
    (gen_parameter  goal_complexity 0.6) True)
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
    (gen_parameter  
      (capture_method  retrieve) 0.2) True)
; 

  (= 
    (gen_parameter  
      (capture_method  clobber) 0.9) True)
; 
 
  (= 
    (gen_parameter  
      (capture_method  hop) 0.5) True)
; 



  (= 
    (gen_parameter  capture_effect 
      (distribution  
        ( (= remove 0.5) 
          (= 
            (possess  player) 0.3) 
          (= 
            (possess  opponent) 0.2)))) True)
; 



;
; 

;
; 


  (= 
    (gen_parameter  player_generalization_level 
      (distribution  
        ( (= any 0.2) (= specific 0.8)))) True)
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
    (gen_parameter  piece_generalization_level 
      (distribution  
        ( (= any 0.5) (= specific 0.5)))) True)
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
    (gen_parameter  more_pieces 0.8) True)
; 


  (= 
    (gen_parameter  more_general_pieces 0.8) True)
; 




;
; 


  (= 
    (gen_parameter  must_capture 0.3) True)
; 


;
; 

;
; 


  (= 
    (gen_parameter  continue_captures 0.1) True)
; 



;
; 

;
; 


  (= 
    (gen_parameter  goal_type 
      (distribution  
        ( (= eradicate 0.5) (= arrive 0.5)))) True)
; 


;
; 

;
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
    (gen_parameter  arrive_goal_player 
      (distribution  
        ( (= player 0.5) (= opponent 0.5)))) True)
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
    (gen_parameter  more_arrival_pieces 0.8) True)
; 



;
; 

;
; 

  (= 
    (gen_parameter  eradicate_goal_player 
      (distribution  
        ( (= player 0.2) (= opponent 0.8)))) True)
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
    (gen_parameter  more_eradicate_pieces 0.8) True)
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
    (gen_parameter  eradicate_generalization_level 
      (distribution  
        ( (= any 0.2) (= specific 0.8)))) True)
; 




;
; 

;
; 


  (= 
    (gen_parameter  specific_promotion 0.4) True)
; 



;
; 


  (= 
    (gen_parameter  promotion_method 
      (distribution  
        ( (= arbitrary 0.3) 
          (= player 0.5) 
          (= opponent 0.1)))) True)
; 



;
; 

;
; 

;
; 


  (= 
    (choose-parameter $Name $Value) 
    ( (forced-gen-parameter $Name 
        (distribution $Dist)) 
      (set-det) 
      (sample-from-distribution 
        (distribution $Dist) $Value)))
; 

  (= 
    (choose-parameter $Name $Value) 
    ( (gen-parameter $Name 
        (distribution $Dist)) 
      (set-det) 
      (sample-from-distribution 
        (distribution $Dist) $Value)))
; 

  (= 
    (choose-parameter $Name $Value) 
    ( (gen-parameter $Name 
        (range $Min $Max)) 
      (set-det) 
      (sample-from-range 
        (range $Min $Max) $Value)))
; 

  (= 
    (choose-parameter $Name $Value) 
    ( (gen-parameter $Name $Int) 
      (integer $Int) 
      (set-det) 
      (adjust-mean $Int $Value)))
; 

  (= 
    (choose-parameter $Name $Value) 
    ( (gen-parameter $Name $Prob) (sample-from-distribution (distribution (:: (= yes $Prob) (= no 1))) $Value)))
; 



  (= 
    (choose-parameter $Name) 
    (choose-parameter $Name yes))
; 




  (= 
    (block-parameter $Name $Items) 
    ( (block-distribution $Name $Items $Dist) (add-symbol  &self (forced_gen_parameter  $Name (distribution  $Dist)))))
; 



  (= 
    (unblock-parameter $Name) 
    (remove-symbol  &self 
      (forced_gen_parameter  $Name 
        (distribution  $Dist))))
; 



  (= 
    (reset-gen-parameters) 
    (remove-all-symbols  &self 
      (forced_gen_parameter  $_ $_)))
; 




  (= 
    (set-gen-parameter $P $V) 
    (det-if-then-else 
      (remove-symbol  &self 
        (gen_parameter  $P $_)) 
      (add-symbol  &self 
        (gen_parameter  $P $V)) 
      (det-if-then otherwise 
        (trace-output 'Unknown generator parameter ~p!~n' 
          (:: $P)))))
; 




;
; 

;
; 

;
; 


  (= 
    (block-distribution $Name $Items $Dist) 
    ( (gen-parameter $Name 
        (distribution $Dist0)) 
      (set-det) 
      (remove-items $Items $Dist0 $Prob $Dist1) 
      (is $P1 
        (/ 1 
          (- 1 $Prob))) 
      (renormalize $Dist1 $P1 $Dist)))
; 



  (= 
    (remove_items  () $D 0 $D) True)
; 

  (= 
    (remove-items 
      (Cons  $I $Is) $Dist1 $Prob $Dist) 
    ( (remove-item $I $Dist1 $Prob1 $Dist2) 
      (remove-items $Is $Dist2 $Prob2 $Dist) 
      (is $Prob 
        (+ $Prob1 $Prob2))))
; 



  (= 
    (remove_item  $I 
      (Cons  
        (= $I $P) $Dist) $P $Dist) True)
; 

  (= 
    (remove-item $I 
      (Cons  $H $T) $P 
      (Cons  $H $Dist)) 
    (remove-item $I $T $P $Dist))
; 



  (= 
    (renormalize  () $_ ()) True)
; 

  (= 
    (renormalize 
      (Cons  
        (= $H $P) $Rest) $Ratio 
      (Cons  
        (= $H $P1) $Rest1)) 
    ( (is $P1 
        (* $P $Ratio)) (renormalize $Rest $Ratio $Rest1)))
; 



;
; 


  (= 
    (adjust_mean  $Int $Int) True)
; 



;
; 

;
; 

;
; 



  (= 
    (show-gen-parameters) 
    ( (whenever 
        (gen-parameter $Name $Val) 
        (, 
          (portray-param (gen-parameter $Name $Val)) 
          (nl))) 
      (getrand $R) 
      (format '~nrandom seed = ~p~n' 
        (:: $R))))
; 



  (= 
    (portray-param (gen-parameter $Name $Val)) 
    (format "<~p> --> ~p~n" 
      (:: $Name $Val)))
; 



  (= 
    (portray-range (range $Min $Max)) 
    (format "[~p .. ~p]" 
      (:: $Min $Max)))
; 



  (= 
    (portray-dist (distribution $Pairs)) 
    (portray-pairs $Pairs))
; 



  (= 
    (portray_pairs  ()) True)
; 

  (= 
    (portray-pairs (Cons  $Pair $Pairs)) 
    ( (portray-pair $Pair) (portray-pairs $Pairs)))
; 



  (= 
    (portray-pair (= $Event $Val)) 
    (format "\n     ~p: ~p" 
      (:: $Event $Val)))
; 



  !(add-portrayals (:: portray-param portray-range portray-dist))
; 


;
; 

;
; 

;
; 



  (= 
    (change-gen-param $Name) 
    ( (read-new-gen-value $Name $New) (set-gen-parameter $Name $New)))
; 



  (= 
    (read-new-gen-value $Name $New) 
    ( (gen-parameter $Name $Value) (read-gen-value $Name $Value $New)))
; 



  (= 
    (read-gen-value $Name $Value $New) 
    ( (format "Enter new settings for parameter <~p>:~n" 
        (:: $Name)) 
      (format "Old parameter setting: ~p~n" 
        (:: $Value)) 
      (read-gen-value $Value $New) 
      (format "New parameter setting: ~p~n" 
        (:: $New))))
; 



  (= 
    (read-gen-value 
      (distribution $Dist) 
      (distribution $New)) 
    ( (set-det) (read-dist $Dist $New)))
; 

  (= 
    (read-gen-value 
      (range $Min1 $Max1) 
      (range $Min $Max)) 
    ( (set-det) (read-range $Min1 $Max1 $Min $Max)))
; 

  (= 
    (read-gen-value $Old $New) 
    (read-new-val new $Old $New))
; 




  (= 
    (read_dist  () ()) True)
; 

  (= 
    (read-dist 
      (Cons  
        (= $Event $Prob1) $Rest1) 
      (Cons  
        (= $Event $Prob2) $Rest2)) 
    ( (read-event $Event $Prob1 $Prob2) (read-dist $Rest1 $Rest2)))
; 



  (= 
    (read-event $Event $Old $New) 
    ( (format "Prob for <~p> (~p): " 
        (:: $Event $Old)) 
      (read $New1) 
      (new-event-val $Event $Old $New1 $New)))
; 



  (= 
    (new-event-val $Event $Old $New1 $New) 
    (new-val $Old $New1 $New))
; 

	

  (= 
    (new-val $Old $New1 $New) 
    (det-if-then-else 
      (= $New1 z) 
      (= $New $Old) 
      (= $New $New1)))
; 




  (= 
    (read-range $Min1 $Max1 $Min $Max) 
    ( (read-new-val min $Min1 $Min) (read-new-val max $Max1 $Max)))
; 



  (= 
    (read-new-val $Name $Old $New) 
    ( (format "<~p> value (~p): " 
        (:: $Name $Old)) 
      (read $New1) 
      (new-val $Old $New1 $New)))
; 






