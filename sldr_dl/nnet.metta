
  !(:: (matrix))


;
;                             Randomise.


;
;                             Seed.

  !(set-random (seed 777))

;
;                              Vector.


  (= 
    (rand-vector 0 $_ Nil) 
    (set-det))
  (= 
    (rand-vector $D 
      (:: $A $B) 
      (Cons  $X $R)) 
    ( (is $D1 
        (- $D 1)) 
      (random $P) 
      (is $X 
        (+ 
          (* $P 
            (- $B $A)) $A)) 
      (rand-vector $D1 
        (:: $A $B) $R) 
      (set-det)))

;
;                               Matrix.


  (= 
    (rand-matrix 0 $_ $_ Nil) 
    (set-det))
  (= 
    (rand-matrix $P $Q 
      (:: $A $B) 
      (Cons  $X $R)) 
    ( (is $P1 
        (- $P 1)) 
      (rand-vector $Q 
        (:: $A $B) $X) 
      (rand-matrix $P1 $Q 
        (:: $A $B) $R) 
      (set-det)))

;
;                                Nnet constructor.



  !(dynamic (/ weight-matrix 2))
  !(dynamic (/ bias-vector 2))
  !(dynamic (/ activation 2))
  !(dynamic (/ layer-input 2))
  !(dynamic (/ layer-output 2))
  !(dynamic (/ layer-out-diff 2))
  !(dynamic (/ layer-in-diff 2))
  !(dynamic (/ layer-bias-grad 2))
  !(dynamic (/ layer-weight-grad 2))



  (= 
    (layer-init $Name $InDim $OutDim $Act 
      (:: $S1 $S2)) 
    ( (remove-all-atoms  &self 
        (weight_matrix  $Name $_)) 
      (remove-all-atoms  &self 
        (bias_vector  $Name $_)) 
      (remove-all-atoms  &self 
        (activation  $Name $_)) 
      (rand-matrix $OutDim $InDim 
        (:: $S1 $S2) $W) 
      (add-atom  &self 
        (weight_matrix  $Name $W)) 
      (rand-vector $OutDim 
        (:: $S1 $S2) $B) 
      (add-atom  &self 
        (bias_vector  $Name $B)) 
      (add-atom  &self 
        (activation  $Name $Act)) 
      (print (:: layer $Name has been initialised)) 
      (nl) 
      (set-det)))

  
;
;                                      Activation Function.


;
;                                      ReLU.



  (= 
    (relu $X 0) 
    ( (< $X 0) (set-det)))
  (= 
    (relu $X 30) 
    ( (> $X 30) (set-det)))
  (= 
    (relu $X $X) 
    (set-det))



  (= 
    (relu-diff $X 0) 
    ( (< $X 0) (set-det)))
  (= 
    (relu-diff $X 0) 
    ( (> $X 30) (set-det)))
  (= 
    (relu-diff $_ 1) 
    (set-det))



  !(discontiguous (/ vec-act 3))
  !(discontiguous (/ vec-act-diff 3))


  (= 
    (vec-act relu $V $R) 
    ( (maplist relu $V $R) (set-det)))

  (= 
    (vec-act-diff relu $V $R) 
    ( (maplist relu-diff $V $R) (set-det)))



;
;                                          Softmax.


  (= 
    (exp $X $Y) 
    ( (is $Y 
        (exp $X)) (set-det)))

  (= 
    (softmax-sub1 $A $X $Y) 
    ( (is $Y 
        (/ $X $A)) (set-det)))

  (= 
    (softmax $V $R) 
    ( (maplist exp $V $P) 
      (sumlist $P $S) 
      (maplist 
        (softmax-sub1 $S) $P $R) 
      (set-det)))


  (= 
    (softmax-diff-sub $_ 1) 
    (set-det))

  (= 
    (softmax-diff $V $R) 
    ( (maplist softmax-diff-sub $V $R) (set-det)))


  (= 
    (vec-act softmax $V $R) 
    ( (softmax $V $R) (set-det)))

  (= 
    (vec-act-diff softmax $V $R) 
    ( (softmax-diff $V $R) (set-det)))

;
;                                          Cross-Entropy Loss.


  (= 
    (ce-sub1 $A $B $S) 
    ( (is $S 
        (- $A $B)) (set-det)))

  (= 
    (neg-t-ln-y $Y $T $R) 
    ( (is $R 
        (* 
          (* -1 $T) 
          (log $Y))) (set-det)))

  (= 
    (ce-error $Y $T $E) 
    ( (maplist neg-t-ln-y $Y $T $P) 
      (sumlist $P $E) 
      (set-det)))

  (= 
    (ce-diff $Y $T $D) 
    ( (maplist ce-sub1 $T $Y $D) (set-det)))

;
;                                          Forward Computation.



  (= 
    (nnet-forward Nil $In $In) 
    (set-det))
  (= 
    (nnet-forward 
      (Cons  $Name $LayerList) $In $Out) 
    ( (weight-matrix $Name $W) 
      (bias-vector $Name $B) 
      (activation $Name $Act) 
      (remove-all-atoms  &self 
        (layer_input  $Name $_)) 
      (add-atom  &self 
        (layer_input  $Name $In)) 
      (transpose $In $InT) 
      (mat-mult-mat $W $InT $X) 
      (transpose $X $XT) 
      (maplist 
        (vec-add-vec $B) $XT $ZT) 
      (maplist 
        (vec-act $Act) $ZT $Y) 
      (remove-all-atoms  &self 
        (layer_output  $Name $_)) 
      (add-atom  &self 
        (layer_output  $Name $Y)) 
      (nnet-forward $LayerList $Y $Out) 
      (set-det)))
;                                         ; Y = Act(W * In + B)

;                                         ;print(X),nl,

;                                         ;print(ZT),nl,

;                                         ;print(Y),nl,

;                                         ; Go to the next layer



;
;                                          Error Computation.


  (= 
    (nnet-comp-error $LayerList $Tgt $Err $Diff) 
    ( (append $_ 
        (:: $Name) $LayerList) 
      (layer-output $Name $Y) 
      (maplist ce-error $Y $Tgt $ErrList) 
      (sumlist $ErrList $ErrTot) 
      (length $ErrList $NumData) 
      (is $Err 
        (/ $ErrTot $NumData)) 
      (maplist ce-diff $Y $Tgt $Diff) 
      (set-det)))
;                                         ;retractall(layer_out_diff(Name,_)),

;                                         ;assert(layer_out_diff(Name,Diff)),



;
;                                          Backward Computation.


  (= 
    (nnet-backward Nil $_ $_) 
    (set-det))
  (= 
    (nnet-backward $LayerList $Diff $LRate) 
    ( (append $L1 
        (:: $Name) $LayerList) 
      (weight-matrix $Name $W) 
      (bias-vector $Name $B) 
      (activation $Name $Act) 
      (layer-output $Name $Y) 
      (maplist 
        (vec-act-diff $Act) $Y $ActDiff) 
      (maplist vec-mult-vec $Diff $ActDiff $BDiff) 
      (transpose $BDiff $BDT) 
      (maplist sumlist $BDT $BGrad) 
      (remove-all-atoms  &self 
        (layer_bias_grad  $Name $_)) 
      (add-atom  &self 
        (layer_bias_grad  $Name $BGrad)) 
      (layer-input $Name $In) 
      (mat-mult-mat $BDT $In $WGrad) 
      (remove-all-atoms  &self 
        (layer_weight_grad  $Name $_)) 
      (add-atom  &self 
        (layer_weight_grad  $Name $WGrad)) 
      (mat-mult-mat $BDiff $W $InDiff) 
      (nnet-backward $L1 $InDiff $LRate) 
      (mat-mult-const $WGrad $LRate $DW) 
      (mat-add-mat $W $DW $WNew) 
      (remove-all-atoms  &self 
        (weight_matrix  $Name $_)) 
      (add-atom  &self 
        (weight_matrix  $Name $WNew)) 
      (vec-mult-const $BGrad $LRate $DB) 
      (vec-add-vec $B $DB $BNew) 
      (remove-all-atoms  &self 
        (bias_vector  $Name $_)) 
      (add-atom  &self 
        (bias_vector  $Name $BNew)) 
      (set-det)))
;                                         ; Next Layer.

;                                         ; Update.




  (= 
    (nnet-train $_ $_ $_ 0 $_) 
    (set-det))
  (= 
    (nnet-train $Nnet $In $Tgt $Iter $LRate) 
    ( (is $I1 
        (- $Iter 1)) 
      (nnet-forward $Nnet $In $_) 
      (nnet-comp-error $Nnet $Tgt $Err $Diff) 
      (printerr $Err) 
      (nnet-backward $Nnet $Diff $LRate) 
      (nnet-train $Nnet $In $Tgt $I1 $LRate)))


  (= 
    (printerr $X) 
    ( (is $Y 
        (* $X 1000000)) 
      (round $Y $Z) 
      (is $R 
        (/ $Z 1000000)) 
      (print $R) 
      (nl) 
      (set-det)))


  (= 
    (try) 
    ( (= $NumHid 32) 
      (layer-init try1 4 $NumHid relu 
        (:: -0.2 0.2)) 
      (layer-init try2 $NumHid $NumHid relu 
        (:: -0.2 0.2)) 
      (layer-init try3 $NumHid $NumHid relu 
        (:: -0.2 0.2)) 
      (layer-init try4 $NumHid $NumHid relu 
        (:: -0.2 0.2)) 
      (layer-init try5 $NumHid $NumHid relu 
        (:: -0.2 0.2)) 
      (layer-init try6 $NumHid 5 softmax 
        (:: -0.2 0.2)) 
      (= $In 
        (:: 
          (:: 2 3 4 5) 
          (:: 6 7 8 9))) 
      (= $Tgt 
        (:: 
          (:: 0 1 0 0 0) 
          (:: 0 0 0 1 0))) 
      (= $Nnet 
        (:: try1 try2 try3 try4 try5 try6)) 
      (nnet-train $Nnet $In $Tgt 100 0.01))) 

