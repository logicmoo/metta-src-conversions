
  !(dynamic (/ root 2))
; 

  !(dynamic (/ root 4))
; 

  !(dynamic (/ node 3))
; 

  !(dynamic (/ d-sub 2))
; 

  !(dynamic (/ gensym-counter 2))
; 

  !(dynamic (/ prediction-counter 2))
; 

  !(dynamic (/ features 1))
; 

  !(dynamic (/ case 1))
; 

  !(dynamic (/ acuity 1))
; 



  (= 
    (learn) 
    ( (initialize) 
      (get-case $X) 
      (cobweb $X) 
      (nl) 
      (nl) 
      (show-classes) 
      (fail)))
; 

  (= learn True)
; 

	

  (= 
    (initialize) 
    ( (abolish node 3) 
      (abolish d-sub 2) 
      (abolish root 2) 
      (abolish root 4) 
      (abolish prediction-counter 2) 
      (abolish gensym-counter 2) 
      (set-det)))
; 



  (= 
    (learn-more) 
    ( (get-case $X) 
      (cobweb $X) 
      (fail)))
; 

  (= learn_more True)
; 



  (= 
    (nmember  $E 
      (Cons  $E $L) 1) True)
; 

  (= 
    (nmember $E 
      (Cons  $_ $R) $P1) 
    ( (nmember $E $R $P) (is $P1 (+ $P 1))))
; 

	

  (= 
    (nth1  1 
      (Cons  $X $_) $X) True)
; 

  (= 
    (nth1 $P1 
      (Cons  $_ $R) $X) 
    ( (> $P1 1) 
      (is $P 
        (- $P1 1)) 
      (nth1 $P $R $X)))
; 
	


  (= 
    (get-case $CaseID) 
    ( (case (Cons  $CaseID $_)) 
      (nl) 
      (nl) 
      (write ' Processing case ') 
      (write $CaseID) 
      (write  ...)))
; 



  (= 
    (get-case-feature $CaseId $Type 
      (:: $Feature $Val)) 
    ( (case (Cons  $CaseId $CaseDescription)) 
      (features $FeatureDescription) 
      (nmember 
        (:: $Type $Feature) $FeatureDescription $Pos) 
      (nth1 $Pos $CaseDescription $Val)))
; 

	

  (= 
    (cobweb $Case) 
    ( (not (get-node $_)) 
      (init-node) 
      (node $Root root 1 1) 
      (new-node-from-case $Case $Root) 
      (assert-node $Root) 
      (msgs (:: nl ' Root initialized with case: ' $Root)) 
      (set-det)))
; 

  (= 
    (cobweb $Case) 
    ( (node $OldRoot root 1 1) 
      (remove-node $OldRoot) 
      (set-det) 
      (copy-node-to-new-node $OldRoot $New) 
      (node $New $_ 1 1) 
      (assert-node $New) 
      (msgs (:: nl ' Root node: ' $OldRoot ' used as new terminal node: ' $New)) 
      (assert-d-sub $OldRoot $New) 
      (new-node-from-case $Case $New2) 
      (node $New2 $_ 1 1) 
      (assert-node $New2) 
      (msgs (:: nl ' Case ' $Case ' becomes new terminal ' $New2)) 
      (assert-d-sub $OldRoot $New2) 
      (incorporate-case-into-node $Case $OldRoot) 
      (node $NewRoot root 2 $_) 
      (assert-node $NewRoot) 
      (msgs (:: nl ' Root changed to: ' $NewRoot)) 
      (set-det)))
; 

  (= 
    (cobweb $Case) 
    ( (node-name $OldRoot root) 
      (remove-node $OldRoot) 
      (set-det) 
      (incorporate-case-into-node $Case $OldRoot) 
      (node-objects $OldRoot $Objects) 
      (is $NewObjects 
        (+ $Objects 1)) 
      (node $NewRoot root $NewObjects $_) 
      (assert-node $NewRoot) 
      (msgs (:: nl ' Root changed to: ' $NewRoot)) 
      (cobweb $NewRoot $Case) 
      (set-det)))
; 


  (= 
    (cobweb  none $_) True)
; 

  (= 
    (cobweb $Parent $Case) 
    ( (best-child $Parent $Case $Best $IBest $Next $DoneRest $RestPred $PartSize $IncPrediction) 
      (set-det) 
      (new-child $Parent $Case $Best $Next $RestPred $PartSize $New $NewPrediction) 
      (set-det) 
      (merge-child $Parent $Case $Best $Next $RestPred $PartSize $Merge $MergePrediction) 
      (set-det) 
      (split-child $Parent $Case $Best $IBest $Next $DoneRest $RestPred $PartSize $SplitPrediction) 
      (set-det) 
      (max-of 
        (:: $IncPrediction $NewPrediction $MergePrediction $SplitPrediction) $BestPrediction) 
      (set-det) 
      (or 
        (, 
          (= $BestPrediction $IncPrediction) 
          (do-incorp $IBest $Best $Merge $New $Case $NewParent)) 
        (or 
          (, 
            (= $BestPrediction $SplitPrediction) 
            (do-split $Best $IBest $New $Merge $Parent $NewParent)) 
          (or 
            (, 
              (= $BestPrediction $MergePrediction) 
              (do-merge $Best $Next $Merge $Parent $IBest $New $NewParent)) 
            (, 
              (= $BestPrediction $NewPrediction) 
              (do-new $Parent $New $IBest $Merge $NewParent))))) 
      (set-det) 
      (cobweb $NewParent $Case)))
; 



  (= 
    (do-incorp $IBest $Best $Merge $New $Case $NewParent) 
    ( (msgs (:: nl ' Incorporating case ' $Case ' into node: ' $IBest)) 
      (move-subs $Best $IBest) 
      (delete-node $Merge) 
      (delete-node $New) 
      (if 
        (terminal-node $Best) 
        (, 
          (ins-node $IBest $Best Nil) 
          (msgs (:: nl ' using old node: ' $Best ' as terminal node.')) 
          (new-node-from-case $Case $New2) 
          (node $New2 $_ 1 1) 
          (assert-node $New2) 
          (msgs (:: nl ' New terminal node: ' $New2)) 
          (ins-node $IBest $New2 Nil) 
          (= $NewParent none)) 
        (, 
          (delete-node $Best) 
          (= $NewParent $IBest)))))
; 



  (= 
    (do-split $Best $IBest $New $Merge $Parent $Parent) 
    ( (msgs (:: nl ' Case splits node: ' $Best)) 
      (delete-node $Best) 
      (delete-node $IBest) 
      (delete-node $New) 
      (delete-node $Merge)))
; 



  (= 
    (do-merge $Best $Next $Merge $Parent $IBest $New $Merge) 
    ( (msgs (:: nl ' Case merges nodes: ' $Best ' and ' $Next nl ' into ' $Merge)) 
      (ins-node $Parent $Merge 
        (:: $Best $Next)) 
      (delete-node $IBest) 
      (delete-node $New)))
; 



  (= 
    (do-new $Parent $New $IBest $Merge none) 
    ( (ins-node $Parent $New Nil) 
      (msgs (:: nl ' New terminal node: ' $New)) 
      (delete-node $IBest) 
      (delete-node $Merge)))
; 



  (= 
    (best-child $Parent $Case $Best $IBest $Next $DoneRest $RestPred $PartSize $IncPrediction) 
    ( (findall $Child 
        (get-d-sub $Parent $Child) 
        (Cons  $C1 
          (Cons  $C2 $DoRest))) 
      (length 
        (Cons  $C1 
          (Cons  $C2 $DoRest)) $PartSize) 
      (copy-and-inc $C1 $Case $IC1) 
      (copy-and-inc $C2 $Case $IC2) 
      (compare-partitions $C1 $IC1 $C2 $IC2 $DoRest Nil $Parent $First $IFirst $Second $ISecond $FirstRestP) 
      (set-det) 
      (best-childs $Parent $Case $DoRest Nil $First $IFirst $Second $ISecond $FirstRestP $Best $IBest $Next $DoneRest $RestPred) 
      (sum-score 
        (:: $IBest) 
        (:: $Next) $Parent $RestPred $IncScore) 
      (node-prediction $Parent $NormPrediction) 
      (is $IncPrediction 
        (/ 
          (- $IncScore $NormPrediction) $PartSize))))
; 



  (= 
    (best-childs $_ $_ Nil $DoneRest $Best $IBest $Next $INext $RestP $Best $IBest $Next $DoneRest $RestP) 
    ( (delete-node $INext) (set-det)))
; 

  (= 
    (best-childs $Parent $Case 
      (Cons  $Try $DoRest) $DoneRest $First $IFirst $Second $ISecond $FirstRestP $Best $IBest $Next $NewDoneRest $RestP) 
    ( (copy-and-inc $Try $Case $ITry) (if (compare-partitions $Second $ISecond $Try $ITry $DoRest (Cons  $First $DoneRest) $Parent $Second $ISecond $Try $ITry $_) (, (delete-node $ITry) (best-childs $Parent $Case $DoRest (Cons  $Try $DoneRest) $First $IFirst $Second $ISecond $FirstRestP $Best $IBest $Next $NewDoneRest $RestP)) (, (delete-node $ISecond) (compare-partitions $First $IFirst $Try $ITry $DoRest (Cons  $Second $DoneRest) $Parent $NFirst $NIFirst $NSecond $NISecond $NFirstRestP) (set-det) (best-childs $Parent $Case $DoRest (Cons  $Second $DoneRest) $NFirst $NIFirst $NSecond $NISecond $NFirstRestP $Best $IBest $Next $NewDoneRest $RestP)))))
; 



  (= 
    (new-child $Parent $Case $Best $Next $RestPred $PartSize $New $NewPrediction) 
    ( (new-node-from-case $Case $New) 
      (node $New $_ 1 1) 
      (assert-node $New) 
      (sum-score 
        (:: $New) 
        (:: $Best $Next) $Parent $RestPred $NewPredictionSum) 
      (node-prediction $Parent $NormPrediction) 
      (is $NewPrediction 
        (/ 
          (- $NewPredictionSum $NormPrediction) 
          (+ $PartSize 1))) 
      (set-det)))
; 



  (= 
    (merge-child $Parent $Case $Best $Next $RestPred 2 $Merge -10000) 
    ( (new-node $Merge) 
      (assert-node $Merge) 
      (set-det)))
; 

  (= 
    (merge-child $Parent $Case $Best $Next $RestPred $PartSize $Merge $MergePrediction) 
    ( (copy-node-to-new-node $Best $Merge) 
      (merge-node-into-node $Next $Merge) 
      (incorporate-case-into-node $Case $Merge) 
      (node-objects $Best $BestO) 
      (node-objects $Next $NextO) 
      (is $MergeObjects 
        (+ 
          (+ $BestO $NextO) 1)) 
      (node-objects $Merge $MergeObjects) 
      (compute-prediction $Merge) 
      (assert-node $Merge) 
      (sum-score 
        (:: $Merge) Nil $Parent $RestPred $MergePredictionSum) 
      (node-prediction $Parent $NormPrediction) 
      (is $MergePrediction 
        (/ 
          (- $MergePredictionSum $NormPrediction) 
          (- $PartSize 1))) 
      (set-det)))
; 




  (= 
    (split-child $Parent $Case $Best $IBest $Next $DoneRest $RestPred $PartSize -10000) 
    ( (terminal-node $Best) (set-det)))
; 

  (= 
    (split-child $Parent $Case $Best $IBest $Next $DoneRest $RestPred $PartSize $SplitPrediction) 
    ( (findall $Child 
        (get-d-sub $Best $Child) 
        (Cons  $C1 $DoRest)) 
      (length 
        (Cons  $C1 $DoRest) $CPartSize) 
      (copy-and-inc $C1 $Case $IC1) 
      (copy-and-inc $Next $Case $INext) 
      (compare-partitions $C1 $IC1 $Next $INext $DoRest $DoneRest $Parent $First $IFirst $Second $ISecond $FirstRestP) 
      (set-det) 
      (best-childs $Parent $Case $DoRest $DoneRest $First $IFirst $Second $ISecond $FirstRestP $CBest $CIBest $CNext $_ $RPred) 
      (sum-score 
        (:: $CIBest) 
        (:: $CNext) $Parent $RPred $SplitPredictionSum) 
      (node-prediction $Parent $NormPrediction) 
      (is $SplitPrediction 
        (/ 
          (- $SplitPredictionSum $NormPrediction) 
          (- 
            (+ $PartSize $CPartSize) 1))) 
      (delete-node $CIBest) 
      (set-det)))
; 

	

  (= 
    (compare-partitions $C1 $IC1 $C2 $IC2 $DoRest $DoneRest $Parent $First $IFirst $Second $ISecond $RestP) 
    ( (sum-score $DoRest $DoneRest $Parent 0 $RestP) 
      (sum-score 
        (:: $C1) 
        (:: $IC2) $Parent $RestP $IC2_Score) 
      (sum-score 
        (:: $IC1) 
        (:: $C2) $Parent $RestP $IC1_Score) 
      (det-if-then-else 
        (> $IC2_Score $IC1_Score) 
        (, 
          (= $First $C2) 
          (= $IFirst $IC2) 
          (= $Second $C1) 
          (= $ISecond $IC1)) 
        (, 
          (= $First $C1) 
          (= $IFirst $IC1) 
          (= $Second $C2) 
          (= $ISecond $IC2))) 
      (set-det)))
; 



  (= 
    (copy-and-inc $Node $Case $INode) 
    ( (new-node $INode) 
      (set-det) 
      (copy-node-to-new-node $Node $INode) 
      (incorporate-case-into-node $Case $INode) 
      (node-objects $Node $Objects) 
      (is $IObjects 
        (+ $Objects 1)) 
      (node-objects $INode $IObjects) 
      (compute-prediction $INode) 
      (assert-node $INode) 
      (set-det)))
; 



  (= 
    (merge-node-into-node $Node $MergeNode) 
    ( (or 
        (, 
          (get-node-nominal-attr $Node $Attr $ValuesCounter) 
          (if 
            (, 
              (remove-node-nominal-attr $MergeNode $Attr $MergeValuesCounter) 
              (sum-value-counter $ValuesCounter $MergeValuesCounter $NewValuesCounter) 
              (assert-node-nominal-attr $MergeNode $Attr $NewValuesCounter)) fail)) True) 
      (or 
        (, 
          (get-node-numeric-attr $Node $Attr $N $SumXiPow2 $SumXi) 
          (if 
            (, 
              (remove-node-numeric-attr $MergeNode $Attr $MergeN $MergeSumXiPow2 $MergeSumXi) 
              (is $NewN 
                (+ $N $MergeN)) 
              (is $NewSumXiPow2 
                (+ $SumXiPow2 $MergeSumXiPow2)) 
              (is $NewSumXi 
                (+ $SumXi $MergeSumXi)) 
              (assert-node-numeric-attr $MergeNode $Attr $NewN $NewSumXiPow2 $NewSumXi)) fail)) True) 
      (set-det)))
; 



  (= 
    (new-node-from-case $Case $Node) 
    ( (new-node $Node) 
      (or 
        (, 
          (get-case-feature $Case nominal 
            (:: $Attr $Val)) 
          (if 
            (assert-node-nominal-attr $Node $Attr 
              (:: (- $Val 1))) fail)) True) 
      (or 
        (, 
          (get-case-feature $Case numeric 
            (:: $Attr $Val)) 
          (if 
            (, 
              (is $SumXiPow2 
                (* $Val $Val)) 
              (assert-node-numeric-attr $Node $Attr 1 $SumXiPow2 $Val)) fail)) True) 
      (set-det)))
; 



  (= 
    (copy-node-to-new-node $Node $NewNode) 
    ( (new-node $NewNode) 
      (or 
        (, 
          (get-node-nominal-attr $Node $Attr $ValuesCounter) 
          (if 
            (assert-node-nominal-attr $NewNode $Attr $ValuesCounter) fail)) True) 
      (or 
        (, 
          (get-node-numeric-attr $Node $Attr $N $SumXiPow2 $SumXi) 
          (if 
            (assert-node-numeric-attr $NewNode $Attr $N $SumXiPow2 $SumXi) fail)) True) 
      (set-det)))
; 



  (= 
    (incorporate-case-into-node $Case $Node) 
    ( (or 
        (, 
          (get-case-feature $Case nominal 
            (:: $Attr $Val)) 
          (if 
            (, 
              (remove-node-nominal-attr $Node $Attr $ValuesCounter) 
              (sum-value-counter $ValuesCounter 
                (:: (- $Val 1)) $NewValuesCounter) 
              (assert-node-nominal-attr $Node $Attr $NewValuesCounter)) fail)) True) 
      (or 
        (, 
          (get-case-feature $Case numeric 
            (:: $Attr $Val)) 
          (if 
            (, 
              (remove-node-numeric-attr $Node $Attr $N $SumXiPow2 $SumXi) 
              (is $NewN 
                (+ $N 1)) 
              (is $NewSumXiPow2 
                (+ $SumXiPow2 
                  (* $Val $Val))) 
              (is $NewSumXi 
                (+ $SumXi $Val)) 
              (assert-node-numeric-attr $Node $Attr $NewN $NewSumXiPow2 $NewSumXi)) fail)) True) 
      (set-det)))
; 

	

  (= 
    (sum-value-counter $ValuesCounter Nil $ValuesCounter) 
    (set-det))
; 

  (= 
    (sum_value_counter  () $ValuesCounter $ValuesCounter) True)
; 

  (= 
    (sum-value-counter 
      (Cons  
        (- $Val $C1) $R1) 
      (Cons  
        (- $Val $C2) $R2) 
      (Cons  
        (- $Val $SumC) $Rest)) 
    ( (is $SumC 
        (+ $C1 $C2)) 
      (set-det) 
      (sum-value-counter $R1 $R2 $Rest)))
; 

  (= 
    (sum-value-counter 
      (Cons  
        (- $Val1 $C1) $R1) 
      (Cons  
        (- $Val2 $C2) $R2) 
      (Cons  
        (- $Val1 $C1) $Rest)) 
    ( (@< $Val1 $Val2) 
      (set-det) 
      (sum-value-counter $R1 
        (Cons  
          (- $Val2 $C2) $R2) $Rest)))
; 

  (= 
    (sum-value-counter 
      (Cons  
        (- $Val1 $C1) $R1) 
      (Cons  
        (- $Val2 $C2) $R2) 
      (Cons  
        (- $Val2 $C2) $Rest)) 
    ( (@< $Val2 $Val1) 
      (set-det) 
      (sum-value-counter 
        (Cons  
          (- $Val1 $C1) $R1) $R2 $Rest)))
; 



  (= 
    (sum_score  () () $_ $Score $Score) True)
; 

  (= 
    (sum-score Nil 
      (Cons  $Node $Rest) $NormNode $Score $Sum_Score) 
    ( (node $Node $_ $Objects $Prediction) 
      (node-objects $NormNode $NormObjects) 
      (is $ZScore 
        (+ 
          (/ 
            (* $Objects $Prediction) $NormObjects) $Score)) 
      (set-det) 
      (sum-score Nil $Rest $NormNode $ZScore $Sum_Score)))
; 

  (= 
    (sum-score 
      (Cons  $Node $Rest) $ToDo $NormNode $Score $Sum_Score) 
    ( (node $Node $_ $Objects $Prediction) 
      (node-objects $NormNode $NormObjects) 
      (is $ZScore 
        (+ 
          (/ 
            (* $Objects $Prediction) $NormObjects) $Score)) 
      (set-det) 
      (sum-score $Rest $ToDo $NormNode $ZScore $Sum_Score)))
; 



  (= 
    (compute-prediction $Node) 
    ( (node-objects $Node $Objects) 
      (add-symbol  &self 
        (prediction_counter  0 0)) 
      (get-node-nominal-attr $Node $_ $ValuesCounter) 
      (if 
        (remove-symbol  &self 
          (prediction_counter  $Sum $Count)) True) 
      (is $NCount 
        (+ $Count 1)) 
      (add-symbol  &self 
        (prediction_counter  $Sum $NCount)) 
      (member 
        (- $_ $C) $ValuesCounter) 
      (if 
        (remove-symbol  &self 
          (prediction_counter  $NNSum $NCount)) True) 
      (is $NSum 
        (+ $NNSum 
          (/ 
            (* $C $C) 
            (* $Objects $Objects)))) 
      (add-symbol  &self 
        (prediction_counter  $NSum $NCount)) 
      (fail)))
; 

  (= 
    (compute-prediction $Node) 
    ( (get-node-numeric-attr $Node $_ $N $SumXiPow2 $SumXi) 
      (if 
        (remove-symbol  &self 
          (prediction_counter  $Sum $Count)) True) 
      (is $NCount 
        (+ $Count 1)) 
      (is $DeviationPow2 
        (- 
          (/ $SumXiPow2 $N) 
          (/ 
            (* $SumXi $SumXi) 
            (* $N $N)))) 
      (abs $DeviationPow2 $PosDeviationPow2) 
      (sqrt $PosDeviationPow2 $Deviation) 
      (is $Pi pi) 
      (sqrt $Pi $S) 
      (is $Const 
        (* 
          (/ 1 4) $S)) 
      (get-acuity $Acuity) 
      (max-of 
        (:: $Deviation $Acuity $Const) $ScoreDeviation) 
      (is $NSum 
        (+ $Sum 
          (/ $Const $ScoreDeviation))) 
      (add-symbol  &self 
        (prediction_counter  $NSum $NCount)) 
      (fail)))
; 

  (= 
    (compute-prediction $Node) 
    ( (remove-symbol  &self 
        (prediction_counter  $Prediction $Count)) 
      (is $NormPrediction 
        (/ $Prediction $Count)) 
      (node-prediction $Node $Prediction) 
      (set-det)))
; 



  (= 
    (get-acuity $Accuity) 
    ( (get-symbols  &self 
        (= 
          (acuity  $Accuity) true)) (set-det)))
; 

  (= 
    (get_acuity  0) True)
; 



  (= 
    (init-node) 
    ( (abolish root 2) 
      (abolish root 4) 
      (or 
        (remove-symbol  &self 
          (gensym_counter  node_ $_)) True) 
      (or 
        (remove-all-symbols  &self 
          (node  root $_ $_)) True) 
      (or 
        (remove-all-symbols  &self 
          (d_sub  root $_)) True) 
      (or 
        (remove-all-symbols  &self 
          (d_sub  $_ root)) True) 
      (set-det)))
; 



  (= 
    (new-node (node $Node $_ $_)) 
    (nonvar $Node))
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
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
    (new-node (node $Node $_ $_)) 
    ( (var $Node) 
      (gensym node- $Node) 
      (dynamic (/ $Node 2)) 
      (dynamic (/ $Node 4)) 
      (abolish $Node 2) 
      (abolish $Node 4) 
      (or 
        (remove-all-symbols  &self 
          (node  $Node $_ $_)) True) 
      (or 
        (remove-all-symbols  &self 
          (d_sub  $Node $_)) True) 
      (or 
        (remove-all-symbols  &self 
          (d_sub  $_ $Node)) True) 
      (set-det)))
; 



  (= 
    (delete-node $Node) 
    ( (remove-node $Node) 
      (node-name $Node $NodeName) 
      (abolish $NodeName 2) 
      (abolish $NodeName 4) 
      (or 
        (, 
          (remove-d-sub $Parent $Node) 
          (remove-d-sub $Node $Child) 
          (assert-d-sub $Parent $Child) 
          (fail)) True) 
      (set-det)))
; 



  (= 
    (terminal-node $Node) 
    (node-objects $Node 1))
; 



  (= 
    (move-subs $Source $Dest) 
    ( (remove-d-sub $Source $Child) 
      (assert-d-sub $Dest $Child) 
      (fail)))
; 

  (= 
    (move-subs $Source $Dest) 
    ( (remove-d-sub $Parent $Source) 
      (assert-d-sub $Parent $Dest) 
      (set-det)))
; 



  (= 
    (ins-node $Parent $New Nil) 
    ( (assert-d-sub $Parent $New) (set-det)))
; 

  (= 
    (ins-node $Parent $New 
      (Cons  $Child $Rest)) 
    ( (or 
        (remove-d-sub $Parent $Child) True) 
      (assert-d-sub $New $Child) 
      (set-det) 
      (ins-node $Parent $New $Rest)))
; 



  (= 
    (node-name 
      (node $Name $_ $_) $Name) 
    ( (nonvar $Name) (set-det)))
; 



  (= 
    (node-objects 
      (node $Name $Objects $_) $Objects) 
    ( (nonvar $Name) 
      (if 
        (var $Objects) 
        (get-node (node $Name $Objects $_)) True) 
      (set-det)))
; 



  (= 
    (node-prediction 
      (node $Name $Objects $Pred) $Pred) 
    ( (nonvar $Name) 
      (or 
        (, 
          (var $Pred) 
          (get-node (node $Name $Objects $Pred))) True) 
      (if 
        (var $Pred) 
        (compute-prediction (node $Name $Objects $Pred)) True) 
      (set-det)))
; 



  (= 
    (node 
      (node $Name $Objects $Pred) $Name $Objects $Pred) 
    ( (nonvar $Name) 
      (if 
        (var $Objects) 
        (get-node (node $Name $Objects $_)) True) 
      (or 
        (, 
          (var $Pred) 
          (get-node (node $Name $Objects $Pred))) True) 
      (if 
        (var $Pred) 
        (compute-prediction (node $Name $Objects $Pred)) True) 
      (set-det)))
; 
	


  (= 
    (get-node (node $Node $Objects $Pred)) 
    (get-symbols  &self 
      (= 
        (node  $Node $Objects $Pred) true)))
; 



  (= 
    (assert-node (node $Node $Objects $Pred)) 
    ( (nonvar $Node) (add-symbol  &self (node  $Node $Objects $Pred))))
; 



  (= 
    (remove-node (node $Node $Objects $Pred)) 
    (remove-symbol  &self 
      (node  $Node $Objects $Pred)))
; 



  (= 
    (get-node-nominal-attr 
      (node $Node $_ $_) $Attr $ValuesCounter) 
    ( (nonvar $Node) 
      (nonvar $Attr) 
      (=.. $Call 
        (:: $Node $Attr $ValuesCounter)) 
      (or 
        (get-symbols  &self 
          (= $Call true)) 
        (if 
          (var $ValuesCounter) 
          (= $ValuesCounter Nil))) 
      (set-det)))
; 

  (= 
    (get-node-nominal-attr 
      (node $Node $_ $_) $Attr $ValuesCounter) 
    ( (nonvar $Node) 
      (var $Attr) 
      (=.. $Call 
        (:: $Node $Attr $ValuesCounter)) 
      (get-symbols  &self 
        (= $Call true))))
; 



  (= 
    (assert_node_nominal_attr  $_ $_ ()) True)
; 

  (= 
    (assert-node-nominal-attr 
      (node $Node $_ $_) $Attr $ValuesCounter) 
    ( (nonvar $Node) 
      (nonvar $Attr) 
      (nonvar $ValuesCounter) 
      (=.. $Call 
        (:: $Node $Attr $ValuesCounter)) 
      (add-symbol  &self $Call)))
; 



  (= 
    (remove-node-nominal-attr 
      (node $Node $_ $_) $Attr $ValuesCounter) 
    ( (nonvar $Node) 
      (nonvar $Attr) 
      (=.. $Call 
        (:: $Node $Attr $ValuesCounter)) 
      (or 
        (remove-symbol  &self $Call) 
        (if 
          (var $ValuesCounter) 
          (= $ValuesCounter Nil))) 
      (set-det)))
; 

  (= 
    (remove-node-nominal-attr 
      (node $Node $_ $_) $Attr $ValuesCounter) 
    ( (nonvar $Node) 
      (=.. $Call 
        (:: $Node $Attr $ValuesCounter)) 
      (remove-symbol  &self $Call)))
; 



  (= 
    (get-node-numeric-attr 
      (node $Node $_ $_) $Attr $N $SumXiPow2 $SumXi) 
    ( (nonvar $Node) 
      (nonvar $Attr) 
      (=.. $Call 
        (:: $Node $Attr $N $SumXiPow2 $SumXi)) 
      (or 
        (get-symbols  &self 
          (= $Call true)) 
        (if 
          (var $N) 
          (, 
            (= $N 0) 
            (= $SumXiPow2 0) 
            (= $SumXi 0)))) 
      (set-det)))
; 

  (= 
    (get-node-numeric-attr 
      (node $Node $_ $_) $Attr $N $SumXiPow2 $SumXi) 
    ( (nonvar $Node) 
      (var $Attr) 
      (=.. $Call 
        (:: $Node $Attr $N $SumXiPow2 $SumXi)) 
      (get-symbols  &self 
        (= $Call true))))
; 



  (= 
    (assert_node_numeric_attr  $_ $_ 0 $_ $_) True)
; 

  (= 
    (assert-node-numeric-attr 
      (node $Node $_ $_) $Attr $N $SumXiPow2 $SumXi) 
    ( (nonvar $Node) 
      (nonvar $Attr) 
      (nonvar $N) 
      (nonvar $SumXiPow2) 
      (nonvar $SumXi) 
      (=.. $Call 
        (:: $Node $Attr $N $SumXiPow2 $SumXi)) 
      (add-symbol  &self $Call)))
; 



  (= 
    (remove-node-numeric-attr 
      (node $Node $_ $_) $Attr $N $SumXiPow2 $SumXi) 
    ( (nonvar $Node) 
      (nonvar $Attr) 
      (=.. $Call 
        (:: $Node $Attr $N $SumXiPow2 $SumXi)) 
      (or 
        (remove-symbol  &self $Call) 
        (if 
          (var $N) 
          (, 
            (= $N 0) 
            (= $SumXiPow2 0) 
            (= $SumXi 0)))) 
      (set-det)))
; 

  (= 
    (remove-node-numeric-attr 
      (node $Node $_ $_) $Attr $N $SumXiPow2 $SumXi) 
    ( (nonvar $Node) 
      (=.. $Call 
        (:: $Node $Attr $N $SumXiPow2 $SumXi)) 
      (remove-symbol  &self $Call)))
; 



  (= 
    (get-d-sub 
      (node $SuperNode $_ $_) 
      (node $SubNode $_ $_)) 
    (get-symbols  &self 
      (= 
        (d_sub  $SuperNode $SubNode) true)))
; 



  (= 
    (assert-d-sub 
      (node $SuperNode $_ $_) 
      (node $SubNode $_ $_)) 
    (add-symbol  &self 
      (d_sub  $SuperNode $SubNode)))
; 



  (= 
    (remove-d-sub 
      (node $SuperNode $_ $_) 
      (node $SubNode $_ $_)) 
    (remove-symbol  &self 
      (d_sub  $SuperNode $SubNode)))
; 



  (= 
    (max-of 
      (Cons  $F $R) $Max) 
    ( (max-of $R $F $Max) (set-det)))
; 

  (= 
    (max_of  () $Max $Max) True)
; 

  (= 
    (max-of 
      (Cons  $F $R) $Best $Max) 
    ( (=< $F $Best) 
      (set-det) 
      (max-of $R $Best $Max)))
; 

  (= 
    (max-of 
      (Cons  $F $R) $_ $Max) 
    (max-of $R $F $Max))
; 



  (= 
    (msgs Nil) 
    (set-det))
; 

  (= 
    (msgs (Cons  $First $Rest)) 
    ( (msg $First) (msgs $Rest)))
; 



  (= 
    (msg (node $ID $Obj $Pred)) 
    ( (set-det) 
      (write $ID) 
      (write (obj=) 
      (write $Obj) 
      (write ',pred=') 
      (write $Pred) 
      (write ))))
; 

  (= 
    (msg $Var) 
    ( (var $Var) 
      (set-det) 
      (write $Var)))
; 

  (= 
    (msg nl) 
    ( (set-det) (nl)))
; 

  (= 
    (msg (nl $N)) 
    ( (set-det) (msg-repeat $N nl)))
; 

  (= 
    (msg sp) 
    ( (set-det) (write ' ')))
; 

  (= 
    (msg (sp $N)) 
    ( (set-det) (msg-repeat $N (outterm ' '))))
; 

  (= 
    (msg (q- $O)) 
    ( (set-det) (write $O)))
; 

  (= 
    (msg $X) 
    ( (set-det) (write $X)))
; 


;
; 


  (= 
    (msg-repeat $N $_) 
    ( (< $N 1) (set-det)))
; 

  (= 
    (msg-repeat $N $Call) 
    ($Call 
      (is $N1 
        (- $N 1)) 
      (msg-repeat $N1 $Call)))
; 



  (= 
    (save-kb $FN) 
    ( (concat $FN .pl $Y) 
      (tell $Y) 
      (print-kb) 
      (told) 
      (set-det)))
; 



  (= 
    (load-kb $FN) 
    ( (clear-kb) 
      (concat $FN .pl $Y) 
      (consult $Y) 
      (set-det)))
; 



  (= 
    (print-kb) 
    ( (if 
        (= $Call 
          (d-sub $SuperNode $SubNode)) True) 
      (get-symbols  &self 
        (= $Call true)) 
      (if 
        (, 
          (writeq $Call) 
          (write .) 
          (nl)) fail)))
; 

  (= 
    (print-kb) 
    ( (get-node $Node) (if (, (writeq $Node) (write .) (nl)) fail)))
; 

  (= 
    (print-kb) 
    ( (get-node $Node) 
      (if 
        (, 
          (node-name $Node $Name) 
          (=.. $Call 
            (:: $Name $Attr $ValuesCounter))) True) 
      (get-symbols  &self 
        (= $Call true)) 
      (if 
        (, 
          (writeq $Call) 
          (write .) 
          (nl)) fail)))
; 

  (= 
    (print-kb) 
    ( (get-node $Node) 
      (if 
        (, 
          (node-name $Node $Name) 
          (=.. $Call 
            (:: $Name $Attr $N $SumXiPow2 $SumXi))) True) 
      (get-symbols  &self 
        (= $Call true)) 
      (if 
        (, 
          (writeq $Call) 
          (write .) 
          (nl)) fail)))
; 

  (= print_kb True)
; 



  (= 
    (clear-kb) 
    ( (get-node $Node) (if (, (node-name $Node $Name) (abolish $Name 2) (abolish $Name 4)) fail)))
; 

  (= 
    (clear-kb) 
    ( (abolish node 3) 
      (abolish d-sub 2) 
      (or 
        (remove-symbol  &self 
          (gensym_counter  node_ $_)) True) 
      (add-symbol  &self 
        (gensym_counter  node_ 0)) 
      (set-det)))
; 



  (= 
    (show) 
    ( (collect-tree root $Tree all) 
      (display-tree $Tree) 
      (set-det)))
; 



  (= 
    (show-classes) 
    ( (collect-tree root $Tree classes) 
      (display-tree $Tree) 
      (set-det)))
; 



  (= 
    (collect_tree  () () $_) True)
; 

  (= 
    (collect-tree 
      (Cons  $Node $Nodes) 
      (Cons  $SubTree $SubTrees) $Type) 
    ( (collect-tree $Node $SubTree $Type) 
      (set-det) 
      (collect-tree $Nodes $SubTrees $Type)))
; 

  (= 
    (collect-tree 
      (Cons  $_ $Nodes) $SubTrees classes) 
    ( (set-det) (collect-tree $Nodes $SubTrees classes)))
; 

  (= 
    (collect-tree $Node 
      (:: $Node $Obj $SubTrees) $Type) 
    ( (get-symbols  &self 
        (= 
          (node  $Node $Obj $_) true)) 
      (or 
        (setof $Sub 
          (get-symbols  &self 
            (= 
              (d_sub  $Node $Sub) true)) $Subs) 
        (, 
          (= $Type all) 
          (= $Subs Nil))) 
      (set-det) 
      (collect-tree $Subs $SubTrees $Type)))
; 



  (= 
    (display-tree (:: $Node $Obj $SubTrees)) 
    ( (write 'Number of cases processed: ') 
      (write $Obj) 
      (nl) 
      (write $Node) 
      (name $Node $String) 
      (length $String $Offset) 
      (display-tree $SubTrees $Offset)))
; 


  (= 
    (display-tree 
      (:: $Node) $Offset) 
    ( (set-det) (display-tree-node $Node $Offset)))
; 

  (= 
    (display-tree 
      (Cons  $Node $Subtrees) $Offset) 
    ( (set-det) 
      (display-tree-node $Node $Offset) 
      (tab $Offset) 
      (display-tree $Subtrees $Offset)))
; 



  (= 
    (display-tree-node 
      (:: $Node $Obj Nil) $Offset) 
    ( (set-det) 
      (write  <-- ) 
      (write $Node) 
      (write  = ) 
      (write $Obj) 
      (nl)))
; 

  (= 
    (display-tree-node 
      (:: $Node $Obj $Subtree) $Offset) 
    ( (write  <-- ) 
      (write $Node) 
      (write  = ) 
      (write $Obj) 
      (name $Node $String1) 
      (length $String1 $SLength) 
      (name $Obj $String2) 
      (length $String2 $NLength) 
      (is $NewOffset 
        (+ 
          (+ 
            (+ 
              (+ $Offset 5) $SLength) 3) $NLength)) 
      (display-tree $Subtree $NewOffset)))
; 



  (= 
    (show-node $Node) 
    ( (collect-attribute-values $Node $Nominal $Numeric) (display-node $Node $Nominal $Numeric)))
; 



  (= 
    (collect-attribute-values $Node $NominalAttValueList $NumericAttValueList) 
    ( (=.. $NominalAttValues 
        (:: $Node $NominalAtt $NominalVals)) 
      (=.. $NumericAttValues 
        (:: $Node $NumericAtt $Number $X $Y)) 
      (findall $NominalAttValues 
        (call $NominalAttValues) $NominalAttValueList) 
      (findall $NumericAttValues 
        (call $NumericAttValues) $NumericAttValueList)))
; 



  (= 
    (if $Cond $Then) 
    ( (call $Cond) 
      (set-det) 
      (call $Then)))
; 

  (= 
    (if  $_ $_) True)
; 


  (= 
    (if $Cond $Then $Else) 
    ( (call $Cond) 
      (set-det) 
      (calltrue $Then)))
; 

  (= 
    (if $_ $_ $Else) 
    (calltrue $Else))
; 

	

  (= 
    (calltrue $Call) 
    ( (call $Call) (set-det)))
; 

  (= 
    (calltrue  $_) True)
; 



  (= 
    (count $VAR $X) 
    ( (remove-symbol  &self 
        (gensym_counter  $VAR $N)) 
      (is $X 
        (+ $N 1)) 
      (add-symbol  &self 
        (gensym_counter  $VAR $X)) 
      (set-det)))
; 

  (= 
    (count $VAR 1) 
    (add-symbol  &self 
      (gensym_counter  $VAR 1)))
; 



  (= 
    (gensym $SYM) 
    (gensym $SYM g))
; 

     
  (= 
    (gensym $N $Sym) 
    ( (count $N $X) 
      (name $N $S1) 
      (name $X $S2) 
      (append $S1 $S2 $S3) 
      (name $Sym $S3)))
; 



  (= 
    (remove-all-symbols  &self $HEAD) 
    ( (var $HEAD) 
      (set-det) 
      (fail)))
; 

  (= 
    (remove-all-symbols  &self 
      (:-  $HEAD $BODY)) 
    ( (var $BODY) 
      (set-det) 
      (or 
        (, 
          (remove-symbol  &self 
            (:-  $HEAD true)) 
          (fail)) True) 
      (or 
        (, 
          (remove-symbol  &self 
            (:-  $HEAD $_)) 
          (fail)) True)))
; 
 
  (= 
    (remove-all-symbols  &self 
      (:-  $HEAD true)) 
    ( (remove-symbol  &self 
        (:-  $HEAD true)) (fail)))
; 

  (= 
    (remove-all-symbols  &self 
      (:-  $HEAD $BODY)) 
    ( (remove-symbol  &self 
        (:-  $HEAD $BODY)) (fail)))
; 

  (= 
    (retractall  $_) True)
; 



  (= 
    (abs $X $Y) 
    ( (< $X 0) 
      (is $Y 
        (* $X -1)) 
      (set-det)))
; 

  (= 
    (abs  $X $X) True)
; 



  (= 
    (sqrt $X $Y) 
    (is $Y 
      (sqrt $X)))
; 



  (= 
    (help) 
    ( (write ' Load data set                    : load-kb(Filename).') 
      (nl) 
      (write ' Process single   file            : learn.') 
      (nl) 
      (write ' Process multiple files           : learn-more.') 
      (nl) 
      (write ' Show hierarchy (classes & cases) : show.') 
      (nl) 
      (write ' Show hierarchy (classes only)    : show-classes.') 
      (nl) 
      (write ' Show node      (not yet impl.)   : show-node(NodeID).') 
      (nl) 
      (write ' Print  result  (cryptical)       : print-kb.') 
      (nl) 
      (write ' Store  result                    : save-kb(Filename).') 
      (nl) 
      (write ' Forget result                    : clear-kb.') 
      (nl)))
; 



  !(help *)
; 



