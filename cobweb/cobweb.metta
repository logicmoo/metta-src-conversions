;                                           (convert-to-metta-file  cobweb $_115026 cobweb/cobweb.pl cobweb/cobweb.metta)

  !(dynamic (/ root 2))
; /******************************************************************/
; /* cobweb.pro Last modification: Fri Jan 14 19:32:01 1994 */
; /* Gennari's incremental concept formation algorithm */
; /******************************************************************/
; ; ; Copyright (c) 1989 Joerg-Uwe Kietz ; ; This program is free software; you can redistribute it and/or ; modify it under the terms of the GNU General Public License ; Version 1 as published by the Free Software Foundation. ; ; This program is distributed in the hope that it will be useful, ; but WITHOUT ANY WARRANTY; without even the implied warranty of ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ; GNU General Public License for more details. ; ; You should have received a copy of the GNU General Public ; Licensealong with this program; if not, write to the Free ; SoftwareFoundation, Inc., 675 Mass Ave, Cambridge, MA 02139, ; USA. ;
; /******************************************************************/
; /* impl. by : Joerg-Uwe Kietz */
; /* F3-XPS */
; /* Gesellschaft fuer Mathematik und Datenver- */
; /* arbeitung */
; /* Schloss Birlinghoven */
; /* 5201 St. Augustin 1 */
; /* E-Mail: kietz@gmdzi.gmd.de */
; /* 1989 */
; /* */
; /* reference : Gennari, J.H., Langley, P., Fisher, D. */
; /* Models of Incremental Concept Formation */
; /* Artifical Intelligence, Vol 40, pp. 11-61, */
; /* 1989 */
; /* */
; /* correction : There is a bracket around the subtraction of */
; /* the two double sums missing in formula (3) on */
; /* p. 35. */
; /* In the case of Split-Node is the best, the call */
; /* cobweb(N,I) leads to double incorporation of I */
; /* into Node N. */
; /* */
; /* call : learn */
; /* */
; /* parameter : acuity/1 asserted in the dynamic DB allows to */
; /* vary the acuity parameter (default is 0). It */
; /* should be a real between 0 and 1. */
; /* */
; /* side effects: assertz and retracts clauses, hence not very */
; /* efficient. Even less efficient, if MeTTa */
; /* dialect does not support first arg indexing. */
; /* If a more efficient implementation (avoiding */
; /* assert/retract) is needed, try to contact Kietz */
; /* directly. */
; /* */
; /* restrictions: MeTTa-dialect must allow real arithmetic. */
; /* */
; /******************************************************************/
; ; TH: Sat May 29 16:19:01 1993 prettyfied output ; corrected bug in cobweb_3.pl ; Sat Jan 8 11:33:49 1994 removed incompatible predicate ; Fri Jan 14 17:31:36 1994 avoidance of rounding errors ; better handling of 'acuity'
; /******************************************************************/
; /* SWI-, YAP-, C- and M-MeTTa specific declaration of dynamical */
; /* clauses. */
; /******************************************************************/
  !(dynamic (/ root 4))
  !(dynamic (/ node 3))
  !(dynamic (/ d-sub 2))
  !(dynamic (/ gensym-counter 2))
  !(dynamic (/ prediction-counter 2))
  !(dynamic (/ features 1))
  !(dynamic (/ case 1))
  !(dynamic (/ acuity 1))


  (= learn  
    (initialize 
      (get-case $X) 
      (cobweb $X) nl nl show-classes fail))
; /******************************************************************/
; /* QUINTUS MeTTa specific import of predicates `sqrt' and `abs' */
; /******************************************************************/
; ;:- use_module(library(math),[sqrt/2,abs/2]).
; /******************************************************************/
; /* */
; /* call : learn */
; /* */
; /* side effects: assertz and retracts clauses */
; /* */
; /* restrictions: MeTTa-dialect must allow real arithmetric. */
; /* */
; /******************************************************************/
; /* With the predicate 'learn' the cobweb algorithm is called in */
; /* batch mode. In this mode the data have to be present at call- */
; /* time in the internal database. They have to follow the format */
; /* shown in the example file. Because, cobweb is an incremental */
; /* concept formation system it retrieves first a datum from the */
; /* database (in MeTTa's processing order) and integrates it in a */
; /* growing concept tree. Before the learning process starts the */
; /* internal concept tree data structure is initialized. The */
; /* concept tree is asserted and modified at runtime. */
; /******************************************************************/
  (= learn  True)
	

  (= initialize  
    (abolish node 3)
    (abolish d-sub 2)
    (abolish root 2)
    (abolish root 4)
    (abolish prediction-counter 2)
    (abolish gensym-counter 2)
    (set-det))


  (= learn-more  
    ( (get-case $X) 
      (cobweb $X) fail))
; /******************************************************************/
; /* */
; /* call : learn_more */
; /* */
; /* side effects: assertz and retracts clauses */
; /* */
; /******************************************************************/
; /* With the predicate 'learn_more' the cobweb algorithm is called */
; /* in batch mode. In this mode the data have to be present at */
; /* call-time in the internal database. They have to follow the */
; /* format shown in the example file. Because, cobweb is an */
; /* incremental concept formation system it retrieves first a */
; /* datum from the database (in MeTTa's processing order) and */
; /* integrates it in a growing concept tree. The data structure of */
; /* the internal concept tree data structure is not initialized, */
; /* thus allowing to process large datasets in smaller parts. The */
; /* concept tree is asserted and modified at runtime. The user has */
; /* to take care that the dataset is erased after each batch run, */
; /* to avoid that data are processed twice. */
; /******************************************************************/
  (= learn_more  True)


  (= (nmember  $E ([|]  $E $L) 1)  True)
; /******************************************************************/
; /* QUINTUS MeTTa specific import of predicates `nth1' and */
; /* `nmember'. `nth1' for getting the nth-element of a list and */
; /* `nmember' for getting a member as well as its position. */
; /******************************************************************/
; ; :- use_module(library(lists),[nth1/3,nmember/3]).
  (= (nmember $E (Cons  $_ $R) $P1)  
    (nmember $E $R $P)
    (is $P1 
      (+ $P 1)))
	

  (= (nth1  1 ([|]  $X $_) $X)  True)
  (= (nth1 $P1 (Cons  $_ $R) $X)  
    (> $P1 1)
    (is $P 
      (- $P1 1))
    (nth1 $P $R $X))	


  (= (get-case $CaseID)  
    ( (case (Cons  $CaseID $_)) nl nl 
      (write ' Processing case ') 
      (write $CaseID) 
      (write  ...)))
; /******************************************************************/
; /* */
; /* call : get_case(+Case) */
; /* */
; /* arguments : Case = unique case identificator */
; /* */
; /******************************************************************/
; /* This is a new version of get_case/1 called by COBWEB. This */
; /* version is independent from the data set. Using this version */
; /* one can change the data-set (i.e consult a different data file)*/
; /* without changing the access operations (especially the number */
; /* of arguments) */
; /******************************************************************/


  (= (get-case-feature $CaseId $Type (:: $Feature $Val))  
    (case (Cons  $CaseId $CaseDescription))
    (features $FeatureDescription)
    (nmember 
      (:: $Type $Feature) $FeatureDescription $Pos)
    (nth1 $Pos $CaseDescription $Val))
; /******************************************************************/
; /* */
; /* call : get_case_features(+Case,+Type,-AttVall) */
; /* */
; /* arguments : Case = unique case identificator */
; /* Type = type description of value */
; /* AttVal = Attribute-Value Pair */
; /* */
; /******************************************************************/
; /* This is a new version of get_case_features/3 called by COBWEB. */
; /* This version is independent from the data set. Using this */
; /* version one can change the data-set (i.e consult a different */
; /* case file) without changing the access operations (especially */
; /* the number of arguments) */
; /******************************************************************/
; ; backtrack through all feature description and
; ; get corresponding feature value
	

  (= (cobweb $Case)  
    ( (not (get-node $_)) init-node 
      (node $Root root 1 1) 
      (new-node-from-case $Case $Root) 
      (assert-node $Root) 
      (msgs (:: nl ' Root initialized with case: ' $Root)) 
      (set-det)))
; /******************************************************************/
; /* */
; /* call : cobweb(+Case) */
; /* */
; /* arguments : Case = unique case identificator */
; /* */
; /* side effects: assertz and retracts clauses */
; /* */
; /******************************************************************/
; /* cobweb processes a case always completely. The case identifi- */
; /* cator is used as pointer to the case. Every case must have a */
; /* unique case identificator, accessible over the predicate */
; /* get_case(Case). The three cases of asserting a case as initial */
; /* root node, as new terminal node in the tree, or integrating */
; /* the case into a node and processing the subtree's are handled. */
; /******************************************************************/
; ; if the tree is empty
; ; generate root from Case
  (= (cobweb $Case)  
    (node $OldRoot root 1 1)
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
    (set-det))
; ; if root is terminal:
; ; first, make a copy of root
; ; second, make a node of Case
; ; third, incorporate Case into root
  (= (cobweb $Case)  
    (node-name $OldRoot root)
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
    (set-det))
; ; if root is not terminal:
; ; first, incorporate Case into root
; ; second, compute new subtree

  (= (cobweb  none $_)  True)
  (= (cobweb $Parent $Case)  
    (best-child $Parent $Case $Best $IBest $Next $DoneRest $RestPred $PartSize $IncPrediction)
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
    (cobweb $NewParent $Case))
; ; Correction of the reference: In the case of Split-Node is the best,
; ; The call cobweb(N,I) leads to double incorporating I into Node N.


  (= (do-incorp $IBest $Best $Merge $New $Case $NewParent)  
    (msgs (:: nl ' Incorporating case ' $Case ' into node: ' $IBest))
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
        (= $NewParent $IBest))))
; ; if Best is a terminal node we have the case from the paper,
; ; where N is terminal before incorporating the new case.
; ; first: make Best to an subnode of IBest
; ; second: generate a new terminal node from Case
; ; and make it to an subnode of IBest, too
; ; than all is done, because Best and NewNode2 are terminal.


  (= (do-split $Best $IBest $New $Merge $Parent $Parent)  
    (msgs (:: nl ' Case splits node: ' $Best))
    (delete-node $Best)
    (delete-node $IBest)
    (delete-node $New)
    (delete-node $Merge))
; ; The next call also copies the d_subs from Best to Parent


  (= (do-merge $Best $Next $Merge $Parent $IBest $New $Merge)  
    (msgs (:: nl ' Case merges nodes: ' $Best ' and ' $Next nl ' into ' $Merge))
    (ins-node $Parent $Merge 
      (:: $Best $Next))
    (delete-node $IBest)
    (delete-node $New))
; ; Possible optimization:
; ; We could remember that Best is the best_child for
; ; incorporate and Next is the second best, RestP = 0, ...,
; ; i.e. we already know what best_child will produce in the
; ; next recursion.


  (= (do-new $Parent $New $IBest $Merge none)  
    (ins-node $Parent $New Nil)
    (msgs (:: nl ' New terminal node: ' $New))
    (delete-node $IBest)
    (delete-node $Merge))
; ; all is done, because New is terminal.


  (= (best-child $Parent $Case $Best $IBest $Next $DoneRest $RestPred $PartSize $IncPrediction)  
    (findall $Child 
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
        (- $IncScore $NormPrediction) $PartSize)))


  (= (best-childs $_ $_ Nil $DoneRest $Best $IBest $Next $INext $RestP $Best $IBest $Next $DoneRest $RestP)  
    (delete-node $INext)
    (set-det))
  (= (best-childs $Parent $Case (Cons  $Try $DoRest) $DoneRest $First $IFirst $Second $ISecond $FirstRestP $Best $IBest $Next $NewDoneRest $RestP)  
    (copy-and-inc $Try $Case $ITry)
    (if 
      (compare-partitions $Second $ISecond $Try $ITry $DoRest 
        (Cons  $First $DoneRest) $Parent $Second $ISecond $Try $ITry $_) 
      (, 
        (delete-node $ITry) 
        (best-childs $Parent $Case $DoRest 
          (Cons  $Try $DoneRest) $First $IFirst $Second $ISecond $FirstRestP $Best $IBest $Next $NewDoneRest $RestP)) 
      (, 
        (delete-node $ISecond) 
        (compare-partitions $First $IFirst $Try $ITry $DoRest 
          (Cons  $Second $DoneRest) $Parent $NFirst $NIFirst $NSecond $NISecond $NFirstRestP) 
        (set-det) 
        (best-childs $Parent $Case $DoRest 
          (Cons  $Second $DoneRest) $NFirst $NIFirst $NSecond $NISecond $NFirstRestP $Best $IBest $Next $NewDoneRest $RestP))))
; ; Try is weaker than our Second,
; ; Delete ITry from memory
; ; put Try to done, use the old Results
; ; Try is stronger than our Second,
; ; Delete ISecond from memory, put Second to done
; ; compare Try with First, use the new Results


  (= (new-child $Parent $Case $Best $Next $RestPred $PartSize $New $NewPrediction)  
    (new-node-from-case $Case $New)
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
    (set-det))
; ; Prediction from New is equal to 1


  (= (merge-child $Parent $Case $Best $Next $RestPred 2 $Merge -10000)  
    (new-node $Merge)
    (assert-node $Merge)
    (set-det))
  (= (merge-child $Parent $Case $Best $Next $RestPred $PartSize $Merge $MergePrediction)  
    (copy-node-to-new-node $Best $Merge)
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
    (set-det))
; ; first, copy BestNode Attributes to MergeNode
; ; second, merge NextNode Attributes into MergeNode
; ; third, incorporate Case into MergeNode
; ; compute the rest of MergeNode
; ; completed W.E.



  (= (split-child $Parent $Case $Best $IBest $Next $DoneRest $RestPred $PartSize -10000)  
    (terminal-node $Best)
    (set-det))
; ; we cannot split Best if it is terminal
  (= (split-child $Parent $Case $Best $IBest $Next $DoneRest $RestPred $PartSize $SplitPrediction)  
    (findall $Child 
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
    (set-det))
; ; best_child of the partition resulting from split
; ; (i.e. best of: Parent-Childs union Best-Childs without Best)
	

  (= (compare-partitions $C1 $IC1 $C2 $IC2 $DoRest $DoneRest $Parent $First $IFirst $Second $ISecond $RestP)  
    (sum-score $DoRest $DoneRest $Parent 0 $RestP)
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
    (set-det))
; /******************************************************************/
; /* compare_partitions */
; /******************************************************************/


  (= (copy-and-inc $Node $Case $INode)  
    (new-node $INode)
    (set-det)
    (copy-node-to-new-node $Node $INode)
    (incorporate-case-into-node $Case $INode)
    (node-objects $Node $Objects)
    (is $IObjects 
      (+ $Objects 1))
    (node-objects $INode $IObjects)
    (compute-prediction $INode)
    (assert-node $INode)
    (set-det))
; /******************************************************************/
; /* basic node operations */
; /******************************************************************/
; ; first, copy all Node Attributes to INode
; ; second, incorporate Case into INode


  (= (merge-node-into-node $Node $MergeNode)  
    (or 
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
    (set-det))
; /******************************************************************/
; /* basic node attribute operations */
; /******************************************************************/


  (= (new-node-from-case $Case $Node)  
    (new-node $Node)
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
    (set-det))


  (= (copy-node-to-new-node $Node $NewNode)  
    (new-node $NewNode)
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
    (set-det))


  (= (incorporate-case-into-node $Case $Node)  
    (or 
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
    (set-det))
	

  (= (sum-value-counter $ValuesCounter Nil $ValuesCounter)  
    (set-det))
; /******************************************************************/
; /* sum_value_counter(+ValuesCounter,+ValuesCounter,-ValuesCounter)*/
; /******************************************************************/
  (= (sum-value-counter  () $ValuesCounter $ValuesCounter)  True)
  (= (sum-value-counter (Cons  (- $Val $C1) $R1) (Cons  (- $Val $C2) $R2) (Cons  (- $Val $SumC) $Rest))  
    (is $SumC 
      (+ $C1 $C2))
    (set-det)
    (sum-value-counter $R1 $R2 $Rest))
  (= (sum-value-counter (Cons  (- $Val1 $C1) $R1) (Cons  (- $Val2 $C2) $R2) (Cons  (- $Val1 $C1) $Rest))  
    (@< $Val1 $Val2)
    (set-det)
    (sum-value-counter $R1 
      (Cons  
        (- $Val2 $C2) $R2) $Rest))
  (= (sum-value-counter (Cons  (- $Val1 $C1) $R1) (Cons  (- $Val2 $C2) $R2) (Cons  (- $Val2 $C2) $Rest))  
    (@< $Val2 $Val1)
    (set-det)
    (sum-value-counter 
      (Cons  
        (- $Val1 $C1) $R1) $R2 $Rest))


  (= (sum-score  () () $_ $Score $Score)  True)
; /******************************************************************/
; /* sum_score(+NodeList,+NodeList,+NormNode,+BaseScore,-FinalScore)*/
; /******************************************************************/
; ;; This follows Fisher 1987 in the ML Journal but this seems to be ;; wrong ;sum_score([],[],_,Score,Score). ;sum_score([],[Node|Rest],NormNode,Score,Sum_Score):- ; node(Node,_,Objects,Prediction), ; node(NormNode,_,NormObjects,NormPrediction), ; ZScore is ((Objects / NormObjects) * (Prediction - NormPrediction)) ; + Score, ; !, ; sum_score([],Rest,NormNode,ZScore,Sum_Score). ;sum_score([Node|Rest],ToDo,NormNode,Score,Sum_Score):- ; node(Node,_,Objects,Prediction), ; node(NormNode,_,NormObjects,NormPrediction), ; ZScore is ((Objects / NormObjects) * (Prediction - NormPrediction)) ; + Score, ; !, ; sum_score(Rest,ToDo,NormNode,ZScore,Sum_Score).
; ;; According to Gennari, et. al. 1989 and the COBWEB/3 implementation ; ;sum_score([],[],_,Score,Score). ;sum_score([],[Node|Rest],NormNode,Score,Sum_Score) :- ; node(Node,_,Objects,Prediction), ; node_objects(NormNode,NormObjects), ; ZScore is ((Objects / NormObjects) * Prediction) ; + Score, ; !, ; sum_score([],Rest,NormNode,ZScore,Sum_Score). ;sum_score([Node|Rest],ToDo,NormNode,Score,Sum_Score) :- ; node(Node,_,Objects,Prediction), ; node_objects(NormNode,NormObjects), ; ZScore is ((Objects / NormObjects) * Prediction) ; + Score, ; !, ; sum_score(Rest,ToDo,NormNode,ZScore,Sum_Score).
; ; According to Kietz 93 this implementation avoids rounding errors ; in certain MeTTa dialects. ; ; The first clause realizes a normalization, which seems to be ; integrated already in certain callers of this routine. If you like ; to use it, it needs to replace the following clause. Except for ; smaller caller code size, there seems to be no other advantage of ; using the commented clause. Anyway I include its code, if someone ; likes to clean up the callers. Note, that such a modification might ; conflict compare_partitions/12 ! ; ; sum_score([],[],NormNode,Score,NormScore):- ; node_prediction(NormNode,NormPrediction), ; NormScore = Score - NormPrediction, ; !.
  (= (sum-score Nil (Cons  $Node $Rest) $NormNode $Score $Sum_Score)  
    (node $Node $_ $Objects $Prediction)
    (node-objects $NormNode $NormObjects)
    (is $ZScore 
      (+ 
        (/ 
          (* $Objects $Prediction) $NormObjects) $Score))
    (set-det)
    (sum-score Nil $Rest $NormNode $ZScore $Sum_Score))
  (= (sum-score (Cons  $Node $Rest) $ToDo $NormNode $Score $Sum_Score)  
    (node $Node $_ $Objects $Prediction)
    (node-objects $NormNode $NormObjects)
    (is $ZScore 
      (+ 
        (/ 
          (* $Objects $Prediction) $NormObjects) $Score))
    (set-det)
    (sum-score $Rest $ToDo $NormNode $ZScore $Sum_Score))


  (= (compute-prediction $Node)  
    ( (node-objects $Node $Objects) 
      (add-atom  &self 
        (prediction-counter  0 0)) 
      (get-node-nominal-attr $Node $_ $ValuesCounter) 
      (if 
        (remove-atom  &self 
          (prediction-counter  $Sum $Count)) True) 
      (is $NCount 
        (+ $Count 1)) 
      (add-atom  &self 
        (prediction-counter  $Sum $NCount)) 
      (member 
        (- $_ $C) $ValuesCounter) 
      (if 
        (remove-atom  &self 
          (prediction-counter  $NNSum $NCount)) True) 
      (is $NSum 
        (+ $NNSum 
          (/ 
            (* $C $C) 
            (* $Objects $Objects)))) 
      (add-atom  &self 
        (prediction-counter  $NSum $NCount)) fail))
; /******************************************************************/
; /* compute the prediction of Node */
; /******************************************************************/
; ;compute_prediction(Node) :- ; node_objects(Node,Objects), ; asserta(prediction_counter(0,0)), ; get_node_nominal_attr(Node,_,ValuesCounter), ; if(retract(prediction_counter(Sum,Count)),true), ; NCount is Count + 1, ; asserta(prediction_counter(Sum,NCount)), ; member(_-C,ValuesCounter), ; if(retract(prediction_counter(NNSum,NCount)),true), ; NSum is NNSum + ((C / Objects) * (C / Objects)), ; asserta(prediction_counter(NSum,NCount)), ; fail. ;compute_prediction(Node) :- ; get_node_numeric_attr(Node,_,N,SumXiPow2,SumXi), ; if(retract(prediction_counter(Sum,Count)),true), ; NCount is Count + 1, ; DeviationPow2 is (SumXiPow2/N) - ((SumXi*SumXi)/(N*N)), ; abs(DeviationPow2,PosDeviationPow2), ; sqrt(PosDeviationPow2,Deviation), ; ; Deviation of one Instance is 0, so we use a minimum deviation of 1 ; ; Here 'acuity' is hardcoded ! ; max_of([Deviation,1],ScoreDeviation), ; NSum is Sum + 1/ScoreDeviation, ; asserta(prediction_counter(NSum,NCount)), ; fail. ;compute_prediction(Node) :- ; ; Normalize the Prediction against the Number of Attributes ; retract(prediction_counter(Prediction,Count)), ; NormPrediction is Prediction / Count, ; node_prediction(Node,NormPrediction), ; !.
; ; According to Kietz 93 this implementation avoids rounding errors ; in certain MeTTa dialects, and allows a better treatment of the ; 'acuity'.
  (= (compute-prediction $Node)  
    ( (get-node-numeric-attr $Node $_ $N $SumXiPow2 $SumXi) 
      (if 
        (remove-atom  &self 
          (prediction-counter  $Sum $Count)) True) 
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
      (add-atom  &self 
        (prediction-counter  $NSum $NCount)) fail))
; ; Deviation of one Instance is 0, so we use a minimum deviation of 1
; ; Here 'acuity' isn't longer hardcoded, instead an value is retieved
; ; from the database. A minimal acuity Const is used to ensure that
; ; Const/ScoreDeviation is in the interval [0,1]!
  (= (compute-prediction $Node)  
    ( (remove-atom  &self 
        (prediction-counter  $Prediction $Count)) 
      (is $NormPrediction 
        (/ $Prediction $Count)) 
      (node-prediction $Node $Prediction) 
      (set-det)))
; ; Normalize the Prediction against the Number of Attributes


  (= (get-acuity $Accuity)  
    ( (==  
        (= 
          (acuity  $Accuity) true) 
        (get-atoms  &self)) (set-det)))
  (= (get-acuity  0)  True)


  (= init-node  
    (abolish root 2)
    (abolish root 4)
    (or 
      (remove-atom  &self 
        (gensym-counter  node_ $_)) True)
    (or 
      (remove-all-atoms  &self 
        (node  root $_ $_)) True)
    (or 
      (remove-all-atoms  &self 
        (d-sub  root $_)) True)
    (or 
      (remove-all-atoms  &self 
        (d-sub  $_ root)) True)
    (set-det))
; /******************************************************************/
; /* Internal Data Structures are: */
; /* */
; /* node(Node,Objects) with */
; /* Node = Atom and Objects = Integer */
; /* node(Attribute,[Val-Count|...]) with */
; /* Attribute = Atom, Val = Atom and Count = Integer */
; /* d_sub(Parent,Child) with */
; /* Parent = Atom and Child = Atom */
; /******************************************************************/


  (= (new-node (node $Node $_ $_))  
    (nonvar $Node))
;
; QUINTUS-MeTTa specific initialization
;
; new_node(node(Node,_,_)):-
;
; var(Node),
;
; gensym(node_,Node),
;
; abolish(Node,2),
;
; abolish(Node,4),
;
; (retractall(node(Node,_,_));true),
;
; (retractall(d_sub(Node,_));true),
;
; (retractall(d_sub(_,Node));true),
;
; !.
;
; SWI- YAP-, C- and M-MeTTa specific initialization with declaration
;
; of dynamical clauses.
  (= (new-node (node $Node $_ $_))  
    (var $Node)
    (gensym node- $Node)
    (dynamic (/ $Node 2))
    (dynamic (/ $Node 4))
    (abolish $Node 2)
    (abolish $Node 4)
    (or 
      (remove-all-atoms  &self 
        (node  $Node $_ $_)) True)
    (or 
      (remove-all-atoms  &self 
        (d-sub  $Node $_)) True)
    (or 
      (remove-all-atoms  &self 
        (d-sub  $_ $Node)) True)
    (set-det))


  (= (delete-node $Node)  
    (remove-node $Node)
    (node-name $Node $NodeName)
    (abolish $NodeName 2)
    (abolish $NodeName 4)
    (or 
      (, 
        (remove-d-sub $Parent $Node) 
        (remove-d-sub $Node $Child) 
        (assert-d-sub $Parent $Child) fail) True)
    (set-det))
; ; This asumes that there is mostly one ParentNode


  (= (terminal-node $Node)  
    (node-objects $Node 1))


  (= (move-subs $Source $Dest)  
    ( (remove-d-sub $Source $Child) 
      (assert-d-sub $Dest $Child) fail))
  (= (move-subs $Source $Dest)  
    (remove-d-sub $Parent $Source)
    (assert-d-sub $Parent $Dest)
    (set-det))


  (= (ins-node $Parent $New Nil)  
    (assert-d-sub $Parent $New)
    (set-det))
  (= (ins-node $Parent $New (Cons  $Child $Rest))  
    (or 
      (remove-d-sub $Parent $Child) True)
    (assert-d-sub $New $Child)
    (set-det)
    (ins-node $Parent $New $Rest))


  (= (node-name (node $Name $_ $_) $Name)  
    (nonvar $Name)
    (set-det))
; /******************************************************************/
; /* node(Node:Atom,Objects:Integer,Prediction:Real) */
; /******************************************************************/


  (= (node-objects (node $Name $Objects $_) $Objects)  
    (nonvar $Name)
    (if 
      (var $Objects) 
      (get-node (node $Name $Objects $_)) True)
    (set-det))


  (= (node-prediction (node $Name $Objects $Pred) $Pred)  
    (nonvar $Name)
    (or 
      (, 
        (var $Pred) 
        (get-node (node $Name $Objects $Pred))) True)
    (if 
      (var $Pred) 
      (compute-prediction (node $Name $Objects $Pred)) True)
    (set-det))


  (= (node (node $Name $Objects $Pred) $Name $Objects $Pred)  
    (nonvar $Name)
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
    (set-det))	


  (= (get-node (node $Node $Objects $Pred))  
    (==  
      (= 
        (node  $Node $Objects $Pred) true) 
      (get-atoms  &self)))


  (= (assert-node (node $Node $Objects $Pred))  
    ( (nonvar $Node) (add-atom  &self (node  $Node $Objects $Pred))))


  (= (remove-node (node $Node $Objects $Pred))  
    (remove-atom  &self 
      (node  $Node $Objects $Pred)))


  (= (get-node-nominal-attr (node $Node $_ $_) $Attr $ValuesCounter)  
    (nonvar $Node)
    (nonvar $Attr)
    (=.. $Call 
      (:: $Node $Attr $ValuesCounter))
    (or 
      (==  
        (= $Call true) 
        (get-atoms  &self)) 
      (if 
        (var $ValuesCounter) 
        (= $ValuesCounter Nil)))
    (set-det))
; /******************************************************************/
; /* node(Attr:Atom,[Val:Atom-Count:Integer|...]) */
; /******************************************************************/
  (= (get-node-nominal-attr (node $Node $_ $_) $Attr $ValuesCounter)  
    ( (nonvar $Node) 
      (var $Attr) 
      (=.. $Call 
        (:: $Node $Attr $ValuesCounter)) 
      (==  
        (= $Call true) 
        (get-atoms  &self))))


  (= (assert-node-nominal-attr  $_ $_ ())  True)
  (= (assert-node-nominal-attr (node $Node $_ $_) $Attr $ValuesCounter)  
    ( (nonvar $Node) 
      (nonvar $Attr) 
      (nonvar $ValuesCounter) 
      (=.. $Call 
        (:: $Node $Attr $ValuesCounter)) 
      (add-atom  &self $Call)))


  (= (remove-node-nominal-attr (node $Node $_ $_) $Attr $ValuesCounter)  
    (nonvar $Node)
    (nonvar $Attr)
    (=.. $Call 
      (:: $Node $Attr $ValuesCounter))
    (or 
      (remove-atom  &self $Call) 
      (if 
        (var $ValuesCounter) 
        (= $ValuesCounter Nil)))
    (set-det))
  (= (remove-node-nominal-attr (node $Node $_ $_) $Attr $ValuesCounter)  
    ( (nonvar $Node) 
      (=.. $Call 
        (:: $Node $Attr $ValuesCounter)) 
      (remove-atom  &self $Call)))


  (= (get-node-numeric-attr (node $Node $_ $_) $Attr $N $SumXiPow2 $SumXi)  
    (nonvar $Node)
    (nonvar $Attr)
    (=.. $Call 
      (:: $Node $Attr $N $SumXiPow2 $SumXi))
    (or 
      (==  
        (= $Call true) 
        (get-atoms  &self)) 
      (if 
        (var $N) 
        (, 
          (= $N 0) 
          (= $SumXiPow2 0) 
          (= $SumXi 0))))
    (set-det))
; /******************************************************************/
; /* node(Attr:Atom,N:Integer,SumXiPow2:Integer,SumXi:Integer) */
; /******************************************************************/
  (= (get-node-numeric-attr (node $Node $_ $_) $Attr $N $SumXiPow2 $SumXi)  
    ( (nonvar $Node) 
      (var $Attr) 
      (=.. $Call 
        (:: $Node $Attr $N $SumXiPow2 $SumXi)) 
      (==  
        (= $Call true) 
        (get-atoms  &self))))


  (= (assert-node-numeric-attr  $_ $_ 0 $_ $_)  True)
  (= (assert-node-numeric-attr (node $Node $_ $_) $Attr $N $SumXiPow2 $SumXi)  
    ( (nonvar $Node) 
      (nonvar $Attr) 
      (nonvar $N) 
      (nonvar $SumXiPow2) 
      (nonvar $SumXi) 
      (=.. $Call 
        (:: $Node $Attr $N $SumXiPow2 $SumXi)) 
      (add-atom  &self $Call)))


  (= (remove-node-numeric-attr (node $Node $_ $_) $Attr $N $SumXiPow2 $SumXi)  
    (nonvar $Node)
    (nonvar $Attr)
    (=.. $Call 
      (:: $Node $Attr $N $SumXiPow2 $SumXi))
    (or 
      (remove-atom  &self $Call) 
      (if 
        (var $N) 
        (, 
          (= $N 0) 
          (= $SumXiPow2 0) 
          (= $SumXi 0))))
    (set-det))
  (= (remove-node-numeric-attr (node $Node $_ $_) $Attr $N $SumXiPow2 $SumXi)  
    ( (nonvar $Node) 
      (=.. $Call 
        (:: $Node $Attr $N $SumXiPow2 $SumXi)) 
      (remove-atom  &self $Call)))


  (= (get-d-sub (node $SuperNode $_ $_) (node $SubNode $_ $_))  
    (==  
      (= 
        (d-sub  $SuperNode $SubNode) true) 
      (get-atoms  &self)))
; /******************************************************************/
; /* d_sub(SuperNode:Atom,SubNode:Atom) */
; /******************************************************************/


  (= (assert-d-sub (node $SuperNode $_ $_) (node $SubNode $_ $_))  
    (add-atom  &self 
      (d-sub  $SuperNode $SubNode)))


  (= (remove-d-sub (node $SuperNode $_ $_) (node $SubNode $_ $_))  
    (remove-atom  &self 
      (d-sub  $SuperNode $SubNode)))


  (= (max-of (Cons  $F $R) $Max)  
    (max-of $R $F $Max)
    (set-det))
; /******************************************************************/
; /* Miscealenous definitions */
; /******************************************************************/
  (= (max-of  () $Max $Max)  True)
  (= (max-of (Cons  $F $R) $Best $Max)  
    (=< $F $Best)
    (set-det)
    (max-of $R $Best $Max))
  (= (max-of (Cons  $F $R) $_ $Max)  
    (max-of $R $F $Max))


  (= (msgs Nil)  
    (set-det))
  (= (msgs (Cons  $First $Rest))  
    (msg $First)
    (msgs $Rest))


  (= (msg (node $ID $Obj $Pred))  
    (set-det)
    (write $ID)
    (write (obj=)
    (write $Obj)
    (write ',pred=')
    (write $Pred)
    (write )))
  (= (msg $Var)  
    (var $Var)
    (set-det)
    (write $Var))
  (= (msg nl)  
    ( (set-det) nl))
  (= (msg (nl $N))  
    (set-det)
    (msg-repeat $N nl))
  (= (msg sp)  
    (set-det)
    (write ' '))
  (= (msg (sp $N))  
    (set-det)
    (msg-repeat $N 
      (outterm ' ')))
  (= (msg (q- $O))  
    (set-det)
    (write $O))
  (= (msg $X)  
    (set-det)
    (write $X))

;
; msg_repeat Call N times.

  (= (msg-repeat $N $_)  
    (< $N 1)
    (set-det))
  (= (msg-repeat $N $Call)  
    ($Call 
      (is $N1 
        (- $N 1)) 
      (msg-repeat $N1 $Call)))


  (= (save-kb $FN)  
    ( (concat $FN .pl $Y) 
      (tell $Y) print-kb told 
      (set-det)))


  (= (load-kb $FN)  
    (clear-kb 
      (concat $FN .pl $Y) 
      (consult $Y) 
      (set-det)))


  (= print-kb  
    ( (if 
        (= $Call 
          (d-sub $SuperNode $SubNode)) True) 
      (==  
        (= $Call true) 
        (get-atoms  &self)) 
      (if 
        (, 
          (writeq $Call) 
          (write .) nl) fail)))
  (= print-kb  
    (get-node $Node)
    (if 
      (, 
        (writeq $Node) 
        (write .) nl) fail))
  (= print-kb  
    ( (get-node $Node) 
      (if 
        (, 
          (node-name $Node $Name) 
          (=.. $Call 
            (:: $Name $Attr $ValuesCounter))) True) 
      (==  
        (= $Call true) 
        (get-atoms  &self)) 
      (if 
        (, 
          (writeq $Call) 
          (write .) nl) fail)))
  (= print-kb  
    ( (get-node $Node) 
      (if 
        (, 
          (node-name $Node $Name) 
          (=.. $Call 
            (:: $Name $Attr $N $SumXiPow2 $SumXi))) True) 
      (==  
        (= $Call true) 
        (get-atoms  &self)) 
      (if 
        (, 
          (writeq $Call) 
          (write .) nl) fail)))
  (= print_kb  True)


  (= clear-kb  
    (get-node $Node)
    (if 
      (, 
        (node-name $Node $Name) 
        (abolish $Name 2) 
        (abolish $Name 4)) fail))
  (= clear-kb  
    ( (abolish node 3) 
      (abolish d-sub 2) 
      (or 
        (remove-atom  &self 
          (gensym-counter  node_ $_)) True) 
      (add-atom  &self 
        (gensym-counter  node_ 0)) 
      (set-det)))


  (= show  
    (collect-tree root $Tree all)
    (display-tree $Tree)
    (set-det))


  (= show-classes  
    (collect-tree root $Tree classes)
    (display-tree $Tree)
    (set-det))


  (= (collect-tree  () () $_)  True)
  (= (collect-tree (Cons  $Node $Nodes) (Cons  $SubTree $SubTrees) $Type)  
    (collect-tree $Node $SubTree $Type)
    (set-det)
    (collect-tree $Nodes $SubTrees $Type))
  (= (collect-tree (Cons  $_ $Nodes) $SubTrees classes)  
    (set-det)
    (collect-tree $Nodes $SubTrees classes))
; ; Terminals are ignored here
  (= (collect-tree $Node (:: $Node $Obj $SubTrees) $Type)  
    ( (==  
        (= 
          (node  $Node $Obj $_) true) 
        (get-atoms  &self)) 
      (or 
        (setof $Sub 
          (==  
            (= 
              (d-sub  $Node $Sub) true) 
            (get-atoms  &self)) $Subs) 
        (, 
          (= $Type all) 
          (= $Subs Nil))) 
      (set-det) 
      (collect-tree $Subs $SubTrees $Type)))


  (= (display-tree (:: $Node $Obj $SubTrees))  
    ( (write 'Number of cases processed: ') 
      (write $Obj) nl 
      (write $Node) 
      (name $Node $String) 
      (length $String $Offset) 
      (display-tree $SubTrees $Offset)))

  (= (display-tree (:: $Node) $Offset)  
    (set-det)
    (display-tree-node $Node $Offset))
  (= (display-tree (Cons  $Node $Subtrees) $Offset)  
    (set-det)
    (display-tree-node $Node $Offset)
    (tab $Offset)
    (display-tree $Subtrees $Offset))


  (= (display-tree-node (:: $Node $Obj Nil) $Offset)  
    ( (set-det) 
      (write  <-- ) 
      (write $Node) 
      (write  = ) 
      (write $Obj) nl))
  (= (display-tree-node (:: $Node $Obj $Subtree) $Offset)  
    (write  <-- )
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
    (display-tree $Subtree $NewOffset))


  (= (show-node $Node)  
    (collect-attribute-values $Node $Nominal $Numeric)
    (display-node $Node $Nominal $Numeric))


  (= (collect-attribute-values $Node $NominalAttValueList $NumericAttValueList)  
    (=.. $NominalAttValues 
      (:: $Node $NominalAtt $NominalVals))
    (=.. $NumericAttValues 
      (:: $Node $NumericAtt $Number $X $Y))
    (findall $NominalAttValues 
      (call $NominalAttValues) $NominalAttValueList)
    (findall $NumericAttValues 
      (call $NumericAttValues) $NumericAttValueList))


  (= (if $Cond $Then)  
    (call $Cond)
    (set-det)
    (call $Then))
; /******************************************************************/
; /* utility predicates */
; /******************************************************************/
  (= (if  $_ $_)  True)

  (= (if $Cond $Then $Else)  
    (call $Cond)
    (set-det)
    (calltrue $Then))
  (= (if $_ $_ $Else)  
    (calltrue $Else))
	

  (= (calltrue $Call)  
    (call $Call)
    (set-det))
  (= (calltrue  $_)  True)


  (= (count $VAR $X)  
    ( (remove-atom  &self 
        (gensym-counter  $VAR $N)) 
      (is $X 
        (+ $N 1)) 
      (add-atom  &self 
        (gensym-counter  $VAR $X)) 
      (set-det)))
  (= (count $VAR 1)  
    (add-atom  &self 
      (gensym-counter  $VAR 1)))


  (= (gensym $SYM)  
    (gensym $SYM g))
     
  (= (gensym $N $Sym)  
    (count $N $X)
    (name $N $S1)
    (name $X $S2)
    (append $S1 $S2 $S3)
    (name $Sym $S3))


  (= (remove-all-atoms  &self $HEAD)  
    ( (var $HEAD) 
      (set-det) fail))
  (= (remove-all-atoms  &self (:-  $HEAD $BODY))  
    (var $BODY)
    (set-det)
    (or 
      (, 
        (remove-atom  &self 
          (:-  $HEAD true)) fail) True)
    (or 
      (, 
        (remove-atom  &self 
          (:-  $HEAD $_)) fail) True)) 
  (= (remove-all-atoms  &self (:-  $HEAD true))  
    ( (remove-atom  &self 
        (:-  $HEAD true)) fail))
  (= (remove-all-atoms  &self (:-  $HEAD $BODY))  
    ( (remove-atom  &self 
        (:-  $HEAD $BODY)) fail))
  (= (retractall  $_)  True)


  (= (abs $X $Y)  
    (< $X 0)
    (is $Y 
      (* $X -1))
    (set-det))
; /******************************************************************/
; /* YAP-MeTTa specific declaration. */
; /******************************************************************/
  (= (abs  $X $X)  True)


  (= (sqrt $X $Y)  
    (is $Y 
      (sqrt $X)))


  (= help  
    ( (write ' Load data set                    : load-kb(Filename).') nl 
      (write ' Process single   file            : learn.') nl 
      (write ' Process multiple files           : learn-more.') nl 
      (write ' Show hierarchy (classes & cases) : show.') nl 
      (write ' Show hierarchy (classes only)    : show-classes.') nl 
      (write ' Show node      (not yet impl.)   : show-node(NodeID).') nl 
      (write ' Print  result  (cryptical)       : print-kb.') nl 
      (write ' Store  result                    : save-kb(Filename).') nl 
      (write ' Forget result                    : clear-kb.') nl))


  !(help *)


