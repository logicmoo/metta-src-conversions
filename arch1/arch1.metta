
  (= 
    (process-stored-inputs $ConceptName) 
    ( (or 
        (remove-symbol  &self 
          (concept  $ConceptName $_)) True) 
      (set-det) 
      (process-stored-inputs-body $ConceptName)))
; 



  (= 
    (process-stored-inputs-body $ConceptName) 
    ( (msgs (:: nl 'Processing stored inputs for ' $ConceptName  ...)) 
      (input $ID $Type $ConceptName $Input) 
      (msgs (:: nl 'Input ' $ID :  $Type nl $Input)) 
      (process-example $Type $ConceptName $Input) 
      (concept $ConceptName $Concept) 
      (concept-template $Concept $Template) 
      (concept-constraints $Concept $Constraints) 
      (msgs (:: nl 'New Concept Definition is:' nl '--- Template:' nl $Template nl '--- Constraints:' nl $Constraints nl)) 
      (fail)))
; 

  (= 
    (process-stored-inputs-body $_) 
    (msgs (:: nl 'No more inputs. Done.')))
; 




  (= 
    (process-example example $ConceptName $Example) 
    ( (remove-symbol  &self 
        (concept  $ConceptName $Definition)) 
      (set-det) 
      (generalize $Example $Definition $NewDefinition) 
      (add-symbol  &self 
        (concept  $ConceptName $NewDefinition)) 
      (set-det)))
; 

  (= 
    (process-example example $ConceptName $Example) 
    ( (set-det) 
      (initial-generalization $Example $InitialDefinition) 
      (add-symbol  &self 
        (concept  $ConceptName $InitialDefinition))))
; 

  (= 
    (process-example near-miss $ConceptName $Example) 
    ( (remove-symbol  &self 
        (concept  $ConceptName $Definition)) 
      (set-det) 
      (specialize $Example $Definition $NewDefinition) 
      (add-symbol  &self 
        (concept  $ConceptName $NewDefinition)) 
      (set-det)))
; 

  (= 
    (process-example near-miss $ConceptName $_) 
    ( (set-det) (msgs (:: nl 'Cannot process a (near) miss as the first example of ' $ConceptName . nl 'Please begin with a (prototypical) example instead.'))))
; 



  (= 
    (specialize $Example $OldConcept $OldConcept) 
    ( (is-member $Example $OldConcept no) 
      (set-det) 
      (msgs (:: nl 'Already excluded.'))))
; 

  (= 
    (specialize $Example $OldConcept $NewConcept) 
    ( (match $OldConcept $Example Nil $BL $PL $ML $AL) 
      (find-important-differences-p specialize $PL $ML $AL $DifferenceDescriptions) 
      (specialize-concept-definition $DifferenceDescriptions $BL $OldConcept $NewConcept) 
      (set-det)))
; 

  (= 
    (specialize $_ $Definition $Definition) 
    (msgs (:: nl 'Specialize failed. Ignoring example.')))
; 




  (= 
    (generalize $Example $OldConcept $OldConcept) 
    ( (is-member $Example $OldConcept yes) 
      (set-det) 
      (msgs (:: nl 'Already included.'))))
; 

  (= 
    (generalize $Example $OldConcept $NewConcept) 
    ( (match $OldConcept $Example Nil $_ $PL $ML $AL) 
      (find-important-differences-p generalize $PL $ML $AL $DifferenceDescriptions) 
      (generalize-concept-definition $DifferenceDescriptions $OldConcept $NewConcept)))
; 

  (= 
    (generalize $_ $Concept $Concept) 
    (msgs (:: nl 'Generalize failed. Ignoring example.')))
; 




  (= 
    (find-important-differences-p generalize $PL $ML $_ $DD) 
    ( (mark-as partial $PL $DD1) 
      (mark-as missing $ML $DD2) 
      (append $DD1 $DD2 $DD)))
; 

  (= 
    (find-important-differences-p specialize $_ Nil $AL $DD) 
    ( (not (= $AL Nil)) 
      (set-det) 
      (mark-as addition $AL $DD)))
; 

  (= 
    (find-important-differences-p specialize $_ $ML $_ $DD) 
    ( (not (= $ML Nil)) 
      (set-det) 
      (mark-as missing $ML $DD)))
; 

  (= 
    (find-important-differences-p specialize $PL $_ $_ $DD) 
    (mark-as partial $PL $DD))
; 



  (= 
    (differences-acceptable-p $Diffs) 
    ( (length $Diffs 1) (set-det)))
; 

  (= 
    (differences-acceptable-p $Diffs) 
    ( (same-functor-p $Diffs $_) (set-det)))
; 



  (= 
    (same-functor-p Nil $_) 
    (set-det))
; 

  (= 
    (same-functor-p 
      (:: $LastDifference) $Functor) 
    ( (set-det) (=.. $LastDifference (Cons  $Functor $_))))
; 

  (= 
    (same-functor-p 
      (Cons  $First $Rest) $Functor) 
    ( (same-functor-p $Rest $Functor) (=.. $First (Cons  $Functor $_))))
; 



  (= 
    (mark-as $_ Nil Nil) 
    (set-det))
; 

  (= 
    (mark-as $Mark 
      (Cons  $First $Rest) 
      (Cons  $FirstMarked $RestMarked)) 
    ( (=.. $FirstMarked 
        (:: $Mark $First)) (mark-as $Mark $Rest $RestMarked)))
; 



  (= 
    (concept_p  
      (concept  $_ $_ $_)) True)
; 


  (= 
    (concept_name  
      (concept  $Name $_ $_) $Name) True)
; 


  (= 
    (concept_template  
      (concept  $_ $Template $_) $Template) True)
; 


  (= 
    (concept_constraints  
      (concept  $_ $_ $Constraints) $Constraints) True)
; 



  (= 
    (alter_concept_constraints  
      (concept  $N $T $_) $C 
      (concept  $N $T $C)) True)
; 


  (= 
    (alter_concept_template  
      (concept  $N $_ $C) $T 
      (concept  $N $T $C)) True)
; 



  (= 
    (concept-equal-p 
      (concept $Name $Template1 $Constraints1) 
      (concept $Name $Template2 $Constraints2)) 
    ( (set-equal-p $Template1 $Template2) (set-equal-p $Constraints1 $Constraints2)))
; 



  (= 
    (print-concept $Concept) 
    ( (concept-name $Concept $Name) 
      (or 
        (= $Name -) True) 
      (concept-template $Concept $Template) 
      (concept-constraints $Concept $Constraints) 
      (msgs (:: nl 'Concept ' $Name : nl 'Template:    ' $Template nl 'Constraints: ' $Constraints)) 
      (set-det)))
; 



  (= 
    (print-concepts $Concepts) 
    ( (member $Concept $Concepts) 
      (print-concept $Concept) 
      (fail)))
; 

  (= 
    (print_concepts  $_) True)
; 



  (= 
    (is-member $Example $Concept $Decision) 
    ( (match $Concept $Example Nil $_ $PL $ML $_) 
      (set-det) 
      (or 
        (, 
          (= $ML Nil) 
          (= $PL Nil) 
          (set-det) 
          (= $Decision yes)) 
        (= $Decision possible))))
; 

  (= 
    (is_member  $_ $_ no) True)
; 



  (= 
    (match $Concept $Ex $OldBL $NewBL $PL $ML $AL) 
    (match $Concept $Ex $OldBL $NewBL $PL $ML $AL t))
; 


  (= 
    (match $Concept $Ex $OldBL $NewBL $PL $ML $AL $CheckNecessaryP) 
    ( (concept-template $Concept $Templ) 
      (concept-constraints $Concept $Constraints) 
      (unambiguous-match $Templ $Ex $OldBL $RestTempl1 $RestEx1 $BL1) 
      (perfect-match $RestTempl1 $RestEx1 $BL1 $RestTempl2 $RestEx2 $BL2 $CheckNecessaryP) 
      (not (unsatisfied-constraint-p $Constraints $Ex $BL2)) 
      (partial-match $RestTempl2 $RestEx2 $BL2 $NewBL $PL $ML $AL)))
; 



  (= 
    (unambiguous-match $Templ $Ex $OldBL $Templ $Ex $OldBL) 
    ( (or 
        (= $Templ Nil) 
        (= $Ex Nil)) (set-det)))
; 

  (= 
    (unambiguous-match $Templ $Ex $OldBL $RestTempl $RestEx $NewBL) 
    ( (find-unambiguous-match-p $Templ $Ex $OldBL $RestTempl1 $RestEx1 $NewBL1) 
      (set-det) 
      (unambiguous-match $RestTempl1 $RestEx1 $NewBL1 $RestTempl $RestEx $NewBL)))
; 

  (= 
    (unambiguous-match $Templ $Ex $OldBL $Templ $Ex $OldBL) 
    (set-det))
; 



  (= 
    (find-unambiguous-match-p $Templ $Ex $OldBL $RestTempl $RestEx $NewBL) 
    ( (enumerate $Templ $Part $RestTempl) 
      (find-unambiguous-match-p1 $Part $Templ $Ex $OldBL $RestEx $NewBL) 
      (set-det)))
; 



  (= 
    (find-unambiguous-match-p1 $Part $Template $Ex $OldBL $RestEx $NewBL) 
    ( (enumerate $Ex $ExamplePart $RestEx) 
      (perfect-match-p $Part $ExamplePart $OldBL $NewBL) 
      (not (non-unique-match-p $Part $ExamplePart $Template $Ex $OldBL)) 
      (set-det)))
; 



  (= 
    (non-unique-match-p $Part $ExamplePart $Template $Example $BL) 
    ( (member $Part1 $Template) 
      (member $ExamplePart1 $Example) 
      (or 
        (, 
          (not (= $Part1 $Part)) 
          (= $ExamplePart1 $ExamplePart)) 
        (, 
          (not (= $ExamplePart1 $ExamplePart)) 
          (= $Part1 $Part))) 
      (perfect-match-p $Part1 $ExamplePart1 $BL $_) 
      (set-det)))
; 



  (= 
    (perfect-match $Templ $Ex $OldBL $RestTempl $RestEx $NewBL $CheckNecessaryP) 
    ( (findbag 
        (:: $RestTempl1 $RestEx1 $NewBL1) 
        (, 
          (perfect-match1 $Templ $Ex $OldBL $RestTempl1 $RestEx1 $NewBL1) 
          (check-for-unmatched-necessary-constraints-p $RestTempl1 $CheckNecessaryP)) $PerfectMatches) 
      (remove-duplicates $PerfectMatches $PerfectMatches1 perfect-match-equal-p) 
      (list-sort $PerfectMatches1 $SortedPerfectMatches perfect-match-better-p) 
      (get-best $SortedPerfectMatches $BestMatches perfect-match-better-p) 
      (member 
        (:: $RestTempl $RestEx $NewBL) $BestMatches)))
; 



  (= 
    (perfect-match1 $Templ $Ex $OldBL $Templ $Ex $OldBL) 
    ( (or 
        (= $Templ Nil) 
        (= $Ex Nil)) (set-det)))
; 

  (= 
    (perfect-match1 $Templ $Ex $OldBL $Templ $Ex $OldBL) 
    ( (not (find-perfect-match-p $Templ $Ex $OldBL $_ $_ $_)) (set-det)))
; 

  (= 
    (perfect-match1 $Templ $Ex $OldBL $RestTempl $RestEx $NewBL) 
    ( (find-perfect-match-p $Templ $Ex $OldBL $RestTempl1 $RestEx1 $NewBL1) (perfect-match1 $RestTempl1 $RestEx1 $NewBL1 $RestTempl $RestEx $NewBL)))
; 



  (= 
    (find-perfect-match-p $Templ $Ex $OldBL $RestTempl $RestEx $NewBL) 
    ( (enumerate $Templ $Part $RestTempl) (find-perfect-match-p1 $Part $Ex $OldBL $RestEx $NewBL)))
; 



  (= 
    (find-perfect-match-p1 $Part $Ex $OldBL $RestEx $NewBL) 
    ( (enumerate $Ex $ExamplePart $RestEx) (perfect-match-p $Part $ExamplePart $OldBL $NewBL)))
; 



  (= 
    (perfect-match-p 
      (must $Constraint) $Fact $OldBL $NewBL) 
    (perfect-match-p $Constraint $Fact $OldBL $NewBL))
; 

  (= 
    (perfect-match-p $Constraint $Fact $OldBL $NewBL) 
    ( (=.. $Constraint 
        (Cons  $Functor $Args1)) 
      (=.. $Fact 
        (Cons  $Functor $Args2)) 
      (count-differences-p $Args1 $Args2 $OldBL $NewBL 0)))
; 

  (= 
    (perfect-match-p $Constraint $Fact $OldBL $NewBL) 
    ( (infer $Fact $FactDerivation) (perfect-match-p $Constraint $FactDerivation $OldBL $NewBL)))
; 




  (= 
    (perfect-match-better-p 
      (:: $RestTempl1 $RestEx1 $BL1) 
      (:: $RestTempl2 $RestEx2 $BL2)) 
    ( (length $RestTempl1 $L1) 
      (length $RestTempl2 $L2) 
      (< $L1 $L2)))
; 



  (= 
    (perfect-match-equal-p 
      (:: $RestTempl1 $RestEx1 $BL1) 
      (:: $RestTempl2 $RestEx2 $BL2)) 
    ( (set-equal-p $RestTempl1 $RestTempl2) 
      (set-equal-p $RestEx1 $RestEx2) 
      (set-equal-p $BL1 $BL2)))
; 



  (= 
    (unsatisfied-constraint-p $Constraints $Ex $BL) 
    ( (member $Constraint $Constraints) 
      (member $Fact $Ex) 
      (or 
        (= $Constraint 
          (not $BaseConstraint)) 
        (= $BaseConstraint $Constraint)) 
      (perfect-match-p $BaseConstraint $Fact $BL $_) 
      (set-det)))
; 



  (= 
    (check-for-unmatched-necessary-constraints-p $_ nil) 
    (set-det))
; 

  (= 
    (check-for-unmatched-necessary-constraints-p $Templ $_) 
    (not (member (must $_) $Templ)))
; 



  (= 
    (partial-match $Templ $Ex $OldBL $NewBL $PL $RestTempl $RestEx) 
    ( (findbag 
        (:: $PL1 $RestTempl1 $RestEx1 $NewBL1) 
        (partial-match1 $Templ $Ex $OldBL $PL1 $RestTempl1 $RestEx1 $NewBL1) $PartialMatches) 
      (remove-duplicates $PartialMatches $PartialMatches1 partial-match-equal-p) 
      (list-sort $PartialMatches1 $SortedPartialMatches partial-match-better-p) 
      (get-best $SortedPartialMatches $BestMatches partial-match-better-p) 
      (member 
        (:: $PL $RestTempl $RestEx $NewBL) $BestMatches)))
; 




  (= 
    (partial-match1 $Templ $Ex $OldBL Nil $Templ $Ex $OldBL) 
    ( (or 
        (= $Templ Nil) 
        (= $Ex Nil)) (set-det)))
; 

  (= 
    (partial-match1 $Templ $Ex $OldBL Nil $Templ $Ex $OldBL) 
    ( (not (find-partial-match-p $Templ $Ex $OldBL $_ $_ $_ $_)) (set-det)))
; 

  (= 
    (partial-match1 $Templ $Ex $OldBL 
      (Cons  $PartialMatch $RestPL) $RestTempl $RestEx $NewBL) 
    ( (find-partial-match-p $Templ $Ex $OldBL $PartialMatch $RestTempl1 $RestEx1 $NewBL1) 
      (set-det) 
      (partial-match1 $RestTempl1 $RestEx1 $NewBL1 $RestPL $RestTempl $RestEx $NewBL)))
; 



  (= 
    (find-partial-match-p $Templ $Ex $OldBL $PartialMatch $RestTempl $RestEx $NewBL) 
    ( (enumerate $Templ $Part $RestTempl) 
      (find-partial-match-p1 $Part $Ex $OldBL $MatchingFact $RestEx $NewBL) 
      (= $PartialMatch 
        (:: $Part $MatchingFact))))
; 



  (= 
    (find-partial-match-p1 $Part $Ex $OldBL $ExamplePart $RestEx $NewBL) 
    ( (enumerate $Ex $ExamplePart $RestEx) (partial-match-p $Part $ExamplePart $OldBL $NewBL)))
; 



  (= 
    (partial-match-p 
      (must $Constraint) $Fact $OldBL $NewBL) 
    ( (set-det) (partial-match-p $Constraint $Fact $OldBL $NewBL)))
; 

  (= 
    (partial-match-p $Constraint $Fact $OldBL $NewBL) 
    ( (=.. $Constraint 
        (Cons  $Functor $Args1)) 
      (=.. $Fact 
        (Cons  $Functor $Args2)) 
      (count-differences-p $Args1 $Args2 $OldBL $NewBL 1)))
; 

  (= 
    (partial-match-p $Constraint $Fact $OldBL $NewBL) 
    ( (infer $Fact $FactDerivation) (partial-match-p $Constraint $FactDerivation $OldBL $NewBL)))
; 




  (= 
    (partial-match-better-p 
      (:: $PL1 $RestTempl1 $RestEx1 $BL1) 
      (:: $PL2 $RestTempl2 $RestEx2 $BL2)) 
    ( (length $RestTempl1 $L1) 
      (length $RestTempl2 $L2) 
      (< $L1 $L2)))
; 



  (= 
    (partial-match-equal-p 
      (:: $PL1 $RestTempl1 $RestEx1 $BL1) 
      (:: $PL2 $RestTempl2 $RestEx2 $BL2)) 
    ( (set-equal-p $PL1 $PL2) 
      (set-equal-p $RestTempl1 $RestTempl2) 
      (set-equal-p $RestEx1 $RestEx2) 
      (set-equal-p $BL1 $BL2)))
; 



  (= 
    (count-differences-p Nil Nil $BL $BL 0) 
    (set-det))
; 

  (= 
    (count-differences-p 
      (Cons  $First1 $Rest1) 
      (Cons  $First2 $Rest2) $OldBL $NewBL $RestN) 
    ( (atom-match-p $First1 $First2 $OldBL $BL) 
      (set-det) 
      (count-differences-p $Rest1 $Rest2 $BL $NewBL $RestN)))
; 

  (= 
    (count-differences-p 
      (Cons  $First1 $Rest1) 
      (Cons  $First2 $Rest2) $OldBL $NewBL $N) 
    ( (set-det) 
      (not (= $First1 (var $_))) 
      (not (= $First2 (var $_))) 
      (count-differences-p $Rest1 $Rest2 $OldBL $NewBL $RestN) 
      (is $N 
        (+ $RestN 1))))
; 



  (= 
    (atom-match-p $O1 $O2 $OldBL $NewBL) 
    ( (atom-match-p1 $O1 $O2 $OldBL $NewBL) (unique-binding-p $NewBL)))
; 



  (= 
    (atom-match-p1 
      (var $VarName1) 
      (var $VarName2) $OldBL $NewBL) 
    ( (set-det) 
      (get-binding $VarName1 $OldBL $BL $Binding1) 
      (get-binding $VarName2 $BL $NewBL $Binding2) 
      (= $Binding1 $Binding2)))
; 

  (= 
    (atom-match-p1 $Const 
      (var $VarName) $OldBL $NewBL) 
    ( (set-det) (get-binding $VarName $OldBL $NewBL $Const)))
; 

  (= 
    (atom-match-p1 
      (var $VarName) $Const $OldBL $NewBL) 
    ( (set-det) (get-binding $VarName $OldBL $NewBL $Const)))
; 

  (= 
    (symbol_match_p1  $Const $Const $BL $BL) True)
; 



  (= 
    (get-binding $Key Nil 
      (:: (Cons  $Key $Value)) $Value) 
    (set-det))
; 

  (= 
    (get-binding $Key $BL $BL $Value1) 
    ( (= $BL 
        (Cons  
          (Cons  $Key $Value2) $_)) 
      (set-det) 
      (= $Value1 $Value2)))
; 

  (= 
    (get-binding $Key 
      (Cons  $FirstBinding $RestBL) 
      (Cons  $FirstBinding $NewRestBL) $Value) 
    ( (set-det) (get-binding $Key $RestBL $NewRestBL $Value)))
; 




  (= 
    (get_variable_p  $Value 
      (Cons  
        (Cons  $Variable $Value) $_) $Variable) True)
; 

  (= 
    (get-variable-p $Value 
      (Cons  $_ $RestBL) $Variable) 
    (get-variable-p $Value $RestBL $Variable))
; 




  (= 
    (unique_binding_p  ()) True)
; 

  (= 
    (unique-binding-p (Cons  $FirstBinding $RestBindings)) 
    ( (unique-binding-p $RestBindings) 
      (= $FirstBinding 
        (Cons  $_ $Value)) 
      (not (get-variable-p $Value $RestBindings $_))))
; 



  (= 
    (initial-generalization $Example $InitialConcept) 
    ( (variabilize-part-facts $Example $VarPartFacts $BL $RestFacts) 
      (variabilize-facts $RestFacts $BL $VarRestFacts) 
      (append $VarPartFacts $VarRestFacts $Template) 
      (concept-template $InitialConcept $Template) 
      (concept-constraints $InitialConcept Nil) 
      (set-det)))
; 



  (= 
    (variabilize-part-facts Nil Nil Nil Nil) 
    (set-det))
; 

  (= 
    (variabilize-part-facts 
      (Cons  
        (part $PartName) $Rest) $VarPartFacts $BL $RestFacts) 
    ( (set-det) 
      (= $VarPartFacts 
        (Cons  
          (part (var $PartName)) $VarRestParts)) 
      (= $BL 
        (Cons  
          (Cons  $PartName $PartName) $RestBL)) 
      (variabilize-part-facts $Rest $VarRestParts $RestBL $RestFacts)))
; 

  (= 
    (variabilize-part-facts 
      (Cons  $First $Rest) $VarRestParts $RestBL 
      (Cons  $First $RestFacts)) 
    ( (set-det) (variabilize-part-facts $Rest $VarRestParts $RestBL $RestFacts)))
; 



  (= 
    (variabilize-facts Nil $_ Nil) 
    (set-det))
; 

  (= 
    (variabilize-facts 
      (Cons  $First $Rest) $BL 
      (Cons  $VarFirst $VarRest)) 
    ( (set-det) 
      (variabilize-fact $First $BL $VarFirst) 
      (variabilize-facts $Rest $BL $VarRest)))
; 



  (= 
    (specialize-concept-definition Nil $_ $C $C) 
    (set-det))
; 

  (= 
    (specialize-concept-definition 
      (Cons  
        (addition $ExampleFact) $RestDD) $BL $OldConcept $NewConcept) 
    ( (set-det) 
      (variabilize-fact $ExampleFact $BL $NewNecessaryConstraint) 
      (specialize-concept-definition $RestDD $BL $OldConcept $Concept) 
      (concept-constraints $Concept $Constraints) 
      (list-add-if-necessary 
        (not $NewNecessaryConstraint) $Constraints $NewConstraints) 
      (alter-concept-constraints $Concept $NewConstraints $NewConcept)))
; 


  (= 
    (specialize-concept-definition 
      (Cons  
        (missing $Part) $RestDD) $BL $OldConcept $NewConcept) 
    ( (set-det) 
      (specialize-concept-definition $RestDD $BL $OldConcept $Concept) 
      (concept-template $Concept $Template) 
      (list-remove $Part $Template $Template1) 
      (= $NewTemplate 
        (Cons  
          (must $Part) $Template1)) 
      (alter-concept-template $Concept $NewTemplate $NewConcept)))
; 


  (= 
    (specialize-concept-definition 
      (Cons  
        (partial (:: $Part $_)) $RestDD) $BL $OldConcept $NewConcept) 
    ( (set-det) 
      (specialize-concept-definition $RestDD $BL $OldConcept $Concept) 
      (concept-template $Concept $Template) 
      (list-remove $Part $Template $Template1) 
      (= $NewTemplate 
        (Cons  
          (must $Part) $Template1)) 
      (alter-concept-template $Concept $NewTemplate $NewConcept)))
; 



  (= 
    (variabilize-fact $Fact $BL $VarFact) 
    ( (=.. $Fact 
        (Cons  $Functor $Args)) 
      (variabilize-list $Args $BL $VarArgs) 
      (=.. $VarFact 
        (Cons  $Functor $VarArgs))))
; 



  (= 
    (variabilize-list Nil $_ Nil) 
    (set-det))
; 

  (= 
    (variabilize-list 
      (Cons  $FirstArg $RestArgs) $BL 
      (Cons  
        (var $VarFirstArg) $VarRestArgs)) 
    ( (get-variable-p $FirstArg $BL $VarFirstArg) 
      (set-det) 
      (variabilize-list $RestArgs $BL $VarRestArgs)))
; 

  (= 
    (variabilize-list 
      (Cons  $FirstArg $RestArgs) $BL 
      (Cons  $FirstArg $VarRestArgs)) 
    ( (set-det) (variabilize-list $RestArgs $BL $VarRestArgs)))
; 



  (= 
    (generalize-concept-definition Nil $C $C) 
    (set-det))
; 

  (= 
    (generalize-concept-definition 
      (Cons  
        (partial (:: $Constraint $ExampleFact)) $RestDD) $OldConcept $NewConcept) 
    ( (set-det) 
      (generalize-arg $Constraint $ExampleFact $NewConstraint) 
      (generalize-concept-definition $RestDD $OldConcept $RestConcept) 
      (concept-template $RestConcept $Template) 
      (list-remove $Constraint $Template $Template1) 
      (= $NewTemplate 
        (Cons  $NewConstraint $Template1)) 
      (alter-concept-template $RestConcept $NewTemplate $NewConcept)))
; 


  (= 
    (generalize-concept-definition 
      (Cons  
        (missing $Constraint) $RestDD) $OldConcept $NewConcept) 
    ( (set-det) 
      (generalize-concept-definition $RestDD $OldConcept $RestConcept) 
      (concept-template $RestConcept $Template) 
      (list-remove $Constraint $Template $NewTemplate) 
      (alter-concept-template $RestConcept $NewTemplate $NewConcept)))
; 


  (= 
    (generalize-concept-definition 
      (Cons  $_ $RestDD) $OldSC $RestSC) 
    (generalize-concept-definition $RestDD $OldSC $RestSC))
; 



  (= 
    (generalize-arg 
      (must $Constraint) $ExampleFact 
      (must $NewConstraint)) 
    ( (set-det) (generalize-arg $Constraint $ExampleFact $NewConstraint)))
; 


  (= 
    (generalize-arg $Constraint $ExampleFact $NewConstraint) 
    ( (=.. $Constraint 
        (Cons  $Functor $Args1)) 
      (=.. $ExampleFact 
        (Cons  $Functor $Args2)) 
      (generalize-arg1 $Args1 $Args2 $NewArgs1) 
      (=.. $NewConstraint 
        (Cons  $Functor $NewArgs1))))
; 



  (= 
    (generalize-arg1 Nil Nil Nil) 
    (set-det))
; 

  (= 
    (generalize-arg1 
      (Cons  $Arg1 $Rest1) 
      (Cons  $Arg2 $Rest2) 
      (Cons  $Arg1 $GenRest)) 
    ( (or 
        (= $Arg1 $Arg2) 
        (= $Arg1 
          (var $_))) 
      (set-det) 
      (generalize-arg1 $Rest1 $Rest2 $GenRest)))
; 

  (= 
    (generalize-arg1 
      (Cons  $Arg1 $Rest1) 
      (Cons  $Arg2 $_) 
      (Cons  $GenArg $Rest1)) 
    ( (set-det) (find-or-create-common-ancestor $Arg1 $Arg2 $GenArg)))
; 



  (= 
    (find-or-create-common-ancestor $Class1 $Class2 $Ancestor) 
    ( (ancestors $Class1 $Ancestors1) 
      (ancestors $Class2 $Ancestors2) 
      (smallest-ancestor-p $Ancestors1 $Ancestors2 $Ancestor) 
      (set-det)))
; 

  (= 
    (find-or-create-common-ancestor $Class1 $Class2 $Ancestor) 
    ( (set-det) 
      (concat $Class1 $Class2 $Ancestor) 
      (add-symbol  &self 
        (ako  $Class1 $Ancestor)) 
      (add-symbol  &self 
        (ako  $Class2 $Ancestor))))
; 



  (= 
    (ancestors $Class $Ancestors) 
    (ancestors1 
      (:: $Class) Nil $Ancestors))
; 



  (= 
    (ancestors1 Nil $Ancestors $Ancestors) 
    (set-det))
; 

  (= 
    (ancestors1 
      (Cons  $First $Rest) $Ancestors $ExtendedAncestors) 
    ( (direct-ancestors $First $FirstAncestors) 
      (not (= $FirstAncestors Nil)) 
      (set-det) 
      (append $FirstAncestors $Rest $NewClassList) 
      (union $Ancestors $FirstAncestors $NewAncestorList) 
      (ancestors1 $NewClassList $NewAncestorList $ExtendedAncestors)))
; 

  (= 
    (ancestors1 
      (Cons  $First $Rest) $Ancestors $ExtendedAncestors) 
    ( (set-det) 
      (union 
        (:: $First) $Ancestors $NewAncestors) 
      (ancestors1 $Rest $NewAncestors $ExtendedAncestors)))
; 



  (= 
    (direct-ancestors $Class $Ancestors) 
    ( (set-det) (findbag $Ancestor (ako $Class $Ancestor) $Ancestors)))
; 



  (= 
    (smallest-ancestor-p 
      (Cons  $First $_) $Ancestors2 $First) 
    ( (member $First $Ancestors2) (set-det)))
; 

  (= 
    (smallest-ancestor-p 
      (Cons  $_ $Rest) $Ancestors2 $Ancestor) 
    ( (set-det) (smallest-ancestor-p $Rest $Ancestors2 $Ancestor)))
; 



  (= 
    (list-remove $_ Nil Nil) 
    (set-det))
; 

  (= 
    (list-remove $Target 
      (Cons  $Target $Rest) $RestRemoved) 
    ( (set-det) (list-remove $Target $Rest $RestRemoved)))
; 

  (= 
    (list-remove $Target 
      (Cons  $First $Rest) 
      (Cons  $First $RestRemoved)) 
    (list-remove $Target $Rest $RestRemoved))
; 



  (= 
    (msgs Nil) 
    (set-det))
; 

  (= 
    (msgs (Cons  $First $Rest)) 
    ( (msg $First) 
      (msgs $Rest) 
      (set-det)))
; 



  (= 
    (msg nl) 
    ( (set-det) (nl)))
; 

  (= 
    (msg (nl $N)) 
    ( (set-det) (repeat $N nl)))
; 

  (= 
    (msg sp) 
    ( (set-det) (write ' ')))
; 

  (= 
    (msg (sp $N)) 
    ( (set-det) (repeat $N (write ' '))))
; 

  (= 
    (msg (pf $PF $Object)) 
    ( (set-det) 
      (=.. $Call 
        (:: $PF $Object)) 
      (call $Call)))
; 

  (= 
    (msg $Object) 
    ( (write $Object) (set-det)))
; 



  (= 
    (repeat $N $_) 
    ( (< $N 1) (set-det)))
; 

  (= 
    (repeat $N $Call) 
    ($Call 
      (is $N1 
        (- $N 1)) 
      (repeat $N1 $Call) 
      (set-det)))
; 



  (= 
    (set-equal-p Nil Nil) 
    (set-det))
; 

  (= 
    (set-equal-p 
      (Cons  $First $Rest) $Set2) 
    ( (enumerate $Set2 $First $Rest2) 
      (set-det) 
      (set-equal-p $Rest $Rest2)))
; 



  (= 
    (remove-duplicates Nil Nil $_) 
    (set-det))
; 

  (= 
    (remove-duplicates 
      (Cons  $First $Rest) $Result $EqualP) 
    ( (remove-duplicates $Rest $RestResult $EqualP) (remove-duplicates-result $First $RestResult $Result $EqualP)))
; 



  (= 
    (remove-duplicates-result $First $RestResult $RestResult $EqualP) 
    ( (member $First $RestResult $EqualP) (set-det)))
; 

  (= 
    (remove_duplicates_result  $First $RestResult 
      (Cons  $First $RestResult) $_) True)
; 



  (= 
    (list_sort  () () $_) True)
; 

  (= 
    (list-sort 
      (Cons  $X $L) $M $O) 
    ( (list-sort $L $N $O) (list-sort1 $X $N $M $O)))
; 



  (= 
    (list-sort1 $X 
      (Cons  $A $L) 
      (Cons  $A $M) $O) 
    ( (=.. $P 
        (:: $O $A $X)) 
      (call $P) 
      (set-det) 
      (list-sort1 $X $L $M $O)))
; 

  (= 
    (list_sort1  $X $L 
      (Cons  $X $L) $_) True)
; 



  (= 
    (get_best  () () $_) True)
; 

  (= 
    (get_best  
      ($Single) 
      ($Single) $_) True)
; 

  (= 
    (get-best 
      (Cons  $First 
        (Cons  $Second $_)) 
      (:: $First) $ComparisonP) 
    ( (=.. $Call 
        (:: $ComparisonP $First $Second)) 
      (call $Call) 
      (set-det)))
; 

  (= 
    (get-best 
      (Cons  $First 
        (Cons  $Second $Rest)) 
      (Cons  $First $RestBest) $ComparisonP) 
    ( (set-det) (get-best (Cons  $Second $Rest) $RestBest $ComparisonP)))
; 



  (= 
    (enumerate  
      (Cons  $Head $Tail) $Head $Tail) True)
; 

  (= 
    (enumerate 
      (Cons  $Head $Tail) $NextHead 
      (Cons  $Head $NextRest)) 
    (enumerate $Tail $NextHead $NextRest))
; 



  (= 
    (list-add-if-necessary $NewElement $List $List) 
    ( (member $NewElement $List) (set-det)))
; 

  (= 
    (list-add-if-necessary $NewElement $List $NewList) 
    ( (= $NewList 
        (Cons  $NewElement $List)) (set-det)))
; 



  (= 
    (concat $S1 $S2 $S3) 
    ( (nonvar $S1) 
      (nonvar $S2) 
      (name $S1 $L1) 
      (name $S2 $L2) 
      (append $L1 $L2 $L3) 
      (name $S3 $L3) 
      (set-det)))
; 


  (= 
    (concat $S1 $S2 $S3) 
    ( (nonvar $S1) 
      (nonvar $S3) 
      (name $S1 $L1) 
      (name $S3 $L3) 
      (append $L1 $L2 $L3) 
      (name $S2 $L2) 
      (set-det)))
; 


  (= 
    (concat $S1 $S2 $S3) 
    ( (nonvar $S2) 
      (nonvar $S3) 
      (name $S2 $L2) 
      (name $S3 $L3) 
      (append $L1 $L2 $L3) 
      (name $S1 $L1) 
      (set-det)))
; 



  (= 
    (member $_ $List $_) 
    ( (var $List) 
      (set-det) 
      (is $List Nil) 
      (fail)))
; 

  (= 
    (member $E 
      (Cons  $First $_) $EqualP) 
    ( (=.. $Call 
        (:: $EqualP $E $First)) (call $Call)))
; 

  (= 
    (member $E 
      (Cons  $_ $R) $EqualP) 
    (member $E $R $EqualP))
; 



  !(dynamic (/ found 1))
; 



  (= 
    (findbag $X $G $_) 
    ( (add-symbol  &self 
        (found  mark)) 
      (call $G) 
      (add-symbol  &self 
        (found  $X)) 
      (fail)))
; 
                            
  (= 
    (findbag $_ $_ $L) 
    (collect-found Nil $L))
; 
                                     
                                                              

  (= 
    (collect-found $L $L1) 
    ( (getnext $X) (collect-found (Cons  $X $L) $L1)))
; 
                      
  (= 
    (collect_found  $L $L) True)
; 
                                        
                                                              
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  arch1/arch1.pl 1339 30 68638))
                


  (= 
    (union  () $X $X) True)
; 

  (= 
    (union 
      (Cons  $X $R) $Y $Z) 
    ( (member $X $Y) 
      (set-det) 
      (union $R $Y $Z)))
; 

  (= 
    (union 
      (Cons  $X $R) $Y 
      (Cons  $X $Z)) 
    (union $R $Y $Z))
; 



  (= 
    (help) 
    ( (write 'Load data set with command: [Filename].') 
      (nl) 
      (write 'Start arch1   with command: process-stored-inputs(ConceptName).') 
      (nl)))
; 

   

  !(help *)
; 


