;                                           (convert_to_metta_file  arch1 $_448468 arch1/arch1.pl arch1/arch1.metta)

  (= (process-stored-inputs $ConceptName)  
    (or 
      (remove-is-symbol  &self 
        (concept  $ConceptName $_)) True)
    (set-det)
    (process-stored-inputs-body $ConceptName))
; /******************************************************************/
; /* ARCH1.PRO Last Modification: Fri Jan 14 19:19:54 1994 */
; /* Winston's incremental learning procedure. */
; /******************************************************************/
; ; ; Copyright (c) 1988 Stefan Wrobel ; ; This program is free software; you can redistribute it and/or ; modify it under the terms of the GNU General Public License ; Version 1 as published by the Free Software Foundation. ; ; This program is distributed in the hope that it will be useful, ; but WITHOUT ANY WARRANTY; without even the implied warranty of ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ; GNU General Public License for more details. ; ; You should have received a copy of the GNU General Public ; Licensealong with this program; if not, write to the Free ; SoftwareFoundation, Inc., 675 Mass Ave, Cambridge, MA 02139, ; USA. ;
; /******************************************************************/
; /* impl. by : Stefan Wrobel */
; /* F3.XPS */
; /* Gesellschaft fuer Mathematik und */
; /* Datenverarbeitung */
; /* Schloss Birlinghoven */
; /* Postfach 1240 */
; /* 5205 St.Augustin 1 */
; /* F.R.G. */
; /* E-Mail: wrobel@gmdzi.gmd.de */
; /* 1988 */
; /* */
; /* reference : ES2ML Tutorial Exercise */
; /* Concept Learning and Concept Formation */
; /* Stefan Wrobel */
; /* */
; /* chapter 11 */
; /* Artificial Intelligence */
; /* Winston */
; /* second edition */
; /* Addison-Wesley, 1984 */
; /* */
; /* call : process_stored_inputs(arch) */
; /* */
; /******************************************************************/
; ; TH Sat May 29 23:25:18 1993 - made some minor modifications
; /******************************************************************/
; /* */
; /* call : process_stored_inputs(+ConceptName) */
; /* */
; /* arguments : ConceptName = Name of example class */
; /* */
; /* side effects: retracts the previous concept, if there was one */
; /* */
; /******************************************************************/
; /* Retracts the previous concept, if there was one and processes */
; /* the example class. */
; /******************************************************************/


  (= (process-stored-inputs-body $ConceptName)  
    (msgs (:: nl 'Processing stored inputs for ' $ConceptName  ...))
    (input $ID $Type $ConceptName $Input)
    (msgs (:: nl 'Input ' $ID :  $Type nl $Input))
    (process-example $Type $ConceptName $Input)
    (concept $ConceptName $Concept)
    (concept-template $Concept $Template)
    (concept-constraints $Concept $Constraints)
    (msgs (:: nl 'New Concept Definition is:' nl '--- Template:' nl $Template nl '--- Constraints:' nl $Constraints nl))
    (fail))
  (= (process-stored-inputs-body $_)  
    (msgs (:: nl 'No more inputs. Done.')))



  (= (process-example example $ConceptName $Example)  
    ( (remove-is-symbol  &self 
        (concept  $ConceptName $Definition)) 
      (set-det) 
      (generalize $Example $Definition $NewDefinition) 
      (add-is-symbol  &self 
        (concept  $ConceptName $NewDefinition)) 
      (set-det)))
; /******************************************************************/
; /* */
; /* call : process_example(+Type,+ConceptName,+Example) */
; /* */
; /* arguments : Type = Classification */
; /* ConceptName = Name of example class */
; /* Example = Structural Description */
; /* */
; /* side effects: assertion of concept definition in database */
; /* modified concept definition in database */
; /* */
; /******************************************************************/
; /* Processes a new input example (representation: see */
; /* arch_1.MeTTa) and adapts the existing concept definition by */
; /* using specialize (if Type is near_miss) or generalize (if Type */
; /* is example). */
; /*****************************************************************/
; /* process it */
  (= (process-example example $ConceptName $Example)  
    ( (set-det) 
      (initial-generalization $Example $InitialDefinition) 
      (add-is-symbol  &self 
        (concept  $ConceptName $InitialDefinition))))
; /* no concept yet - this is the initial input. Perform */
; /* special initial generalization on it */
  (= (process-example near-miss $ConceptName $Example)  
    ( (remove-is-symbol  &self 
        (concept  $ConceptName $Definition)) 
      (set-det) 
      (specialize $Example $Definition $NewDefinition) 
      (add-is-symbol  &self 
        (concept  $ConceptName $NewDefinition)) 
      (set-det)))
; /* process it */
  (= (process-example near-miss $ConceptName $_)  
    (set-det)
    (msgs (:: nl 'Cannot process a (near) miss as the first example of ' $ConceptName . nl 'Please begin with a (prototypical) example instead.')))
; /* oops - we got a near miss as a first example. Reject it. */


  (= (specialize $Example $OldConcept $OldConcept)  
    (is-member $Example $OldConcept no)
    (set-det)
    (msgs (:: nl 'Already excluded.')))
; /******************************************************************/
; /* */
; /* call : specialize(+Example,+Definition,-NewDefinition) */
; /* */
; /* arguments : Example = Structural Description */
; /* Definition = Current Concept Definition */
; /* NewDefinition = New Concept Definition */
; /* */
; /******************************************************************/
; /* Example is a near miss with respect to Definition (see */
; /* arch_1.MeTTa for representations). This procedure tries to */
; /* specialize the concept definition such that Example is */
; /* excluded. This is done by strengthening the necessary */
; /* conditions of Definition (adding must/must_not links in */
; /* Winston's terminology). Note that this Winston-style */
; /* specialization procedure needs a difference between the */
; /* existing sufficient conditions and the example; if we */
; /* ever overgeneralize (i.e., a near miss exactly matches */
; /* the sufficient conditions), this procedure doesn't know */
; /* how to recover. */
; /******************************************************************/
; /* match failed, i.e., this example is already excluded */
; /* by the existing definition */
  (= (specialize $Example $OldConcept $NewConcept)  
    (match $OldConcept $Example Nil $BL $PL $ML $AL)
    (find-important-differences-p specialize $PL $ML $AL $DifferenceDescriptions)
    (specialize-concept-definition $DifferenceDescriptions $BL $OldConcept $NewConcept)
    (set-det))
; /* try to find the most important differences between the Example */
; /* and the definition */
; /* check if we're happy with this match */
; /* o.k., we found some interesting differences */
  (= (specialize $_ $Definition $Definition)  
    (msgs (:: nl 'Specialize failed. Ignoring example.')))



  (= (generalize $Example $OldConcept $OldConcept)  
    (is-member $Example $OldConcept yes)
    (set-det)
    (msgs (:: nl 'Already included.')))
; /******************************************************************/
; /* */
; /* call : generalize(+Example,+Definition,-NewDefinition) */
; /* */
; /* arguments : Example = Structural Description */
; /* Definition = Current Concept Definition */
; /* NewDefinition = New Concept Definition */
; /* */
; /******************************************************************/
; /* Example is a positive example with respect to Definition (see */
; /* arch_1.MeTTa for representations). This procedure tries to */
; /* generalize the concept definition such that Example is */
; /* included. This is done by weakening the sufficient conditions */
; /* of Definition. */
; /******************************************************************/
; /* this example is already included by the existing definition */
  (= (generalize $Example $OldConcept $NewConcept)  
    (match $OldConcept $Example Nil $_ $PL $ML $AL)
    (find-important-differences-p generalize $PL $ML $AL $DifferenceDescriptions)
    (generalize-concept-definition $DifferenceDescriptions $OldConcept $NewConcept))
; /* try to find the most important differences between the Example */
; /* and the definition */
; /* check if we're happy with this match */
; /* o.k., we found some interesting differences */
  (= (generalize $_ $Concept $Concept)  
    (msgs (:: nl 'Generalize failed. Ignoring example.')))



  (= (find-important-differences-p generalize $PL $ML $_ $DD)  
    (mark-as partial $PL $DD1)
    (mark-as missing $ML $DD2)
    (append $DD1 $DD2 $DD))
; /**********************************************************************/
; /* */
; /* call : find_important_differences_p(Operation, */
; /* +PartialList, */
; /* +AdditionList, */
; /* +MissingList, */
; /* -DifferenceDescription)*/
; /* */
; /**********************************************************************/
; /* Inspect the differences in PartialList, AdditionList, and */
; /* MissingList, and decide which ones to use as the basis for */
; /* concept modification. Operation is either "specialize" or */
; /* "generalize". DifferenceDescriptions is the list of such */
; /* differences, with a descriptor indicating their type: */
; /* */
; /* - partial_match([DefinitionConstraint,ExampleFact]) */
; /* - addition(ExampleFact) */
; /* - missing(DefinitionConstraint) */
; /* */
; /**********************************************************************/
; /* For generalization, additional facts in the example are */
; /* not interesting. Return partial matches and missing constraints. */
; /**********************************************************************/
  (= (find-important-differences-p specialize $_ Nil $AL $DD)  
    (not (= $AL Nil))
    (set-det)
    (mark-as addition $AL $DD))
; /***********************************************************************/
; /* For specialization, both missing constraints and additional facts */
; /* are interesting. If there are both, prefer the missing constraint. */
; /***********************************************************************/
; /* differences_acceptable_p(AL), */
  (= (find-important-differences-p specialize $_ $ML $_ $DD)  
    (not (= $ML Nil))
    (set-det)
    (mark-as missing $ML $DD))
; /* differences_acceptable_p(ML), */
  (= (find-important-differences-p specialize $PL $_ $_ $DD)  
    (mark-as partial $PL $DD))


  (= (differences-acceptable-p $Diffs)  
    (length $Diffs 1)
    (set-det))
; /* a single difference is always fine */
  (= (differences-acceptable-p $Diffs)  
    (same-functor-p $Diffs $_)
    (set-det))
; /* more than one must have the same functor to be acceptable */


  (= (same-functor-p Nil $_)  
    (set-det))
  (= (same-functor-p (:: $LastDifference) $Functor)  
    (set-det)
    (=.. $LastDifference 
      (Cons  $Functor $_)))
  (= (same-functor-p (Cons  $First $Rest) $Functor)  
    (same-functor-p $Rest $Functor)
    (=.. $First 
      (Cons  $Functor $_)))


  (= (mark-as $_ Nil Nil)  
    (set-det))
  (= (mark-as $Mark (Cons  $First $Rest) (Cons  $FirstMarked $RestMarked))  
    (=.. $FirstMarked 
      (:: $Mark $First))
    (mark-as $Mark $Rest $RestMarked))


  (= (concept_p  (concept  $_ $_ $_))  True)
; /***********************************************************************/
; /* The evolving concept definition is represented by two lists of facts*/
; /* (which may contain symbolic vars "var(...)"). The first list is the */
; /* "template", a structural description of the concept that is matched */
; /* one-to-one to the example, i.e., each part of the template */
; /* "consumes" one part of the example when matched. The second list */
; /* contains the "constraints", which are logical conditions on concept */
; /* members that do not "consume" example parts when they get matched. */
; /* Negated facts go in the constraint slot. Within the template, */
; /* necessary parts can be marked as necessary by enclosing them with */
; /* "must(...)"; constraints are always interpreted as necessary. */
; /***********************************************************************/
; /* A concept is represented as: */
; /* */
; /* concept(NAME,TEMPLATE_LIST,CONSTRAINT_LIST) */
; /* */
; /* e.g., concept(arch, */
; /* [part(var(o1)),part(var(o2)),part(var(o3)), */
; /* isa(var(o1),brick), isa(var(o2),brick), */
; /* isa(var(o3),brick), left_of(var(o2),var(o1)), */
; /* must(supports(var(o1),var(o3))), */
; /* must(supports(var(o2),var(o3)))], */
; /* [not(touches(var(o1),var(o2)))]). */
; /***********************************************************************/
; /* The following predicates give access to the parts of a concept: */
; /***********************************************************************/

  (= (concept_name  (concept  $Name $_ $_) $Name)  True)

  (= (concept_template  (concept  $_ $Template $_) $Template)  True)

  (= (concept_constraints  (concept  $_ $_ $Constraints) $Constraints)  True)


  (= (alter_concept_constraints  (concept  $N $T $_) $C (concept  $N $T $C))  True)
; /***********************************************************************/
; /* The following predicates alternate parts of a concept: */
; /***********************************************************************/

  (= (alter_concept_template  (concept  $N $_ $C) $T (concept  $N $T $C))  True)


  (= (concept-equal-p (concept $Name $Template1 $Constraints1) (concept $Name $Template2 $Constraints2))  
    (set-equal-p $Template1 $Template2)
    (set-equal-p $Constraints1 $Constraints2))
; /***********************************************************************/
; /* Two concepts are equal if both have the same name and the sets of */
; /* their templates and constraints are equal. This predicate will not */
; /* notice isomorphic concepts with different variable names. */
; /***********************************************************************/


  (= (print-concept $Concept)  
    (concept-name $Concept $Name)
    (or 
      (= $Name -) True)
    (concept-template $Concept $Template)
    (concept-constraints $Concept $Constraints)
    (msgs (:: nl 'Concept ' $Name : nl 'Template:    ' $Template nl 'Constraints: ' $Constraints))
    (set-det))
; /* make sure Name is bound to something reasonable */


  (= (print-concepts $Concepts)  
    (member $Concept $Concepts)
    (print-concept $Concept)
    (fail))
  (= (print_concepts  $_)  True)


  (= (is-member $Example $Concept $Decision)  
    (match $Concept $Example Nil $_ $PL $ML $_)
    (set-det)
    (or 
      (, 
        (= $ML Nil) 
        (= $PL Nil) 
        (set-det) 
        (= $Decision yes)) 
      (= $Decision possible)))
; /******************************************************************/
; /* */
; /* call : is_member(+Example,+Concept,-Decision) */
; /* */
; /* arguments : Example = Structural Description */
; /* Concept = Current Concept Definition */
; /* Decision = Truth Value */
; /* */
; /******************************************************************/
; /* Classifies Example according to Concept. Three cases are */
; /* possible: */
; /* */
; /* (a) Example meets the sufficient conditions of the current */
; /* Concept Definition, so it must be a member of the */
; /* concept (Decision = yes) */
; /* (b) Example does not meet the necessary conditions, so it */
; /* cannot be a member (Decision = no) */
; /* (c) Example meets the necessary conditions, but not the */
; /* sufficient conditions (Decision = possible) */
; /* */
; /******************************************************************/
; /* match succeeds only if necessary conditions */
; /* (incl. constraints) are met */
; /* check if all non-necessary conditions were */
; /* matched also */
  (= (is_member  $_ $_ no)  True)


  (= (match $Concept $Ex $OldBL $NewBL $PL $ML $AL)  
    (match $Concept $Ex $OldBL $NewBL $PL $ML $AL t))
; /********************************************************************/
; /* Pattern Matcher */
; /********************************************************************/
; /* */
; /* call : match(+Concept,+Example, */
; /* +OldBindingList,-NewBindingList, */
; /* -PartialMatchList,-MissingList, */
; /* -AdditionList,CheckNecessaryConditions) */
; /* */
; /* arguments : Concept = Current Concept Definition */
; /* Example = Structural Description */
; /* OldBindingList = List of Current Bindings */
; /* NewBindingList = List of New Bindings */
; /* PartialMatchList = List of Partial Matched Pairs */
; /* MissingList = List of Missing Example Facts */
; /* AdditionList = List of Missing Concept Parts */
; /* CheckNecessaryConditions = see descripton */
; /* */
; /* properties : The predicate is backtrackable and returns all */
; /* equally good matches on backtracking. */
; /* */
; /********************************************************************/
; /* Match Example with Concept Definition. Example and Concept are */
; /* represented as in "arch_1.pro". Concept may contain facts with */
; /* symbolic variables as arguments ("var(<varname>)"), all other */
; /* symbols are treated as constants. No guarantee if you call match */
; /* with expressions that contain unbound (PROLOG-) variables. */
; /* OldBindingList is the list of bindings (list of [var|value] */
; /* pairs) to respect when performing the match. The constraints of */
; /* Concept are handled properly (they do not "consume" parts in the */
; /* example and may include negated facts). The match does not */
; /* succeed unless all constraints are met. If CheckNecessary- */
; /* Conditions is non-nil (or left out), match makes sure that no */
; /* match leaves a necessary part of Definition unmatched (those */
; /* marked with "must(...)"). If CheckNecessaryConditions is nil, */
; /* that check is not made (useful if you want to generalize on */
; /* necessary conditions, too). */
; /* */
; /* Match returns: */
; /* */
; /* - the NewBindingList with any additional bindings that were */
; /* made (a superset of OldBindingList) */
; /* - PartialMatchList, the list of pairs */
; /* ([Constraint,ExampleFact]) that were partially matched */
; /* - AdditionList, the list of facts present in the example */
; /* without a counterpart in the definition */
; /* - MissingList, the list of facts in the definition without a */
; /* a counterpart in the example */
; /* */
; /********************************************************************/

  (= (match $Concept $Ex $OldBL $NewBL $PL $ML $AL $CheckNecessaryP)  
    (concept-template $Concept $Templ)
    (concept-constraints $Concept $Constraints)
    (unambiguous-match $Templ $Ex $OldBL $RestTempl1 $RestEx1 $BL1)
    (perfect-match $RestTempl1 $RestEx1 $BL1 $RestTempl2 $RestEx2 $BL2 $CheckNecessaryP)
    (not (unsatisfied-constraint-p $Constraints $Ex $BL2))
    (partial-match $RestTempl2 $RestEx2 $BL2 $NewBL $PL $ML $AL))
; /* try to match as many parts as possible unambiguously */
; /* for the rest, try to match as many as possible perfectly */
; /* check other constraints (negated conditions, etc.) */
; /* ("Must" conditions are checked in perfect_match, */
; /* to cut off false matches as early as possible) */
; /* among whatever is still left, try to find as many partial */
; /* matches as possible - whatever is left there is */
; /* missing/additional */


  (= (unambiguous-match $Templ $Ex $OldBL $Templ $Ex $OldBL)  
    (or 
      (= $Templ Nil) 
      (= $Ex Nil))
    (set-det))
; /********************************************************************/
; /* unambiguous_match */
; /********************************************************************/
; /* if either Def or Ex are empty, we can't do anything */
  (= (unambiguous-match $Templ $Ex $OldBL $RestTempl $RestEx $NewBL)  
    (find-unambiguous-match-p $Templ $Ex $OldBL $RestTempl1 $RestEx1 $NewBL1)
    (set-det)
    (unambiguous-match $RestTempl1 $RestEx1 $NewBL1 $RestTempl $RestEx $NewBL))
  (= (unambiguous-match $Templ $Ex $OldBL $Templ $Ex $OldBL)  
    (set-det))
; /* we couldn't find another unambiguous match - just return */


  (= (find-unambiguous-match-p $Templ $Ex $OldBL $RestTempl $RestEx $NewBL)  
    (enumerate $Templ $Part $RestTempl)
    (find-unambiguous-match-p1 $Part $Templ $Ex $OldBL $RestEx $NewBL)
    (set-det))
; /* can we find an unambiguous match for FirstPart? */
; /* yes, fine */


  (= (find-unambiguous-match-p1 $Part $Template $Ex $OldBL $RestEx $NewBL)  
    (enumerate $Ex $ExamplePart $RestEx)
    (perfect-match-p $Part $ExamplePart $OldBL $NewBL)
    (not (non-unique-match-p $Part $ExamplePart $Template $Ex $OldBL))
    (set-det))
; /* they must at least match */
; /* and do so uniquely */
; /* fine */


  (= (non-unique-match-p $Part $ExamplePart $Template $Example $BL)  
    (member $Part1 $Template)
    (member $ExamplePart1 $Example)
    (or 
      (, 
        (not (= $Part1 $Part)) 
        (= $ExamplePart1 $ExamplePart)) 
      (, 
        (not (= $ExamplePart1 $ExamplePart)) 
        (= $Part1 $Part)))
    (perfect-match-p $Part1 $ExamplePart1 $BL $_)
    (set-det))


  (= (perfect-match $Templ $Ex $OldBL $RestTempl $RestEx $NewBL $CheckNecessaryP)  
    (findbag 
      (:: $RestTempl1 $RestEx1 $NewBL1) 
      (, 
        (perfect-match1 $Templ $Ex $OldBL $RestTempl1 $RestEx1 $NewBL1) 
        (check-for-unmatched-necessary-constraints-p $RestTempl1 $CheckNecessaryP)) $PerfectMatches)
    (remove-duplicates $PerfectMatches $PerfectMatches1 perfect-match-equal-p)
    (list-sort $PerfectMatches1 $SortedPerfectMatches perfect-match-better-p)
    (get-best $SortedPerfectMatches $BestMatches perfect-match-better-p)
    (member 
      (:: $RestTempl $RestEx $NewBL) $BestMatches))
; /********************************************************************/
; /* Finding perfect matches involves guessing. To avoid returning a */
; /* bad match only because of a bad first guess, we need to look at */
; /* all possible matches and return the best one (since by looking */
; /* at the first N matches, we cannot always be sure there won't be */
; /* a better one if we backtrack once more). */
; /********************************************************************/
; /* find all perfect matches */
; /* get all matches with the top score */
; /* return one of them (backtrackable) */


  (= (perfect-match1 $Templ $Ex $OldBL $Templ $Ex $OldBL)  
    (or 
      (= $Templ Nil) 
      (= $Ex Nil))
    (set-det))
; /* if either Templ or Ex are empty, we can't do anything */
  (= (perfect-match1 $Templ $Ex $OldBL $Templ $Ex $OldBL)  
    (not (find-perfect-match-p $Templ $Ex $OldBL $_ $_ $_))
    (set-det))
; /* we couldn't find another perfect match - just return */
  (= (perfect-match1 $Templ $Ex $OldBL $RestTempl $RestEx $NewBL)  
    (find-perfect-match-p $Templ $Ex $OldBL $RestTempl1 $RestEx1 $NewBL1)
    (perfect-match1 $RestTempl1 $RestEx1 $NewBL1 $RestTempl $RestEx $NewBL))


  (= (find-perfect-match-p $Templ $Ex $OldBL $RestTempl $RestEx $NewBL)  
    (enumerate $Templ $Part $RestTempl)
    (find-perfect-match-p1 $Part $Ex $OldBL $RestEx $NewBL))
; /* can we find a perfect match for FirstPart? */


  (= (find-perfect-match-p1 $Part $Ex $OldBL $RestEx $NewBL)  
    (enumerate $Ex $ExamplePart $RestEx)
    (perfect-match-p $Part $ExamplePart $OldBL $NewBL))
; /* they must match */


  (= (perfect-match-p (must $Constraint) $Fact $OldBL $NewBL)  
    (perfect-match-p $Constraint $Fact $OldBL $NewBL))
; /********************************************************************/
; /* */
; /* call : perfect_match_p(+Constraint,+Fact,+OldBL,-NewBL) */
; /* */
; /* arguments : Constraint = Current Concept Definition */
; /* Fact = Structural Description */
; /* OldBL = OldBindingList */
; /* NewBL = NewBindingList */
; /* */
; /********************************************************************/
; /* Succeed if Constraint and Fact match perfectly, returning the new*/
; /* binding list, or fail otherwise. */
; /* Second possibility: we are able to use a theorem that uses the */
; /* existing example fact to derive a fact that matches. See */
; /* arch_1.pro for some sample inferences. */
; /********************************************************************/
  (= (perfect-match-p $Constraint $Fact $OldBL $NewBL)  
    (=.. $Constraint 
      (Cons  $Functor $Args1))
    (=.. $Fact 
      (Cons  $Functor $Args2))
    (count-differences-p $Args1 $Args2 $OldBL $NewBL 0))
  (= (perfect-match-p $Constraint $Fact $OldBL $NewBL)  
    (infer $Fact $FactDerivation)
    (perfect-match-p $Constraint $FactDerivation $OldBL $NewBL))



  (= (perfect-match-better-p (:: $RestTempl1 $RestEx1 $BL1) (:: $RestTempl2 $RestEx2 $BL2))  
    (length $RestTempl1 $L1)
    (length $RestTempl2 $L2)
    (< $L1 $L2))
; /********************************************************************/
; /* Decides if one perfect match is better than another by a simple */
; /* measure: more parts of the template matched */
; /********************************************************************/


  (= (perfect-match-equal-p (:: $RestTempl1 $RestEx1 $BL1) (:: $RestTempl2 $RestEx2 $BL2))  
    (set-equal-p $RestTempl1 $RestTempl2)
    (set-equal-p $RestEx1 $RestEx2)
    (set-equal-p $BL1 $BL2))
; /********************************************************************/
; /* True if two perfect matches are equal */
; /********************************************************************/


  (= (unsatisfied-constraint-p $Constraints $Ex $BL)  
    (member $Constraint $Constraints)
    (member $Fact $Ex)
    (or 
      (= $Constraint 
        (not $BaseConstraint)) 
      (= $BaseConstraint $Constraint))
    (perfect-match-p $BaseConstraint $Fact $BL $_)
    (set-det))
; /********************************************************************/
; /* unsatisfied_constraint_p */
; /********************************************************************/


  (= (check-for-unmatched-necessary-constraints-p $_ nil)  
    (set-det))
  (= (check-for-unmatched-necessary-constraints-p $Templ $_)  
    (not (member (must $_) $Templ)))


  (= (partial-match $Templ $Ex $OldBL $NewBL $PL $RestTempl $RestEx)  
    (findbag 
      (:: $PL1 $RestTempl1 $RestEx1 $NewBL1) 
      (partial-match1 $Templ $Ex $OldBL $PL1 $RestTempl1 $RestEx1 $NewBL1) $PartialMatches)
    (remove-duplicates $PartialMatches $PartialMatches1 partial-match-equal-p)
    (list-sort $PartialMatches1 $SortedPartialMatches partial-match-better-p)
    (get-best $SortedPartialMatches $BestMatches partial-match-better-p)
    (member 
      (:: $PL $RestTempl $RestEx $NewBL) $BestMatches))
; /********************************************************************/
; /* */
; /* call : partial_match(!Template,!Example, */
; /* !OldBindingList,?NewBindingList, */
; /* ?PartialMatchList,?MissingList, */
; /* ?AdditionList) */
; /* */
; /* arguments : Template = Current Template */
; /* Example = Structural Description */
; /* OldBindingList = List of Current Bindings */
; /* NewBindingList = List of New Bindings */
; /* PartialMatchList = List of Partial Matched Pairs */
; /* MissingList = List of Missing Example Facts */
; /* AdditionList = List of Missing Concept Parts */
; /* */
; /********************************************************************/
; /* Finding partial matches involves guessing. To avoid returning a */
; /* bad match only because of a bad first guess, we need to look at */
; /* all possible matches and return the best one (since by looking at*/
; /* the first N matches, we cannot always be sure there won't be a */
; /* better one if we backtrack once more). Anything that can't be */
; /* matched partially is returned as missing (left-over template */
; /* parts) or additional (left-over example parts). */
; /********************************************************************/
; /* find all partial matches */
; /* get all matches with the top score */
; /* return one of them (backtrackable) */



  (= (partial-match1 $Templ $Ex $OldBL Nil $Templ $Ex $OldBL)  
    (or 
      (= $Templ Nil) 
      (= $Ex Nil))
    (set-det))
; /* if either Templ or Ex are empty, we can't do anything */
  (= (partial-match1 $Templ $Ex $OldBL Nil $Templ $Ex $OldBL)  
    (not (find-partial-match-p $Templ $Ex $OldBL $_ $_ $_ $_))
    (set-det))
; /* we couldn't find another partial match - just return */
  (= (partial-match1 $Templ $Ex $OldBL (Cons  $PartialMatch $RestPL) $RestTempl $RestEx $NewBL)  
    (find-partial-match-p $Templ $Ex $OldBL $PartialMatch $RestTempl1 $RestEx1 $NewBL1)
    (set-det)
    (partial-match1 $RestTempl1 $RestEx1 $NewBL1 $RestPL $RestTempl $RestEx $NewBL))


  (= (find-partial-match-p $Templ $Ex $OldBL $PartialMatch $RestTempl $RestEx $NewBL)  
    (enumerate $Templ $Part $RestTempl)
    (find-partial-match-p1 $Part $Ex $OldBL $MatchingFact $RestEx $NewBL)
    (= $PartialMatch 
      (:: $Part $MatchingFact)))
; /* can we find a partial match for Part? */
; /* yes, fine */


  (= (find-partial-match-p1 $Part $Ex $OldBL $ExamplePart $RestEx $NewBL)  
    (enumerate $Ex $ExamplePart $RestEx)
    (partial-match-p $Part $ExamplePart $OldBL $NewBL))
; /* they must match */


  (= (partial-match-p (must $Constraint) $Fact $OldBL $NewBL)  
    (set-det)
    (partial-match-p $Constraint $Fact $OldBL $NewBL))
; /********************************************************************/
; /* */
; /* call : partial_match_p(+Constraint,+Fact,+OldBL,-NewBL) */
; /* */
; /* arguments : Constraint = */
; /* Fact = */
; /* OldBL = List of Current Bindings */
; /* NewBL = List of New Bindings */
; /* */
; /********************************************************************/
; /* Succeed if Constraint and Fact match partially, returning the new*/
; /* binding list, or fail otherwise. */
; /* Second possibility: we are able to use a theorem that uses the */
; /* existing example fact to derive a fact that matches. See */
; /* arch_1.pro for some sample inferences. */
; /********************************************************************/
  (= (partial-match-p $Constraint $Fact $OldBL $NewBL)  
    (=.. $Constraint 
      (Cons  $Functor $Args1))
    (=.. $Fact 
      (Cons  $Functor $Args2))
    (count-differences-p $Args1 $Args2 $OldBL $NewBL 1))
  (= (partial-match-p $Constraint $Fact $OldBL $NewBL)  
    (infer $Fact $FactDerivation)
    (partial-match-p $Constraint $FactDerivation $OldBL $NewBL))



  (= (partial-match-better-p (:: $PL1 $RestTempl1 $RestEx1 $BL1) (:: $PL2 $RestTempl2 $RestEx2 $BL2))  
    (length $RestTempl1 $L1)
    (length $RestTempl2 $L2)
    (< $L1 $L2))
; /********************************************************************/
; /* Decides if one partial match is better than another by a simple */
; /* measure: more parts of the template matched */
; /********************************************************************/


  (= (partial-match-equal-p (:: $PL1 $RestTempl1 $RestEx1 $BL1) (:: $PL2 $RestTempl2 $RestEx2 $BL2))  
    (set-equal-p $PL1 $PL2)
    (set-equal-p $RestTempl1 $RestTempl2)
    (set-equal-p $RestEx1 $RestEx2)
    (set-equal-p $BL1 $BL2))


  (= (count-differences-p Nil Nil $BL $BL 0)  
    (set-det))
; /********************************************************************/
; /* */
; /* call : count_differences_p(+List1,+List2,+OldBL,-NewBL, */
; /* -NoOfDifferences) */
; /* */
; /* arguments : List1 = */
; /* List2 = */
; /* OldBL = List of Current Bindings */
; /* NewBL = List of New Bindings */
; /* NoOfDifferences = Number of Differences */
; /* */
; /********************************************************************/
; /* NoOfDifferences is the number of positions in which list1 and */
; /* list2 differ, returning new binding list. Fails if the two lists */
; /* don't have the same length. */
; /********************************************************************/
  (= (count-differences-p (Cons  $First1 $Rest1) (Cons  $First2 $Rest2) $OldBL $NewBL $RestN)  
    (atom-match-p $First1 $First2 $OldBL $BL)
    (set-det)
    (count-differences-p $Rest1 $Rest2 $BL $NewBL $RestN))
; /* those two matched - compute differences for tail of list */
  (= (count-differences-p (Cons  $First1 $Rest1) (Cons  $First2 $Rest2) $OldBL $NewBL $N)  
    (set-det)
    (not (= $First1 (var $_)))
    (not (= $First2 (var $_)))
    (count-differences-p $Rest1 $Rest2 $OldBL $NewBL $RestN)
    (is $N 
      (+ $RestN 1)))
; /* oops - they didn't match, increase count (but only if */
; /* both were constants), there can't be partial matches */
; /* with incorrectly bound vars) */


  (= (atom-match-p $O1 $O2 $OldBL $NewBL)  
    (atom-match-p1 $O1 $O2 $OldBL $NewBL)
    (unique-binding-p $NewBL))
; /********************************************************************/
; /* */
; /* call : atom_match_p(+Object1,+Object2,BindingList) */
; /* */
; /* arguments : Object1 = */
; /* Object2 = */
; /* BindingList = List of Current Bindings */
; /********************************************************************/
; /* Succeeds iff Object1 and Object2 can be unified using the */
; /* bindings in BindingList. Variables are marked "var(varname)" */
; /* (i.e., they are not MeTTa variables). Handles atoms and */
; /* variables only. Note: we require bindings to be unique, i.e. */
; /* invertible mappings from vars to values. atom_match_p checks for */
; /* that after performing a match. */
; /********************************************************************/


  (= (atom-match-p1 (var $VarName1) (var $VarName2) $OldBL $NewBL)  
    (set-det)
    (get-binding $VarName1 $OldBL $BL $Binding1)
    (get-binding $VarName2 $BL $NewBL $Binding2)
    (= $Binding1 $Binding2))
; /* matching two variables - give them a MeTTa variable */
; /* as binding, so when one of them gets matched later on, */
; /* that binding is propagated */
  (= (atom-match-p1 $Const (var $VarName) $OldBL $NewBL)  
    (set-det)
    (get-binding $VarName $OldBL $NewBL $Const))
; /* we require bindings to be unique (one part in object A */
; /* can match only one part in object B */
  (= (atom-match-p1 (var $VarName) $Const $OldBL $NewBL)  
    (set-det)
    (get-binding $VarName $OldBL $NewBL $Const))
  (= (symbol_match_p1  $Const $Const $BL $BL)  True)


  (= (get-binding $Key Nil (:: (Cons  $Key $Value)) $Value)  
    (set-det))
; /********************************************************************/
; /* */
; /* call : get_binding(+Key,+BindingList,-Value) */
; /* */
; /* arguments : Key = */
; /* BindingList = List of Current Bindings */
; /* Value = */
; /* */
; /********************************************************************/
; /* Value is the binding for Key in BindingList (first occurence). If*/
; /* key is not bound in BindingList, a PROLOG variable is returned */
; /* as Value. If the binding list is empty, Key doesn't have a value*/
; /* yet, add it (as an unbound variable) */
; /********************************************************************/
  (= (get-binding $Key $BL $BL $Value1)  
    (= $BL 
      (Cons  
        (Cons  $Key $Value2) $_))
    (set-det)
    (= $Value1 $Value2))
; /* this split is necessary for cases where Value1 is */
; /* bound by the caller */
  (= (get-binding $Key (Cons  $FirstBinding $RestBL) (Cons  $FirstBinding $NewRestBL) $Value)  
    (set-det)
    (get-binding $Key $RestBL $NewRestBL $Value))



  (= (get_variable_p  $Value (Cons  (Cons  $Variable $Value) $_) $Variable)  True)
; /********************************************************************/
; /* */
; /* call : get_variable_p(+Value,+BindingList,-Variable) */
; /* */
; /* arguments : Value = */
; /* BindingList = List of Current Bindings */
; /* Variable = */
; /* */
; /********************************************************************/
; /* The "reverse" of get_binding: given a value, returns the first */
; /* Variable bound to that value in BindingList (backtracking returns*/
; /* second, etc.). Fails if there is none. */
; /********************************************************************/
  (= (get-variable-p $Value (Cons  $_ $RestBL) $Variable)  
    (get-variable-p $Value $RestBL $Variable))



  (= (unique_binding_p  ())  True)
; /********************************************************************/
; /* */
; /* call : unique_binding_p(BindingList) */
; /* */
; /* arguments : BindingList = List of Current Bindings */
; /* */
; /********************************************************************/
; /* Suceeds if no binding value occurs more than once */
; /********************************************************************/
  (= (unique-binding-p (Cons  $FirstBinding $RestBindings))  
    (unique-binding-p $RestBindings)
    (= $FirstBinding 
      (Cons  $_ $Value))
    (not (get-variable-p $Value $RestBindings $_)))


  (= (initial-generalization $Example $InitialConcept)  
    (variabilize-part-facts $Example $VarPartFacts $BL $RestFacts)
    (variabilize-facts $RestFacts $BL $VarRestFacts)
    (append $VarPartFacts $VarRestFacts $Template)
    (concept-template $InitialConcept $Template)
    (concept-constraints $InitialConcept Nil)
    (set-det))
; /********************************************************************/
; /* */
; /* call : initial_generalization(+Example, */
; /* -InitialDefinition) */
; /* */
; /* arguments : Example = Structural Description */
; /* InitialDefinition = Generalized Definition */
; /* */
; /********************************************************************/
; /* Winston interprets all nodes as variables, except those found */
; /* in special positions (eg., second argument of an isa link). In */
; /* our representation, this means we have to mark the constants */
; /* found in the first example as variables. We use a different */
; /* heuristic: all parts (introduced with the "part" predicate) are */
; /* interpreted as variables. The result constitutes the initial */
; /* sufficient conditions, the necessary conditions are still empty, */
; /* as we haven't seen counterexamples yet. */
; /********************************************************************/
; /* now variabilize the other facts, marking only the */
; /* part names as variable */


  (= (variabilize-part-facts Nil Nil Nil Nil)  
    (set-det))
  (= (variabilize-part-facts (Cons  (part $PartName) $Rest) $VarPartFacts $BL $RestFacts)  
    (set-det)
    (= $VarPartFacts 
      (Cons  
        (part (var $PartName)) $VarRestParts))
    (= $BL 
      (Cons  
        (Cons  $PartName $PartName) $RestBL))
    (variabilize-part-facts $Rest $VarRestParts $RestBL $RestFacts))
  (= (variabilize-part-facts (Cons  $First $Rest) $VarRestParts $RestBL (Cons  $First $RestFacts))  
    (set-det)
    (variabilize-part-facts $Rest $VarRestParts $RestBL $RestFacts))


  (= (variabilize-facts Nil $_ Nil)  
    (set-det))
  (= (variabilize-facts (Cons  $First $Rest) $BL (Cons  $VarFirst $VarRest))  
    (set-det)
    (variabilize-fact $First $BL $VarFirst)
    (variabilize-facts $Rest $BL $VarRest))


  (= (specialize-concept-definition Nil $_ $C $C)  
    (set-det))
; /********************************************************************/
; /* */
; /* call : specialize_concept_definition( */
; /* +DifferenceDescriptions, */
; /* +BindingList,+OldConcept, */
; /* -NewConcept) */
; /* */
; /* arguments : DifferenceDescriptions = Difference Description */
; /* BindingList = List of Current Bindings */
; /* OldConcept = Current Concept Def. */
; /* NewConcept = New Concept Definition */
; /* */
; /********************************************************************/
; /* Specialize the supplied concept definition by adding constraints */
; /* to its necessary conditions. Based on Winston's require-link */
; /* and forbid-link heuristics. */
; /********************************************************************/
  (= (specialize-concept-definition (Cons  (addition $ExampleFact) $RestDD) $BL $OldConcept $NewConcept)  
    (set-det)
    (variabilize-fact $ExampleFact $BL $NewNecessaryConstraint)
    (specialize-concept-definition $RestDD $BL $OldConcept $Concept)
    (concept-constraints $Concept $Constraints)
    (list-add-if-necessary 
      (not $NewNecessaryConstraint) $Constraints $NewConstraints)
    (alter-concept-constraints $Concept $NewConstraints $NewConcept))
; /* an additional fact was found in the example - use */
; /* forbid-link heuristic */
; /* treat rest of differences */
; /* add negative constraint to constraints slot */

  (= (specialize-concept-definition (Cons  (missing $Part) $RestDD) $BL $OldConcept $NewConcept)  
    (set-det)
    (specialize-concept-definition $RestDD $BL $OldConcept $Concept)
    (concept-template $Concept $Template)
    (list-remove $Part $Template $Template1)
    (= $NewTemplate 
      (Cons  
        (must $Part) $Template1))
    (alter-concept-template $Concept $NewTemplate $NewConcept))
; /* a constraint was missing from the example - use require-link */
; /* heuristic */
; /* treat rest of differences */
; /* replace Part by must(Part) */

  (= (specialize-concept-definition (Cons  (partial (:: $Part $_)) $RestDD) $BL $OldConcept $NewConcept)  
    (set-det)
    (specialize-concept-definition $RestDD $BL $OldConcept $Concept)
    (concept-template $Concept $Template)
    (list-remove $Part $Template $Template1)
    (= $NewTemplate 
      (Cons  
        (must $Part) $Template1))
    (alter-concept-template $Concept $NewTemplate $NewConcept))
; /* a partial match - treat like missing constraint */
; /* replace Part by must(Part) */


  (= (variabilize-fact $Fact $BL $VarFact)  
    (=.. $Fact 
      (Cons  $Functor $Args))
    (variabilize-list $Args $BL $VarArgs)
    (=.. $VarFact 
      (Cons  $Functor $VarArgs)))
; /********************************************************************/
; /* */
; /* call : variabilize_fact(+Fact,+BindingList,-VarFact) */
; /* */
; /* arguments : Fact = */
; /* BindingList = List of Current Bindings */
; /* VarFact = */
; /* */
; /********************************************************************/
; /* Replaces each argument in Fact by the first variable in */
; /* BindingList which has that argument as its binding; if there is */
; /* none, the argumnet is left as it is. */
; /********************************************************************/


  (= (variabilize-list Nil $_ Nil)  
    (set-det))
  (= (variabilize-list (Cons  $FirstArg $RestArgs) $BL (Cons  (var $VarFirstArg) $VarRestArgs))  
    (get-variable-p $FirstArg $BL $VarFirstArg)
    (set-det)
    (variabilize-list $RestArgs $BL $VarRestArgs))
  (= (variabilize-list (Cons  $FirstArg $RestArgs) $BL (Cons  $FirstArg $VarRestArgs))  
    (set-det)
    (variabilize-list $RestArgs $BL $VarRestArgs))


  (= (generalize-concept-definition Nil $C $C)  
    (set-det))
; /********************************************************************/
; /* */
; /* call : generalize_concept_definition( */
; /* +DifferenceDescriptions, */
; /* +OldConcept,-NewConcept) */
; /* */
; /* arguments : DifferenceDescriptions = Difference Description */
; /* OldConcept = Current Concept Defi. */
; /* NewConcept = New Concept Definition */
; /* */
; /********************************************************************/
; /* Generalize the supplied concept definition by weakening and/or */
; /* removing constraints from its sufficient conditions NOTE: if you */
; /* pass differences involving necessary conditions ("must"), */
; /* generalization may result in the concept covering negative */
; /* examples again. */
; /********************************************************************/
  (= (generalize-concept-definition (Cons  (partial (:: $Constraint $ExampleFact)) $RestDD) $OldConcept $NewConcept)  
    (set-det)
    (generalize-arg $Constraint $ExampleFact $NewConstraint)
    (generalize-concept-definition $RestDD $OldConcept $RestConcept)
    (concept-template $RestConcept $Template)
    (list-remove $Constraint $Template $Template1)
    (= $NewTemplate 
      (Cons  $NewConstraint $Template1))
    (alter-concept-template $RestConcept $NewTemplate $NewConcept))
; /* a partial match was found, i.e., one arg in Constraint and */
; /* Factis different. If there is common parent in the ako */
; /* hierarchy for both, use the parent in the new constraint. */
; /* If there isn't, use a new variable */
; /* treat rest of differences */

  (= (generalize-concept-definition (Cons  (missing $Constraint) $RestDD) $OldConcept $NewConcept)  
    (set-det)
    (generalize-concept-definition $RestDD $OldConcept $RestConcept)
    (concept-template $RestConcept $Template)
    (list-remove $Constraint $Template $NewTemplate)
    (alter-concept-template $RestConcept $NewTemplate $NewConcept))
; /* a constraint was missing from the example - use */
; /* drop-link heuristic */

  (= (generalize-concept-definition (Cons  $_ $RestDD) $OldSC $RestSC)  
    (generalize-concept-definition $RestDD $OldSC $RestSC))
; /* this must be an additional example fact - don't know */
; /* what to do with them, so ignore and treat rest of */
; /* differences */


  (= (generalize-arg (must $Constraint) $ExampleFact (must $NewConstraint))  
    (set-det)
    (generalize-arg $Constraint $ExampleFact $NewConstraint))

  (= (generalize-arg $Constraint $ExampleFact $NewConstraint)  
    (=.. $Constraint 
      (Cons  $Functor $Args1))
    (=.. $ExampleFact 
      (Cons  $Functor $Args2))
    (generalize-arg1 $Args1 $Args2 $NewArgs1)
    (=.. $NewConstraint 
      (Cons  $Functor $NewArgs1)))


  (= (generalize-arg1 Nil Nil Nil)  
    (set-det))
  (= (generalize-arg1 (Cons  $Arg1 $Rest1) (Cons  $Arg2 $Rest2) (Cons  $Arg1 $GenRest))  
    (or 
      (= $Arg1 $Arg2) 
      (= $Arg1 
        (var $_)))
    (set-det)
    (generalize-arg1 $Rest1 $Rest2 $GenRest))
  (= (generalize-arg1 (Cons  $Arg1 $Rest1) (Cons  $Arg2 $_) (Cons  $GenArg $Rest1))  
    (set-det)
    (find-or-create-common-ancestor $Arg1 $Arg2 $GenArg))
; /* o.k., this is it - can we generalize them? */


  (= (find-or-create-common-ancestor $Class1 $Class2 $Ancestor)  
    (ancestors $Class1 $Ancestors1)
    (ancestors $Class2 $Ancestors2)
    (smallest-ancestor-p $Ancestors1 $Ancestors2 $Ancestor)
    (set-det))
  (= (find-or-create-common-ancestor $Class1 $Class2 $Ancestor)  
    ( (set-det) 
      (concat $Class1 $Class2 $Ancestor) 
      (add-is-symbol  &self 
        (ako  $Class1 $Ancestor)) 
      (add-is-symbol  &self 
        (ako  $Class2 $Ancestor))))
; /* create a new name */
; /* and record its relation to existing classes */


  (= (ancestors $Class $Ancestors)  
    (ancestors1 
      (:: $Class) Nil $Ancestors))


  (= (ancestors1 Nil $Ancestors $Ancestors)  
    (set-det))
  (= (ancestors1 (Cons  $First $Rest) $Ancestors $ExtendedAncestors)  
    (direct-ancestors $First $FirstAncestors)
    (not (= $FirstAncestors Nil))
    (set-det)
    (append $FirstAncestors $Rest $NewClassList)
    (union $Ancestors $FirstAncestors $NewAncestorList)
    (ancestors1 $NewClassList $NewAncestorList $ExtendedAncestors))
  (= (ancestors1 (Cons  $First $Rest) $Ancestors $ExtendedAncestors)  
    (set-det)
    (union 
      (:: $First) $Ancestors $NewAncestors)
    (ancestors1 $Rest $NewAncestors $ExtendedAncestors))
; /* no ancestors */


  (= (direct-ancestors $Class $Ancestors)  
    (set-det)
    (findbag $Ancestor 
      (ako $Class $Ancestor) $Ancestors))


  (= (smallest-ancestor-p (Cons  $First $_) $Ancestors2 $First)  
    (member $First $Ancestors2)
    (set-det))
  (= (smallest-ancestor-p (Cons  $_ $Rest) $Ancestors2 $Ancestor)  
    (set-det)
    (smallest-ancestor-p $Rest $Ancestors2 $Ancestor))


  (= (list-remove $_ Nil Nil)  
    (set-det))
; /********************************************************************/
; /* */
; /* call : list_remove(+Target,+List,-Rest) */
; /* */
; /* arguments : Target = */
; /* List = */
; /* Rest = List without Target */
; /* */
; /********************************************************************/
; /* Remove all elements of List that unify with Target, return the */
; /* Rest. */
; /********************************************************************/
  (= (list-remove $Target (Cons  $Target $Rest) $RestRemoved)  
    (set-det)
    (list-remove $Target $Rest $RestRemoved))
  (= (list-remove $Target (Cons  $First $Rest) (Cons  $First $RestRemoved))  
    (list-remove $Target $Rest $RestRemoved))


  (= (msgs Nil)  
    (set-det))
; /********************************************************************/
; /* */
; /* call : msgs(+List) */
; /* */
; /* arguments : List = of Elements which should be displayed on */
; /* current output device */
; /* */
; /********************************************************************/
; /* Display the Elements of the List on the current output device. */
; /* The following constants are handled in a special way: */
; /* */
; /* nl - forces a single linefeed */
; /* nl(X) - forces the number of linefeeds given by X */
; /* sp - forces a single space */
; /* nl(X) - forces the number of spaces given by X */
; /* pf(X,Y) - uses print function X to display Y */
; /* */
; /* Every other element is outputed in the normal "print"-way. */
; /********************************************************************/
  (= (msgs (Cons  $First $Rest))  
    (msg $First)
    (msgs $Rest)
    (set-det))


  (= (msg nl)  
    (set-det)
    (nl))
  (= (msg (nl $N))  
    (set-det)
    (repeat $N nl))
  (= (msg sp)  
    (set-det)
    (write ' '))
  (= (msg (sp $N))  
    (set-det)
    (repeat $N 
      (write ' ')))
  (= (msg (pf $PF $Object))  
    (set-det)
    (=.. $Call 
      (:: $PF $Object))
    (call $Call))
  (= (msg $Object)  
    (write $Object)
    (set-det))


  (= (repeat $N $_)  
    (< $N 1)
    (set-det))
; /********************************************************************/
; /* */
; /* call : repeat(+Number,+Call) */
; /* */
; /* arguments : Number = Number of times */
; /* Call = Procedure call */
; /* */
; /********************************************************************/
; /* Repeats Call Number times. */
; /********************************************************************/
  (= (repeat $N $Call)  
    ($Call 
      (is $N1 
        (- $N 1)) 
      (repeat $N1 $Call) 
      (set-det)))


  (= (set-equal-p Nil Nil)  
    (set-det))
  (= (set-equal-p (Cons  $First $Rest) $Set2)  
    (enumerate $Set2 $First $Rest2)
    (set-det)
    (set-equal-p $Rest $Rest2))


  (= (remove-duplicates Nil Nil $_)  
    (set-det))
; /********************************************************************/
; /* remove_duplicates */
; /********************************************************************/
; /* with an additional EqualP */
; /********************************************************************/
  (= (remove-duplicates (Cons  $First $Rest) $Result $EqualP)  
    (remove-duplicates $Rest $RestResult $EqualP)
    (remove-duplicates-result $First $RestResult $Result $EqualP))


  (= (remove-duplicates-result $First $RestResult $RestResult $EqualP)  
    (member $First $RestResult $EqualP)
    (set-det))
  (= (remove_duplicates_result  $First $RestResult (Cons  $First $RestResult) $_)  True)


  (= (list_sort  () () $_)  True)
; /********************************************************************/
; /* */
; /* call : list_sort(+List,-SortedList,+ComparisonPredicate) */
; /* */
; /* arguments : List = */
; /* SortedList = */
; /* ComparisonPredicateDifference = */
; /* */
; /********************************************************************/
; /* An insertion sort ! */
; /* SortedList has the same members as List and is sorted according */
; /* to ComparisonPredicate (a before b if ComparisonPredicate(a,b)). */
; /********************************************************************/
  (= (list-sort (Cons  $X $L) $M $O)  
    (list-sort $L $N $O)
    (list-sort1 $X $N $M $O))


  (= (list-sort1 $X (Cons  $A $L) (Cons  $A $M) $O)  
    (=.. $P 
      (:: $O $A $X))
    (call $P)
    (set-det)
    (list-sort1 $X $L $M $O))
  (= (list_sort1  $X $L (Cons  $X $L) $_)  True)


  (= (get_best  () () $_)  True)
; /********************************************************************/
; /* */
; /* call : get_best(+SortedList,-Best,+OrderP) */
; /* */
; /* arguments : SortedList = */
; /* Best = */
; /* OrderP = */
; /* */
; /********************************************************************/
; /* get_best returns the elements with the top score in SortedList */
; /* (which must be sorted according to OrderP). */
; /********************************************************************/
  (= (get_best  ($Single) ($Single) $_)  True)
  (= (get-best (Cons  $First (Cons  $Second $_)) (:: $First) $ComparisonP)  
    (=.. $Call 
      (:: $ComparisonP $First $Second))
    (call $Call)
    (set-det))
; /* the next match is worse - we've found all the best matches */
  (= (get-best (Cons  $First (Cons  $Second $Rest)) (Cons  $First $RestBest) $ComparisonP)  
    (set-det)
    (get-best 
      (Cons  $Second $Rest) $RestBest $ComparisonP))
; /* keep looking */


  (= (enumerate  (Cons  $Head $Tail) $Head $Tail)  True)
; /********************************************************************/
; /* */
; /* call : enumerate(!List,?Head,?Rest) */
; /* */
; /* arguments : List = */
; /* Head = */
; /* Rest = */
; /* */
; /********************************************************************/
; /* Enumerates heads of List on backtracking, Rest is always List */
; /* with current Head removed. */
; /********************************************************************/
  (= (enumerate (Cons  $Head $Tail) $NextHead (Cons  $Head $NextRest))  
    (enumerate $Tail $NextHead $NextRest))


  (= (list-add-if-necessary $NewElement $List $List)  
    (member $NewElement $List)
    (set-det))
; /********************************************************************/
; /* */
; /* call : list_add_if_necessary(+NewElement,+List,-NewList) */
; /* */
; /* arguments : NewElementList = */
; /* List = */
; /* NewList = */
; /* */
; /********************************************************************/
; /* Adds NewElement to List if it is not already there; result is */
; /* NewList */
; /********************************************************************/
  (= (list-add-if-necessary $NewElement $List $NewList)  
    (= $NewList 
      (Cons  $NewElement $List))
    (set-det))


  (= (concat $S1 $S2 $S3)  
    (nonvar $S1)
    (nonvar $S2)
    (name $S1 $L1)
    (name $S2 $L2)
    (append $L1 $L2 $L3)
    (name $S3 $L3)
    (set-det))
; /********************************************************************/
; /* */
; /* call : concat(Atom1,Atom2,Atom3) */
; /* */
; /* arguments : Atom1 = */
; /* Atom2 = */
; /* Atom3 = */
; /* */
; /********************************************************************/
; /* Concatenates Atom1 and Atom2 to Atom3, or splits Atom3 in two */
; /* ways if Atom1 is instantiated than the rest of Atom3 is returned */
; /* in Atom2 or if Atom2 is instantiated than the Atom1 becomes the */
; /* prefix of Atom3 without Atom1. Two arguments must be instantiated*/
; /* otherwise the predicate fails. */
; /********************************************************************/

  (= (concat $S1 $S2 $S3)  
    (nonvar $S1)
    (nonvar $S3)
    (name $S1 $L1)
    (name $S3 $L3)
    (append $L1 $L2 $L3)
    (name $S2 $L2)
    (set-det))

  (= (concat $S1 $S2 $S3)  
    (nonvar $S2)
    (nonvar $S3)
    (name $S2 $L2)
    (name $S3 $L3)
    (append $L1 $L2 $L3)
    (name $S1 $L1)
    (set-det))


  (= (member $_ $List $_)  
    (var $List)
    (set-det)
    (is $List Nil)
    (fail))
; /********************************************************************/
; /* */
; /* call : member(Element,List,EqualP) */
; /* */
; /* arguments : Element = */
; /* List = */
; /* EqualP = */
; /* */
; /********************************************************************/
; /* The common member predicate extend by an additional EqualP to be */
; /* used for membership test. */
; /********************************************************************/
  (= (member $E (Cons  $First $_) $EqualP)  
    (=.. $Call 
      (:: $EqualP $E $First))
    (call $Call))
  (= (member $E (Cons  $_ $R) $EqualP)  
    (member $E $R $EqualP))


  !(dynamic (/ found 1))


  (= (findbag $X $G $_)  
    ( (add-is-symbol  &self 
        (found  mark)) 
      (call $G) 
      (add-is-symbol  &self 
        (found  $X)) 
      (fail)))                            
  (= (findbag $_ $_ $L)  
    (collect-found Nil $L))                                     
                                                              

  (= (collect-found $L $L1)  
    (getnext $X)
    (collect-found 
      (Cons  $X $L) $L1))                      
  (= (collect_found  $L $L)  True)                                        
                                                              
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  arch1/arch1.pl 1339 30 68638))
                


  (= (union  () $X $X)  True)
  (= (union (Cons  $X $R) $Y $Z)  
    (member $X $Y)
    (set-det)
    (union $R $Y $Z))
  (= (union (Cons  $X $R) $Y (Cons  $X $Z))  
    (union $R $Y $Z))


  (= (help)  
    (write 'Load data set with command: [Filename].')
    (nl)
    (write 'Start arch1   with command: process-stored-inputs(ConceptName).')
    (nl))
   

  !(help *)

