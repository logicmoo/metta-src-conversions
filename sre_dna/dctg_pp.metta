;                                           (convert_to_metta_file  dctg_pp $_459326 sre_dna/dctg_pp.pl sre_dna/dctg_pp.metta)

;
; Brian Ross
;
; Dec 10, 1998

;
; DCTG post-processor: generates relevant tables needed by GP processing.
;
; 
;
; 1. dctg_rule_info(Name, ID, Call, MinDepth, Type)
;
; Name = name of rule
;
; ID = DCTG ID label
;
; Call = call image for DCTG rule invocation
;
; MinDepth = minimum depth to terminal generation
;
; Type = terminal, nonterminal
;
; 
;
; 2. dctg_id_table(Name, IDList, TermList, NontermList)
;
; Name = functor name
;
; IDList = list of rule ID's for that functor
;
; TermList, NontermList = lists of terminals, nonterminals
;
; 
;
; Some assumptions:
;
; - Grammar rules should be uniquely identifiable by functor name.
;
; Should not use same functor name for different arity grammar rules!


  (= (make-grammar-table)  
    (cleanup-grammar-data)
    (make-rule-id-list)
    (generate-rule-data)
    (enhance-rule-id-list)
    (set-det))


  (= (cleanup-grammar-data)  
    ( (remove-all-atoms  &self 
        (dctg_rule_info  $_ $_ $_ $_)) 
      (remove-all-atoms  &self 
        (dctg_id_table  $_ $_ $_ $_)) 
      (set-det)))

;
; get_rule_name constructs a call to a DCTG rule, such that all the args are
;
; variable except for the node structure, which has the ID number set.
;
; This uniquely identifies a DCTG rule header.


  (= (get-rule-name $Call2)  
    ( (==  
        (= 
          (semantic_rule  $ID $_ $Call $_) $_) 
        (get-atoms  &self)) 
      (=.. $Call 
        (Cons  $Name $Args)) 
      (clone-list $Args $T) 
      (append $T 
        (:: 
          (node $_ $_ $ID) $_ $_) $AllArgs) 
      (=.. $Call2 
        (Cons  $Name $AllArgs))))

;
; Given a list, clone_list creates a list of the same size,
;
; but with uninst. args


  (= (clone-list Nil Nil)  
    (set-det))
  (= (clone-list (Cons  $_ $T) (Cons  $_ $T2))  
    (clone-list $T $T2)
    (set-det))

;
; generate_rule_data gets all the rule headers (Calls) and finds: (i) the
;
; minimum depth to terminals for each rule; (ii) whether a rules is a
;
; terminal or nonterminal. Result saved in dctg_rule_info/4.


  (= (generate-rule-data)  
    (findall $Call 
      (get-rule-name $Call) $Calls)
    (rem-dups $Calls $Calls2)
    (grammar-depth-top-loop $Calls2 Nil Nil $Calls3)
    (grammar-type-top-loop $Calls2 Nil Nil $Terminal)
    (set-rule-data $Calls3 $Terminal)
    (set-det))

;
; grammar_depth_top_loop(Calls, Known, MinCalls, Known2)
;
; Processes rules until all have min depths found, or no changes occurred
;
; (which means there's a problem with the rules).
;
; Calls - rules to process
;
; Known - rules with min depths
;
; MinCalls - overall minimum for entire rules, to be used in goal analysis
;
; Known - new Known set
;
; 
;
; Algorithm for depth determination:
;
; Repeat until no unknown rules (hopefully! else there's infinite recursion)
;
; Process all unknown rules:
;
; If all goals have known minimum depths in a rule
;
; then find maximum of these and add to Known list
;
; Process all Known rules:
;
; If a new rule has been added to Known (not in Minimum list)
;
; then add it to Minimum list.


  (= (grammar-depth-top-loop Nil $Known $_ $Known)  
    (set-det))
  (= (grammar-depth-top-loop $Calls $Known $MinCalls $Known3)  
    (process-rules $Calls $Known $MinCalls Nil $Known2 $Unknown)
    (find-rule-mins $Known2 $MinCalls $MinCalls2)
    (det-if-then-else 
      (, 
        (length $Calls $L) 
        (length $Unknown $L)) 
      (, 
        (write 'Problem - ') 
        (write $L) 
        (write ' rules cannot terminate:') 
        (nl) 
        (writelist $Unknown) 
        (nl) 
        (write 'these terminated - ') 
        (nl) 
        (writelist $Known2) 
        (nl) 
        (write 'These are mincalls - ') 
        (nl) 
        (writelist $MinCalls2) 
        (nl) 
        (fail)) 
      (grammar-depth-top-loop $Unknown $Known2 $MinCalls2 $Known3))
    (set-det))
; ; if no changes...

;
; process_rules(Calls, Known, MinCalls, Unknown, Known2, Unknown2):
;
; Calls - to process
;
; Known, Unknown - rules with known/unknown minima
;
; MinCalls - solved rules (can be used in body analyses of other rules)
;
; Known2, Unknown2 - final values of above
;
; Find depth of body for rules. If available, add rule to Known, else Unknown.


  (= (process-rules Nil $Known $_ $Unknown $Known $Unknown)  
    (set-det))
  (= (process-rules (Cons  $Call $Rest) $Known $MinCalls $Unknown $Known2 $Unknown2)  
    ( (copy-term $Call $Call2) 
      (==  
        (= $Call2 $Body) 
        (get-atoms  &self)) 
      (find-min-depth-body $Body $MinCalls 0 $BodyDepth) 
      (set-det) 
      (is $MinD 
        (+ $BodyDepth 1)) 
      (process-rules $Rest 
        (Cons  
          (, $Call $MinD) $Known) $MinCalls $Unknown $Known2 $Unknown2)))
  (= (process-rules (Cons  $Call $Rest) $Known $MinCalls $Unknown $Known2 $Unknown2)  
    (set-det)
    (process-rules $Rest $Known $MinCalls 
      (Cons  $Call $Unknown) $Known2 $Unknown2))

;
; find_min_depth_body(Body, MinCalls, MinDSoFar, MinD)
;
; Finds the depth value of body (max of all min vals of goals in body).
;
; Fails if an goal with unknown depth is found.


  (= (find-min-depth-body (, $Goal $Rest) $MinCalls $MinDSoFar $MinD)  
    (is-a-rule-call $Goal)
    (set-det)
    (find-min-depth $Goal $MinCalls $Val)
    (is $MinDSoFar2 
      (max $Val $MinDSoFar))
    (find-min-depth-body $Rest $MinCalls $MinDSoFar2 $MinD))
  (= (find-min-depth-body (, $_ $Rest) $MinCalls $MinDSoFar $MinD)  
    (set-det)
    (find-min-depth-body $Rest $MinCalls $MinDSoFar $MinD))
  (= (find-min-depth-body $Goal $MinCalls $MinDSoFar $MinD)  
    (is-a-rule-call $Goal)
    (set-det)
    (find-min-depth $Goal $MinCalls $Val)
    (is $MinD 
      (max $Val $MinDSoFar)))
  (= (find-min-depth-body $_ $_ $MinD $MinD)  
    (set-det))

;
; find_min_depth searches for goal name in list, and returns corresp.
;
; depth if found.


  (= (find-min-depth $Goal (Cons  (, $G $M) $_) $M)  
    (=.. $Goal 
      (Cons  $G $_))
    (set-det))
  (= (find-min-depth $Goal (Cons  $_ $R) $M)  
    (find-min-depth $Goal $R $M)
    (set-det))

;
; is_a_rule_call checks if a goal refers to a DCTG rule


  (= (is-a-rule-call $Goal)  
    (=.. $Goal 
      (Cons  $Name $_))
    (dctg-id-table $Name $_ $_ $_)
    (set-det))

;
; find_rule_mins(Calls, MinCalls, MinCalls2):
;
; Checks the current Known list for new overall minimum depths.
;
; If a goal shows up in Known for the first time (ie. not in MinCalls)
;
; then it's depth value must be the minimum for that rule set: add it as such.


  (= (find-rule-mins Nil $MinCalls $MinCalls)  
    (set-det))
  (= (find-rule-mins (Cons  (, $Call $Depth) $Rest) $MinCalls $MinCalls2)  
    (=.. $Call 
      (Cons  $CallName $_))
    (not (member (, $CallName $_) $MinCalls))
    (set-det)
    (find-rule-mins $Rest 
      (Cons  
        (, $CallName $Depth) $MinCalls) $MinCalls2))
  (= (find-rule-mins (Cons  $_ $Rest) $MinCalls $MinCalls2)  
    (find-rule-mins $Rest $MinCalls $MinCalls2))

;
; abstract_member checks if functor names match


  (= (abstract-member $GoalName (Cons  (, $First $_) $_))  
    (=.. $First 
      (Cons  $GoalName $_)))	
  (= (abstract-member $GoalName (Cons  $_ $Rest))  
    (abstract-member $GoalName $Rest))	

;
; find_minimum_depth(Name, Calls, MinSoFar, Min):
;
; Finds the minimum depth value for Name in list of Calls.


  (= (find_minimum_depth  $_ () $D $D)  True)
  (= (find-minimum-depth $CallName (Cons  (, $Call $D) $Rest) $MinSoFar $MinDepth)  
    (=.. $Call 
      (Cons  $CallName $_))
    (is $NewMin 
      (min $D $MinSoFar))
    (find-minimum-depth $CallName $Rest $NewMin $MinDepth)
    (set-det))
  (= (find-minimum-depth $CallName (Cons  $_ $Rest) $MinSoFar $MinDepth)  
    (find-minimum-depth $CallName $Rest $MinSoFar $MinDepth)
    (set-det))


;
; grammar_type_top_loop(Calls, Terms, Nonterms, Terms2):
;
; Calls - rules to process
;
; Terms, Nonterms - terminals and nonterminals so far
;
; Terms2 - final results of above
;
; 
;
; Determine if rules can be classified as terminals.
;
; Processing continues until no change in the set of rules that are unknown -
;
; these leftovers are classified as 'nonterminals'.
;
; First, user-override is checked. If it fail's then analysis done.
;
; See 'rule_type' for more details.
;
; Note that intermediate nonterminal determination is done as well;
;
; this could be deleted in the future to save some processing.


  (= (grammar-type-top-loop $Calls $Terms $Nonterms $Terms2)  
    (grammar-type-loop $Calls Nil $Terms $Nonterms $Unknown $Terms3 $Nonterms3)
    (det-if-then-else 
      (, 
        (length $Calls $A) 
        (length $Unknown $A)) 
      (= $Terms3 $Terms2) 
      (grammar-type-top-loop $Unknown $Terms3 $Nonterms3 $Terms2))
    (set-det))


  (= (grammar-type-loop Nil $Unknown $Term $Nonterm $Unknown $Term $Nonterm)  
    (set-det))
  (= (grammar-type-loop (Cons  $Call $Rest) $Unknown $Term $Nonterm $Unknown2 $Term2 $Nonterm2)  
    (user-override $Call $Term $Nonterm $Term3 $Nonterm3)
    (grammar-type-loop $Rest $Unknown $Term3 $Nonterm3 $Unknown2 $Term2 $Nonterm2))
  (= (grammar-type-loop (Cons  $Call $Rest) $Unknown $Term $Nonterm $Unknown2 $Term2 $Nonterm2)  
    ( (copy-term $Call $Call2) 
      (==  
        (= $Call2 $Body) 
        (get-atoms  &self)) 
      (goal-type $Call $Body $Rest $Unknown $Term $Nonterm $Unknown3 $Term3 $Nonterm3) 
      (grammar-type-loop $Rest $Unknown3 $Term3 $Nonterm3 $Unknown2 $Term2 $Nonterm2)))

;
; user_override(Call, Term, Nonterm, Term2, Nonterm2):
;
; Call - rule head to process
;
; Term, Nonterm - list of rules identified as terms and nonterms
;
; Term2, Nonterm2 - final values of Term, Nonterm
;
; If the user has Call functor name in dctg_override parameter lists, then
;
; add that call to term or nonterm as appropriate. Otherwise, fail.


  (= (user-override $Call $Term $Nonterm (Cons  $Call $Term) $Nonterm)  
    (=.. $Call 
      (Cons  $Name $_))
    (dctg-override-P $OverTerm $_)
    (member $Name $OverTerm)
    (set-det))
  (= (user-override $Call $Term $Nonterm $Term (Cons  $Call $Nonterm))  
    (=.. $Call 
      (Cons  $Name $_))
    (dctg-override-P $_ $OverNonterm)
    (member $Name $OverNonterm)
    (set-det))
	

;
; goal_type(Call, Body, Unknown, Term, Nonterm, Unknown2, Term2, Nonterm2)
;
; Call - current head of rule being analyzed
;
; Body - body of current rule
;
; Rest - rules not yet processed
;
; Unknown - unknown classifications
;
; Term, Nonterm - Rules known to be terminal or nonterminal
;
; Unknown2,Term2,Nonterm2 - Final values of results
;
; 
;
; This performs abstract interp of a single rule body.
;
; The order of tests in the following is critical.
;
; 1. If goal is a nonterm, then that rule is a nonterm.
;
; 2. If goal is same as rule name, then that rule is a nonterm.
;
; 3. If a goal is unknown, or not yet processed,
;
; then that rule is unknown.
;
; 4. Else if the rest of the goals in that clause are term,
;
; then that rule is terminal.


  (= (goal-type $Call $Goals $_ $U $T $NT $U $T (Cons  $Call $NT))  
    (det-if-then-else 
      (= $Goals 
        (, $A $_)) True 
      (= $Goals $A))
    (or 
      (abstract-member2 $A $NT) 
      (same-goal $Call $A))
    (set-det))
; ; 1, 2
  (= (goal-type $Call $Goals $Rest $U $T $NT (Cons  $Call $U) $T $NT)  
    (det-if-then-else 
      (= $Goals 
        (, $A $_)) True 
      (= $Goals $A))
    (or 
      (abstract-member2 $A $U) 
      (abstract-member2 $A $Rest))
    (set-det))
; ; 3
  (= (goal-type $Call (, $_ $B) $Rest $U $T $NT $U2 $T2 $NT2)  
    (set-det)
    (goal-type $Call $B $Rest $U $T $NT $U2 $T2 $NT2))
  (= (goal_type  $Call $_ $_ $U $T $NT $U (Cons  $Call $T) $NT)  True) ;
; 4


;
; abstract_member2 checks if functor names match


  (= (abstract-member2 $Goal (Cons  $First $_))  
    (same-goal $Goal $First))
  (= (abstract-member2 $Goal (Cons  $_ $Rest))  
    (abstract-member2 $Goal $Rest))	


  (= (same-goal $A $B)  
    (=.. $A 
      (Cons  $N $_))
    (=.. $B 
      (Cons  $N $_))
    (set-det))

;
; save depths, term/nonterm in dctg_rule_info assertions


  (= (set-rule-data Nil $_)  
    (set-det))
  (= (set-rule-data (Cons  (, $Rule $Depth) $Rest) $Terminal)  
    ( (=.. $Rule 
        (Cons  $Name $Args)) 
      (append $_ 
        (:: 
          (node $_ $_ $ID) $_ $_) $Args) 
      (det-if-then-else 
        (member $Rule $Terminal) 
        (= $Type terminal) 
        (= $Type nonterminal)) 
      (add-is-symbol  &self 
        (dctg_rule_info  $Name $ID $Rule $Depth $Type)) 
      (set-rule-data $Rest $Terminal) 
      (set-det)))

;
; make_rule_id_list makes a table giving rule name and the ID numbers of its
;
; associated rules. Last 2 args are placeholders for term and nonterm lists,
;
; to be set later.


  (= (make-rule-id-list)  
    (findall 
      (, $Name $IDs) 
      (make-rule-id-list2 $Name $IDs) $RuleIDs)
    (make-id-entries $RuleIDs)
    (set-det))


  (= (make-rule-id-list2 $Name $RuleIDs2)  
    (bagof $ID 
      (get-rule-stuff $Name $ID) $RuleIDs)
    (rem-dups $RuleIDs $RuleIDs2))


  (= (get-rule-stuff $Name $ID)  
    ( (==  
        (= 
          (semantic_rule  $ID $_ $Call $_) $_) 
        (get-atoms  &self)) (=.. $Call (Cons  $Name $_))))


  (= (make-id-entries Nil)  
    (set-det))
  (= (make-id-entries (Cons  (, $Name $IDs) $Rest))  
    ( (add-is-symbol  &self 
        (dctg_id_table  $Name $IDs $_ $_)) 
      (make-id-entries $Rest) 
      (set-det)))

;
; enhance each dctg_id_table entry with list of which rules are terminal,
;
; and which are nonterminal.


  (= (enhance-rule-id-list)  
    ( (remove-is-symbol  &self 
        (dctg_id_table  $Name $IDs $_ $_)) 
      (identify-type $IDs $Terms $Nonterms) 
      (add-is-symbol  &self 
        (dctg_id_table  $Name $IDs $Terms $Nonterms)) 
      (fail)))
  (= enhance_rule_id_list  True)


  (= (identify_type  () () ())  True)
  (= (identify-type (Cons  $ID $Rest) (Cons  $ID $Terms) $Nonterms)  
    (dctg-rule-info $_ $ID $_ $_ terminal)
    (set-det)
    (identify-type $Rest $Terms $Nonterms))
  (= (identify-type (Cons  $ID $Rest) $Terms (Cons  $ID $Nonterms))  
    (identify-type $Rest $Terms $Nonterms))


