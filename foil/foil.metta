;                                           (convert-to-metta-file  foil $_234340 foil/foil.pl foil/foil.metta)

  !(ensure-loaded (library occurs))
; /*************************************************************************** FILE: FOIL.PL PROGRAMMER: John Zelle DATE: 2/10/92 DESCRIPTION: A MeTTa implementation of the FOIL (First Order Inductive Learning) algorithm for learning relational concept definitions. For backgound on the algorithm, see : Quinlan, J. R., "Learning Logical Definitions from Relations," in Machine Learning, 5, 1990. The version presented here is somewhat simplified in that it uses a much weaker test to constrain recursive predicates (a recursive call must contain vars not found in the head of of a clause, and may not introduce any unbound vars), and it does not incorporate encoding length restrictions to handle noisy data. There is also no post-processing of clauses to simplify learned definitions, although this would be relatively easy to add. This is a very simple implementation which recomputes tuple sets "on the fly". Don't expect it to run like the wind. LANGUAGE: Quintus MeTTa v. 3.1 MODIFIED: 3/30/92 (JMZ) Added determinate literals. See article: Quinlan, J. R., "Determinate Literals in Inductive Logic Programming," in Proceedings of the Eighth International Workshop in Machine Learning, 1991 INPUT FORM: The background knowledge for predicate induction is represented as "existential" predicates asserted in the module, foil_input. By existential is is meant that the definitions there must be fully constructive to avoid instantiation errors when running FOIL. EXAMPLE: Definition of "list" from Quinlan's paper. This is an example input file. After consulting this definition of the module foil_input, the definition of list can be learned by invoking: foil(list(_)). ---------------------------------------------------------------------- ;Background and tuples for learning definition of list. ; This heading should be on every file used to provide ; test data for FOIL :- module(foil_input, [foil_predicates/1, foil_use_negations/1, foil_det_lit_bound/1]). foil_predicates([ list/1, null/1, components/3 ]). foil_use_negations(false). ; Don't use negations of foil_predicates foil_det_lit_bound(0). ; Don't add any determinate literals ; In general, this is a depth limit on ; the search for determinate literals ; Definitions of background predicates null([]). components([a], a, []). components([b, [a], d], b, [[a], d]). components([[a],d], [a], [d]). components([d], d, []). components([e|f], e, f). list([]). list([a]). list([b, [a], d]). list([[a], d]). list([d]). --------------------------------------------------------------------------- ***************************************************************************/
  !(ensure-loaded (library basics))
  !(ensure-loaded (library lists))
  !(ensure-loaded (library between))
  !(ensure-loaded (library math))
  !(use-module 
    (library ordsets) 
    (:: (/ ord-add-element 3)))

;
; Run foil to attempt finding a definition for Goal and then print
;
; out the resulting clauses. e. g. foil(list(_)).

  (= (foil $Goal)  
    (foil $Goal $Clauses)
    (format "~n~nFOUND DEFINITION:~n~n" Nil)
    (portray-clauses $Clauses))

;
; Pretty print a list of clauses.

  (= (portray-clauses Nil)  nl)
  (= (portray-clauses (Cons  $H $T))  
    (portray-clause $H)
    (portray-clauses $T))

;
; run foil and print running time stats.

  (= (foil-time $Goal)  
    (statistics runtime 
      (Cons  $T0 $_))
    (foil $Goal)
    (statistics runtime 
      (Cons  $T1 $_))
    (is $T 
      (- $T1 $T0))
    (format "~nRun Time = ~3d sec.~n" 
      (:: $T)))

;
; Find the positive examples of Goal and construct a set of negative
;
; examples using a closed world hypothesis.

  (= (get-examples $Goal $Pos $Neg)  
    (findall $Goal 
      (with-self  
        (foil-input *) $Goal) $Pos)
    (create-negatives $Pos $Neg))

;
; Clauses is the set of clauses defining Goal found by FOIL. Negative examples
;
; are provided by a closed world assumption.

  (= (foil $Goal $Clauses)  
    (get-examples $Goal $Positives $Negatives)
    (foil-loop $Positives $Goal $Negatives Nil $Clauses))

;
; FOIL provided explicit negative examples.
  (= (foil $Goal $Negatives $Clauses)  
    (findall $Goal 
      (with-self  
        (foil-input *) $Goal) $Positives)
    (foil-loop $Positives $Goal $Negatives Nil $Clauses))

;
; Each iteration constructs a clause. Pos is positive examples left to
;
; be covered, Goal is the concept being defined, Neg is a list of the
;
; negative examples and Clauses0 is the list of clauses found in previous
;
; iterations.

  (= (foil-loop $Pos $Goal $Neg $Clauses0 $Clauses)  
    (det-if-then-else 
      (= $Pos Nil) 
      (= $Clauses $Clauses0) 
      (, 
        (format "~nUncovered positives adding a clause~n~w~n" 
          (:: $Pos)) 
        (extend-clause-loop $Neg $Pos 
          (= $Goal  True) $Clause) 
        (format "~n~nClause Found: ~n" Nil) 
        (portray-clause $Clause) 
        (uncovered-examples $Clause $Pos $Pos1) 
        (foil-loop $Pos1 $Goal $Neg 
          (Cons  $Clause $Clauses0) $Clauses))))

;
; Add antecendents to Clause0 until it covers no negatives. Or no more info
;
; gain is achieved.

  (= (extend-clause-loop $Nxs0 $Pxs0 $Clause0 $Clause)  
    (det-if-then-else 
      (= $Nxs0 Nil) 
      (= $Clause $Clause0) 
      (, 
        (format "~n     Current Clause: ~w ~n     --Specializing~n" 
          (:: $Clause0)) 
        (format "covered negatives~n~w~n" 
          (:: $Nxs0)) 
        (format "covered positives~n~w~n" 
          (:: $Pxs0)) 
        (generate-possible-extensions $Clause0 $Ls) 
        (info-value $Clause0 $Pxs0 $Nxs0 $Info) 
        (best-next-clause $Ls $Nxs0 $Pxs0 $Clause0 $Info 0 $Clause0 $Clause1) 
        (det-if-then-else 
          (== $Clause0 $Clause1) 
          (, 
            (foil-det-lit-bound $DLB) 
            (format "No Improvement -- Trying Determinate Literals~n" Nil) 
            (bounded-determinate-literals $DLB $Ls $Clause0 $Pxs0 $Nxs0 $Ds) 
            (det-if-then-else 
              (= $Ds Nil) 
              (, 
                (format "No Determinate Literals Found~n" Nil) 
                (covered-examples $Clause1 $Nxs0 $Nxs1) 
                (format "WARINING--clause covers negatives~n~w~n" 
                  (:: $Nxs1)) 
                (= $Clause $Clause1)) 
              (, 
                (format "Adding Determinate Literals: ~w~n" 
                  (:: $Ds)) 
                (add-literals $Ds $Clause0 $Clause2) 
                (covered-examples $Clause2 $Nxs0 $Nxs1) 
                (extend-clause-loop $Nxs1 $Pxs0 $Clause2 $Clause)))) 
          (, 
            (covered-examples $Clause1 $Pxs0 $Pxs1) 
            (covered-examples $Clause1 $Nxs0 $Nxs1) 
            (extend-clause-loop $Nxs1 $Pxs1 $Clause1 $Clause))))))
	
;
; Find the clause which is an extension of Clause by a single literal and
;
; provides maximum info gain over the original clause.

  (= (best-next-clause  () $_ $_ $_ $_ $_ $Clause $Clause)  True)
  (= (best-next-clause (Cons  $L $Ls) $Nxs $Pxs $Clause $Info $Gain0 $Best0 $Best)  
    (add-literal $L $Clause $Best1)
    (compute-gain $Nxs $Pxs $Info $Best1 $Gain1)
    (det-if-then-else 
      (> $Gain1 $Gain0) 
      (best-next-clause $Ls $Nxs $Pxs $Clause $Info $Gain1 $Best1 $Best) 
      (det-if-then-else 
        (=:= $Gain1 $Gain0) 
        (, 
          (choose-tie-clause $Best0 $Best1 $Best2) 
          (best-next-clause $Ls $Nxs $Pxs $Clause $Info $Gain0 $Best2 $Best)) 
        (best-next-clause $Ls $Nxs $Pxs $Clause $Info $Gain0 $Best0 $Best))))
; ; format("~w ~4f~n", [L, Gain1]),


  (= (choose-tie-clause (= $A1  $B1) (= $A2  $B2) $C)  
    (variables-in $B1 $V1)
    (length $V1 $N1)
    (variables-in $B2 $V2)
    (length $V2 $N2)
    (det-if-then-else 
      (< $N2 $N1) 
      (= $C 
        (= $A2  $B2)) 
      (= $C 
        (= $A1  $B1))))
	      
;
; For a set of positive and negative examples Pxs and Nxs, compute the
;
; information gain of Clause over a clause which produces a split having
;
; Info, as it's "information value" on these examples.

  (= (compute-gain $Nxs $Pxs $Info $Clause $Gain)  
    (covered-examples $Clause $Pxs $Retained)
    (length $Retained $R)
    (det-if-then-else 
      (=:= $R 0) 
      (= $Gain 0) 
      (, 
        (info-value $Clause $Pxs $Nxs $Info1) 
        (is $Gain 
          (* $R 
            (- $Info $Info1))))))
	
;
; Compute the information matric for the set of positive and negative
;
; tuples which result from applying Clause to the examples Pxs and NXs

  (= (info-value $Clause $Pxs $Nxs $Value)  
    (tuples $Clause $Pxs $Ptuples)
    (length $Ptuples $P)
    (det-if-then-else 
      (=:= $P 0) 
      (= $Value 0) 
      (, 
        (tuples $Clause $Nxs $Ntuples) 
        (length $Ntuples $N) 
        (is $Temp 
          (/ $P 
            (+ $P $N))) 
        (log $Temp $Temp1) 
        (is $Value 
          (* $Temp1 -1.442695)))))

;
; Add a literal to the right end of a clause

  (= (add-literal $L (= $A  $B) (= $A  $B1))  
    (det-if-then-else 
      (= $B True) 
      (= $B1 $L) 
      (= $B1 
        (, $B $L))))


  (= (add-literals $Ls $Clause0 $Clause)  
    (det-if-then-else 
      (= $Ls Nil) 
      (= $Clause $Clause0) 
      (, 
        (= $Ls 
          (Cons  $L $Ls1)) 
        (add-literal $L $Clause0 $Clause1) 
        (add-literals $Ls1 $Clause1 $Clause))))
	

;
; Construct a list representing the set of variables in Term.

  (= (variables-in $A $Vs)  
    (variables-in $A Nil $Vs))
; /* Changed from setof definition to correctly handle dterminate literals code with clauses containing \+ */
	
  (= (variables-in $A $V0 $V)  
    (var $A)
    (set-det)
    (ord-add-element $V0 $A $V))
  (= (variables-in $A $V0 $V)  
    (ground $A)
    (set-det)
    (= $V $V0)) 
  (= (variables-in $Term $V0 $V)  
    (functor $Term $_ $N)
    (variables-in-args $N $Term $V0 $V))


  (= (variables-in-args $N $Term $V0 $V)  
    (det-if-then-else 
      (=:= $N 0) 
      (= $V $V0) 
      (, 
        (arg $N $Term $Arg) 
        (variables-in $Arg $V0 $V1) 
        (is $N1 
          (- $N 1)) 
        (variables-in-args $N1 $Term $V1 $V))))

;
; Given a clause and a list of examples, construct the list of tuples
;
; for the clause. A tuple is the binding of values to variables such
;
; that the clause can be used to prove the example.

  (= (tuples (= $A  $B) $Xs $Tuples)  
    (variables-in 
      (= $A  $B) $Vars)
    (variables-in $A $HeadVars)
    (length $HeadVars $N1)
    (length $Vars $N2)
    (det-if-then-else 
      (=:= $N1 $N2) 
      (findall $Vars 
        (, 
          (member $A $Xs) 
          (not (not (with-self  (foil-input *) $B)))) $Tuples) 
      (findall $Vars 
        (, 
          (member $A $Xs) 
          (with-self  
            (foil-input *) $B)) $Tuples)))
; ; shortcut - only need 1 proof if no new variables.

;
; Xs1 are the examples from Xs that can be proved with the clause

  (= (covered-examples (= $A  $B) $Xs $Xs1)  
    (findall $A 
      (, 
        (member $A $Xs) 
        (not (not (with-self  (foil-input *) $B)))) $Xs1))

;
; Xs1 are the examples from Xs that cannot be proved with the clause.

  (= (uncovered-examples (= $A  $B) $Xs $Xs1)  
    (findall $A 
      (, 
        (member $A $Xs) 
        (not (with-self  (foil-input *) $B))) $Xs1))

;
; ---------------------------------------------------------------------------
;
; Ugly code to generate possible literals


  (= (generate-possible-extensions (= $A  $B) $Extensions)  
    (variables-in 
      (= $A  $B) $OldVars)
    (bagof $L 
      (candidate-literal $A $OldVars $L) $Extensions))


  (= (possible-unification  () () ())  True)
  (= (possible-unification (Cons  $H $T) (Cons  $H $Result) (Cons  $H $Vars))  
    (possible-unification $T $Result $Vars))
  (= (possible-unification (Cons  $H $T) (Cons  $H $T1) $Vs)  
    (possible-unification $T $T1 $Vs)
    (member $V $Vs)
    (= $H $V))


  (= (list-of-n-from $Elements $N $List0 $List)  
    (det-if-then-else 
      (is $N 0) 
      (= $List $List0) 
      (, 
        (is $N1 
          (- $N 1)) 
        (member $E $Elements) 
        (list-of-n-from $Elements $N1 
          (Cons  $E $List0) $List))))


  (= (possible-new-vars  true $_ ())  True)
  (= (possible-new-vars False $N $L)  
    (length $L $N))
  (= (possible-new-vars False $N $L)  
    (> $N 0)
    (is $N1 
      (- $N 1))
    (possible-new-vars False $N1 $L))


  (= (bind-vars $Lit $Vars $Index)  
    (det-if-then-else 
      (= $Vars Nil) True 
      (, 
        (= $Vars 
          (Cons  $H $T)) 
        (arg $Index $Lit $H) 
        (is $Index1 
          (+ $Index 1)) 
        (bind-vars $Lit $T $Index1))))


  (= (recursion-check $G $Pred $Arity $Flag)  
    (det-if-then-else 
      (functor $G $Pred $Arity) 
      (= $Flag True) 
      (= $Flag False)))


  (= (candidate-literal $Goal $OldVars $Lit)  
    (foil-predicates $Preds)
    (member 
      (/ $Pred $Arity) $Preds)
    (functor $L $Pred $Arity)
    (recursion-check $Goal $Pred $Arity $RecursionFlag)
    (is $MaxNewVars 
      (- $Arity 1))
    (possible-new-vars $RecursionFlag $MaxNewVars $NewVars)
    (length $NewVars $NewVarPositions)
    (is $OldVarPositions 
      (- $Arity $NewVarPositions))
    (list-of-n-from $OldVars $OldVarPositions Nil $OldVarSeq)
    (recursion-safe $RecursionFlag $Goal $OldVarSeq)
    (possible-unification $NewVars $NewVarSeq $_)
    (subseq $VarSeq $OldVarSeq $NewVarSeq)
    (bind-vars $L $VarSeq 1)
    (or 
      (= $Lit $L) 
      (, 
        (foil-use-negations True) 
        (= $Lit 
          (not $L)))))


  (= (recursion-safe $RecursionFlag $Goal $OldVarSeq)  
    (det-if-then-else 
      (= $RecursionFlag True) 
      (not (, (numbervars $Goal 0 $_) (ground $OldVarSeq))) True))
;
; ---------------------------------------------------------------------------
;
; Closed World Assumption


  (= (create-universe $Universe)  
    (setof $Term 
      (term-of-ext-def $Term) $Universe))


  (= (term-of-ext-def $Term)  
    ( (foil-predicates $PredSpecs) 
      (member 
        (/ $Pred $Arity) $PredSpecs) 
      (functor $Goal $Pred $Arity) 
      (with-self  
        (foil-input *) $Goal) 
      (between 1 $Arity $ArgPos) 
      (arg $ArgPos $Goal $Term)))


  (= (create-negatives (Cons  $P $Ps) $Negatives)  
    (functor $P $F $N)
    (functor $Template $F $N)
    (create-universe $Universe)
    (setof $Template 
      (, 
        (arguments-are-members $Template $N $Universe) 
        (not (member $Template (Cons  $P $Ps)))) $Negatives))
	      

  (= (arguments-are-members $Term $N $Universe)  
    (det-if-then-else 
      (> $N 0) 
      (, 
        (arg $N $Term $Arg) 
        (member $Arg $Universe) 
        (is $N1 
          (- $N 1)) 
        (arguments-are-members $Term $N1 $Universe)) True))
	
;
; ---------------------------------------------------------------------------
;
; Determinate Literals


  (= (determinate $L $Vars $PTuples $NTuples)  
    (binds-new-var $L $Vars)
    (determ-cover $PTuples $L $Vars)
    (determ-partial-cover $NTuples $L $Vars))
; /* determinate(+Lit, +Vars, +PTuples, +NTuples) -- holds if Lit is a determinate literal wrt the bindings for Vars as represented in PTuples and NTuples. */


  (= (binds-new-var (not $_) $_)  
    ( (set-det) fail))
  (= (binds-new-var $L $Vars)  
    (variables-in $L $LVars)
    (member $V $LVars)
    (not (contains-var $V $Vars))
    (set-det))


  (= (determ-cover  () $_ $_)  True)
  (= (determ-cover (Cons  $T $Ts) $Lit $Vars)  
    (findall $Lit 
      (, 
        (= $Vars $T) 
        (with-self  
          (foil-input *) $Lit)) 
      (:: $_))
    (determ-cover $Ts $Lit $Vars))


  (= (determ-partial-cover  () $_ $_)  True)
  (= (determ-partial-cover (Cons  $T $Ts) $Lit $Vars)  
    (findall $Lit 
      (, 
        (= $Vars $T) 
        (with-self  
          (foil-input *) $Lit)) $Xs)
    (or 
      (= $Xs Nil) 
      (= $Xs 
        (:: $_)))
    (determ-partial-cover $Ts $Lit $Vars))


  (= (determinate-literals1 $Cands $Body $Vars $PTuples $NTuples $DLits)  
    (bagof $X 
      (, 
        (member $X $Cands) 
        (determinate $X $Vars $PTuples $NTuples) 
        (not (, (numbervars $Vars 0 $_) (ante-memberchk $X $Body)))) $DLits))


  (= (determinate-literals $Cands $Clause $Pxs $Nxs $DLits)  
    (variables-in $Clause $Vars)
    (tuples $Clause $Pxs $PTuples)
    (tuples $Clause $Nxs $NTuples)
    (= $Clause 
      (= $_  $Body))
    (determinate-literals1 $Cands $Body $Vars $PTuples $NTuples $DLits))


  (= (bounded-determinate-literals 0 $_ $_ $_ $_ Nil)  
    (set-det))
  (= (bounded-determinate-literals $Bound $Cands (= $A  $B) $Pxs $Nxs $DLits)  
    (determinate-literals $Cands 
      (= $A  $B) $Pxs $Nxs $DLits0)
    (reachable-antes $Bound $A $DLits0 $DLits))


  (= (ante-memberchk $A $A)  
    (set-det))
  (= (ante-memberchk $A (, $B $C))  
    (det-if-then-else 
      (ante-memberchk $A $B) True 
      (ante-memberchk $A $C)))


  (= (reachable-antes $Bound $H $Cands $Antes)  
    (variables-in $H $Vs)
    (expand-by-var-chain $Bound $Cands $Vs Nil $Antes))
; /* reachable_antes(+Bound, +H, +Cands, -Antes) -- Antes is the list of literals from Cands which can be "connected" to H by some chain of variables of length <= Bound. */


  (= (expand-by-var-chain $Bound $Cands $Vars $As0 $As)  
    (det-if-then-else 
      (=:= $Bound 0) 
      (= $As $As0) 
      (, 
        (partition-on-vars $Cands $Vars $Haves $Havenots) 
        (det-if-then-else 
          (= $Haves Nil) 
          (= $As $As0) 
          (, 
            (append $As0 $Haves $As1) 
            (variables-in $As1 $Vars1) 
            (is $Bound1 
              (- $Bound 1)) 
            (expand-by-var-chain $Bound1 $Havenots $Vars1 $As1 $As))))))


  (= (partition-on-vars  () $_ () ())  True)
  (= (partition-on-vars (Cons  $C $Cs) $Vars $Hs $Hnots)  
    (det-if-then-else 
      (, 
        (member $V $Vars) 
        (contains-var $V $C)) 
      (, 
        (= $Hs 
          (Cons  $C $Hs1)) 
        (= $Hnots $Hnots1)) 
      (, 
        (= $Hs $Hs1) 
        (= $Hnots 
          (Cons  $C $Hnots1))))
    (partition-on-vars $Cs $Vars $Hs1 $Hnots1))



