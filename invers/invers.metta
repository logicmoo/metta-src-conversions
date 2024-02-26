;                                           (convert_to_metta_file  invers $_141654 invers/invers.pl invers/invers.metta)

  !(dynamic (/ flat 1))
; /******************************************************************/
; /* INVERS.PL Last Modification: Fri Jan 14 19:24:41 1994 */
; /* Two versions of the two main operators for inverse resolution. */
; /******************************************************************/
; ; ; Copyright (c) 1989,1990 Thomas Hoppe ; ; This program is free software; you can redistribute it and/or ; modify it under the terms of the GNU General Public License ; Version 1 as published by the Free Software Foundation. ; ; This program is distributed in the hope that it will be useful, ; but WITHOUT ANY WARRANTY; without even the implied warranty of ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ; GNU General Public License for more details. ; ; You should have received a copy of the GNU General Public ; License along with this program; if not, write to the Free ; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, ; USA. ;
; /******************************************************************/
; /* impl. by : Thomas Hoppe */
; /* Mommsenstr. 50 */
; /* D-10629 Berlin */
; /* F.R.G. */
; /* E-Mail: hoppet@cs.tu-berlin.de */
; /* 1989/90 */
; /* */
; /* reference : Muggleton, S., Buntine, W., Machine Invention */
; /* of First-order Predicates by Inverting Resolu- */
; /* tion, Proceedings of the International Workshop */
; /* on Machine Learning, Ann Arbor, Morgan Kaufmann */
; /* 1988. */
; /* */
; /* Rouveirol, C., Puget, J.-F., A Simple Solution */
; /* For Inverting Resolution, in: K. Morik, Procee- */
; /* dings of the 4th European Working Session on */
; /* Machine Learning, Montpellier, Pitman Publishing*/
; /* 1989. */
; /* */
; /* call : see invers_1.pro */
; /* */
; /* of First-order Predicates by Inverting Resolu- */
; /* tion, Proceedings of the International Workshop */
; /* on Machine Learning, Ann Arbor, Morgan Kaufmann */
; /* 1988. */
; /* */
; /* Rouveirol, C., Puget, J.-F., A Simple Solution */
; /* For Inverting Resolution, in: K. Morik, Procee- */
; /* dings of the 4th European Working Session on */
; /* Machine Learning, Montpellier, Pitman Publishing*/
; /* 1989. */
; /* */
; /* call : see invers_1.pro */
; /* */
; /******************************************************************/
; /* This file contains two versions for the main operators of */
; /* inverse resolution, absorption and intra-construction, as */
; /* they are described by Stephen Muggleton and Wray Buntine. */
; /* */
; /* The versions numbered 1 are working on definite Horn-clauses */
; /* without function symbols. The versions numbered 2 are working */
; /* on definite Horn-clauses with function symbols. The basic */
; /* difference is made up by the predicates 'flatten' and */
; /* 'unflatten', and is described in Rouveirol & Puget. */
; /* */
; /* The main predicates 'absorption1', 'absorption2', 'intra- */
; /* construction1' and 'intra-construction2' use some predicates */
; /* importet from the library entry 'logic', which should be */
; /* consulted befor runing the examples. */
; /* */
; /* Instead of using a generalisation algorithm based on the */
; /* 'dropping condition rule' as described by Rouveirol & Puget */
; /* I have developed a 'fast' generalisation routine, which in the*/
; /* end gives the same results as 'dropping condition'. Its nice */
; /* advantage is that it delivers the first generalisation in */
; /* linear time, although this is an exponential problem. Of */
; /* course in general, exponentiallity cannot be avoided. */
; /* But through delaying it, we can save some time in easy cases. */
; /******************************************************************/
; ; TH Tue Feb 23 12:38:06 1993 ; - added missing definition of element_v ; - introduced dynamic/2 (below) for making system dependency ; more obvious ; TH Fri Mar 5 11:40:21 1993 ; - removed some findall/3 calls, since it's not built-in in ; C-MeTTa, introduced instead findbag, which is bagof with ; reversed list order ; - numbervars is not built-in in C-MeTTa, thus I included ; instvars, although the output is now ugly, it works ; TH Sun May 30 14:20:37 1993 ; - made some minor modifications
; /******************************************************************/
; /* Some SWI-, C-, M- and YAP-MeTTa dependent declarations. */
; /******************************************************************/
  !(dynamic (/ internal 1))

;
; Dependent on your MeTTa dialect (whether it requires dynamic
;
; declarations or not, you may need to substitute the Body of the
;
; following clause with `true'.


  (= (dynamic $N $A)  
    (dynamic (/ $N $A)))
; ; true.

;
; C-MeTTa doesn't know numbervars (quite strange, isn't it?)
;
; Thus it needs to be emulated (see comment at the end) !

;
; numbervars(T,Min,Max) :-
;
; instvars(T,Min,Max).

;
; SWI-MeTTa nows only of numbervars/4, but that's no problem with
;
; the following clause


  (= (numbervars $T $Min $Max)  
    (numbervars $T %VAR $Min $Max))


  !(:: (logic *))


  (= (absorption1 $Clause $Resolvent $InducedClause)  
    (split $Clause $ClauseHead $ClauseBody)
    (split $Resolvent $ResolventHead $ResolventBody)
    (step-1 $ClauseBody $ResolventBody $RestClauseBody $Subst_2)
    (subst 
      (:: $ClauseHead) $Subst_2 
      (:: $NewClauseHead))
    (step-2 $NewClauseHead $RestClauseBody $IntermediaryBody)
    (join $ResolventHead $IntermediaryBody $IntermediaryClause)
    (copy $IntermediaryClause $InducedClause))
; /******************************************************************/
; /* */
; /* call : absorption1(+Clause,+Resolvent,-InducedClause) */
; /* */
; /* arguments : Clause = definite clause */
; /* Resolvent = definite clause */
; /* InducedClause = induced definite clause */
; /* */
; /******************************************************************/
; /* works on definite clauses without function-symbols */
; /******************************************************************/
        

  (= (split (= $Head  True) $Head Nil)  
    (set-det))
  (= (split (= $Head  $Body) $Head $BodyList)  
    (split $Body $_ $BodyList)
    (set-det))
  (= (split (, $Prem $Prems) $_ (Cons  $Prem $RestPrems))  
    (split $Prems $_ $RestPrems)
    (set-det))
  (= (split  $Prem $_ ($Prem))  True)


  (= (join $Head $PremList (= $Head  $Prems))  
    (join $PremList $Prems))

  (= (join (:: $Prem) $Prem)  
    (set-det))
  (= (join (Cons  $Prem $Prems) (, $Prem $RestPrems))  
    (join $Prems $RestPrems))


  (= (step-1 $ClauseBody $ResolventBody $ResolventRest $Subst)  
    (choose-common-parts $ResolventBody $ClauseBody $Beta1 $Beta2)
    (not (not (subset $Beta1 $ClauseBody)))
    (substitution $ClauseBody $Beta1 $Subst)
    (subtract-v $Beta1 $ResolventBody $ResolventRest))


  (= (choose-common-parts $List1 $List2 $Sublist1 $Sublist2)  
    (findbag $MGT 
      (, 
        (member $Pred $List1) 
        (functor $Pred $N $A) 
        (functor $MGT $N $A)) $MGTS1)
    (findbag $MGT 
      (, 
        (member $Pred $List2) 
        (functor $Pred $N $A) 
        (functor $MGT $N $A)) $MGTS2)
    (set-det)
    (dropping-condition-intersection $MGTS1 $MGTS2 $MGTS)
    (copy $MGTS $Sublist1)
    (copy $MGTS $Sublist2)
    (subset $Sublist1 $List1)
    (subset $Sublist2 $List2))
; /********************************************************************/
; /* Determination of the most special subsets of two non-ground */
; /* predicate sets, on backtracking it delivers the next more general*/
; /* subsets. The nice property, of this routine is, that the first */
; /* common_subpart is found in linear time, although the determina- */
; /* tion of all common_subparts needs exponential time. */
; /********************************************************************/


  (= (dropping_condition_intersection  () $_ ())  True)
  (= (dropping-condition-intersection (Cons  $X $R) $Y (Cons  $X $Z))  
    (member $X1 $Y)
    (not (not (= $X $X1)))
    (set-det)
    (dropping-condition-delete $X $Y $Y1)
    (dropping-condition-intersection $R $Y1 $Z))
  (= (dropping-condition-intersection (Cons  $X $R) $Y $Z)  
    (dropping-condition-intersection $R $Y $Z))
     

  (= (dropping-condition-delete $X (Cons  $Y $Ys) $Ys)  
    (not (not (= $X $Y)))
    (set-det))
  (= (dropping-condition-delete $X (Cons  $Y $Ys) (Cons  $Y $Zs))  
    (dropping-condition-delete $X $Ys $Zs)) 


  (= (step-2 $ClauseHead $RestClauseBody $IntermediaryBody)  
    (union-v 
      (:: $ClauseHead) $RestClauseBody $IntermediaryBody))
; ; Union_v helps us to avoid some redundant premisses
    

  (= (absorption2 $Clause $Resolvent $InducedClause)  
    (flatten $Clause $ClauseHead $ClauseBody)
    (flatten $Resolvent $ResolventHead $ResolventBody)
    (step-1 $ClauseBody $ResolventBody $RestClauseBody $Subst_2)
    (subst 
      (:: $ClauseHead) $Subst_2 
      (:: $NewClauseHead))
    (step-2 $NewClauseHead $RestClauseBody $IntermediaryBody)
    (unflatten 
      (:: $ResolventHead) $IntermediaryBody $IntermediaryClause)
    (copy $IntermediaryClause $InducedClause))
; /******************************************************************/
; /* */
; /* call : absorption2(+Clause,+Resolvent,-InducedClause) */
; /* */
; /* arguments : Clause = definite clause */
; /* Resolvent = definite clause */
; /* InducedClause = induced definite clause */
; /* */
; /******************************************************************/
; /* works on definite clauses with function-symbols */
; /******************************************************************/


  (= (flatten (= $Conclusion  $Conjunction) $NewConclusion $NewPremisses)  
    (flatten $Conclusion 
      (:: $NewConclusion) $FunctionPrems1 $Dictionary)
    (flatten $Conjunction $Prems $FunctionPrems2 $Dictionary)
    (union-v $FunctionPrems1 $FunctionPrems2 $FunctionPrems)
    (union-v $FunctionPrems $Prems $NewPremisses)
    (set-det))
; /********************************************************************/
; /* Representation change procedures for inverse resolution. */
; /* The basic idea behind these procedures is to re-represent n-ary */
; /* function symbols by n+1-ary predicate symbols, where the */
; /* additional argument delivers the value of the function. A */
; /* discussion of this can be found in Rouveirol & Puget's paper. */
; /********************************************************************/

  (= (flatten (, $Prem $Prems) $NewPremisses $FunctionPrems $Dictionary)  
    (flatten $Prem $NewPrem $FunctionPrems1 $Dictionary)
    (flatten $Prems $NewPrems $FunctionPrems2 $Dictionary)
    (union-v $NewPrem $NewPrems $NewPremisses)
    (union-v $FunctionPrems1 $FunctionPrems2 $FunctionPrems)
    (set-det))

  (= (flatten  () () () $_)  True)
  (= (flatten (Cons  $Const $Terms) (Cons  $Var $NewVars) $ResultPrems $Dictionary)  
    (atomic $Const)
    (lookup $Const $Dictionary $Var)
    (conc $Const -p $NewName)
    (flat-assertion $NewName $Const)
    (=.. $Pred 
      (:: $NewName $Var))
    (flatten $Terms $NewVars $NewPrems $Dictionary)
    (set-det)
    (union-v 
      (:: $Pred) $NewPrems $ResultPrems))
  (= (flatten (Cons  $Term $Terms) (Cons  $Term $NewVars) $NewPrems $Dictionary)  
    (var $Term)
    (set-det)
    (flatten $Terms $NewVars $NewPrems $Dictionary))
  (= (flatten (Cons  $Term $Terms) (Cons  $NewVar $NewVars) $NewPrems $Dictionary)  
    (=.. $Term 
      (Cons  $N $Args))
    (conc $N -p $NN)
    (flatten $Args $NewTerms $Prems $Dictionary)
    (append $NewTerms 
      (:: $NewVar) $NewArgs)
    (=.. $NewTerm 
      (Cons  $NN $NewArgs))
    (flat-assertion $NN $NewTerms $N $NewTerms)
    (flatten $Terms $NewVars $NP $Dictionary)
    (set-det)
    (union-v 
      (:: $NewTerm) $NP $ERG)
    (union-v $Prems $ERG $NewPrems))

  (= (flatten True Nil Nil $_)  
    (set-det))
  (= (flatten False Nil Nil $_)  
    (set-det))
  (= (flatten (not $Pred) (:: (not $NewPred)) $NewPrems $Dictionary)  
    (set-det)
    (flatten $Pred 
      (:: $NewPred) $NewPrems $Dictionary))
  (= (flatten $Pred (:: $Pred) Nil $Dictionary)  
    (atomic $Pred)
    (set-det))
  (= (flatten $Pred (:: $NewPred) $NewPrems $Dictionary)  
    (set-det)
    (=.. $Pred 
      (Cons  $N $Args))
    (flatten $Args $NewArgs $NewPrems $Dictionary)
    (=.. $NewPred 
      (Cons  $N $NewArgs)))


  (= (flat-assertion $Name $Term)  
    ( (=.. $NewRelation 
        (:: $Name $Term)) 
      (copy $NewRelation $NR) 
      (skolemize 
        (:: $NR) 0 $_) 
      (==  
        (= 
          (flat  $NR) true) 
        (get-atoms  &self)) 
      (==  
        (= 
          (flat  $NewRelation) true) 
        (get-atoms  &self)) 
      (set-det)))
  (= (flat-assertion $Name $Term)  
    ( (=.. $NewRelation 
        (:: $Name $Term)) 
      (add-is-symbol  &self 
        (flat  $NewRelation)) 
      (set-det)))

  (= (flat-assertion $Name1 $Args $Name2 $Terms)  
    ( (=.. $OldTerm 
        (Cons  $Name2 $Terms)) 
      (append $Args 
        (:: $OldTerm) $NewArgs) 
      (=.. $NewRelation 
        (Cons  $Name1 $NewArgs)) 
      (copy $NewRelation $NR) 
      (skolemize 
        (:: $NR) 0 $_) 
      (==  
        (= 
          (flat  $NR) true) 
        (get-atoms  &self)) 
      (==  
        (= 
          (flat  $NewRelation) true) 
        (get-atoms  &self)) 
      (set-det)))
  (= (flat-assertion $Name1 $Args $Name2 $Terms)  
    ( (=.. $OldTerm 
        (Cons  $Name2 $Terms)) 
      (append $Args 
        (:: $OldTerm) $NewArgs) 
      (=.. $NewRelation 
        (Cons  $Name1 $NewArgs)) 
      (add-is-symbol  &self 
        (flat  $NewRelation))))


  (= (unflatten (:: $Head) $BodyList (= $Head  $Body))  
    (unflatten $BodyList $Body)
    (set-det))
    
  (= (unflatten  () true)  True)
  (= (unflatten (:: $Prem) $Prem)  
    (functor $Prem $N $A)
    (functor $P $N $A)
    (not (==  (= (flat  $P) true) (get-atoms  &self)))
    (set-det)
    (not (==  (= (flat  $Prem) true) (get-atoms  &self)))
    (set-det))
  (= (unflatten (Cons  $Prem $Prems) (, $Prem $RestPrems))  
    (functor $Prem $N $A)
    (functor $P $N $A)
    (not (==  (= (flat  $P) true) (get-atoms  &self)))
    (set-det)
    (not (==  (= (flat  $Prem) true) (get-atoms  &self)))
    (unflatten $Prems $RestPrems)
    (set-det))
  (= (unflatten (Cons  $Prem $Prems) $RestPrems)  
    ( (==  
        (= 
          (flat  $Prem) true) 
        (get-atoms  &self)) (unflatten $Prems $RestPrems)))    
	    

  (= (intra-construction1 $Resolvent1 $Resolvent2 $InducedRules)  
    (split $Resolvent1 $Resolvent1Head $Resolvent1Body)
    (split $Resolvent2 $Resolvent2Head $Resolvent2Body)
    (step-1 $Resolvent1Head $Resolvent1Body $Resolvent2Head $Resolvent2Body $ClauseHead $Alpha $Resolvent1BodyRest $Resolvent2BodyRest $Subst1 $Subst2)
    (step-2 $ClauseHead $Alpha $Resolvent1BodyRest $Subst1 $Resolvent2BodyRest $Subst2 $Vars)
    (step-3-1 $ClauseHead $Vars $Alpha $Resolvent1BodyRest $Subst1 $Resolvent2BodyRest $Subst2 $InducedRules))
; /******************************************************************/
; /* */
; /* call : intra_construction1(+Resolvent1,+Resolvent2, */
; /* -InducedRules) */
; /* */
; /* arguments : Resolvent1 = definite clause */
; /* Resolvent2 = definite clause */
; /* InducedRules = list of three induced definite */
; /* clauses */
; /* */
; /******************************************************************/
; /* works on definite clauses without function-symbols */
; /******************************************************************/


  (= (step-1 $Resolvent1Head $Resolvent1Body $Resolvent2Head $Resolvent2Body (:: $ClauseHead) $Alpha $Resolvent1BodyRest $Resolvent2BodyRest $Subst1 $Subst2)  
    (choose-common-parts $Resolvent1Body $Resolvent2Body $AlphaTheta1 $AlphaTheta2)
    (length $AlphaTheta1 $X)
    (length $AlphaTheta2 $X)
    (> $X 0)
    (subtract-v $AlphaTheta1 $Resolvent1Body $Resolvent1BodyRest)
    (subtract-v $AlphaTheta2 $Resolvent2Body $Resolvent2BodyRest)
    (lgg 
      (:: $Resolvent1Head $AlphaTheta1) 
      (:: $Resolvent2Head $AlphaTheta2) 
      (:: $ClauseHead $Alpha))
    (substitution 
      (:: $ClauseHead $Alpha) 
      (:: $Resolvent1Head $AlphaTheta1) $Subst1)
    (substitution 
      (:: $ClauseHead $Alpha) 
      (:: $Resolvent2Head $AlphaTheta2) $Subst2))
; ; if AlphaTheta is empty we gain nothing, thus
; ; lgg doesn't take the order of literals into account, initially it was ; only designed for terms, that doesn't matter because choose_common_subpart ; ensures a common predicate order through copying the MGT and instantiating ; afterwards.
; ; At least we determine the substitutions theta 1 and theta 2


  (= (step-2 $ClauseHead $Alpha $Resolvent1BodyRest $Subst1 $Resolvent2BodyRest $Subst2 $Vars)  
    (varlist $Resolvent1BodyRest $RBR1Vars)
    (varlist $Resolvent2BodyRest $RBR2Vars)
    (varlist $ClauseHead $CHVars)
    (varlist $Alpha $AVars)
    (inv-subst $RBR1 $Subst1 $RBR1Vars)
    (inv-subst $RBR2 $Subst2 $RBR2Vars)
    (or 
      (= $CHVars Nil) 
      (, 
        (intersection-v $CHVars $AVars $XI) 
        (not (== $XI Nil))))
    (union-v $CHVars $AVars $Xs)
    (union-v $RBR1 $RBR2 $RBRU)
    (intersection-v $RBRU $Xs $Vars)
    (set-det))
; ; The head and the generalized body part must have (at least) ; a variable in common
; ; This is different as in the paper of Rouveirol/Puget. If we would ; use their method, we couldn't even obtain their results, but ; with this it works. This error was confirmed by Celine Rouveirol


  (= (step-3-1 (:: $ClauseHead) $Vars $Alpha $Resolvent1BodyRest $Subst1 $Resolvent2BodyRest $Subst2 (:: $R1 $R2 $R3))  
    (gensym $Name P)
    (=.. $X 
      (Cons  $Name $Vars))
    (subst 
      (:: $X) $Subst1 
      (:: $X1))
    (subst 
      (:: $X) $Subst2 
      (:: $X2))
    (join $X1 $Resolvent1BodyRest $QR1)
    (join $X2 $Resolvent2BodyRest $QR2)
    (copy $QR1 $NQR1)
    (copy $QR2 $NQR2)
    (numbervars $NQR1 0 $_)
    (numbervars $NQR2 0 $_)
    (nl)
    (write ' Predicate invention')
    (nl)
    (nl)
    (tab 3)
    (write $NQR1)
    (nl)
    (tab 3)
    (write $NQR2)
    (nl)
    (nl)
    (write ' Has this predicate a meaning (y/n)? ')
    (read $Answer)
    (nl)
    (or 
      (, 
        (= $Answer n) 
        (set-det) 
        (fail)) 
      (, 
        (= $Answer y) 
        (set-det)))
    (write ' How shall I name the predicate ? ')
    (read $NewName)
    (nl)
    (=.. $NP 
      (Cons  $NewName $Vars))
    (subst 
      (:: $NP) $Subst1 
      (:: $NP1))
    (subst 
      (:: $NP) $Subst2 
      (:: $NP2))
    (join $NP1 $Resolvent1BodyRest $R1)
    (join $NP2 $Resolvent2BodyRest $R2)
    (append 
      (:: $NP) $Alpha $ClauseBody)
    (join $ClauseHead $ClauseBody $R3)
    (set-det))
    

  (= (intra-construction2 $Resolvent1 $Resolvent2 $InducedClauses)  
    (flatten $Resolvent1 $Resolvent1Head $Resolvent1Body)
    (flatten $Resolvent2 $Resolvent2Head $Resolvent2Body)
    (step-1 $Resolvent1Head $Resolvent1Body $Resolvent2Head $Resolvent2Body $ClauseHead $Alpha $Resolvent1BodyRest $Resolvent2BodyRest $Subst1 $Subst2)
    (step-2 $ClauseHead $Alpha $Resolvent1BodyRest $Subst1 $Resolvent2BodyRest $Subst2 $Vars)
    (step-3-2 $ClauseHead $Vars $Alpha $Resolvent1BodyRest $Subst1 $Resolvent2BodyRest $Subst2 $InducedClauses))
; /******************************************************************/
; /* */
; /* call : intra_construction2(+Resolvent1,+Resolvent2, */
; /* -InducedRules) */
; /* */
; /* arguments : Resolvent1 = definite clause */
; /* Resolvent2 = definite clause */
; /* InducedRules = list of three induced definite */
; /* clauses */
; /* */
; /******************************************************************/
; /* works on definite clauses with function-symbols */
; /******************************************************************/
    

  (= (step-3-2 $ClauseHead $Vars $Alpha $Resolvent1BodyRest $Subst1 $Resolvent2BodyRest $Subst2 (:: $R1 $R2 $R3))  
    (gensym $Name P)
    (=.. $X 
      (Cons  $Name $Vars))
    (subst 
      (:: $X) $Subst1 $X1)
    (subst 
      (:: $X) $Subst2 $X2)
    (unflatten $X1 $Resolvent1BodyRest $QR1)
    (unflatten $X2 $Resolvent2BodyRest $QR2)
    (copy $QR1 $NQR1)
    (copy $QR2 $NQR2)
    (numbervars $NQR1 0 $_)
    (numbervars $NQR2 0 $_)
    (nl)
    (write ' Predicate invention')
    (nl)
    (nl)
    (tab 3)
    (write $NQR1)
    (nl)
    (tab 3)
    (write $NQR2)
    (nl)
    (nl)
    (write ' Has this predicate a meaning (y/n)? ')
    (read $Answer)
    (nl)
    (or 
      (, 
        (= $Answer n) 
        (set-det) 
        (fail)) 
      (, 
        (= $Answer y) 
        (set-det)))
    (write ' How shall I name the predicate ? ')
    (read $NewName)
    (nl)
    (=.. $NP 
      (Cons  $NewName $Vars))
    (subst 
      (:: $NP) $Subst1 $NP1)
    (subst 
      (:: $NP) $Subst2 $NP2)
    (unflatten $NP1 $Resolvent1BodyRest $R1)
    (unflatten $NP2 $Resolvent2BodyRest $R2)
    (append 
      (:: $NP) $Alpha $ClauseBody)
    (unflatten $ClauseHead $ClauseBody $R3)
    (set-det))
    

  (= (subst  () $_ ())  True)
; /********************************************************************/
; /* Some predicates for substituting and for inverse substituting */
; /* terms according to a given Subst. */
; /********************************************************************/
  (= (subst (Cons  $X $Xs) $Subst (Cons  $Y $Ys))  
    (member 
      (/ $U $V) $Subst)
    (== $X $U)
    (= $Y $V)
    (subst $Xs $Subst $Ys)
    (set-det))
  (= (subst (Cons  $Prem $Prems) $Subst (Cons  $NewPrem $NewPrems))  
    (=.. $Prem 
      (Cons  $F $Args))
    (subst $Args $Subst $NewArgs)
    (=.. $NewPrem 
      (Cons  $F $NewArgs))
    (subst $Prems $Subst $NewPrems)
    (set-det))
  (= (subst (Cons  $X $Xs) $Subst (Cons  $X $Ys))  
    (subst $Xs $Subst $Ys))


  (= (inv_subst  () $_ ())  True)
  (= (inv-subst (Cons  $X $Xs) $Subst (Cons  $Y $Ys))  
    (member 
      (/ $U $V) $Subst)
    (== $Y $V)
    (= $X $U)
    (inv-subst $Xs $Subst $Ys)
    (set-det))
  (= (inv-subst (Cons  $Prem $Prems) $Subst (Cons  $NewPrem $NewPrems))  
    (=.. $NewPrem 
      (Cons  $F $NewArgs))
    (inv-subst $Args $Subst $NewArgs)
    (=.. $Prem 
      (Cons  $F $Args))
    (inv-subst $Prems $Subst $NewPrems)
    (set-det))
  (= (inv-subst (Cons  $X $Xs) $Subst (Cons  $X $Ys))  
    (inv-subst $Xs $Subst $Ys))


  (= (union_v  () $X $X)  True)
; /******************************************************************/
; /* Some set theoretical predicates, variables are not unified */
; /******************************************************************/
  (= (union-v (Cons  $X $R) $Y $Z)  
    (member $U $Y)
    (== $U $X)
    (set-det)
    (union-v $R $Y $Z))
  (= (union-v (Cons  $X $R) $Y (Cons  $X $Z))  
    (union-v $R $Y $Z))


  (= (intersection_v  () $_ ())  True)
  (= (intersection-v (Cons  $X $R) $Y (Cons  $X $Z))  
    (member $U $Y)
    (== $U $X)
    (set-det)
    (intersection-v $R $Y $Z))
  (= (intersection-v (Cons  $X $R) $Y $Z)  
    (intersection-v $R $Y $Z))


  (= (subset  () ())  True)
  (= (subset (Cons  $X $Subset) (Cons  $X $List))  
    (subset $Subset $List))
  (= (subset $Subset (Cons  $_ $List))  
    (subset $Subset $List))
    

  (= (subtract-v (Cons  $X $Xs) $Ys $Zs)  
    (member $Y $Ys)
    (== $X $Y)
    (delete-v $X $Ys $Y1s)
    (subtract-v $Xs $Y1s $Zs)
    (set-det))
  (= (subtract-v (Cons  $X $Xs) $Ys $Zs)  
    (subtract-v $Xs $Ys $Zs))
  (= (subtract_v  () $Xs $Xs)  True)


  (= (delete-v $X (Cons  $Y $Ys) $Ys)  
    (== $X $Y)
    (set-det))
  (= (delete-v $X (Cons  $Y $Ys) (Cons  $Y $Zs))  
    (delete-v $X $Ys $Zs)) 
    

  (= (count $VAR $X)  
    ( (dynamic $VAR 1) 
      (=.. $P1 
        (:: $VAR $N)) 
      (remove-is-symbol  &self $P1) 
      (is $X 
        (+ $N 1)) 
      (=.. $P2 
        (:: $VAR $X)) 
      (add-is-symbol  &self $P2) 
      (set-det)))
; /******************************************************************/
; /* Creation of unique identifiers */
; /******************************************************************/
  (= (count $VAR 1)  
    ( (dynamic $VAR 1) 
      (=.. $P 
        (:: $VAR 1)) 
      (add-is-symbol  &self $P)))


  (= (gensym $SYM $N)  
    (count $N $X)
    (conc $N $X $SYM))


  (= (copy $A $B)  
    (or 
      (, 
        (add-is-symbol  &self 
          (internal  $A)) 
        (remove-is-symbol  &self 
          (internal  $B)) 
        (set-det)) 
      (, 
        (remove-is-symbol  &self 
          (internal  $_)) 
        (fail))))
; /******************************************************************/
; /* Making a copy of the variables occuring in a MeTTa term */
; /******************************************************************/


  (= (lookup $Key (d $Key $X $Left $Right) $Value)  
    (set-det)
    (= $X $Value))
; /******************************************************************/
; /* Construction and retrieval of an dictionary */
; /******************************************************************/
  (= (lookup $Key (d $Key1 $X $Left $Right) $Value)  
    (@< $Key $Key1)
    (lookup $Key $Left $Value))
  (= (lookup $Key (d $Key1 $X $Left $Right) $Value)  
    (@> $Key $Key1)
    (lookup $Key $Right $Value))


  (= (conc $STR1 $STR2 $STR3)  
    (nonvar $STR1)
    (nonvar $STR2)
    (name $STR1 $S1)
    (name $STR2 $S2)
    (append $S1 $S2 $S3)
    (name $STR3 $S3))
; /******************************************************************/
; /* Concatenation of atoms */
; /******************************************************************/
  (= (conc $STR1 $STR2 $STR3)  
    (nonvar $STR1)
    (nonvar $STR3)
    (name $STR1 $S1)
    (name $STR3 $S3)
    (append $S1 $S2 $S3)
    (name $STR2 $S2))
  (= (conc $STR1 $STR2 $STR3)  
    (nonvar $STR2)
    (nonvar $STR3)
    (name $STR2 $S2)
    (name $STR3 $S3)
    (append $S1 $S2 $S3)
    (name $STR1 $S1))


  (= (varlist $X $L)  
    (varlist $X Nil $L)
    (set-det))
; /******************************************************************/
; /* Determination of the set of vars occuring in a MeTTa expression*/
; /******************************************************************/
   
  (= (varlist $T $L $LO)  
    (nonvar $T)
    (=.. $T 
      (Cons  $F $A))
    (set-det)
    (varlist1 $A $L $LO))
  (= (varlist $X $L $L)  
    (var $X)
    (element-v $X $L)
    (set-det))
  (= (varlist $X $L (Cons  $X $L))  
    (var $X)
    (set-det))


  (= (varlist1 (Cons  $T $A) $L $LO)  
    (varlist1 $A $L $L1)
    (set-det)
    (varlist $T $L1 $LO))
  (= (varlist1  () $L $L)  True)


  (= (element-v $Element1 (Cons  $Element2 $Tail))  
    (== $Element1 $Element2))
  (= (element-v $Element (Cons  $_ $Tail))  
    (element-v $Element $Tail))


  (= (findbag $X $G $_)  
    ( (add-is-symbol  &self 
        (yk_found  mark)) 
      (call $G) 
      (add-is-symbol  &self 
        (yk_found  $X)) 
      (fail)))
; /******************************************************************/
; /* We cannot use bagof/3 since it reverses the output list order !*/
; /******************************************************************/            
  (= (findbag $_ $_ $L)  
    (yk-collect-found Nil $L))                     
                                               

  (= (yk-collect-found $Acc $L)  
    (yk-getnext $X)
    (yk-collect-found 
      (Cons  $X $Acc) $L))      
  (= (yk_collect_found  $X $X)  True)                        
                                               
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  invers/invers.pl 614 33 26398))



  (= (help)  
    (write 'Load example calls with command: [Filename].')
    (nl)
    (write 'Run example with command: test1(X).')
    (nl)
    (write 'Run example with command: test2(X).')
    (nl)
    (write 'Run example with command: test3(X).')
    (nl)
    (write 'Run example with command: test4(X).')
    (nl)
    (write 'Run example with command: test5(X).')
    (nl)
    (write 'Run example with command: test6(X).')
    (nl)
    (write 'Run example with command: test7(X).')
    (nl)
    (write 'Run example with command: test8(X).')
    (nl))
; /******************************************************************/
; /* instvars instantiates variables by a '_'(<Number>) structure. */
; /* Unfortunately a pretty print will result in uglier terms in */
; /* some MeTTa dialects */
; /******************************************************************/
; ; instvars(REXPR,MIN,MAX) :- ; REXPR =.. [_|ARGS], yap_instvar(ARGS,MIN,MAX), ! . ; ; yap_instvar([],N,N) . ; yap_instvar([FIRST|REST],N,N_OUT) :- ; var(FIRST), !, Y =.. ['_',N], FIRST = Y, ; N_PLUS_1 is N + 1, yap_instvar(REST,N_PLUS_1,N_OUT) . ; yap_instvar([FIRST|REST],N,N_OUT) :- ; skel(FIRST), !, FIRST =.. [_|SKEL_ARGS], ; yap_instvar(SKEL_ARGS,N,N_PLUS_M), ; yap_instvar(REST,N_PLUS_M,N_OUT) . ; yap_instvar([_|REST],N,N_OUT) :- ; yap_instvar(REST,N,N_OUT) . ; ; skel(Z) :- ; nonvar(Z), ; (list(Z), !, length(Z,N), N>0; ; Z =.. [_|LIST], length(LIST,N), N>0), ! . ; ; list([]). ; list([A|B]) :- list(B) .


  !(help *)

