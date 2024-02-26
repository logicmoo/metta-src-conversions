;                                           (convert_to_metta_file  interpreter $_170804 miles/interpreter.pl miles/interpreter.metta)

;
; MODULE interpreter EXPORTS

  !(module interpreter 
    (:: 
      (/ solve 3) 
      (/ solve-once 3) 
      (/ failed-proof 1) 
      (/ prooftrees 3) 
      (/ proof-close 2) 
      (/ prove1 2) 
      (/ prove3 2) 
      (/ prove4 3) 
      (/ prove5 2) 
      (/ proof-path 4) 
      (/ set-proof-depth 0) 
      (/ t-interpreter 2) 
      (/ ip-part1 2) 
      (/ ip-part2 3)))

;
; IMPORTS
  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ interpretable-predicate 1)))
  !(use-module 
    (home div-utils) 
    (:: 
      (/ insert-unique 3) 
      (/ identical-member 2) 
      (/ append-all 2) 
      (/ mysetof 3)))
  !(use-module 
    (home bu-basics) 
    (:: 
      (/ head 3) 
      (/ body 3) 
      (/ assumption 3)))
  !(use-module 
    (home environment) 
    (:: (/ satisfiable 1)))
  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
  !(use-module-if-exists 
    (library unify) 
    (:: (/ unify 2)))


;
; METAPREDICATES
;
; none


  !(dynamic (, (/ failed-proof 1) (/ tag 1) (/ prooftrees 3) (/ depth-bound 1) (/ depth-exceeded 0) (/ depth-exceeded 3))) 



;
; ***********************************************************************
;
; *
;
; * module: interpreter.pl
;
; *
;
; * author: B.Jung, M.Mueller, I.Stahl, B.Tausend date:12/92
;
; *
;
; * changed:
;
; *
;
; * description: different interpreters working on the knowledge base
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


;
; ***********************************************************************
;
; *
;
; * predicate: ip_part1/2
;
; *
;
; * syntax: ip_part1(+Goal,-Proof)
;
; *
;
; * args: Goal: an uncovered positive example
;
; * Proof: a failing proof for the positive example
;
; *
;
; * description: works exactly as the general interpreter solve0/2. The only
;
; * difference is that instead of failing when a system goal is failing
;
; * or a proof is looping or rules are missing, the interpreter
;
; * continues, assuming that the failing goals should be correct
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (ip-part1 $Goal $Proof)  
    ( (remove-all-atoms  &self 
        (tag  $_)) 
      (add-is-symbol  &self 
        (tag  $Goal)) 
      (gen-depth $D $Delta) 
      (ipp1 $Goal $D $Delta $Proof $Proof Nil)))
 

  (= (ipp1 True $D $Delta $Proof $Poi $_)  
    (set-det)
    (= $Poi Nil))

  (= (ipp1 no-rule $_ $_ $_ $_ $_)  
    (set-det))

  (= (ipp1 (, $A $B) $D $Delta $Proof $Poi $Ancestors)  
    (set-det)
    (= $Poi 
      (Cons  $PoiA $PoiB))
    (ipp1 $A $D $Delta $Proof 
      (:: $PoiA) $Ancestors)
    (ipp1 $B $D $Delta $Proof $PoiB $Ancestors))
 
  (= (ipp1 $A $D $Delta $Proof $Poi $Ancestors)  
    (interpretable-predicate $A)
    (set-det)
    (det-if-then-else 
      (> $D 0) True 
      (, 
        (add-is-symbol  &self 
          (tag  $A)) 
        (fail)))
    (det-if-then-else 
      (identical-member $A $Ancestors) 
      (= $Poi 
        (:: (:: -1 $A looping))) 
      (, 
        (is $D1 
          (- $D 1)) 
        (ipp1-rule $D $Delta $Proof $Poi $I $A $B) 
        (= $Poi 
          (:: (:: $I $A $PoiB))) 
        (ipp1 $B $D1 $Delta $Proof $PoiB 
          (Cons  $A $Ancestors)))))

  (= (ipp1 $A $D $Delta $Proof $Poi $_)  
    (det-if-then-else 
      (call $A) 
      (= $Poi 
        (:: (:: sys $A Nil))) 
      (= $Poi 
        (:: (:: sys $A fail)))))




  (= (ipp1-rule $_ $_ $_ $_ $I $A $B)  
    (get-clause $I $A $B $_ $_))
  (= (ipp1-rule $D $Delta $Proof $Poi $_ $A no-rule)  
    (det-if-then-else 
      (get-clause $_ $A $_ $_ $_) fail 
      (= $Poi 
        (:: (:: -1 $A no-rules)))))


;
; ***********************************************************************
;
; *
;
; * predicate: ip_part2/3
;
; *
;
; * syntax: ip_part2(+Proofs,+Goal,-Uncovered_Atoms)
;
; *
;
; * args: Proofs: failing proofs determined by ip_part1,
;
; * Goal: uncovered positive example Uncovered_Atoms: Atoms that make
;
; * Goal succeed, if they were covered by the kb
;
; *
;
; * description: the satisfiability of each subgoal within failing proof is
;
; * determined. For that, the oracle might be necessary.
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (ip-part2 (Cons  $P $_) $Goal $UA)  
    (ipp2 $P Nil Nil $UA))
  (= (ip-part2 (Cons  $_ $R) $Goal $UA)  
    (ip-part2 $R $Goal $UA))


  (= (ipp2 (:: $I $H looping) $_ $L (Cons  (with_self  $I $H) $L))  
    (set-det))
  (= (ipp2 (:: sys $_ fail) (Cons  (with_self  $I $A) $_) $L (Cons  (with_self  $I $A) $L))  
    (set-det))
  (= (ipp2 (:: $_ $_ Nil) $_ $L $L)  
    (set-det))
  (= (ipp2 (:: $_ $H no-rules) $_ $L (Cons  (with_self  -1 $H) $L))  
    (set-det))
  (= (ipp2 (:: $I $H $SG) $Ancestors $L $L1)  
    (det-if-then-else 
      (satisfiable $SG) 
      (ipp2-list $SG 
        (Cons  
          (with_self  $I $H) $Ancestors) $L $L1) 
      (= $L1 
        (Cons  
          (with_self  $I $H) $L))))



  (= (ipp2_list  () $_ $L $L)  True)
  (= (ipp2-list (Cons  $G $R) $A $L $L2)  
    (ipp2 $G $A $L $L1)
    (ipp2-list $R $A $L1 $L2))


;
; ***********************************************************************
;
; *
;
; * predicate: proof_path/4
;
; *
;
; * syntax: proof_path(+Ex,+Pred,+Type,-ClauseIDs)
;
; *
;
; * args: Ex: example for p/n
;
; * Pred = p(X1,..,Xn): most general term of p/n
;
; * Type = typei(Xi) for an argument of p/n
;
; * ClauseIDs: list of clauseIDs that have beed used for proving
;
; * typei(ei) for the ith argument of Ex
;
; *
;
; * description: simulates the proof of typei(ei) for the ith argument of Ex
;
; * and collects the indices of all used clauses
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (proof-path $Ex $P $T $Ts)  
    (copy-term 
      (, $P $T) 
      (, $Ex $T0))
    (proof-path $T0 Nil $Ts))

  (= (proof-path True $T $T)  
    (set-det))
  (= (proof-path (, $A $B) $T $T2)  
    (set-det)
    (proof-path $A $T $T1)
    (proof-path $B $T1 $T2))

  (= (proof-path $A $T $T)  
    (=.. $A 
      (Cons  all $_))
    (set-det))
  (= (proof-path $A $T $T)  
    (=.. $A 
      (Cons  $T1 $_))
    (or 
      (= $T1 is-symbol) 
      (or 
        (= $T1 symbolic) 
        (= $T1 number)))
    (set-det)
    (call $A))

  (= (proof-path $A $T $T1)  
    (get-clause $I $A $B $_ type)
    (proof-path $B $T $T0)
    (insert-unique $I $T0 $T1))


;
; ***********************************************************************
;
; *
;
; * predicate: t_interpreter/2
;
; *
;
; * syntax: t_interpreter(+Goal,+ClauseList)
;
; *
;
; * args: Goal: goal type(Arg), Arg ground
;
; * ClauseList: List of clauses defining different types
;
; *
;
; * description: proves type(Arg) from ClauseList as kb
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (t-interpreter True $_)  
    (set-det))
  (= (t-interpreter (, $A $B) $CL)  
    (set-det)
    (t-interpreter $A $CL)
    (t-interpreter $B $CL))
  (= (t-interpreter $C $_)  
    (=.. $C 
      (Cons  $P $_))
    (or 
      (= $P is-symbol) 
      (or 
        (= $P number) 
        (= $P symbolic)))
    (set-det)
    (call $C))
  (= (t-interpreter $C $CL)  
    (copy-term $CL $CL1)
    (member 
      (= $C  $B) $CL1)
    (t-interpreter $B $CL))



;
; ************************************************************************
;
; *
;
; * predicate: solve/3
;
; *
;
; * syntax: solve(+Goal,-Mark,-Proofs)
;
; *
;
; * args: Goal: ground atom or rule with ground head
;
; * Mark: success or fail
;
; * Proofs: all succeeding/failing proofs according to Mark
;
; *
;
; * description: format for Proofs: [P1,..,Pn]
;
; * where Pi = [ID,Head,PBody] where ID is the ID of the
;
; * applied rule (sys for system predicates, -1 if no rule
;
; * is applicable), Head is the instantiation of the rule head,
;
; * and PBody is the proof of the rule body. PBody is of the form
;
; * - [], if Head is true
;
; * - fail, if Head is a failing syspred
;
; * - looping if the proof is looping on Head
;
; * - no_rules if no rules match Head
;
; * - depth_exceeded if the proof fails because of depth bound
;
; * Maximum depth for proofs: 50
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also:
;
; *
;
; ************************************************************************


  (= (solve (= $H  $B) $Mark $Proofs)  
    (det-if-then-else 
      (ground $H) 
      (solve $B $Mark $Proofs) 
      (, 
        (set-det) 
        (fail))))

  (= (solve $Goal $Mark $Proofs)  
    (det-if-then-else 
      (setof $Proof 
        (^ $Goal 
          (solve0 $Goal $Proof)) $Proofs0) 
      (= $Mark success) 
      (, 
        (bagof $FProof 
          (failed-proof $FProof) $Proofs00) 
        (= $Mark fail) 
        (det-if-then-else depth-exceeded 
          (, 
            (setof $EProof 
              (^ $A 
                (depth-exceeded $A $EProof 
                  (:: (:: -1 $A depth-exceeded)))) $EProofs0) 
            (append $EProofs0 $Proofs00 $Proofs0)) 
          (= $Proofs0 $Proofs00))))
    (append-all $Proofs0 $Proofs1)
    (proof-close $Proofs1 $Proofs))


;
; ***********************************************************************
;
; *
;
; * predicate: solve_once/3
;
; *
;
; * syntax: solve_once(+Goal,-Mark,-Proof)
;
; *
;
; * args: as solve/3
;
; *
;
; * description: proves Goal only once
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (solve-once (= $H  $B) $Mark $Proofs)  
    (det-if-then-else 
      (ground $H) 
      (solve-once $B $Mark $Proofs) 
      (, 
        (set-det) 
        (fail))))

  (= (solve-once $Goal $Mark $Proofs)  
    (det-if-then-else 
      (solve0 $Goal $Proof) 
      (, 
        (= $Proofs0 
          (:: $Proof)) 
        (= $Mark success)) 
      (, 
        (bagof $FProof 
          (failed-proof $FProof) $Proofs0) 
        (= $Mark fail)))
    (append-all $Proofs0 $Proofs1)
    (proof-close $Proofs1 $Proofs))



;
; ***********************************************************************
;
; *
;
; * predicate: proof_close/2
;
; *
;
; * syntax: proof_close(+Proofs,-Proofs)
;
; *
;
; * args: Proofs as for solve/3
;
; *
;
; * description: closes the open lists in Proofs
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (proof-close $X Nil)  
    (var $X)
    (set-det))
  (= (proof-close (Cons  (:: $J $H $B1) $R1) (Cons  (:: $J $H $B2) $R2))  
    (proof-close $B1 $B2)
    (proof-close $R1 $R2))
  (= (proof-close $X $X)  
    (atomic $X)
    (set-det))


;
; ***********************************************************************
;
; *
;
; * predicate: solve0/2
;
; *
;
; * syntax: solve0(+Goal,-Proof)
;
; *
;
; * args: Goal: ground atom, Proof: one possible proof for Goal
;
; *
;
; * description:
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (solve0 $Goal $Proof)  
    ( (remove-all-atoms  &self 
        (tag  $_)) 
      (remove-all-atoms  &self 
        (failed_proof  $_)) 
      (remove-all-atoms  &self 
        (depth_exceeded  $_ $_ $_)) 
      (remove-all-atoms  &self depth_exceeded) 
      (gen-depth $D $Delta) 
      (solve2 $Goal $D $Delta $Proof $Proof Nil)))



  (= (solve2 True $_ $_ $_ Nil $_)  
    (set-det))

 
  (= (solve2 (, $A $B) $D $Delta $Proof $Poi $Ancestors)  
    (set-det)
    (= $Poi 
      (Cons  $PoiA $PoiB))
    (solve2 $A $D $Delta $Proof 
      (:: $PoiA) $Ancestors)
    (solve2 $B $D $Delta $Proof $PoiB $Ancestors))
    
 
  (= (solve2 $A $D $Delta $Proof $Poi $Ancestors)  
    (interpretable-predicate $A)
    (set-det)
    (det-if-then-else 
      (= $D 0) 
      (, 
        (add-is-symbol  &self 
          (tag  $A)) 
        (add-is-symbol  &self 
          (depth_exceeded  $A $Proof $Poi)) 
        (fail)) 
      (det-if-then-else 
        (identical-member $A $Ancestors) 
        (, 
          (= $Poi 
            (:: (:: -1 $A looping))) 
          (det-if-then-else 
            (< $D $Delta) 
            (add-is-symbol  &self 
              (failed_proof  $Proof)) True) 
          (fail)) 
        (, 
          (is $D1 
            (- $D 1)) 
          (solve-rule $D1 $Delta $Proof $Poi $I $A $B) 
          (= $Poi 
            (:: (:: $I $A $PoiB))) 
          (solve2 $B $D1 $Delta $Proof $PoiB 
            (Cons  $A $Ancestors))))))
; ; A is in KB

  (= (solve2 $A $D $Delta $Proof $Poi $_)  
    (det-if-then-else 
      (, 
        (predicate-property $A built-in) 
        (on-exception $_ 
          (call $A) fail)) 
      (= $Poi 
        (:: (:: sys $A Nil))) 
      (, 
        (= $Poi 
          (:: (:: sys $A fail))) 
        (det-if-then-else 
          (< $D $Delta) 
          (add-is-symbol  &self 
            (failed_proof  $Proof)) True) 
        (fail))))
; ; A is built-in
; ; exception handling


  (= (solve2 $A $D $D $Proof $Poi $_)  
    ( (not depth-exceeded) 
      (= $Poi 
        (:: (:: -1 $A no-rules))) 
      (add-is-symbol  &self 
        (failed_proof  $Proof)) 
      (fail)))
; ; no rules at all for initial goal


;
; ***********************************************************************
;
; *
;
; * predicate: solve_rule/7
;
; *
;
; * syntax: solve_rule(+D,+Delta,+Proof,+Proof_Poi,-ID,+Goal,-Body)
;
; *
;
; * args: D,Delta: depth bounds for iterative deepening
;
; * Proof,Proof_Poi: intermediate Proof of the toplevel goal (open list)
;
; * Goal: current goal
;
; * ID,Body: id and body of a kb-rule matchin Goal (if any)
;
; *
;
; * description:
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (solve-rule $_ $_ $_ $_ $I $A $B)  
    (functor $A $F $N)
    (functor $A1 $F $N)
    (get-clause $I $A1 $B $_ $_)
    (unify $A $A1))
  (= (solve-rule $D $Delta $Proof $Poi $_ $A $_)  
    (= $Poi 
      (:: (:: -1 $A no-more-rules)))
    (det-if-then-else 
      (< $D $Delta) 
      (add-is-symbol  &self 
        (failed_proof  $Proof)) True)
    (fail))


;
; ***********************************************************************
;
; *
;
; * predicate: gen_depth/2
;
; *
;
; * syntax: gen_depth(D, Delta)
;
; *
;
; * args: D,Delta: integers
;
; *
;
; * description: generates depth bound for the interative deepening
;
; * theorem prover. Delta is the difference between D and
;
; * the former depth (not to create duplicate proofs)
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (gen-depth $D $Delta)  
    (depth-bound $N)
    (det-if-then-else 
      (number $N) 
      (det-if-then-else 
        (>= $N 3) 
        (gen-depth 3 $D 3 $Delta) 
        (gen-depth $N $D $N $Delta)) 
      (gen-depth 3 $D 3 $Delta)))
; /***** gen_depth without maximum depth***** gen_depth(D, Delta) :- gen_depth(3, 100, D, Delta). gen_depth(D, Delta, D, Delta). gen_depth(D0, _, D, Delta) :- Delta1 is D0 div 2 + 1, D1 is D0 + Delta1, ( tag(_) -> retractall(tag(_)), gen_depth(D1, Delta1, D, Delta) ; fail ). *******/
; ; D = new depth for proofs
; ; Delta = new D - former D           

 

  (= (gen_depth  $D $D $Delta $Delta)  True)
 
  (= (gen-depth $D0 $D $_ $Delta)  
    (det-if-then-else 
      (tag $_) 
      (, 
        (remove-all-atoms  &self 
          (tag  $_)) 
        (is $Delta1 
          (+ 
            (div $D0 2) 1)) 
        (is $D1 
          (+ $D0 $Delta1)) 
        (depth-bound $Max) 
        (det-if-then-else 
          (number $Max) 
          (det-if-then-else 
            (=< $D1 $Max) 
            (remove-all-atoms  &self 
              (depth_exceeded  $_ $_ $_)) 
            (, 
              (add-is-symbol  &self depth_exceeded) 
              (fail))) True) 
        (gen-depth $D1 $D $Delta1 $Delta)) fail))


  (= (set-proof-depth)  
    ( (nl) 
      (nl) 
      (write 'Speficy maximum depth for theorem prover (number or n for unbound proofs): ') 
      (read $N) 
      (remove-all-atoms  &self 
        (depth_bound  $_)) 
      (add-is-symbol  &self 
        (depth_bound  $N))))


;
; ***********************************************************************
;
; *
;
; * predicate: prove1/2
;
; *
;
; * syntax: prove1(+Clause, -Proof)
;
; *
;
; * args: Clause: clause in list form ( [H:p,L1:n,L2:n,..])
;
; * Proof: list of all literals used to prove clause
;
; *
;
; * description:
;
; * prove1 tries to match Clause against literals in this kb,
;
; * use for clause reduction wrt theta-subsumption (Plotkin).
;
; *
;
; * example:
;
; *
;
; * peculiarities:
;
; *
;
; * see also: Buntine,1988. Plotkin,1970.
;
; *
;
; ***********************************************************************

  (= (prove1 $CL $SProof)  
    (ini-prove1 $CL $CL1)
    (prove1a $CL1 Nil $SProof))
; /* prove1([H|T],Proof):- prove1(H,HProof), prove1(T,TProof), append(HProof,TProof,Proof). prove1([],[]). prove1(L:S,[L:S]):- member(S,[n,r]), body(L,_O,_I). prove1(L:p,[L:p]):- head(L,_O,_I). */
; ;*********************************************************************** ;* ;* predicate: prove1/2 ;* ;* syntax: prove1(+Clause, -Proof) ;* ;* args: Clause: clause in list form ( [H:p,L1:n,L2:n,..]) ;* Proof: list of all literals used to prove clause ;* ;* description: ;* prove1 tries to match Clause against literals in this kb, ;* use for clause reduction wrt theta-subsumption (Plotkin). ;* This is a more efficient version for embedding Clause in the kb: ;* (IRENE) ;* a list CL1 = [Lit:Sign:Litlist|_] is constructed from Clause ;* where Litlist is the list of literals in the kb (if Sign = p, literals ;* head(L,_,_), else body(L,_,_)) unifiable with Lit. CL1 is sorted ;* ascendingly according to the length of Litlist. If there is an ;* empty Litlist in CL1, prove1a fails and backtracking occurs. ;* Else Lit is unified with a literal in Litlist (backtracking point), ;* and the remaining list CL1 is updated. ;* ;* example: ;* ;* peculiarities: ;* ;* see also: Buntine,1988. Plotkin,1970. ;* ;***********************************************************************


  (= (prove1a  () $SP $SP)  True)
;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/interpreter.pl 618 26 17908))

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/interpreter.pl 619 13 17937))



  (= (ini_prove1  () ())  True)
;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/interpreter.pl 634 20 18344))


;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/interpreter.pl 636 16 18376))


  (= (insert_prove1  $X $L (Cons  $X $L))  True)


  (= (adapt_prove1  () $_ $_ ())  True)
;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/interpreter.pl 643 18 18567))

;                                           (error
;                                             (syntax_error  operator_clash)
;                                             (file  miles/interpreter.pl 646 18 18664))



;
; ***********************************************************************
;
; *
;
; * predicate: prove3/2
;
; *
;
; * syntax: prove3(+CL,-CL)
;
; *
;
; * args: CL: clause body in list notation
;
; *
;
; * description: embedd CL in skolemized body of an example clause
;
; * (body/3 entries in the kb)
;
; * used for absorption, saturation
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (prove3 (Cons  $A $B) (Cons  $ProofA $ProofB))  
    (prove3 $A $ProofA)
    (prove3 $B $ProofB))
  (= (prove3  () ())  True)

  (= (prove3 (with_self  $A (n)) (with_self  $A (n)))  
    (body $A $_ $_))
  (= (prove3 (with_self  $A (r)) (with_self  $A (r)))  
    (body $A $_ $_))


;
; ***********************************************************************
;
; *
;
; * predicate: prove4/3
;
; *
;
; * syntax: prove4(+CL,-Uncovered,-Proof)
;
; *
;
; * args: CL: clause in list notation
;
; * Uncovered = H/M, where M in {new_head,new_body}
;
; * or Uncovered = [] if all literals are covered
;
; * Proof = [[Lit,N],...] where N in {head,body}
;
; *
;
; * description: embeds CL in skolemized example clause (head/3,body/3 entries)
;
; * allows 1 uncovered literal (= the resolution literal) & returns it
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (prove4 (Cons  $H $More) $Uncovered (Cons  $ProofH $ProofRest))  
    (prove4 $H $Uncovered $ProofH)
    (prove4 $More $Uncovered $ProofRest))


  (= (prove4 Nil Nil Nil)  
    (set-det))
  (= (prove4 Nil $_ Nil)  
    (set-det))
  (= (prove4 (with_self  $H (n)) $_ (:: $H body))  
    (body $H $_ $_))
  (= (prove4 (with_self  $H (r)) $_ (:: $H body))  
    (body $H $_ $_))
  (= (prove4 (with_self  $H (p)) $_ (:: $H head))  
    (head $H $_ $_))
  (= (prove4 (with_self  $H (n)) $Uncovered Nil)  
    (var $Uncovered)
    (= $Uncovered 
      (/ $H new-head)))
  (= (prove4 (with_self  $H (r)) $Uncovered Nil)  
    (var $Uncovered)
    (= $Uncovered 
      (/ $H new-head)))
  (= (prove4 (with_self  $H (p)) $Uncovered Nil)  
    (var $Uncovered)
    (= $Uncovered 
      (/ $H new-body)))


;
; ***********************************************************************
;
; *
;
; * predicate: prove5/2
;
; *
;
; * syntax: prove5(+HS,+RuleIDs)
;
; *
;
; * args: HS: skolemized clause head, RuleIDs: list of ruleIDs
;
; *
;
; * description: tries to infer HS from assumptions and the rules in
;
; * RuleIDs
;
; *
;
; * example:
;
; *
;
; * peculiarities: none
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (prove5 $H $_)  
    (assumption $H $_ $_))
  (= (prove5  true $_)  True)
  (= (prove5 $H $RULES)  
    (get-clause $ID $H True $_ $_)
    (member $ID $RULES))
  (= (prove5 (, $L1 $L2) $RULES)  
    (prove5 $L1 $RULES)
    (prove5 $L2 $RULES))
  (= (prove5 $H $RULES)  
    (get-clause $ID $H $B $_ $_)
    (member $ID $RULES)
    (prove5 $B $RULES))


