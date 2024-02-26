;                                           (convert-to-metta-file  evaluation $_262676 miles/evaluation.pl miles/evaluation.metta)
;
; MODULE evaluation EXPORTS


  !(module evaluation 
    (:: 
      (/ covered-pos-examples 1) 
      (/ covered-neg-examples 1) 
      (/ all-covered-examples 1) 
      (/ complexity 2) 
      (/ clear-evaluation 0) 
      (/ evaluated 1) 
      (/ change-evaluated 1) 
      (/ eval-examples 0) 
      (/ eval-pos-examples 1) 
      (/ complete-chk 0) 
      (/ correct-chk 0) 
      (/ fp 1) 
      (/ fpo 1) 
      (/ fp-hyp 1) 
      (/ ip 1) 
      (/ herbrand-base-ff 1) 
      (/ ivonTunterE 1) 
      (/ ivonBundE 1) 
      (/ code-length 2) 
      (/ encoding-length-examples 1) 
      (/ encoding-length-clause 2)))
; ; Compute complete evaluation for all examples
; ; AND clauses in kb
; ; Compute evaluation for all pos examples in kb
; ; check completeness, all pos examples covered?
; ; check correctness, no neg examples covered?

;
; IMPORTS
  !(use-module 
    (home div-utils) 
    (:: 
      (/ make-unique 2) 
      (/ insert-unique 4) 
      (/ sort-by-length 3) 
      (/ mysetof 3) 
      (/ remove 3) 
      (/ append-all 2) 
      (/ sum 2) 
      (/ identical-make-unique 2) 
      (/ best 2) 
      (/ remove-variant 3) 
      (/ make-unique 2) 
      (/ fak 2) 
      (/ fak1 3) 
      (/ nueberk 3) 
      (/ log2 2) 
      (/ log2nueberk 3) 
      (/ sum-of-logs 3)))
  !(use-module 
    (home environment) 
    (:: (/ ask-for 1)))
  !(use-module 
    (home var-utils) 
    (:: 
      (/ term-size 2) 
      (/ vars 2) 
      (/ skolemize 3)))
  !(use-module 
    (home kb) 
    (:: 
      (/ get-example 3) 
      (/ ex 3) 
      (/ known 6) 
      (/ assertallz 1) 
      (/ get-predlist 1) 
      (/ get-evaluation 2) 
      (/ delete-example 1) 
      (/ delete-clause 1) 
      (/ get-clause 5)))
  !(use-module 
    (home interpreter) 
    (:: 
      (/ prooftrees 3) 
      (/ solve-once 3) 
      (/ proof-close 2) 
      (/ solve 3) 
      (/ ip-part1 2) 
      (/ ip-part2 3)))
  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
  !(use-module-if-exists 
    (library subsumes) 
    (:: 
      (/ subsumes-chk 2) 
      (/ variant 2)))
  !(use-module-if-exists 
    (library occurs) 
    (:: (/ sub-term 2)))
  !(use-module-if-exists 
    (library math) 
    (:: (/ pow 3)))


;
; METAPREDICATES
;
; none



  !(dynamic (/ evaluated 1))


;
; ***********************************************************************
;
; *
;
; * module: evaluation.pl
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
; * description: evaluation of (parts of) the knowledge base
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
; * predicate: ip/1
;
; *
;
; * syntax: ip(-UA_List)
;
; *
;
; * args: -UA_List ... list of ground atoms
;
; *
;
; * description: Shapiro's algorithm for diagnosing finite failure
;
; * ip in our framework. Returns a set of ground atoms that
;
; * has to be covered to make all uncovered positive
;
; * examples succeed.
;
; * Allows backtracking on alternative sets of ground atoms
;
; * that make all examples succeed.
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


  (= (ip $UA_List)  
    (det-if-then-else 
      (evaluated no) eval-examples True)
    (mysetof $E 
      (^ $I 
        (^ $Trees 
          (, 
            (get-example $I $E +) 
            (prooftrees $I fail $Trees)))) $Elist)
    (set-det)
    (ip-list $Elist Nil $UA_List1)
    (make-unique $UA_List1 $UA_List))


  (= (ip-list  () $L $L)  True)
  (= (ip-list (Cons  $E $R) $L $L2)  
    (ip0 $E $UAs)
    (append $L $UAs $L1)
    (ip-list $R $L1 $L2))



;
; ***********************************************************************
;
; *
;
; * predicate:
;
; *
;
; * syntax: ip(+UA,-UAs)
;
; *
;
; * args:
;
; *
;
; * description: UA is an uncovered atom, i.e. both prooftrees(I,fail,Trees)
;
; * and ex(I,UA,+) are in the knowledge base.
;
; * UAs is a list [A1,...,An] such that a proof of UA would
;
; * succeed if A1 through An were covered by the knowledge base.
;
; * Cave!: Extensive oracle interaction
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


  (= (ip0 $Goal $UAs)  
    (setof $Proof 
      (^ $Goal 
        (ip-part1 $Goal $Proof)) $Proofs0)
    (append-all $Proofs0 $Proofs1)
    (proof-close $Proofs1 $Proofs)
    (set-det)
    (ip-part2 $Proofs $Goal $UAs0)
    (make-unique $UAs0 $UAs))


;
; ***********************************************************************
;
; *
;
; * predicate: fp/1
;
; *
;
; * syntax: fp(-OR)
;
; *
;
; * args: OR:
;
; *
;
; * description: a kind of shapiro's contradiction backtracing that
;
; * aims to detect possibly overgeneral clauses.
;
; * As it does not use an oracle, all possibly overgeneral
;
; * clauses are considered and a minimal combination
;
; * such that all negative examples become uncovered is
;
; * returned.
;
; * Allows backtracking to an alternative set of possibly
;
; * overgeneral clauses
;
; * OR is a list [I:E,...], where I is the index of a possibly
;
; * overgeneral clause and E is the set of wrong (head-)instantiations of
;
; * clause I that should be excluded by specializing I.
;
; * OR is a minimal selection of possibly overgeneral clauses such
;
; * that by specialising them all negative examples become uncovered.
;
; * On backtracking, the second selection is returned, and so on.
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


  (= (fp $OR)  
    (det-if-then-else 
      (evaluated no) eval-examples True)
    (bagof $TL 
      (^ $I 
        (^ $E 
          (^ $Trees 
            (^ $P 
              (, 
                (get-example $I $E -) 
                (prooftrees $I success $Trees) 
                (member $P $Trees) 
                (fp $P Nil $TL)))))) $TList)
    (collect-indices $TList Nil $Indices)
    (or-subsets $Indices $TList $OR_List)
    (set-det)
    (best $OR_List $OR))
  (= (fp  ())  True)


;
; ***********************************************************************
;
; *
;
; * predicate: fp/3
;
; *
;
; * syntax: fp(+Prooftree,+L,-L)
;
; *
;
; * args: Prooftree is a prooftree for a succeeding negative example
;
; * L = [...,ID:[G1,..,Gn],...] where ID is a clause index and
;
; * G1,..,Gn are the head instantiations the clause has been applied with
;
; * during the proof Prooftree.
;
; *
;
; * description: collects clauses and goals that have been used during
;
; * a successfull proof of a negative example
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

  (= (fp (:: sys $_ $_) $L $L)  
    (set-det))
  (= (fp (:: $I $A $SG) $L $L2)  
    (fp-list $SG $L $L1)
    (insert-unique $I $A $L1 $L2))


  (= (fp-list  () $L $L)  True)
  (= (fp-list (Cons  $G $R) $L $L2)  
    (fp-list $R $L $L1)
    (fp $G $L1 $L2))



;
; ***********************************************************************
;
; *
;
; * predicate: fp_hyp/1
;
; *
;
; * syntax: fp_hyp(-OR)
;
; *
;
; * args: OR:
;
; *
;
; * description: as fp/1, but considers only clauses with label 'hypo' as
;
; * possibly overgeneral
;
; * Allows backtracking to an alternative set of possibly
;
; * overgeneral clauses
;
; * OR is a list [I:E,...], where I is the index of a possibly
;
; * overgeneral clause and E is the set of wrong (head-)instantiations of
;
; * clause I that should be excluded by specializing I.
;
; * OR is a minimal selection of possibly overgeneral clauses such
;
; * that by specialising them all negative examples become uncovered.
;
; * On backtracking, the second selection is returned, and so on.
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


  (= (fp-hyp $OR)  
    (det-if-then-else 
      (evaluated no) eval-examples True)
    (bagof $TL 
      (^ $I 
        (^ $E 
          (^ $Trees 
            (^ $P 
              (, 
                (get-example $I $E -) 
                (prooftrees $I success $Trees) 
                (member $P $Trees) 
                (fp-hyp $P Nil $TL)))))) $TList)
    (collect-indices $TList Nil $Indices)
    (or-subsets $Indices $TList $OR_List)
    (set-det)
    (best $OR_List $OR))
  (= (fp-hyp  ())  True)


;
; ***********************************************************************
;
; *
;
; * predicate: fp_hyp/3
;
; *
;
; * syntax: fp_hyp(+Prooftree,+L,-L)
;
; *
;
; * args: Prooftree is a prooftree for a succeeding negative example
;
; * L = [...,ID:[G1,..,Gn],...] where ID is a clause index of a clause with
;
; * label 'hypo', and
;
; * G1,..,Gn are the head instantiations the clause has been applied with
;
; * during the proof Prooftree.
;
; *
;
; * description: collects clauses and goals that have been used during
;
; * a successfull proof of a negative example
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

  (= (fp-hyp (:: sys $_ $_) $L $L)  
    (set-det))
  (= (fp-hyp (:: $I $A $SG) $L $L2)  
    (fp-hyp-list $SG $L $L1)
    (det-if-then-else 
      (get-clause $I $_ $_ $_ hypo) 
      (insert-unique $I $A $L1 $L2) 
      (= $L2 $L1)))


  (= (fp-hyp-list  () $L $L)  True)
  (= (fp-hyp-list (Cons  $G $R) $L $L2)  
    (fp-hyp-list $R $L $L1)
    (fp-hyp $G $L1 $L2))



;
; ***********************************************************************
;
; *
;
; * predicate: fpo/1
;
; *
;
; * syntax: fpo(-OR)
;
; *
;
; * args: OR:
;
; *
;
; * description: as fp/1, but uses oracle
;
; * Allows backtracking to an alternative set of possibly
;
; * overgeneral clauses
;
; * OR is a list [I:E,...], where I is the index of a possibly
;
; * overgeneral clause and E is the set of wrong (head-)instantiations of
;
; * clause I that should be excluded by specializing I.
;
; * OR is a minimal selection of possibly overgeneral clauses such
;
; * that by specialising them all negative examples become uncovered.
;
; * On backtracking, the second selection is returned, and so on.
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


  (= (fpo $OR)  
    (det-if-then-else 
      (evaluated no) eval-examples True)
    (bagof $TL 
      (^ $I 
        (^ $E 
          (^ $Trees 
            (^ $P 
              (, 
                (get-example $I $E -) 
                (prooftrees $I success $Trees) 
                (member $P $Trees) 
                (fpo $P Nil $TL $_)))))) $TList)
    (collect-indices $TList Nil $Indices)
    (or-subsets $Indices $TList $OR_List)
    (set-det)
    (best $OR_List $OR))
  (= (fpo  ())  True)


;
; ***********************************************************************
;
; *
;
; * predicate: fpo/4
;
; *
;
; * syntax: fpo(+Prooftree,+L,-L,-M)
;
; *
;
; * args: Prooftree is a prooftree for a succeeding negative example
;
; * L = [...,ID:[G1,..,Gn],...] where ID is a clause index and
;
; * G1,..,Gn are the head instantiations the clause has been applied with
;
; * during the proof Prooftree.
;
; * M indicates whether Prooftree is successful in the oracle-simulation (ok)
;
; * or not (not_ok)
;
; *
;
; * description: collects wrong clauses and goals that have been used during
;
; * a successfull proof of a negative example (uses oracle)
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

  (= (fpo (:: sys $_ $_) $L $L ok)  
    (set-det))
  (= (fpo (:: $I $A $SG) $L $L1 $X)  
    (det-if-then-else 
      (ask-for $A) 
      (fpo-list $SG $L $L1 $X) 
      (, 
        (insert-unique $I $A $L $L1) 
        (= $X not-ok))))


  (= (fpo-list  () $L $L ok)  True)
  (= (fpo-list (Cons  $G $R) $L $L2 $X)  
    (fpo $G $L $L1 $Xg)
    (det-if-then-else 
      (= $Xg ok) 
      (fpo-list $R $L1 $L2 $X) 
      (= $X $Xg)))


;
; ***********************************************************************
;
; *
;
; * predicate: collect_indices/3
;
; *
;
; * syntax: collect_indices(+L,+Accu,-Accu)
;
; *
;
; * args: L = [[I1:[G11,..,G1n],...,Im:[Gm1,...,Gmn]],...]
;
; * Accu = [I1,...,Ik]
;
; *
;
; * description: given the list of lists produced by fp/3, all indices of
;
; * clauses that participated in successful proofs of negative examples
;
; * are collected
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


  (= (collect-indices  () $L $L)  True)
  (= (collect-indices (Cons  $X $R) $L $L2)  
    (c-indices $X $L $L1)
    (collect-indices $R $L1 $L2))


  (= (c-indices  () $L $L)  True)
  (= (c-indices (Cons  (with-self  $I $_) $R) $L $L2)  
    (c-indices $R $L $L1)
    (det-if-then-else 
      (member $I $L1) 
      (= $L2 $L1) 
      (= $L2 
        (Cons  $I $L1))))



;
; ***********************************************************************
;
; *
;
; * predicate: or_subsets/3
;
; *
;
; * syntax: or_subsets(+Indices,+Tlist,-OR_List)
;
; *
;
; * args: Indices ... list of indices of clauses that participated in successful
;
; * proofs of negative examples
;
; * Tlist = [[I:[G1,..,Gn],..],..] list of lists produced by fp/3
;
; * OR_List = list of lists [I:E,..] where I is the index of a possibly
;
; * overgeneral clause and E is the set of wrong (head-)instantiations of
;
; * clause I that should be excluded by specializing I. OR_List is sorted
;
; * ascendingly according to the length of the sublists
;
; *
;
; * description: selects all possible combinations of possibly overgeneral clauses
;
; * such that by specialising them all negative examples become uncovered.
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


  (= (or-subsets $IX $TL $ORL)  
    (initialize-or-subsets $IX $IX $TL $TL1)
    (or-all-subsets $TL1 Nil $TL2)
    (sort-by-length $TL2 Nil $ORL))


;
; ***********************************************************************
;
; *
;
; * predicate: initialize_or_subsets/4
;
; *
;
; * syntax: initialize_or_subsets(+IX,+IX,+TL,-TL1)
;
; *
;
; * args: IX list of clauseIDs
;
; * TL = [[I:CoveredI,J:CoveredJ,...],...] resulting from fp/3.
;
; * each sublist in TL corresponds to a successful proof of a negative
;
; * example
;
; *
;
; * description: TL1 contains for each I in IX and entry [I:A]:IX1:TLI,
;
; * where IX1 = IX - {I} and TLI results from TL by deleting every
;
; * sublist [J:CJ,..,I:CI,...] that contains I:CI, and accumulating
;
; * the head instances of I in A.
;
; * The set TLI contains all proofs of negative examples that are
;
; * still possible if clause I is excluded (e.g. specialised).
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


  (= (initialize-or-subsets  () $_ $_ ())  True)
;                                           (error
;                                             (syntax-error  operator_clash)
;                                             (file  miles/evaluation.pl 478 41 15079))



;
; ***********************************************************************
;
; *
;
; * predicate: remove_conjuncts/5
;
; *
;
; * syntax: remove_conjuncts(+I,+TL,-TLI,+A,-A)
;
; *
;
; * args: I .. clause Index, TL = [[I:CI,J:CJ,...],...],
;
; * A = [G1,..,Gn] head instances of I
;
; *
;
; * description: removes from TL every sublist containing I:CI, and accumulates
;
; * CI in A. Each sublist in TL corresponds to a successful proof of
;
; * a negative example. If clause I is assumed to be overgeneral and
;
; * therefore excluded, the proof fails and the remaining clauses that
;
; * have been used need not be specialised. Therefore, the sublist is
;
; * removed from TL.
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


  (= (remove-conjuncts  $_ () () $A $A)  True)
  (= (remove-conjuncts $I (Cons  $X $R) $R1 $A $A2)  
    (member 
      (with-self  $I $E) $X)
    (set-det)
    (append $E $A $A0)
    (identical-make-unique $A0 $A1)
    (remove-conjuncts $I $R $R1 $A1 $A2))
  (= (remove-conjuncts $I (Cons  $X $R) (Cons  $X $R1) $A $A1)  
    (remove-conjuncts $I $R $R1 $A $A1))


;
; ***********************************************************************
;
; *
;
; * predicate: or_all_subsets/3
;
; *
;
; * syntax: or_all_subsets(+TL1,+Accu,-Accu)
;
; *
;
; * args: TL1 = [IXS:IXR:TLI,...] where IXS = [I:CI,...], IXR the indices not
;
; * occurring in IXS, and TLI the remaining proofs of negative examples
;
; * Accu = [IXS,...]
;
; *
;
; * description: tests every combination of clause indices whether all proofs
;
; * of negative examples are excluded when the clauses are assumed to be
;
; * overgeneral. A combination IXS is successful, if all proofs are excluded,
;
; * i.e. TLI = [].
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

;                                           (error
;                                             (syntax-error  operator_clash)
;                                             (file  miles/evaluation.pl 541 20 17217))

;                                           (error
;                                             (syntax-error  operator_clash)
;                                             (file  miles/evaluation.pl 543 20 17286))


  (= (or-all-subsets  () $L $L)  True)


  (= (or-asubsets  () $_ $_ $_ ())  True)
;                                           (error
;                                             (syntax-error  operator_clash)
;                                             (file  miles/evaluation.pl 550 39 17474))



;
; ***********************************************************************
;
; *
;
; * predicate: eval_pos_examples/1
;
; *
;
; * syntax: eval_pos_examples ( - List_of_Exs )
;
; *
;
; * args:
;
; *
;
; * description: Evaluate (= try to prove) all positive examples, return a list of the
;
; * ones which *cannot* be proved (empty list if successful).
;
; *
;
; * example:
;
; *
;
; * peculiarities: Output-argument looks like [exID1:Fact1, exID2:Fact2, ...].
;
; * !!! Procedure does not compute evaluation for clauses!!
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (eval-pos-examples $Exlist)  
    ( (remove-all-atoms  &self 
        (prooftrees  $_ $_ $_)) (findall (with-self  $I $Fact) (, (ex $I $Fact +) (solve-once $Fact fail $_)) $Exlist)))        



;
; ******************************************************************************
;
; *
;
; * predicate: eval_examples/0
;
; *
;
; * syntax:
;
; *
;
; * args:
;
; *
;
; * description: One should use this to compute the evaluation for kb clauses!
;
; * - asserts for each example ID prooftrees(ID,Mark,Proofs), where
;
; * Mark in {success,fail} and Proofs are the successful/failing
;
; * proofs accordingly
;
; * - determines the evaluation of each rule in the kb according to
;
; * the current examples
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
; ******************************************************************************


  (= eval-examples  
    (evaluated yes)
    (set-det))

  (= eval-examples  
    ( (remove-all-atoms  &self 
        (prooftrees  $_ $_ $_)) eval-examples1 
      (set-det) 
      (change-evaluated yes)))

  (= eval-examples1  
    ( (ex $I $Fact $_) 
      (solve $Fact $M $Proofs) 
      (add-atom  &self 
        (prooftrees  $I $M $Proofs)) fail))
  (= eval-examples1  
    (bagof 
      (with-self  $I $Proofs) 
      (prooftrees $I success $Proofs) $Plist)
    (findall 
      (known $J $H $B $Clist $L $_) 
      (, 
        (get-clause $J $H $B $Clist $L) 
        (delete-clause $J)) $Klist)
    (compute-evaluation $Klist $Plist $Klist1)
    (assertallz $Klist1)
    (set-det))
; ; don't use bagof here!
  (= eval_examples1  True)               ;
; in case there are no examples


  (= clear-evaluation  
    ( (remove-all-atoms  &self 
        (prooftrees  $_ $_ $_)) (change-evaluated no)))


  (= (change-evaluated $X)  
    ( (remove-all-atoms  &self 
        (evaluated  $_)) (add-atom  &self (evaluated  $X)))) 


;
; ***********************************************************************
;
; *
;
; * predicate:correct_chk/0
;
; *
;
; * syntax:
;
; *
;
; * args:
;
; *
;
; * description: fails when first *negative* example covered
;
; *
;
; * example:
;
; *
;
; * peculiarities: Does not compute evaluation for clauses!!
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= correct-chk  
    (det-if-then-else 
      (evaluated no) 
      (det-if-then-else 
        (, 
          (ex $ID $Fact -) 
          (solve-once $Fact success $_)) fail True) 
      (det-if-then-else 
        (, 
          (ex $ID $Fact -) 
          (prooftrees $ID success $_)) fail True)))

;
; ***********************************************************************
;
; *
;
; * predicate: complete_chk/0
;
; *
;
; * syntax:
;
; *
;
; * args:
;
; *
;
; * description: fails if not all *positive* examples covered
;
; *
;
; * example:
;
; *
;
; * peculiarities: Does not compute evaluation for clauses!!
;
; *
;
; * see also:
;
; *
;
; * origin: kb.pl (Irene/Markus)
;
; *
;
; ***********************************************************************


  (= complete-chk  
    (det-if-then-else 
      (evaluated no) 
      (det-if-then-else 
        (, 
          (ex $ID $Fact +) 
          (solve-once $Fact fail $_)) fail True) 
      (det-if-then-else 
        (, 
          (ex $ID $Fact +) 
          (prooftrees $ID fail $_)) fail True)))



;
; ***********************************************************************
;
; *
;
; * predicate:compute_evaluation/3
;
; *
;
; * syntax: compute_evaluation(+Klist,+Plist,-Klist)
;
; *
;
; * args: Klist ... list of kb-entries [known(I,H,B,Clist,Label,E),...]
;
; * where E is the evaluation of clause I
;
; * Plist ... list of all successfule Proofs using Klist
;
; * = [I:Proofs,...] where prooftrees(I,success,Proofs) in kb
;
; *
;
; * description: computes for each kb-entry in Klist the evaluation
;
; * E = evaluation(RA,NPos,Pos,NNeg,Neg,UNPos,UPos,UNNeg,UNeg), where
;
; * RA ... #applications of the clause
;
; * NPos ... #definitively positive examples covered by the clause
;
; * Pos ... list of definitively positive examples covered by the clause
;
; * NNeg ... #definitively negative examples covered by the clause
;
; * Neg ... list of definitively negative examples covered by the clause
;
; * UNPos ... #probably positive examples covered by the clause
;
; * i.e. instantiations of the clause used in successful proofs of positive
;
; * examples
;
; * UPos ... list of probably positive examples covered by the clause
;
; * UNNeg ... #probably negative examples covered by the clause
;
; * i.e. instantiations of the clause used in successful proofs of negative
;
; * examples
;
; * UNeg ... list of probably negative examples covered by the clause
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


  (= (compute-evaluation  () $_ ())  True)
  (= (compute-evaluation (Cons  (known $I $H $B $Clist $O $_) $R) $Plist (Cons  (known $I $H $B $Clist $O (evaluation $RA $NPos $Pos $NNeg $Neg $UNPos $UPos $UNNeg $UNeg)) $R1))  
    (compute-evaluation $R $Plist $R1)
    (compute-eval $Plist $I $RA $NPos $Pos $NNeg $Neg $UNPos $UPos $UNNeg $UNeg))



  (= (compute-eval  () $_ 0 0 () 0 () 0 () 0 ())  True)
  (= (compute-eval (Cons  (with-self  $I $Proofs) $R) $J $RA $NPos $Pos $NNeg $Neg $UNPos $UPos $UNNeg $UNeg)  
    (compute-eval $R $J $RA0 $NPos0 $Pos0 $NNeg0 $Neg0 $UNPos0 $UPos0 $UNNeg0 $UNeg0)
    (det-if-then-else 
      (ex $I $_ -) 
      (, 
        (compute-eval $Proofs t $I $J $RA0 $RA $NNeg0 $NNeg $Neg0 $Neg $UNNeg0 $UNNeg $UNeg0 $UNeg) 
        (= $NPos $NPos0) 
        (= $Pos $Pos0) 
        (= $UNPos $UNPos0) 
        (= $UPos $UPos0)) 
      (, 
        (compute-eval $Proofs t $I $J $RA0 $RA $NPos0 $NPos $Pos0 $Pos $UNPos0 $UNPos $UPos0 $UPos) 
        (= $NNeg $NNeg0) 
        (= $Neg $Neg0) 
        (= $UNNeg $UNNeg0) 
        (= $UNeg $UNeg0))))

  (= (compute-eval  () $_ $_ $_ $RA $RA $N $N $L $L $UN $UN $UL $UL)  True)
  (= (compute-eval (Cons  (:: $I $H $B) $R) $T $K $J $RA $RA3 $N $N3 $L $L3 $UN $UN3 $UL $UL3)  
    (compute-eval $R $T $K $J $RA $RA1 $N $N1 $L $L1 $UN $UN1 $UL $UL1)
    (compute-eval $B b $K $J $RA1 $RA2 $N1 $N2 $L1 $L2 $UN1 $UN2 $UL1 $UL2)
    (det-if-then-else 
      (== $I $J) 
      (, 
        (is $RA3 
          (+ $RA2 1)) 
        (det-if-then-else 
          (== $T t) 
          (det-if-then-else 
            (member 
              (with-self  $_ $H) $L2) 
            (, 
              (= $L3 $L2) 
              (= $N3 $N2) 
              (= $UN3 $UN2) 
              (= $UL3 $UL2)) 
            (, 
              (is $N3 
                (+ $N2 1)) 
              (= $L3 
                (Cons  
                  (with-self  $K $H) $L2)) 
              (= $UN3 $UN2) 
              (= $UL3 $UL2))) 
          (det-if-then-else 
            (member 
              (with-self  $_ $H) $UL2) 
            (, 
              (= $L3 $L2) 
              (= $N3 $N2) 
              (= $UN3 $UN2) 
              (= $UL3 $UL2)) 
            (, 
              (is $UN3 
                (+ $UN2 1)) 
              (= $UL3 
                (Cons  
                  (with-self  $K $H) $UL2)) 
              (= $N3 $N2) 
              (= $L3 $L2))))) 
      (, 
        (= $RA3 $RA2) 
        (= $L3 $L2) 
        (= $N3 $N2) 
        (= $UN3 $UN2) 
        (= $UL3 $UL2))))


;
; ***********************************************************************
;
; *
;
; * predicate: covered_pos_examples/1
;
; *
;
; * syntax: covered_examples(-CE)
;
; *
;
; * args: CE ... list of IDs of covered positive examples
;
; *
;
; * description: returns IDs of all covered positive examples
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


  (= (covered-pos-examples $Bag)  
    (det-if-then-else 
      (evaluated no) eval-examples True)
    (findall $ID 
      (, 
        (get-example $ID $_ +) 
        (prooftrees $ID success $_)) $Bag)
    (set-det))


;
; ***********************************************************************
;
; *
;
; * predicate: covered_neg_examples/1
;
; *
;
; * syntax: covered_neg_examples(-CE)
;
; *
;
; * args: CE ... list of IDs of covered negative examples
;
; *
;
; * description: returns IDs of all covered negative examples
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


  (= (covered-neg-examples $Bag)  
    (det-if-then-else 
      (evaluated no) eval-examples True)
    (findall $ID 
      (, 
        (get-example $ID $_ -) 
        (prooftrees $ID success $_)) $Bag)
    (set-det))



;
; ***********************************************************************
;
; *
;
; * predicate: all_covered_examples/1
;
; *
;
; * syntax: all_covered_examples(-CE)
;
; *
;
; * args: CE ... list of IDs of covered negative examples
;
; *
;
; * description: returns IDs of all covered examples
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


  (= (all-covered-examples $Bag)  
    (det-if-then-else 
      (evaluated no) eval-examples True)
    (findall $ID 
      (, 
        (get-example $ID $_ $_) 
        (prooftrees $ID success $_)) $Bag)
    (set-det))



;
; ***********************************************************************
;
; *
;
; * predicate: complexity/2
;
; *
;
; * syntax: complexity(+ClauseID,-Size)
;
; *
;
; * args:
;
; *
;
; * description: for kb references
;
; * complexity/2 calculates the size of a clause,
;
; * defined to be the number of constant and function
;
; * symbol occurences in the literals of the clause.
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


  (= (complexity $I $C)  
    (integer $I)
    (get-clause $I $_ $_ $Clause $_)
    (compute-complexity $Clause $C)
    (set-det))



;
; ***********************************************************************
;
; *
;
; * predicate: complexity/2
;
; *
;
; * syntax: complexity(+CL,-Size)
;
; *
;
; * args:
;
; *
;
; * description: for clauses in list representation
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
; ***********************************************************************

  (= (complexity $Clause $C)  
    (= $Clause 
      (Cons  
        (with-self  $H p) $_))
    (compute-complexity $Clause $C)
    (set-det))


;
; ***********************************************************************
;
; *
;
; * predicate: complexity/2
;
; *
;
; * syntax: complexity(List_of_ClauseIDs,-Size)
;
; *
;
; * args:
;
; *
;
; * description: for a list of kb references
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
; ***********************************************************************
 
  (= (complexity (Cons  $ID $List) $C)  
    (integer $ID)
    (findall $Com 
      (, 
        (member $I 
          (Cons  $ID $List)) 
        (get-clause $I $_ $_ $Clause $_) 
        (compute-complexity $Clause $Com)) $Bag)
    (sum $Bag $C)
    (set-det))
	

;
; ***********************************************************************
;
; *
;
; * predicate: complexity/2
;
; *
;
; * syntax: complexity(+Term,-Size)
;
; *
;
; * args:
;
; *
;
; * description: for arbitrary MeTTa terms ( but not integers)
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

  (= (complexity $Term $Complexity)  
    (term-size $Term $Complexity)
    (set-det))



;
; ***********************************************************************
;
; *
;
; * predicate: complexity/2
;
; *
;
; * syntax: complexity(+usr,-Size)
;
; *
;
; * args:
;
; *
;
; * description: for all clauses with label usr
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

  (= (complexity usr $C)  
    (findall $I 
      (, 
        (get-clause $_ $_ $_ $Clause usr) 
        (compute-complexity $Clause $I)) $Bag)
    (sum $Bag $C)
    (set-det))


;
; ***********************************************************************
;
; *
;
; * predicate: complexity/2
;
; *
;
; * syntax: complexity(+examples,-Size)
;
; *
;
; * args:
;
; *
;
; * description: for all examples
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

  (= (complexity examples $C)  
    (findall $I 
      (, 
        (get-example $_ $Clause $_) 
        (compute-complexity $Clause $I)) $Bag)
    (sum $Bag $C)
    (set-det))



;
; ***********************************************************************
;
; *
;
; * predicate: compute_complexity/2
;
; *
;
; * syntax: compute_complexity(+CL,-Size)
;
; *
;
; * args: CL ... clause in list represenation
;
; *
;
; * description: complexity for a clause in list representation
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


  (= (compute-complexity  () 0)  True)
  (= (compute-complexity (Cons  (with-self  $L $_) $More) $C)  
    (term-size $L $C1)
    (compute-complexity $More $C2)
    (is $C 
      (+ $C1 $C2)))


;
; ***********************************************************************
;
; *
;
; * predicate: ivonTunterE/1
;
; *
;
; * syntax: ivonTunterE(-ITE)
;
; *
;
; * args: ITE... information content of T, given E
;
; * (only for funtion-free T and E!!)
;
; *
;
; * description: Given evidence E for T. Then if T|=E, then
;
; * I(T|E) = I(T) + I(E|T). If T = B & H, then T compresses
;
; * the examples E if I(T|E) =< I(B & E)
;
; * Precondition: B, T, E function-free!
;
; *
;
; * How to compute I(T) and I(E|T) (for function-free T,E):
;
; * - I(E|T) = log2( (|M+(T)| |E+|) ) + log2( (|M-(T)| |E-(T)|) )
;
; * - P(T) .. #Pred. Symbols in T
;
; * C(T) .. #Constants in T
;
; * V(T) .. max number of vars of any clause in T
;
; * a .. max arity of any pred. symbol in T
;
; * l .. max cardinality of the body of any clause in T
;
; * |T| .. #clauses in T
;
; *
;
; * |A(T)| =< P(T)*(C(T) + V(T))^a
;
; * |CL(T)| =< |A(T)| * (|A(T)| l)
;
; * I(T) = log2( (|CL(T)| |T|) )
;
; * where (a b) == (a) = n!/(k!*(n-k)!)
;
; * (b)
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
; * see also: L. DeRaedth, S. Muggleton: ILP: Theory and Methods
;
; * submitted to Journal of LP
;
; *
;
; ***********************************************************************


  (= (ivonTunterE $ITE)  
    (findall 
      (/ $P $A) 
      (, 
        (get-clause $_ $_ $_ $CL $_) 
        (member 
          (with-self  $F $_) $CL) 
        (functor $F $P $A)) $Predlist0)
    (make-unique $Predlist0 $Predlist)
    (max-arity $Predlist $A)
    (length $Predlist $PT)
    (findall $C 
      (, 
        (get-clause $_ $H $B $_ $_) 
        (sub-term $C 
          (, $H $B)) 
        (atomic $C) 
        (\== $C True)) $Clist0)
    (make-unique $Clist0 $Clist)
    (length $Clist $CT)
    (findall 
      (/ $V $L2) 
      (, 
        (get-clause $_ $_ $_ $CL $_) 
        (length $CL $L1) 
        (is $L2 
          (- $L1 1)) 
        (vars $CL $VL) 
        (length $VL $V)) $LList)
    (maxvars $LList $VT)
    (max-arity $LList $L)
    (findall $ID 
      (get-clause $ID $_ $_ $_ $_) $IDL)
    (length $IDL $BT)
    (ivonT $PT $CT $VT $A $L $BT $IT)
    (ivonEunterT $Predlist $CT $IET)
    (is $ITE 
      (+ $IT $IET)))




  (= (ivonT $PT0 $CT0 $VT0 $A0 $L0 $BT0 $IT)  
    (is $PT 
      (float $PT0))
    (is $CT 
      (float $CT0))
    (is $VT 
      (float $VT0))
    (is $A 
      (float $A0))
    (is $L 
      (float $L0))
    (is $BT 
      (float $BT0))
    (is $X1 
      (+ $CT $VT))
    (pow $X1 $A $X2)
    (is $AT 
      (* $PT $X2))
    (nueberk $AT $L $X3)
    (is $CLT 
      (* $AT $X3))
    (log2nueberk $CLT $BT $IT))


  (= (ivonEunterT $Predlist $CT $IET)  
    (all-symbols $Predlist $CT $HT0)
    (mTplus $CT $MTP0)
    (is $HT 
      (float $HT0))
    (is $MTP 
      (float $MTP0))
    (is $MTM 
      (- $HT $MTP))
    (findall $P 
      (get-example $_ $P +) $PL)
    (length $PL $PLN0)
    (findall $N 
      (get-example $_ $N -) $NL)
    (length $NL $NLN0)
    (is $PLN 
      (float $PLN0))
    (is $NLN 
      (float $NLN0))
    (log2nueberk $MTP $PLN $LX)
    (log2nueberk $MTM $NLN $LY)
    (is $IET 
      (+ $LX $LY)))

;
; ***********************************************************************
;
; *
;
; * predicate: ivonBundE/1
;
; *
;
; * syntax: ivonBundE(-IBE)
;
; *
;
; * args: ITE... information content of B & E
;
; * (only for funtion-free B and E!!)
;
; *
;
; * description: computes information content of B & E.
;
; * If T = B & H, then T compresses
;
; * the examples E if I(T|E) =< I(B & E)
;
; * Precondition: B, T, E function-free!
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
; * see also: L. DeRaedth, S. Muggleton: ILP: Theory and Methods
;
; * submitted to Journal of LP
;
; *
;
; ***********************************************************************


  (= (ivonBundE $IBE)  
    (findall 
      (/ $P $A) 
      (, 
        (get-clause $_ $_ $_ $CL $_) 
        (member 
          (with-self  $F $_) $CL) 
        (functor $F $P $A)) $Predlist00)
    (findall 
      (/ $P1 $A1) 
      (, 
        (get-example $_ $F $_) 
        (functor $F $P1 $A1)) $Predlist01)
    (append $Predlist00 $Predlist01 $Predlist0)
    (make-unique $Predlist0 $Predlist)
    (max-arity $Predlist $A)
    (length $Predlist $PT)
    (findall $C 
      (, 
        (get-clause $_ $H $B $_ $_) 
        (sub-term $C 
          (, $H $B)) 
        (atomic $C) 
        (\== $C True)) $Clist00)
    (findall $C1 
      (, 
        (get-example $_ $H1 $_) 
        (sub-term $C1 $H1) 
        (atomic $C1) 
        (\== $C1 True)) $Clist01)
    (append $Clist00 $Clist01 $Clist0)
    (make-unique $Clist0 $Clist)
    (length $Clist $CT)
    (findall 
      (/ $V $L2) 
      (, 
        (get-clause $_ $_ $_ $CL $_) 
        (length $CL $L1) 
        (is $L2 
          (- $L1 1)) 
        (vars $CL $VL) 
        (length $VL $V)) $LList)
    (maxvars $LList $VT)
    (max-arity $LList $L)
    (findall $ID 
      (get-clause $ID $_ $_ $_ $_) $IDL00)
    (findall $ID1 
      (get-example $ID1 $_ $_) $IDL01)
    (append $IDL00 $IDL01 $IDL)
    (length $IDL $BT)
    (ivonT $PT $CT $VT $A $L $BT $IBE))


;
; ***********************************************************************
;
; *
;
; * predicate: all_atoms/3
;
; *
;
; * syntax: all_atoms(+Predlist,+No_constants,-No_atoms)
;
; *
;
; * args: Predlist = [p1/arity1,...,pn/arityn] list of pred. symbols and
;
; * their arities
;
; * No_constants.... number c of constants in the current theory
;
; * No_atoms = number of atoms that can be built from the preds
;
; * in predlist and the c constants
;
; * = c^arity1 + .... + c^arityn
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


  (= (all-symbols  () $_ 0)  True)
  (= (all-symbols (Cons  (/ $_ $A0) $R) $CT0 $HT)  
    (all-symbols $R $CT0 $HT0)
    (is $CT 
      (float $CT0))
    (is $A 
      (float $A0))
    (pow $CT $A $X)
    (is $HT 
      (+ $HT0 $X)))


;
; ***********************************************************************
;
; *
;
; * predicate: max_arity/2, maxvars/2, maxi/3
;
; *
;
; * syntax: max_arity(+Plist,-A), maxvars(+Vlist,-V), maxi(+X,+Y,-Z)
;
; *
;
; * args: Plist = [_/n1,...,_/nn] for numbers ni, A is the max of the ni
;
; * Vlist = [_/n1,...,_/nn] for numbers ni, V is the max of the ni
;
; * Z is the max of X and Y
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


  (= (max-arity (:: (/ $_ $A)) $A)  
    (set-det))   
  (= (max-arity (Cons  (/ $_ $A) $R) $C)  
    (max-arity $R $B)
    (maxi $A $B $C))


  (= (maxvars (:: (/ $A $_)) $A)  
    (set-det))
  (= (maxvars (Cons  (/ $A $_) $R) $C)  
    (maxvars $R $B)
    (maxi $A $B $C))


  (= (maxi $A $B $C)  
    (det-if-then-else 
      (>= $A $B) 
      (= $C $A) 
      (= $C $B)))

;
; ***********************************************************************
;
; *
;
; * predicate: herbrand_base_ff/1
;
; *
;
; * syntax: herbrand_base_ff(-M)
;
; *
;
; * args: M .. reduced list of atoms entailed by the current
;
; * function-free theory
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


  (= (herbrand-base-ff $M)  
    (findall $H 
      (get-clause $_ $H True $_ $_) $M00)
    (reduce-hb $M00 $M0)
    (findall $ID 
      (, 
        (get-clause $ID $_ $B $_ $_) 
        (\== $B True)) $IDlist)
    (herbrand-base-ff $IDlist $M0 $M))

  (= (herbrand-base-ff $IDlist $M $M2)  
    (herbrand-base-ff $IDlist $M $M $M1 $Mark)
    (det-if-then-else 
      (== $Mark changed) 
      (herbrand-base-ff $IDlist $M1 $M2) 
      (= $M2 $M1)))

  (= (herbrand-base-ff  () $_ $M $M not_changed)  True)
  (= (herbrand-base-ff (Cons  $ID $R) $M $M1 $M4 $Mark)  
    (herbrand-base-ff $R $M $M1 $M2 $Mark0)
    (get-clause $ID $H $B $_ $_)
    (findall $H1 
      (match-body $H $B $M $H1) $HL)
    (append $HL $M2 $M3)
    (make-unique $M3 $M31)
    (reduce-hb $M31 $M4)
    (det-if-then-else 
      (remove-variant $M2 $M4 Nil) 
      (= $Mark $Mark0) 
      (= $Mark changed)))


  (= (match-body $H $B $M $H1)  
    (copy-term 
      (, $H $B) 
      (, $H1 $B1))
    (copy-term $M $M1)
    (match-body $B1 $M1))

  (= (match-body (, $A $B) $M)  
    (set-det)
    (member $A $M)
    (match-body $B $M))
  (= (match-body $A $M)  
    (member $A $M))


  (= (reduce-hb $L $L1)  
    (reduce-hb $L $L $L1))
  (= (reduce-hb  () $_ ())  True)
  (= (reduce-hb (Cons  $H $R) $L $R2)  
    (reduce-hb $R $L $R1)
    (det-if-then-else 
      (not (sub-contained-in $H $L)) 
      (= $R2 
        (Cons  $H $R1)) 
      (= $R2 $R1)))


  (= (sub-contained-in $H (Cons  $H1 $R))  
    (det-if-then-else 
      (, 
        (\== $H1 $H) 
        (subsumes-chk $H1 $H)) True 
      (sub-contained-in $H $R)))


;
; ***********************************************************************
;
; *
;
; * predicate: mTplus/2
;
; *
;
; * syntax: mTplus(+No_constants,-MT)
;
; *
;
; * args: No_constants... number c of constants in T
;
; * MT ... size of M+(T) for theory T
;
; *
;
; * description: determines first the reduced Herbrand base of T, i.e.
;
; * a list [A1,...,An] where Ai are atoms that might contain variables.
;
; * The size of M+(T) is then
;
; * |vars(A1)|^c + .... + |vars(An)|^c
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


  (= (mTplus $CT $MT)  
    (herbrand-base-ff $M)
    (hb-plus $M $CT $MT))


  (= (hb-plus  () $_ 0)  True)
  (= (hb-plus (Cons  $T $R) $CT0 $MT)  
    (hb-plus $R $CT0 $MT1)
    (vars $T $V)
    (length $V $VN0)
    (is $CT 
      (float $CT0))
    (is $VN 
      (float $VN0))
    (pow $CT $VN $X)
    (is $MT 
      (+ $MT1 $X)))


;
; ***********************************************************************
;
; *
;
; * predicate: code_length/2
;
; *
;
; * syntax: code_length(+Term,-CL)
;
; *
;
; * args: CL .. code length of Term
;
; *
;
; * description: code length of a term a la R. Wirth/S. Muggleton:
;
; * let sym(Term) be all symbols in Term, and N the number of
;
; * all symbol occurrences in Term. Let ps be the relative
;
; * frequency of symbol s in Term. Then
;
; * code_length(Term)= N * sum_{s in sym(Term)} -ps log2ps
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


  (= (code-length $S $L)  
    (skolemize $S $_ $S0)
    (symbol-frequencies $S0 Nil $SymS)
    (relative-frequencies $SymS 0 $N $SymS1)
    (code-length1 $SymS1 $L0)
    (is $L 
      (* $N $L0)))


  (= (code-length1  () 0)  True)
  (= (code-length1 (Cons  $F $R) $L)  
    (code-length1 $R $L0)
    (log2 $F $LF)
    (is $L1 
      (* $F 
        (- $LF)))
    (is $L 
      (+ $L0 $L1)))


  (= (relative-frequencies  () $N $N ())  True)
  (= (relative-frequencies (Cons  (/ (/ $_ $_) $M) $R) $N0 $N (Cons  $RM $R1))  
    (is $N1 
      (+ $N0 $M))
    (relative-frequencies $R $N1 $N $R1)
    (is $RM 
      (/ $M $N)))


  (= (symbol-frequencies $X $L $L1)  
    (atomic $X)
    (set-det)
    (update-frequency-list $L $X 0 $L1))
  (= (symbol-frequencies $X $L $L1)  
    (functor $X $F $N)
    (update-frequency-list $L $F $N $L0)
    (symbol-frequencies $N $X $L0 $L1))

  (= (symbol-frequencies 0 $_ $L $L)  
    (set-det))
  (= (symbol-frequencies $N $X $L $L2)  
    (is $N1 
      (- $N 1))
    (symbol-frequencies $N1 $X $L $L1)
    (arg $N $X $Xn)
    (symbol-frequencies $Xn $L1 $L2))


  (= (update-frequency-list  () $F $N ((/  (/  $F $N) 1)))  True)
  (= (update-frequency-list (Cons  (/ (/ $F $N) $M) $R) $F $N (Cons  (/ (/ $F $N) $M1) $R))  
    (set-det)
    (is $M1 
      (+ $M 1)))
  (= (update-frequency-list (Cons  $X $R) $F $N (Cons  $X $R1))  
    (update-frequency-list $R $F $N $R1))



;
; ***********************************************************************
;
; *
;
; * predicate: encoding_length_examples/1, encoding_length_clause/2
;
; *
;
; * syntax: encoding_length_examples(-EE)
;
; * encoding_length_clause(+CL,-EC)
;
; *
;
; * args: EE, EC.. floats
;
; * CL... clause in list representation
;
; *
;
; * description: encoding length a la Quinlan:
;
; * for examples: PN.. no of pos ex., NN.. no. of neg ex, U = PN + NN
;
; * EE = log2(U) + log2((U PN))
;
; * for clauses: N.. length of Clause, Preds.. no of preds,
;
; * A .. no of poss. args
;
; * EC = (sum_{i=1}^{N} bits for literal i)/log2(N!)
;
; * bits for literali = 1 + log2(Preds) + log2(A)
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


  (= (encoding-length-examples $X)  
    (mysetof $ID 
      (^ $F 
        (get-example $ID $F +)) $PL)
    (length $PL $PN)
    (mysetof $ID1 
      (^ $F1 
        (get-example $ID1 $F1 -)) $NL)
    (length $NL $NN)
    (is $U 
      (+ $PN $NN))
    (log2 $U $LU)
    (is $U1 
      (float $U))
    (is $PN1 
      (float $PN))
    (log2nueberk $U1 $PN1 $Y)
    (is $X 
      (+ $LU $Y)))


  (= (encoding-length-clause $CL $EL)  
    (length $CL $N)
    (is $N1 
      (float $N))
    (sum-of-logs 1.0 $N1 $LNF)
    (encoding-length-lits $CL $Lits0)
    (get-predlist $PList)
    (length $PList $Preds)
    (log2 $Preds $LPreds)
    (is $Lits 
      (+ 
        (+ $Lits0 $N) 
        (* $N $LPreds)))
    (is $EL 
      (/ $Lits $LNF)))


  (= (encoding-length-lits (Cons  (with-self  $H p) $R) $M)  
    (functor $H $_ $N)
    (=.. $H 
      (Cons  $_ $Args))
    (log2 $N $LN)
    (encoding-length-lits $R $Args $M1)
    (is $M 
      (+ $M1 $LN)))

  (= (encoding-length-lits (Cons  (with-self  $L $_) $R) $Args $M)  
    (length $Args $LA)
    (log2 $LA $M0)
    (=.. $L 
      (Cons  $_ $Args1))
    (append $Args1 $Args $Args2)
    (identical-make-unique $Args2 $Args3)
    (encoding-length-lits $R $Args3 $M1)
    (is $M 
      (+ $M0 $M1)))
  (= (encoding-length-lits  () $_ 0)  True) 
   


