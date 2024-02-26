;                                           (convert_to_metta_file  ebg $_176472 ebg/ebg.pl ebg/ebg.metta)

  (= (prove-1 (, $HEAD $REST))  
    (set-det)
    (prove-1 $HEAD)
    (prove-1 $REST))
; /******************************************************************/
; /* EBG.PRO Last Modification: Fri Jan 14 19:23:23 1994 */
; /* Different meta-interpreters for Mitchell's explanation-based */
; /* generalisation and partial evaluation */
; /******************************************************************/
; ; ; Copyright (c) 1988 Thomas Hoppe ; ; This program is free software; you can redistribute it and/or ; modify it under the terms of the GNU General Public License ; Version 1 as published by the Free Software Foundation. ; ; This program is distributed in the hope that it will be useful, ; but WITHOUT ANY WARRANTY; without even the implied warranty of ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ; GNU General Public License for more details. ; ; You should have received a copy of the GNU General Public ; Licensealong with this program; if not, write to the Free ; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, ; USA. ;
; /******************************************************************/
; /* impl. by : Thomas Hoppe */
; /* Mommsenstr. 50 */
; /* D-10629 Berlin */
; /* F.R.G. */
; /* E-Mail: hoppet@cs.tu-berlin.de */
; /* 1988 */
; /* */
; /* reference : Explanation-Based Generalisation as Resolution */
; /* Theorem Proving, Smadar T. Kedar-Cabelli, L. */
; /* Thorne McCharty, Proceedings of the Fourth */
; /* International Workshop on Machine Learning, */
; /* Irvine, Morgan Kaufmann Publishers, California, */
; /* 1987 */
; /* */
; /* Explanation-Based Generalisation = Partial */
; /* Evaluation, van Harmelen, F., Bundy, A., */
; /* Research Note, AI 36, 1988. */
; /* */
; /******************************************************************/
; ; TH Sun May 30 00:06:23 1993 - made some minor modifications
; /* This is YAP-MeTTa specific */
; ; :- do_not_compile_expressions.
; /******************************************************************/
; /* */
; /* call : prove_1 (+GOAL) */
; /* */
; /* arguments : GOAL = instantiated goal for a proof */
; /* */
; /* properties : backtrackable */
; /* */
; /******************************************************************/
; /* prove_1 implements a PROLOG meta-interpreter, which succeds if */
; /* GOAL is deducible from a PROLOG program. */
; /* In opposite to the literature the last clause is necessary to */
; /* handle PROLOG built-in predicates corret. */
; /******************************************************************/
  (= (prove-1 $FACT)  
    (==  
      (= $FACT true) 
      (get-atoms  &self)))
  (= (prove-1 $GOAL)  
    ( (==  
        (= $GOAL $PREMISSES) 
        (get-atoms  &self)) (prove-1 $PREMISSES)))
  (= (prove-1 $GOAL)  
    (call $GOAL))


  (= (prove-2 (, $HEAD $REST) $PROOF)  
    (set-det)
    (prove-2 $HEAD $HEAD_PROOF)
    (prove-2 $REST $REST_PROOF)
    (append $HEAD_PROOF $REST_PROOF $PROOF))
; /******************************************************************/
; /* */
; /* call : prove_2 (+GOAL,-PROOF) */
; /* */
; /* arguments : GOAL = instantiated goal for a proof */
; /* PROOF = proof tree for GOAL */
; /* */
; /* properties : backtrackable */
; /* */
; /******************************************************************/
; /* prove_2 is the extention of prove_1 to deliver the PROOF for */
; /* the GOAL, if GOAL is deducible from a PROLOG program. */
; /* In opposite to the literature the last clause is necessary to */
; /* handle PROLOG built-in predicates corret. */
; /* On backtracking it gives the next possible prove. */
; /******************************************************************/
  (= (prove-2 $FACT (:: $FACT))  
    (==  
      (= $FACT true) 
      (get-atoms  &self)))
  (= (prove-2 $GOAL $PROOF)  
    ( (==  
        (= $GOAL $PREMISSES) 
        (get-atoms  &self)) 
      (not (== $PREMISSES True)) 
      (prove-2 $PREMISSES $PREM_PROOF) 
      (append 
        (:: $GOAL) 
        (:: $PREM_PROOF) $PROOF)))
  (= (prove-2 $GOAL (:: $GOAL))  
    (call $GOAL))


  (= (prove-3 (, $HEAD $REST) $PROOF)  
    (set-det)
    (prove-3 $HEAD $HEAD_PROOF)
    (prove-3 $REST $REST_PROOF)
    (append $HEAD_PROOF $REST_PROOF $PROOF))
; /******************************************************************/
; /* */
; /* call : prove_3 (+GOAL,-PROOF) */
; /* */
; /* arguments : GOAL = instantiated goal for a proof */
; /* PROOF = proof tree for GOAL */
; /* */
; /* properties : backtrackable */
; /* */
; /******************************************************************/
; /* prove_3 is an extention of prove_2 to deliver generalized */
; /* PROOFS for the GOAL, if GOAL is deducible from a pure PROLOG */
; /* program. */
; /* The call 'not (not clause(FACT,true))' is a special PROLOG */
; /* trick, which succeds if 'clause(FACT,true)' holds, without */
; /* instanciating GOAL. */
; /* A sublist of length 1 in the PROOF denotes a fact, which has */
; /* to be instanciated to fulfill the proof. */
; /* The first element of a list is the subtree goal. */
; /* On backtracking it gives the next possible prove path. */
; /******************************************************************/
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  ebg/ebg.pl 124 8 6850))

  (= (prove-3 $GOAL $PROOF)  
    ( (==  
        (= $GOAL $PREMISSES) 
        (get-atoms  &self)) 
      (not (== $PREMISSES True)) 
      (prove-3 $PREMISSES $PREM_PROOF) 
      (append 
        (:: $GOAL) 
        (:: $PREM_PROOF) $PROOF)))
  (= (prove_3  $GOAL ($GOAL))  True)


  (= (prove-4 (or $HEAD $REST) $PROOF)  
    (set-det)
    (or 
      (prove-4 $HEAD $PROOF) 
      (prove-4 $REST $PROOF)))
; /******************************************************************/
; /* */
; /* call : prove_4 (+GOAL,-FACTS) */
; /* */
; /* arguments : GOAL = uninstantiated goal for a proof */
; /* FACTS = List of FACTS which must hold for a */
; /* proof of GOAL */
; /* */
; /* properties : backtrackable */
; /* */
; /******************************************************************/
; /* prove_4 is an extention of prove_3 to delivers the generalized */
; /* FACTS that must be true for a proof of GOAL, if GOAL is */
; /* deducible from a pure PROLOG program. */
; /* With this predicat a partial evaluation of the theory according*/
; /* to the GOAL is possible */
; /* On backtracking it gives the next partial evaluation. */
; /******************************************************************/ 
  (= (prove-4 (, $HEAD $REST) $PROOF)  
    (set-det)
    (prove-4 $HEAD $HEAD_PROOF)
    (prove-4 $REST $REST_PROOF)
    (append $HEAD_PROOF $REST_PROOF $PROOF))
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  ebg/ebg.pl 155 8 8557))
 
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  ebg/ebg.pl 157 8 8632))

  (= (prove_4  $GOAL ($GOAL))  True)


  (= (prove-5 (, $HEAD $REST) (Cons  $PROOF_HEAD $PROOF_REST) $LIST)  
    (set-det)
    (prove-5 $HEAD $PROOF_HEAD $PROOF_LIST1)
    (prove-5 $REST $PROOF_REST $PROOF_LIST2)
    (append $PROOF_LIST1 $PROOF_LIST2 $LIST))
; /******************************************************************/
; /* */
; /* call : prove_5 (+GOAL,-PROOF,-FACTS) */
; /* */
; /* arguments : GOAL = uninstantiated goal for a proof */
; /* PROOF = proof tree for GOAL */
; /* FACTS = List of FACTS which must hold for a */
; /* proof of GOAL */
; /* */
; /* properties : backtrackable */
; /* */
; /******************************************************************/
; /* prove_5 is a combination of prove_3 and prove_4 to delivers the*/
; /* generalized FACTS that must be true for a proof of GOAL and the*/
; /* proof path, if GOAL is deducible from a pure PROLOG program. */
; /* So one can get the information which predicates must be */
; /* instantiated to a particular prove path. */
; /* On backtracking it gives the next possible prove path and the */
; /* predicats that must be instanciated. */
; /******************************************************************/
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  ebg/ebg.pl 187 8 10387))

  (= (prove-5 $GOAL (:: $GOAL $PROOF) $LIST)  
    ( (==  
        (= $GOAL $PREMISSES) 
        (get-atoms  &self)) 
      (not (== $PREMISSES True)) 
      (prove-5 $PREMISSES $PROOF $LIST)))
  (= (prove_5  $GOAL ($GOAL) ($GOAL))  True)


  (= (prove-6 (, $HEAD $REST) (, $GEN_HEAD $GEN_REST) $LIST)  
    (set-det)
    (prove-6 $HEAD $GEN_HEAD $LIST1)
    (prove-6 $REST $GEN_REST $LIST2)
    (append $LIST1 $LIST2 $LIST))
; /******************************************************************/
; /* */
; /* call : prove_6 (+GOAL1,+GOAL2,-FACTS) */
; /* */
; /* arguments : GOAL1 = instantiated goal for a paticular proof */
; /* GOAL2 = generalized goal */
; /* FACTS = List of FACTS which must hold for a */
; /* proof of GOAL */
; /* */
; /* properties : backtrackable */
; /* */
; /******************************************************************/
; /* prove_6 is an extention of prove_4 to handle the operationality*/
; /* criterion mentioned by Mitchell/Keller/Kedar-Cabelli (1986). */
; /* It delivers the generalized FACTS that must be true for a */
; /* paticular proof of GOAL1, if GOAL1 is deducible from a PROLOG */
; /* program. */
; /* On backtracking it gives the next possible prove path. */
; /******************************************************************/
  (= (prove-6 $GOAL $GEN_GOAL (:: $GEN_GOAL))  
    (operational $GOAL)
    (set-det))
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  ebg/ebg.pl 218 8 12142))



  (= (listify  ($H) $H)  True)
; /******************************************************************/
; /* */
; /* call : listify (LIST,PREMISSES) */
; /* */
; /* arguments : LIST = normal PROLOG list */
; /* PREMISSES = normal PROLOG and-concatenated */
; /* premisse_list */
; /* */
; /* properties : backtrackable, symmetric */
; /* */
; /******************************************************************/
; /* listify builds a PROLOG and-cocatenated premisse list out of */
; /* every normal list, respecively vice versa. */
; /* One predicat must be instantiated. */
; /******************************************************************/
  (= (listify (Cons  $H $R) (, $H $S))  
    (listify $R $S))


  (= (copy $TERM1 $TERM2)  
    ( (add-is-symbol  &self 
        (internal  $TERM1)) 
      (remove-is-symbol  &self 
        (internal  $TERM2)) 
      (set-det)))
; /******************************************************************/
; /* */
; /* call : copy (+TERM1,-TERM2) */
; /* */
; /* arguments : TERM1 = normal PROLOG term */
; /* TERM2 = normal PROLOG term */
; /* */
; /******************************************************************/
; /* copy makes copy's of every PROLOG-Term, with the special */
; /* database trick to ensure that new variables are generated in */
; /* the output term. */
; /******************************************************************/
; ; inst(TERM1,TERM2),


  (= (operational $A)  
    (==  
      (= $A true) 
      (get-atoms  &self)))
; /******************************************************************/
; /* */
; /* call : operational (+TERM) */
; /* */
; /* arguments : TERM = normal PROLOG term */
; /* */
; /******************************************************************/
; /* operational is an predicat for the decision of operaionality */
; /* in EBL-based algorithms. It's definition must be changed */
; /* depending on the operationality criterion's of a particular */
; /* implementation. */
; /* The first clause ensures that normal PROLOG facts are */
; /* operational. */
; /* The second clause is a check for built-in predicats which are */
; /* not defined, but can be evaluated. */
; /******************************************************************/
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  ebg/ebg.pl 277 8 15501))



  (= (ebg $GOAL $RULE)  
    (functor $GOAL $F $N)
    (functor $COPY $F $N)
    (call $GOAL)
    (prove-6 $GOAL $COPY $ZWERG1)
    (listify $ZWERG1 $ZWERG2)
    (= $RULE 
      (= $COPY  $ZWERG2)))
; /******************************************************************/
; /* */
; /* call : ebg (GOAL,RULE) */
; /* */
; /* arguments : GOAL = a goal which is to be proven */
; /* RULE = a generalized rule for the proof */
; /* */
; /******************************************************************/
; /* ebg is a predicate which first proves a goal to find a */
; /* solution, and afterwards it takes the solution and generates a */
; /* rule out of the proof tree. The solution is returned as a */
; /* generalisation for the prove tree. */
; /******************************************************************/


  (= (help)  
    (write 'Load example theories and start EBG with command: [Filename].')
    (nl))


  !(help *)

