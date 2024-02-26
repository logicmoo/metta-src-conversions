;                                           (convert-to-metta-file  environment $_22828 miles/environment.pl miles/environment.metta)
;
; MODULE environment EXPORTS


  !(module environment 
    (:: 
      (/ oracle 1) 
      (/ oracle 2) 
      (/ oracle 3) 
      (/ satisfiable 1) 
      (/ ask-for 1) 
      (/ ask-for-ex 1) 
      (/ confirm 2) 
      (/ get-ci 2)))


;
; IMPORTS
  !(use-module 
    (home kb) 
    (:: 
      (/ get-clause 5) 
      (/ delete-clause 1) 
      (/ store-clause 4) 
      (/ interpretable-predicate 1) 
      (/ get-example 3) 
      (/ store-ex 3) 
      (/ rename 3) 
      (/ delete-all 1)))
  !(use-module 
    (home show-utils) 
    (:: 
      (/ show-clauses 1) 
      (/ show-names 0)))
  !(use-module-if-exists 
    (library prompt) 
    (:: (/ prompt 1)))
  !(use-module-if-exists 
    (library ask) 
    (:: 
      (/ yesno 1) 
      (/ yesno 2) 
      (/ ask-chars 4)))
  !(use-module-if-exists 
    (library sets) 
    (:: (/ union 3)))
  !(use-module-if-exists 
    (library subsumes) 
    (:: (/ subsumes-chk 2)))

;
; METAPREDICATES
;
; none


;
; ***********************************************************************
;
; *
;
; * module: envirnonment.pl
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
; *
;
; * description: procedures for oracle interaction
;
; * 1. membership queries - oracle/1
;
; * 2. existential queries - oracle/2
;
; * 3. subset queries - oracle/2
;
; * 4. name queries - oracle/2
;
; * 5. general questions with default answers - oracle/3
;
; *
;
; * see also:
;
; *
;
; *
;
; ***********************************************************************

;
; ***********************************************************************
;
; *
;
; * predicate: satisfiable/1
;
; *
;
; * syntax: satisfiable(+SG_list)
;
; *
;
; * args: SG_list ... list of subgoals [...,[ID,Subgoal,Proof],...]
;
; *
;
; * description: each Subgoal in SG_list is tested on satisfiability.
;
; * The oracle is used if the satisfiability of Subgoal can not be
;
; * decided on the available knowledge
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


  (= (satisfiable  ())  True)
  (= (satisfiable (Cons  (:: $_ $H $_) $R))  
    (ask-for $H)
    (satisfiable $R))



;
; ***********************************************************************
;
; *
;
; * predicate: ask_for/1
;
; *
;
; * syntax: ask_for(+Goal)
;
; *
;
; * args: Goal is a ground atom
;
; *
;
; * description: succeds if Goal is valid in the kb, or declared to be
;
; * valid by the oracle
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


  (= (ask-for $Lit)  
    (det-if-then-else 
      (interpretable-predicate $Lit) 
      (det-if-then-else 
        (or 
          (get-clause $_ $Lit True $_ $_) 
          (get-example $_ $Lit +)) True 
        (det-if-then-else 
          (get-example $_ $Lit -) fail 
          (det-if-then-else 
            (ground $Lit) 
            (det-if-then-else 
              (oracle $Lit) 
              (store-ex $Lit + $_) 
              (, 
                (store-ex $Lit - $_) fail)) fail))) $Lit))



;
; ***********************************************************************
;
; *
;
; * predicate: ask_for_ex/1
;
; *
;
; * syntax: ask_for(+Goal)
;
; *
;
; * args: Goal is an atom
;
; *
;
; * description: succeds if Goal is valid in the kb, or declared to be
;
; * valid by the oracle
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


  (= (ask-for-ex $Lit)  
    (det-if-then-else 
      (interpretable-predicate $Lit) 
      (det-if-then-else 
        (or 
          (get-clause $_ $Lit True $_ $_) 
          (get-example $_ $Lit +)) True 
        (det-if-then-else 
          (get-example $_ $Lit -) fail 
          (det-if-then-else 
            (oracle $Lit $Lit) 
            (store-ex $Lit + $_) fail))) $Lit))


;
; ***********************************************************************
;
; *
;
; * predicate: term_help/0
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
; * description: prompts a help line
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



  (= term-help  
    (prompt 'Please enter a proper PROLOG-term followed by a full-stop and RETURN'))


;
; ***********************************************************************
;
; *
;
; * predicate: oracle/1
;
; *
;
; * syntax: oracle( + Literal)
;
; *
;
; * args:
;
; *
;
; * description: membership queries:
;
; * "Is the following literal always true?" -> succeeds iff oracle answers yes
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
;
; ;;oracle(mappend(A,B,C)):- !,append(A,B,C).

  (= (oracle $Lit)  
    (nl 
      (prompt 'Is the following literal always true:') nl nl 
      (portray-clause $Lit) nl 
      (set-det) 
      (yesno '> (y/n) ')))


;
; ***********************************************************************
;
; *
;
; * predicate: oracle/2
;
; *
;
; * syntax: oracle( + List_of_Clause_Ids, - Example_Id)
;
; *
;
; * args:
;
; *
;
; * description: subset queries
;
; * "Are the following clauses always true?"
;
; * If not, the user might supply a counter example.
;
; *
;
; * example:
;
; *
;
; * peculiarities: fails only if the oracle answers "no" AND does not
;
; * supply a counter example
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************

  (= (oracle (Cons  $Id1 $Rest) $NegexId)  
    (nl 
      (prompt 'Are the following clauses always true:') nl 
      (show-clauses (Cons  $Id1 $Rest)) 
      (set-det) 
      (det-if-then-else 
        (yesno '> (y/n) ') True 
        (det-if-then 
          (yesno '> Would You like to give a counter-example' no) 
          (, repeat 
            (prompt '> Please enter negative example as Prolog-term: ') 
            (read $Ex) 
            (det-if-then-else 
              (= $Ex h) 
              (, term-help fail) 
              (, 
                (store-ex $Ex - $NegexId) 
                (set-det))))))))
           

;
; ***********************************************************************
;
; *
;
; * predicate: oracle/2
;
; *
;
; * syntax: oracle( + PnameAtom, - NewNameAtom)
;
; *
;
; * args:
;
; *
;
; *
;
; * description: name queries
;
; * "How would you like to call predicate pXYZ", where pXYZ is a new predicate.
;
; * The oracle may use every atom as answer. However, the atom "list"
;
; * causes the system to show every known predicate symbol within the knowledge base
;
; *
;
; * example:
;
; *
;
; * peculiarities: the predicate returns the new predicate name, but does not
;
; * replace the old name by the new one within the kb.
;
; *
;
; * see also:
;
; *
;
; ***********************************************************************

  (= (oracle $Pname $Newname)  
    ( (functor $Pname $_ 0) nl 
      (prompt 'How would You like to call predicate ') 
      (write $Pname) 
      (write  ?) 
      (set-det) repeat 
      (ask-chars '> Please enter a name or "list" followed by RETURN' 1 40 $A1) 
      (symbol-chars $A2 $A1) 
      (det-if-then-else 
        (== $A2 list) 
        (, 
          (prompt 'So far the following predicates have been defined in the knowledge-base:') nl show-names fail) 
        (, 
          (= $Newname $A2) 
          (set-det)))))


;
; ***********************************************************************
;
; *
;
; * predicate: oracle/2
;
; *
;
; * syntax: oracle(+Lit, -InstLit)
;
; *
;
; * args:
;
; *
;
; * description: existential queries
;
; * "Is there a correct instance of the following literal?"
;
; * If yes, the oracle supplies an instance -> InstLit
;
; * Else, the predicate fails
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

  (= (oracle $Lit $InstLit)  
    (nl 
      (prompt 'Is there a correct instance of the following literal:') nl nl 
      (portray-clause $Lit) nl 
      (set-det) 
      (yesno '> (y/n) ') repeat 
      (prompt '> Please enter an instance: ') 
      (read $InstLit) 
      (det-if-then-else 
        (= $InstLit h) 
        (, term-help fail) 
        (det-if-then-else 
          (subsumes-chk $Lit $InstLit) 
          (set-det) 
          (, 
            (prompt 'This is no instantiation of the literal!') fail)))))


;
; ***********************************************************************
;
; *
;
; * predicate: oracle/3
;
; *
;
; * syntax: oracle( + QuestionAtom, ? DefaultAtom, - AnswerAtom)
;
; *
;
; * args:
;
; *
;
; * description: general questions with default answers
;
; * If no default is necessary, use '_' as second argument
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

  (= (oracle $Question $Default $Answer)  
    ( (symbol-chars $Question $Qlist) 
      (append 
        (:: 62 32) $Qlist $P1list) 
      (det-if-then-else 
        (var $Default) 
        (symbol-chars $Prompt $P1list) 
        (, 
          (symbol-chars $Default $Dlist) 
          (append $Dlist 
            (:: 93) $D1list) 
          (append $P1list 
            (Cons  32 
              (Cons  91 $D1list)) $P2list) 
          (symbol-chars $Prompt $P2list))) nl 
      (ask-chars $Prompt 0 255 $Alist) 
      (det-if-then-else 
        (, 
          (== $Alist Nil) 
          (nonvar $Default)) 
        (= $Answer $Default) 
        (symbol-chars $Answer $Alist))))


;
; ***********************************************************************
;
; *
;
; * predicate: confirm/2
;
; *
;
; * syntax: confirm(+Clause_IDs,+Oldterm)
;
; *
;
; * args: Clause_IDs .. list of clauseIDs,
;
; * Oldterm.. term of the predicate to be replaced
;
; *
;
; * description: confirm new clauses and rename the new predicate (using the oracle)
;
; * if oracle refuses the new clauses, delete 'em.
;
; * if they are accepted, delete the old ones (see g2_op).
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


  (= (confirm $Clause_ids $L)  
    (oracle $Clause_ids $Ex)
    (var $Ex)
    (functor $L $Oldname $_)
    (oracle $Oldname $Newname)
    (rename $Clause_ids $Oldname $Newname)
    (set-det))

  (= (confirm $Clause_ids $_)  
    ( (delete-all $Clause_ids) nl 
      (write 'New clauses deleted.') fail))



;
; ************************************************************************
;
; *
;
; * predicate: get_ci/2
;
; *
;
; * syntax: get_ci(+L,-L)
;
; *
;
; * args: L ... list of clauseIDs
;
; *
;
; * description: reads the IDs of the Ci used for the g2-operator one
;
; * by one
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


  (= (get-ci $Sofar $CC)  
    (oracle 'Please enter a resolvent ID followed by RETURN' stop $Answer)
    (\== $Answer stop)
    (symbol-chars $Answer $Idc)
    (det-if-then-else 
      (, 
        (number-chars $Id $Idc) 
        (union $Sofar 
          (:: $Id) $Sofarnew)) True 
      (= $Sofarnew $Sofar))
    (set-det)
    (get-ci $Sofarnew $CC))
  (= (get-ci $CC $CC)  
    (set-det))	

