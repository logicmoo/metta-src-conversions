;
;                        MODULE kb EXPORTS


  !(module kb 
    (:: 
      (/ init-kb 1) 
      (/ init-kb 2) 
      (/ save-kb 1) 
      (/ consult-kb 1) 
      (/ gen-id 1) 
      (/ id-count 1) 
      (/ store-clause 4) 
      (/ store-clauses 2) 
      (/ store-clauses 3) 
      (/ store-ex 3) 
      (/ get-example 3) 
      (/ get-clause 5) 
      (/ get-fact 4) 
      (/ get-evaluation 2) 
      (/ clear-kb 0) 
      (/ delete-clause 1) 
      (/ delete-example 1) 
      (/ delete-all 1) 
      (/ random-ex 1) 
      (/ two-random-ex 2) 
      (/ two-random-uncovered-ex 2) 
      (/ two-random-ex-from-list 3) 
      (/ i-random-ex 2) 
      (/ shortest-clause 1) 
      (/ shortest-clause 2) 
      (/ two-shortest-clauses 2) 
      (/ two-shortest-clauses 3) 
      (/ shortest-ex 1) 
      (/ shortest-uncovered-ex 1) 
      (/ shortest-uncovered-ex 2) 
      (/ two-shortest-ex 2) 
      (/ two-shortest-uncovered-ex 2) 
      (/ all-shortest-ex 1) 
      (/ all-shortest-uncovered-ex 1) 
      (/ no-rules 0) 
      (/ no-pos-examples 0) 
      (/ no-neg-examples 0) 
      (/ no-examples 0) 
      (/ flatten-kb 0) 
      (/ flatten-rules 0) 
      (/ unflatten-kb 0) 
      (/ delete-covered-examples 0) 
      (/ get-predlist 1) 
      (/ rename 3) 
      (/ known 6) 
      (/ ex 3) 
      (/ assertallz 1) 
      (/ interpretable-predicate 1)))
;                        ; Read, store and evaluate background knowledge

;                        ; and examples, label them 'usr'

;                        ; same as before, using specified label

;                         ; Save knowledge base to qof-file

;                         ; Consult qof-file without additional processing

;                         ; generates a new kb id

;                         ; last generated id

;                         ; Store horn-clause or Clist in kb

;                          ; Store a list of Clauses in kb

;                          ; as store_clauses,but returns clauseIDs

;                          ; Store example in kb

;                          ;          example          |

;                          ; Retrieve clause           | from knowledge base

;                           ;          fact             |

;                           ;          clause-evaluation|

;                           ; Remove everything

;                           ; Remove clauses one by one

;                           ; Remove examples one by one

;                            ; one random pos. example

;                            ; 2 random pos. examples

;                            ; 2 random uncovered pos. examples

;                            ; 2 random examples from given list

;                            ; i random pos. examples

;                             ; shortest clause

;                             ; shortest clause with label

;                             ; 2 shortest clauses

;                             ; 2 shortest clauses with label

;                             ; shortest pos. example

;                              ; shortest uncovered pos. example

;                              ; shortest ex from list

;                              ; 2 shortest pos. examples

;                              ; 2 shortest pos. uncovered ex

;                              ; list of all shortest pos. examples

;                               ; list of all shortest uncovered pos. examples



;
;                        IMPORTS

  !(use-module 
    (home div-utils) 
    (:: 
      (/ body2list 2) 
      (/ mysetof 3)))
  !(use-module 
    (home evaluation) 
    (:: 
      (/ eval-examples 0) 
      (/ complexity 2) 
      (/ evaluated 1) 
      (/ change-evaluated 1)))
  !(use-module 
    (home argument-types) 
    (:: 
      (/ type-restriction 2) 
      (/ verify-types 0)))
  !(use-module 
    (home flatten) 
    (:: 
      (/ flatten-clause 2) 
      (/ unflatten-clause 2)))
  !(use-module 
    (home interpreter) 
    (:: (/ prooftrees 3)))
  !(use-module-if-exists (library prompt))
  !(use-module-if-exists 
    (library ask) 
    (:: (/ yesno 1)))
  !(use-module-if-exists 
    (library basics) 
    (:: 
      (/ nonmember 2) 
      (/ member 2)))
  !(use-module-if-exists 
    (library random) 
    (:: (/ random-select 3)))

;
;                            METAPREDICATES

;
;                             none



  !(dynamic (, (/ id-count 1) (/ ex 3) (/ known 6)))


;
;                             ***********************************************************************

;
;                             *

;
;                             * module: kb.pl

;
;                              *

;
;                              * author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92

;
;                              *

;
;                              * changed:

;
;                              *

;
;                               * description:	- knowledge base handling

;
;                               *              - flatten / unflatten knowledge base ( clauses & examples)

;
;                               *              - heuristics to select examples from kb randomly

;
;                               *		  or according to their complexities.

;
;                               *		  It is assumed that the examples' current evaluation

;
;                                *		  corresponds to the current rules.

;
;                                *

;
;                                * see also:

;
;                                *

;
;                                ***********************************************************************



;
;                                 ***********************************************************************

;
;                                 *

;
;                                 * predicate: 	gen_id/1

;
;                                  *

;
;                                  * syntax:	gen_id(-New)

;
;                                  *

;
;                                  * args: -New kbID

;
;                                  *

;
;                                   * description:	generates a new kb id

;
;                                   *

;
;                                   * example:

;
;                                   *

;
;                                   * peculiarities:

;
;                                    *

;
;                                    * see also:

;
;                                    *

;
;                                    ***********************************************************************



  (= 
    (gen-id $New) 
    ( (remove-atom  &self 
        (id_count  $Old)) 
      (is $New 
        (+ $Old 1)) 
      (add-atom  &self 
        (id_count  $New))))
  (= 
    (gen-id 1) 
    (add-atom  &self 
      (id_count  1)))


;
;                                      ****************************************************************************************

;
;                                      *

;
;                                      *  predicate: init_kb/1,2

;
;                                      *

;
;                                       *  syntax: init_kb (+Filename)

;
;                                       *          init_kb (+Filename, +Label)

;
;                                       *

;
;                                       *  args:

;
;                                       *

;
;                                        *  description: the file Filename may contain Horn clauses "H:-B." and

;
;                                        *       "H.", examples "ex(Fact,Class)" and comments ";* blabla".

;
;                                        *       Examples are stored in the kb as "ex(ID, Fact, Class)",

;
;                                        *       clauses as

;
;                                        *          "known(ID,Head,Body,Clist,Label,evaluation(1,2,3,4,5,6,7,8,9))".

;
;                                         *       where ID ... unique kb identifier (a natural number)

;
;                                         *             Class ... +,-,?

;
;                                         *             Clist ... clause in list representation

;
;                                         *                       [head:p, body1:n, body2:n, body3:r, ...]. Each literal is

;
;                                         *                       marked p(positiv), n(negativ) or r(negativ + redundant)

;
;                                         *             Label ... e.g. the generating operator

;
;                                         *                       default used for init_kb/1: usr

;
;                                         *             evaluation ... of the clauses w.r.t. the examples:

;
;                                         *               1... #applications of the clause

;
;                                         *               2... #definitively positive examples covered by the clause

;
;                                         *               3... list of definitively positive examples covered by the clause

;
;                                         *                    of the form [...exID:Fact........]

;
;                                         *               4... #definitively negative  examples covered by the clause

;
;                                         *               5... list of definitively negative  examples covered by the clause

;
;                                         *                    of the form [...exID:Fact........]

;
;                                         *               6... #probably positive examples covered by the clause

;
;                                         *                    i.e. instantiations of the clause used in successful

;
;                                         *                    proofs of positive examples

;
;                                         *               7... list of probably positive examples covered by the clause

;
;                                         *                    [...exID:Fact........] where exID is the example of which the

;
;                                         *                    proof uses fact as subgoal

;
;                                         *               8... #probably negative  examples covered by the clause

;
;                                         *                    i.e. instantiations of the clause used in successful

;
;                                         *                     proofs of negative examples

;
;                                         *               9... list of probably negative examples covered by the clause

;
;                                         *                    [...exID:Fact........] where exID is the example of which the

;
;                                         *                    proof uses fact as subgoal

;
;                                         *

;
;                                         *       For each example, all possible prooftrees are stored in the kb:

;
;                                         *       "prooftrees(ID,M,Trees)" where M is success or fail and Trees contains

;
;                                         *       all successful or failing proofs of example ID.

;
;                                         *

;
;                                         *       init_kb can be used successively for different files

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:

;
;                                         *

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         *****************************************************************************************



  (= 
    (init-kb $Filename) 
    (init-kb $Filename usr))

  (= 
    (init-kb $Filename $Origin) 
    ( (open $Filename read $S) 
      (repeat) 
      (read $S $Term) 
      (store-term $Term $Origin) 
      (close $S) 
      (set-det) 
      (eval-examples) 
      (verify-types)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	consult_kb/1

;
;                                         *

;
;                                         * syntax:	consult_kb(+ Filename)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	Restore knowledge base from qof-file which was produced by save_kb/1.

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (consult-kb $Filename) 
    ( (clear-kb) $Filename))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	save_kb/1

;
;                                         *

;
;                                         * syntax:	save_kb(+ Filename)

;
;                                         *

;
;                                         * args: Filename: name of a file (.qof)

;
;                                         *

;
;                                         * description:	 Save snapshot of current knowledge base as compiled file

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities: suffix, .qof is recommended for Filename.

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (save-kb $Filename) 
    (save-predicates 
      (:: 
        (/ known 6) 
        (/ ex 3) 
        (/ prooftrees 3) 
        (/ id-count 1) 
        (/ type-restriction 2) 
        (/ evaluated 1)) $Filename))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	clear_kb/0

;
;                                         *

;
;                                         * syntax:	-

;
;                                         *

;
;                                         * args: 	none

;
;                                         *

;
;                                         * description:	deletes all rules and examples from the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (clear-kb) 
    ( (remove-all-atoms  &self 
        (known  $_ $_ $_ $_ $_ $_)) 
      (remove-all-atoms  &self 
        (ex  $_ $_ $_)) 
      (remove-all-atoms  &self 
        (prooftrees  $_ $_ $_)) 
      (remove-all-atoms  &self 
        (id_count  $_)) 
      (remove-all-atoms  &self 
        (type_restriction  $_ $_)) 
      (remove-all-atoms  &self 
        (evaluated  $_))))
;                                         ;nl,yesno('Delete all knowledge and examples (y/n) '),





;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	store_term/2

;
;                                         *

;
;                                         * syntax:	store_term(+Term,+Label)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	stores clause Term or example Term read from the

;
;                                         *		input file during init_kb in the kb using known/6

;
;                                         * 		or ex/3

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (store-term end-of-file $_) 
    (set-det))
  (= 
    (store-term 
      (ex $F $C) $_) 
    ( (gen-id $ID) 
      (add-atom  &self 
        (ex  $ID $F $C)) 
      (set-det) 
      (fail)))
  (= 
    (store-term 
      (= $H $B) $O) 
    ( (body2list $B $L) 
      (gen-id $ID) 
      (add-atom  &self 
        (known  $ID $H $B 
          (Cons  
            (:  $H p) $L) $O 
          (evaluation  0 0 () 0 () 0 () 0 ()))) 
      (set-det) 
      (fail)))
  (= 
    (store-term 
      (type-restriction $M $A) $_) 
    ( (add-atom  &self 
        (:  argument_types 
          (type_restriction  $M $A))) 
      (set-det) 
      (fail)))
  (= 
    (store-term $H $O) 
    ( (gen-id $ID) 
      (add-atom  &self 
        (known  $ID $H true 
          ( (:  $H p)) $O 
          (evaluation  0 0 () 0 () 0 () 0 ()))) 
      (set-det) 
      (fail)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	store_clause/4

;
;                                         *

;
;                                         * syntax:	store_clause (?MeTTa-clause,?clause-list,+label,-ID)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	Store new clause in knowledge base (provide either horn-clause

;
;                                         *	 or clause-list), label it and receive the unique clause-ID.

;
;                                         *       If store_clause is called with ID instantiated, it will fail if ID is

;
;                                         *       already in use in the knowledge-base. If not, ID will be used.

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (store-clause $A $B $_ $ID) 
    (det-if-then-else 
      (, 
        (nonvar $ID) 
        (or 
          (known $ID $_ $_ $_ $_ $_) 
          (ex $ID $_ $_))) 
      (, 
        (set-det) 
        (fail)) 
      (det-if-then-else 
        (, 
          (var $A) 
          (var $B)) 
        (, 
          (set-det) 
          (fail)) fail)))
  (= 
    (store-clause 
      (= $H $B) 
      (Cons  
        (with_self  $H 
          (p)) $L) $Label $ID) 
    ( (body2list $B $L) 
      (det-if-then-else 
        (var $ID) 
        (gen-id $ID) 
        (, 
          (id-count $Top) 
          (=< $ID $Top))) 
      (det-if-then-else 
        (var $Label) 
        (= $Label usr) True) 
      (add-atom  &self 
        (known  $ID $H $B 
          (Cons  
            (:  $H p) $L) $Label 
          (evaluation  0 0 () 0 () 0 () 0 ()))) 
      (set-det) 
      (change-evaluated no)))
  (= 
    (store-clause $H 
      (:: (with_self  $H (p))) $Label $ID) 
    ( (det-if-then-else 
        (var $ID) 
        (gen-id $ID) 
        (, 
          (id-count $Top) 
          (=< $ID $Top))) 
      (det-if-then-else 
        (var $Label) 
        (= $Label usr) True) 
      (add-atom  &self 
        (known  $ID $H true 
          ( (:  $H p)) $Label 
          (evaluation  0 0 () 0 () 0 () 0 ()))) 
      (set-det) 
      (change-evaluated no)))



;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	store_clauses/2

;
;                                         *

;
;                                         * syntax:	store_clauses(+List_of_Clauses,+Label)

;
;                                         *

;
;                                         * args: List_of_Clauses ... list of MeTTa clauses

;
;                                         *

;
;                                         * description:	Same as store_clause/4 for a list of clauses

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (store_clauses  () $_) True)
  (= 
    (store-clauses 
      (Cons  $C $R) $Label) 
    ( (store-clause $C $_ $Label $_) (store-clauses $R $Label)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	store_clauses/3

;
;                                         *

;
;                                         * syntax:	store_clauses(+List_of_Clauses,+Label,-IDlist)

;
;                                         *

;
;                                         * args: List_of_Clauses ... list of MeTTa clauses

;
;                                         *       IDlist... kb-ids for the clauses

;
;                                         *

;
;                                         * description:	Same as store_clauses/2, but returns IDs of the clauses

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************


  (= 
    (store_clauses  () $_ ()) True)
  (= 
    (store-clauses 
      (Cons  $C $R) $Label 
      (Cons  $ID $R1)) 
    ( (store-clause $C $_ $Label $ID) (store-clauses $R $Label $R1)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	store_ex/3

;
;                                         *

;
;                                         * syntax:	store_ex(?fact,?classification,-ID)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	Store new example in knowledge base and receive the

;
;                                         *	unique identification number.

;
;                                         * 	If it is called with ID already instantiated: see above.

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (store-ex $F $Class $ID) 
    ( (ex $ID1 $F1 $Class1) 
      (== $F $F1) 
      (set-det) 
      (= $Class $Class1) 
      (= $ID $ID1)))
  (= 
    (store-ex $F $_ $ID) 
    (det-if-then-else 
      (, 
        (nonvar $ID) 
        (or 
          (ex $ID $_ $_) 
          (known $ID $_ $_ $_ $_ $_))) 
      (, 
        (set-det) 
        (fail)) 
      (det-if-then-else 
        (var $F) 
        (, 
          (set-det) 
          (fail)) fail)))
  (= 
    (store-ex $Fact $Class $ID) 
    ( (det-if-then-else 
        (var $ID) 
        (gen-id $ID) 
        (, 
          (id-count $Top) 
          (=< $ID $Top))) 
      (add-atom  &self 
        (ex  $ID $Fact $Class)) 
      (set-det) 
      (change-evaluated no)))



;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: get_example/3

;
;                                         *            get_clause/5

;
;                                         *            get_fact/4

;
;                                         *            get_evaluation/2

;
;                                         *

;
;                                         * syntax: get_example (? ID, ? Example, ? Classification)

;
;                                         *         get_clause (? ID, ? Head, ? Body, ? Clist, ? Label)

;
;                                         *         get_fact (? ID, ? Fact, ? Clist, ? Label)

;
;                                         *         get_evaluation (+ ID, - Evaluation)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	read example/clause/fact or clause evaluation from the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (get-example $ID $F $C) 
    (ex $ID $F $C))

  (= 
    (get-clause $ID $H $B $L $O) 
    (known $ID $H $B $L $O $_))        

  (= 
    (get-fact $ID $F $L $O) 
    (known $ID $F True $L $O $_))

  (= 
    (get-evaluation $ID $Eval) 
    (known $ID $_ $_ $_ $_ $Eval))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	delete_clause/1 , delete_example/1, delete_all/1

;
;                                         *

;
;                                         * syntax: delete_clause(+ ID) , delete_example(+ ID),

;
;                                         *         delete_all(+list_of_clauseIDs)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	delete clause(s)/example(s) with identifier(s) ID(list_of_clauseIDs)

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (delete-clause $ID) 
    ( (remove-atom  &self 
        (known  $ID $_ $_ $_ $_ $_)) (change-evaluated no)))

  (= 
    (delete-example $ID) 
    ( (remove-atom  &self 
        (ex  $ID $_ $_)) (change-evaluated no)))



  (= 
    (delete-all Nil) 
    (set-det)) 
  (= 
    (delete-all (Cons  $Id1 $Rest)) 
    ( (delete-clause $Id1) 
      (set-det) 
      (delete-all $Rest)))
  (= 
    (delete-all (Cons  $Id1 $Rest)) 
    ( (delete-example $Id1) 
      (set-det) 
      (delete-all $Rest)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: interpretable_predicate/1

;
;                                         *

;
;                                         * syntax:	interpretable_predicate(-Term)

;
;                                         *

;
;                                         * args: Term .. MeTTa term with principal funtor P/N

;
;                                         *

;
;                                         * description:	succeeds if rules or examples for P/N are in the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (interpretable-predicate $A) 
    ( (functor $A $F $N) 
      (functor $A1 $F $N) 
      (or 
        (get-clause $_ $A1 $_ $_ $_) 
        (get-example $_ $A1 $_))))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: assertallz/1

;
;                                         *

;
;                                         * syntax:    assertallz(+List)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	asserts all elements of List at the end of the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (assertallz  ()) True)
  (= 
    (assertallz (Cons  $X $R)) 
    ( (add-atom  &self $X) (assertallz $R)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	rename_clause/3

;
;                                         *

;
;                                         * syntax:	rename (+ ID_list,+ Old_name,+ New_name )

;
;                                         *

;
;                                         * args: Old_name, New_name ... atoms

;
;                                         *

;
;                                         * description: rename	every occurence of predicate 'Old_name' to 'New_name'

;
;                                         *		in a set of clauses given as

;
;                                         * 		a list of kb-references (Id-list). 'New_name' should be atomic.

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (rename Nil $_ $_) 
    (set-det))
  (= 
    (rename 
      (Cons  $Id1 $Rest) $Old $New) 
    ( (get-clause $Id1 $_ $_ $Clist $Label) 
      (rename-clause $Clist $NewClause $Old $New) 
      (delete-clause $Id1) 
      (store-clause $_ $NewClause $Label $Id1) 
      (set-det) 
      (rename $Rest $Old $New)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	rename_clause/4

;
;                                         *

;
;                                         * syntax: rename_clause(+CL,-CL1,+Old,+New)

;
;                                         *

;
;                                         * args: CL,CL1.. clauses in list representation

;
;                                         *       Old, New atoms

;
;                                         *

;
;                                         * description:	replaces each literal Old(...) within CL with New(...)

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (rename-clause Nil Nil $_ $_) 
    (set-det))
  (= 
    (rename-clause 
      (Cons  
        (with_self  $Lit $X) $Rest) 
      (Cons  
        (with_self  $NewLit $X) $NewRest) $Old $New) 
    ( (det-if-then-else 
        (=.. $Lit 
          (Cons  $Old $Args)) 
        (=.. $NewLit 
          (Cons  $New $Args)) 
        (= $NewLit $Lit)) 
      (set-det) 
      (rename-clause $Rest $NewRest $Old $New)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	random_ex/1

;
;                                         *

;
;                                         * syntax: random_ex(-ID)

;
;                                         *

;
;                                         * args: ID exampleID

;
;                                         *

;
;                                         * description:	chooses randomly an example from the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (random-ex $ID1) 
    ( (findall $ID 
        (get-example $ID $_ +) $Bag) (random-select $ID1 $Bag $_)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: two_random_ex/2

;
;                                         *

;
;                                         * syntax: two_random_ex(-ID1,-ID2)

;
;                                         *

;
;                                         * args: ID1,ID2 exampleIDs

;
;                                         *

;
;                                         * description:	chooses randomly two examples from the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (two-random-ex $ID1 $ID2) 
    ( (findall $ID 
        (get-example $ID $_ +) $Bag) 
      (or 
        (random-select $ID1 $Bag $Residue) 
        (select $ID1 $Bag $Residue)) 
      (or 
        (random-select $ID2 $Residue $_) 
        (select $ID2 $Residue $_))))
        ;
;                                          sometimes random_select/3 doesn't work properly



  (= 
    (two-random-ex-from-list $List $ID1 $ID2) 
    ( (random-select $ID1 $List $Residue) (random-select $ID2 $Residue $_)))


  (= 
    (two-random-uncovered-ex $ID1 $ID2) 
    ( (findall $ID 
        (, 
          (with_self  
            (kb) 
            (prooftrees $ID fail $_)) 
          (get-example $ID $_ +)) $Uncovered) (two-random-ex-from-list $Uncovered $ID1 $ID2)))        


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: i_random_ex/2

;
;                                         *

;
;                                         * syntax: i_random_ex(+I,-ExIDs)

;
;                                         *

;
;                                         * args: I .. number

;
;                                         *

;
;                                         * description:	selects randomly I examples from the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (i-random-ex $I $Examples) 
    ( (> $I 0) 
      (findall $ID 
        (get-example $ID $_ +) $Bag) 
      (length $Bag $J) 
      (det-if-then-else 
        (=< $J $I) 
        (= $Examples $Bag) 
        (i-random-ex $I $Bag $Examples))))
  (= 
    (i-random-ex 0 $_ Nil) 
    (set-det))
  (= 
    (i-random-ex $N $Bag 
      (Cons  $ID $Rest)) 
    ( (random-select $ID $Bag $Residue) 
      (is $M 
        (- $N 1)) 
      (i-random-ex $M $Residue $Rest)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: shortest_clause/1

;
;                                         *

;
;                                         * syntax: shortest_clause(-ID:C)

;
;                                         *

;
;                                         * args: ID .. clauseID, C ... complexity of the corresponding clause

;
;                                         *

;
;                                         * description:	selects the shortest clause from the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (shortest-clause (with_self  $ID1 $C1)) 
    (shortest-clause $_ 
      (with_self  $ID1 $C1))) 
        
  (= 
    (shortest-clause $Label 
      (with_self  $ID1 $C1)) 
    ( (findall 
        (with_self  $ID $C) 
        (, 
          (get-clause $ID $_ $_ $Clause $Label) 
          (complexity $Clause $C)) $Bag) (shortest $Bag (with_self  $ID1 $C1) $_)))
       

;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: two_shortest_clauses/2

;
;                                         *

;
;                                         * syntax: two_shortest_clauses(-ID1:CL1,-ID2:CL2)

;
;                                         *

;
;                                         * args: ID1/2 .. clauseIDs, CL1/2 ... complexities of the corresponding clauses

;
;                                         *

;
;                                         * description:	selects two shortest clauses from kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (two-shortest-clauses 
      (with_self  $ID1 $C1) 
      (with_self  $ID2 $C2)) 
    (two-shortest-clauses $_ 
      (with_self  $ID1 $C1) 
      (with_self  $ID2 $C2)))

  (= 
    (two-shortest-clauses $Label 
      (with_self  $ID1 $C1) 
      (with_self  $ID2 $C2)) 
    ( (findall 
        (with_self  $ID $C) 
        (, 
          (get-clause $ID $_ $_ $Clause $Label) 
          (complexity $Clause $C)) $Bag) (two-shortest $Bag (with_self  $ID1 $C1) (with_self  $ID2 $C2))))



;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: shortest_ex/1

;
;                                         *

;
;                                         * syntax: shortest_ex(-ID:C)

;
;                                         *

;
;                                         * args: ID .. exID, C .. complexity of the corresponding example

;
;                                         *

;
;                                         * description:	selects the shortest example from kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (shortest-ex (with_self  $ID1 $C1)) 
    ( (findall 
        (with_self  $ID $C) 
        (, 
          (get-example $ID $Ex +) 
          (complexity $Ex $C)) $Bag) (shortest $Bag (with_self  $ID1 $C1) $_)))
       

;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	two_shortest_ex/2

;
;                                         *

;
;                                         * syntax: two_shortest_ex(-ID1:C1,-ID2:C2)

;
;                                         *

;
;                                         * args: ID1/2 .. exIDs, C1/2 .. complexities of the corresponding examples

;
;                                         *

;
;                                         * description:	selects two shortest example from kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (two-shortest-ex 
      (with_self  $ID1 $C1) 
      (with_self  $ID2 $C2)) 
    ( (findall 
        (with_self  $ID $C) 
        (, 
          (get-example $ID $Ex +) 
          (complexity $Ex $C)) $Bag) (two-shortest $Bag (with_self  $ID1 $C1) (with_self  $ID2 $C2))))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	shortest_uncovered_ex/1

;
;                                         *

;
;                                         * syntax: shortest_uncovered_ex(-ExID)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	selects the shortest example that is not covered by the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (shortest-uncovered-ex $ID1) 
    ( (findall 
        (with_self  $ID $C) 
        (, 
          (with_self  
            (kb) 
            (prooftrees $ID fail $_)) 
          (get-example $ID $Ex +) 
          (complexity $Ex $C)) $Uncovered) (shortest $Uncovered (with_self  $ID1 $_) $Residue)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	shortest_uncovered_ex/2

;
;                                         *

;
;                                         * syntax: shortest_uncovered_ex(+ExIds,-ExId)

;
;                                         *

;
;                                         * args: ExIds .. list of Ids of uncovered examples

;
;                                         *

;
;                                         * description:	selects the shortest example among ExIds

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************


  (= 
    (shortest-uncovered-ex $Uncovered $ID1) 
    ( (add-complexities $Uncovered $Bag) (shortest $Bag (with_self  $ID1 $_) $Residue)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	two_shortest_uncovered_ex/2

;
;                                         *

;
;                                         * syntax: two_shortest_uncovered_ex(-ExID1,-ExID2)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	selects two shortest examples that are not covered by the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (two-shortest-uncovered-ex $ID1 $ID2) 
    ( (findall 
        (with_self  $ID $C) 
        (, 
          (with_self  
            (kb) 
            (prooftrees $ID fail $_)) 
          (get-example $ID $Ex +) 
          (complexity $Ex $C)) $Uncovered) (two-shortest $Uncovered (with_self  $ID1 $_) (with_self  $ID2 $_))))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	all_shortest_ex/1

;
;                                         *

;
;                                         * syntax: all_shortest_ex(-ExIds)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description: selects all shortest examples from kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (all-shortest-ex $Bag) 
    ( (shortest-ex (with_self  $_ $C1)) (findall $ID (, (get-example $ID $Ex +) (complexity $Ex $C1)) $Bag)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	all_shortest_uncovered_ex/1

;
;                                         *

;
;                                         * syntax: all_shortest_uncovered_ex(-ExIds)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description: selects all shortest uncovered examples from kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (all-shortest-uncovered-ex $Bag) 
    ( (findall 
        (with_self  $ID $C) 
        (, 
          (with_self  
            (kb) 
            (prooftrees $ID fail $_)) 
          (get-example $ID $Ex +) 
          (complexity $Ex $C)) $Uncovered) 
      (shortest $Uncovered 
        (with_self  $_ $C1) $_) 
      (findall $ID2 
        (member 
          (with_self  $ID2 $C1) $Uncovered) $Bag)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: two_shortest /3

;
;                                         *

;
;                                         * syntax: two_shortest(+Bag,-ID1:C1,-ID2:C2)

;
;                                         *

;
;                                         * args: Bag = [ ID:C, ...] , where ID refers to example with complexity C

;
;                                         *       ID1/2 ...exampleIDs, C1/2 ... corresponding complexities

;
;                                         *

;
;                                         * description:

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (two-shortest $Bag 
      (with_self  $ID1 $C1) 
      (with_self  $ID2 $C2)) 
    ( (shortest $Bag 
        (with_self  $ID1 $C1) $Residue) 
      (shortest $Residue 
        (with_self  $ID2 $C2) $_) 
      (set-det)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	shortest/3

;
;                                         *

;
;                                         * syntax: shortest(+Bag,-ID:C,-Residue)

;
;                                         *

;
;                                         * args: Bag, Residue = [ ID:C, ...] , where ID is the complexity of ID

;
;                                         *       ID ...kbID, C ... corresponding complexity

;
;                                         *

;
;                                         * description:	selects the shortest ID from Bag wrt complexity, Residue is the rest

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (shortest  
      ( (:  $ID $C)) 
      (:  $ID $C) ()) True)
  (= 
    (shortest 
      (Cons  
        (with_self  $ID1 $C1) $Rest) 
      (with_self  $ID $C) $Residue) 
    ( (shortest $Rest 
        (with_self  $ID2 $C2) $Residue2) 
      (| 
        (det-if-then 
          (< $C1 $C2) 
          (, 
            (= $ID $ID1) 
            (= $C $C1) 
            (= $Residue $Rest))) 
        (det-if-then otherwise 
          (, 
            (= $ID $ID2) 
            (= $C $C2) 
            (= $Residue 
              (Cons  
                (with_self  $ID1 $C1) $Residue2))))) 
      (set-det)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:add_complexities/2

;
;                                         *

;
;                                         * syntax: add_complexities(+L,-Pairs)

;
;                                         *

;
;                                         * args: L = [ID:kb_entry_for_ID,...],

;
;                                         *       Pairs = [ID:complexity_of_kb_entry_for_ID,...]

;
;                                         *

;
;                                         * description:

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (add_complexities  () ()) True)

  (= 
    (add-complexities 
      (Cons  
        (with_self  $ID $Ex) $More) 
      (Cons  
        (with_self  $ID $C) $MorePairs)) 
    ( (complexity $Ex $C) (add-complexities $More $MorePairs)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	no_rules/0, no_pos_examples/0,

;
;                                         *              no_neg_examples/0, no_examples/0

;
;                                         *

;
;                                         * syntax:

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	tests kb on the different properties

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (no-rules) 
    (not (get-clause $_ $_ $_ $_ $_)))


  (= 
    (no-pos-examples) 
    (not (get-example $_ $_ +)))


  (= 
    (no-neg-examples) 
    (not (get-example $_ $_ -)))


  (= 
    (no-examples) 
    (not (get-example $_ $_ $_)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	delete_covered_examples/0

;
;                                         *

;
;                                         * syntax:

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	deletes examples explained by the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (delete-covered-examples) 
    ( (findall $I 
        (, 
          (get-evaluation $I $Eval) 
          (arg 3 $Eval $CoveredEx) 
          (member 
            (with_self  $ID $_) $CoveredEx) 
          (delete-example $ID)) $_) (set-det)))
                     

;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: flatten_rules/0

;
;                                         *

;
;                                         * syntax:

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	flattens all clauses in the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (flatten-rules) 
    ( (findall 
        (with_self  $ID 
          (with_self  $C $Label)) 
        (, 
          (get-clause $ID $_ $_ $C $Label) 
          (delete-clause $ID)) $Bag1) 
      (store-flat-clauses $Bag1) 
      (set-det)))

;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: flatten_kb/0

;
;                                         *

;
;                                         * syntax:

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description: flattens all clauses and examples in the kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (flatten-kb) 
    ( (findall 
        (with_self  $ID 
          (with_self  $C $Label)) 
        (, 
          (get-clause $ID $_ $_ $C $Label) 
          (delete-clause $ID)) $Bag1) 
      (store-flat-clauses $Bag1) 
      (findall 
        (with_self  $ID 
          (with_self  
            (:: (with_self  $C (p))) 
            (ex))) 
        (get-example $ID $C +) $Bag2) 
      (store-flat-clauses $Bag2) 
      (set-det)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: unflatten_kb/0

;
;                                         *

;
;                                         * syntax:

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	unflattens a flat kb

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (unflatten-kb) 
    ( (findall 
        (with_self  $ID 
          (with_self  $C $Label)) 
        (, 
          (get-clause $ID $_ $_ $C $Label) 
          (delete-clause $ID)) $Bag) (store-unflat-clauses $Bag)))

;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: store_flat_clauses/1

;
;                                         *

;
;                                         * syntax: store_flat_clauses(+CL)

;
;                                         *

;
;                                         * args: CL = [ID:C:Label,...] where ID is clause- or exampleID, C is the corresponding

;
;                                         *	 clause in list notation and Label is the clause label or "ex" if examples are

;
;                                         *       flattened

;
;                                         *

;
;                                         * description:	 store flat clauses preferably with their old Id.

;
;                                         *               After flattening, examples become clauses; they get a new Id

;
;                                         *               while their unflat form remains in the kb.

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (store_flat_clauses  ()) True)
  (= 
    (store-flat-clauses (Cons  (with_self  $ID (with_self  $C $Label)) $More)) 
    ( (flatten-clause $C $CFlat) 
      (or 
        (store-clause $_ $CFlat $Label $ID) 
        (store-clause $_ $CFlat $Label $_)) 
      (store-flat-clauses $More)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: store_unflat_clauses/1

;
;                                         *

;
;                                         * syntax: store_unflat_clauses(+CL)

;
;                                         *

;
;                                         * args: CL = [ID:C:Label,...] where ID is a clause- or exampleID, C is the corresponding

;
;                                         *	 clause in list notation and Label is the clause label or "ex"

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	if Label \= ex, C is unflattened and replaced in the kb by

;
;                                         *      the unflat version

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (store_unflat_clauses  ()) True)
  (= 
    (store-unflat-clauses (Cons  (with_self  $ID (with_self  $C (ex))) $More)) 
    ( (set-det) (store-unflat-clauses $More)))
  (= 
    (store-unflat-clauses (Cons  (with_self  $ID (with_self  $C $Label)) $More)) 
    ( (\== $Label ex) 
      (set-det) 
      (unflatten-clause $C $CUnFlat) 
      (store-clause $_ $CUnFlat $Label $ID) 
      (store-unflat-clauses $More)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: get_predlist/1

;
;                                         *

;
;                                         * syntax: get_predlist(-Predlist)

;
;                                         *

;
;                                         * args:  Predlist =  [P:PVars|_]

;
;                                         *

;
;                                         * description: selects all predicates with a type restriction from kb

;
;                                         *        & adapts type restrictions by transfomation in a list [X:Tx,...]

;
;                                         *        of variables X and types Tx

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (get-predlist $Predlist) 
    ( (mysetof 
        (with_self  $P $N) 
        (^ $I 
          (^ $H 
            (^ $B 
              (^ $CL 
                (^ $L 
                  (, 
                    (get-clause $I $H $B $CL $L) 
                    (\== $L type) 
                    (functor $H $P $N))))))) $Plist) (get-pred $Plist $Predlist)))
;                                         ;   mysetof(P:PVars,
;                                         ;           Vars^( type_restriction(P,Vars),
;                                         ;                  adapt_v(Vars,PVars)), Predlist).




  (= 
    (get_pred  () ()) True)
  (= 
    (get-pred 
      (Cons  
        (with_self  $Pred $N) $R) 
      (Cons  
        (with_self  $P $PVars) $R1)) 
    ( (get-pred $R $R1) 
      (functor $P $Pred $N) 
      (det-if-then-else 
        (type-restriction $P $Vars) 
        (adapt-v $Vars $PVars) 
        (, 
          (=.. $P 
            (Cons  $_ $Vars)) 
          (adapt-v1 $Vars $PVars)))))

;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: adapt_v/2

;
;                                         *

;
;                                         * syntax: adapt_v(+TR,-Vars)

;
;                                         *

;
;                                         * args:  TR: [Tx(X),...] type restrictions for variables X of a predicate

;
;                                         *        Vars: [X:Tx,...]

;
;                                         *

;
;                                         * description: transforms a set of type restrictions Tx(X) into

;
;                                         *        a set X:Tx of variables X and types Tx

;
;                                         *

;
;                                         * example: adapt_v([list(A),atom(B)],[A:list,B:atom]

;
;                                         *

;
;                                         * peculiarities:	none

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (adapt_v  () ()) True)
  (= 
    (adapt-v 
      (Cons  $T $R) 
      (Cons  
        (with_self  $X $Tx) $R1)) 
    ( (adapt-v $R $R1) (=.. $T (:: $Tx $X))))


  (= 
    (adapt_v1  () ()) True)
  (= 
    (adapt-v1 
      (Cons  $X $R) 
      (Cons  
        (with_self  $X 
          (all)) $R1)) 
    (adapt-v1 $R $R1))


