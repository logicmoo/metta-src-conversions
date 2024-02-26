;
;                       MODULE flatten EXPORTS


  !(module flatten 
    (:: 
      (/ flatten-term 7) 
      (/ flatten-term 2) 
      (/ flatten-literal 6) 
      (/ flatten-literal 2) 
      (/ flatten-clause 6) 
      (/ unflatten-clause 2) 
      (/ flatten-clause 2) 
      (/ unflatten-clause 3)))

;
;                       IMPORTS

  !(use-module 
    (home div-utils) 
    (:: 
      (/ clist-to-prolog 2) 
      (/ list-to-struct 2)))
  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
  !(use-module-if-exists 
    (library strings) 
    (:: 
      (/ concat-symbol 3) 
      (/ midstring 6) 
      (/ substring 5)))
  !(use-module-if-exists 
    (library occurs) 
    (:: 
      (/ sub-term 2) 
      (/ contains-var 2)))

;
;                         METAPREDICATES

;
;                          none



;
;                         ***********************************************************************

;
;                         *

;
;                          * module: flatten.pl

;
;                          *

;
;                          * author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92

;
;                          *

;
;                          * changed:

;
;                           *

;
;                           * description: Rouveirol's representation change to  function free

;
;                           *		Horn clauses.

;
;                           * 		Shared variables are deteced.

;
;                           *		Following the later versions of flattening('90,'91)

;
;                            *		identical terms are only represented once thru a

;
;                            *		new body literal. The older version (1989) introduced

;
;                            *		for each occurence of a term a unique new body literal.

;
;                            *		( the newer approach might not always be more adequate)

;
;                            *

;
;                             * peculiarities: In the process of flattening all literals that are

;
;                             *		  introduced for functions end with the suffix "_p".

;
;                             *		  In return, when unflattening a clause it is assumed

;
;                             *		  that every predicate symbol ending in "_p" stems from

;
;                             *		  a function. This assumption is made because the names

;
;                              *		  for functions and predicates need to be distinct.

;
;                              *

;
;                              *    DON'T FLATTEN ANY CLAUSE CONTAINING LITERALS ENDING IN "_p" !!!

;
;                              *

;
;                              * see also:

;
;                               *

;
;                               ***********************************************************************



;
;                               ***********************************************************************

;
;                                *

;
;                                * predicate:	flatten_term/7

;
;                                *

;
;                                * syntax:	flatten_term(+Term, +NewVar , +OldSubstituion,

;
;                                *                           -NewSubstitution,+OldBackground,

;
;                                 *                           -NewBackground, -Literals)

;
;                                 *

;
;                                 * args:	Term:	term to be replace by NewVar, e.g. [a,b]

;
;                                 *		NewVar: new variable

;
;                                 * 		OldSubstitution: list of substitutions that have already

;
;                                  *			been performed while flattening a clause.

;
;                                  *			This way shared variables / terms are detected.

;
;                                  *			e.g. [], [ X/a , Y/[b] ]

;
;                                  * 		NewSubstitution: = OldSubstitution + [ NewVar/Term ]

;
;                                  *		OldBackground: old list of predicate definitions

;
;                                   *		NewBackground: new ...

;
;                                   *			motivation: e.g. let term be "red".

;
;                                   * 			the resulting literal is " red(X) " which is

;
;                                   *			true iff X=red. Therefore

;
;                                   *			NewBackground = OldBackground + [ red(red) ]

;
;                                    *		Literals: list of literals to replace function

;
;                                    *

;
;                                    * description:

;
;                                    *

;
;                                    * example:

;
;                                     *

;
;                                     * peculiarities: can't flatten integers

;
;                                     *

;
;                                     * see also:

;
;                                     *

;
;                                      ***********************************************************************



  (= 
    (flatten-term $Lin $Lout) 
    (flatten-term $Lin $_ Nil $_ Nil $_ $Lout))

;
;                                        flatten_term(+,-,+,-,+,-,-)


;
;                                        known terms              ; change: represent only vars once

  (= 
    (flatten-term $Term $Var $S $S $Bg $Bg Nil) 
    ( (member 
        (/ $Var $Term1) $S) 
      (== $Term $Term1) 
      (set-det)))
;                                        ; var(Term),               ;        new !!!



;
;                                         Variables

;
;                                        flatten_term( X, V, S,[(V/X)|S],[]):- var(X),!.

  (= 
    (flatten-term $X $X $S $S $Bg $Bg Nil) 
    ( (var $X) (set-det)))

;
;                                          empty list

  (= 
    (flatten-term Nil $V $S 
      (Cons  
        (/ $V Nil) $S) $Bg 
      (Cons  
        (nil-p Nil) $Bg) 
      (:: (nil-p $V))) 
    (set-det))

;
;                                          other atoms

  (= 
    (flatten-term $A $V $S 
      (Cons  
        (/ $V $A) $S) $Bg 
      (Cons  $B $Bg) 
      (:: $L)) 
    ( (atom $A) 
      (set-det) 
      (concat-atom 
        (:: $A p) - $Functor) 
      (=.. $L 
        (:: $Functor $V)) 
      (=.. $B 
        (:: $Functor $A))))

;
;                                          integers

  (= 
    (flatten-term $Int $V $S 
      (Cons  
        (/ $V $Int) $S) $Bg 
      (Cons  $B $Bg) 
      (:: $L)) 
    ( (integer $Int) 
      (set-det) 
      (map-function-to-pred $Int $PredName) 
      (=.. $L 
        (:: $PredName $V)) 
      (=.. $B 
        (:: $PredName $Int))))
        

;
;                                          list

  (= 
    (flatten-term 
      (Cons  $A $B) $V $S $Snew $Bg 
      (Cons  
        (cons-p $A $B 
          (Cons  $A $B)) $Bg2) $Literals) 
    ( (set-det) 
      (flatten-term $A $V1 $S $S1 $Bg $Bg1 $Literals1) 
      (flatten-term $B $V2 $S1 $Snew1 $Bg1 $Bg2 $Literals2) 
      (= $Snew 
        (Cons  
          (/ $V 
            (Cons  $A $B)) $Snew1)) 
      (append $Literals1 $Literals2 $Literals3) 
      (= $Literals 
        (Cons  
          (cons-p $V1 $V2 $V) $Literals3))))


;
;                                          other functions

  (= 
    (flatten-term $Function $V $S $Snew $Bg 
      (Cons  $BgPredicate $Bg1) $Literals) 
    ( (=.. $Function 
        (Cons  $Functor $Args)) 
      (flatten-args $Args $Vs $S $Snew1 $Bg $Bg1 $Literals1) 
      (= $Snew 
        (Cons  
          (/ $V $Function) $Snew1)) 
      (append $Vs 
        (:: $V) $NewArgs) 
      (concat-atom 
        (:: $Functor p) - $NewFunctor) 
      (=.. $Predicate 
        (Cons  $NewFunctor $NewArgs)) 
      (append $Args 
        (:: $Function) $BgArgs) 
      (=.. $BgPredicate 
        (Cons  $NewFunctor $BgArgs)) 
      (= $Literals 
        (Cons  $Predicate $Literals1))))
;                                         ; build new predicate of arity n+1





  (= 
    (flatten_args  () () $S $S $Bg $Bg ()) True)
  (= 
    (flatten-args 
      (Cons  $A $Args) 
      (Cons  $V $Vars) $S $Snew $Bg $Bg1 $Literals) 
    ( (flatten-term $A $V $S $Snew1 $Bg $Bg2 $L1) 
      (flatten-args $Args $Vars $Snew1 $Snew $Bg2 $Bg1 $L2) 
      (append $L1 $L2 $Literals)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: flatten_literal/2

;
;                                         *

;
;                                         * syntax: flatten_literal(+Lit,-Lit_list)

;
;                                         *

;
;                                         * args: Lit .. Literal, Lit_list .. list of literals

;
;                                         *

;
;                                         * description:	returns the list of literals Lit has to be replaced with

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
    (flatten-literal $In $Out) 
    (flatten-literal $In Nil $_ Nil $_ $Out))


;
;                                          flatten_literal(+,+,-,+,-,-)


  (= 
    (flatten-literal True $S $S $Bg $Bg Nil) 
    (set-det))

  (= 
    (flatten-literal $Predicate $S $Snew $Bg $Bg1 $Literals) 
    ( (=.. $Predicate 
        (Cons  $Functor $Args)) 
      (flatten-args $Args $Vars $S $Snew $Bg $Bg1 $Literals1) 
      (=.. $NewPredicate 
        (Cons  $Functor $Vars)) 
      (= $Literals 
        (Cons  $NewPredicate $Literals1))))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: flatten_literals/2

;
;                                         *

;
;                                         * syntax: flatten_literals(+Body,+OldSubst,-NewSubst,

;
;                                         *                          +OldBackground,-NewBackground,-Literals)

;
;                                         *

;
;                                         * args: Body.. clause body

;
;                                         *       OldSubst: list of substitutions that have already

;
;                                         *                 been performed.

;
;                                         * 	 NewSubst: = OldSubst + additional substitutions for Body

;
;                                         *	 OldBackground: old list of predicate definitions

;
;                                         *	 NewBackground: new ...

;
;                                         *	 Literals: list of literals to replace Body

;
;                                         *

;
;                                         * description:	flattens clause body

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
    (flatten-literals 
      (, $A $B) $S $Snew $Bg $Bg1 $Literals) 
    ( (set-det) 
      (flatten-literal $A $S $Snew1 $Bg $Bg2 $Literals1) 
      (flatten-literals $B $Snew1 $Snew $Bg2 $Bg1 $Literals2) 
      (append $Literals1 $Literals2 $Literals)))
;                                         ; cut, to prevent 2nd clause



  (= 
    (flatten-literals $A $S $Snew $Bg $Bg1 $Literals) 
    (flatten-literal $A $S $Snew $Bg $Bg1 $Literals))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: flatten_clause/2

;
;                                         *

;
;                                         * syntax: flatten_clause(+ClauseIn,-ClauseOut)

;
;                                         *

;
;                                         * args: clauses in MeTTa notation, i.e. ( head :- body )

;
;                                         *	 or list notation, i.e. [ head:p , b1:n, b2:n, ... ]

;
;                                         *

;
;                                         * description:	flatten a clause

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
    (flatten-clause $In $Out) 
    ( (= $In 
        (Cons  
          (with_self  $_ 
            (p)) $_)) 
      (set-det) 
      (clist-to-prolog $In $F) 
      (flatten-clause $F $G) 
      (clist-to-prolog $Out $G) 
      (set-det)))
;                                         ; list notation



  (= 
    (flatten-clause $In $Out) 
    ( (flatten-clause $In Nil $_ Nil $_ $Out) (set-det)))

  (= 
    (flatten-clause $Clause $S $Snew $Bg $Bg1 $ClauseOut) 
    ( (=.. $Clause 
        (:: :- $Head $Body)) 
      (=.. $Head 
        (Cons  $Functor $Args)) 
      (flatten-args $Args $Vars $S $Snew1 $Bg $Bg2 $Literals1) 
      (=.. $NewHead 
        (Cons  $Functor $Vars)) 
      (flatten-literals $Body $Snew1 $Snew $Bg2 $Bg1 $Literals2) 
      (append $Literals1 $Literals2 $Literals) 
      (list-to-struct $Literals $StrucLits) 
      (=.. $ClauseOut 
        (:: :- $NewHead $StrucLits))))
;                                         ; flatten head

;                                         ; flatten body




;
;                                         ************************************************************************

;
;                                         *

;
;                                         * predicates: substitute_in_literals/4

;
;                                         *	       substitute_in_literal/4

;
;                                         *             substitute_args/4

;
;                                         * syntax: substitute_in_literals(+Var,+Term,+OldLiterals,-NewLiterals)

;
;                                         *	   substitute_in_literal(+Var,+Term,+OldLiteral,-NewLiteral)

;
;                                         *         substitute_args(+Var,+Term,+OldArgs,-NewArgs)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description: replaces all occurences of Var in OldLiterals with Term

;
;                                         *	        and outputs NewLiterals.

;
;                                         *	        Note that also occurences of Var in subterms of args are

;
;                                         *	        detected.

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
;                                         ***********************************************************************


;
;                                          substitute all occurences of Var in LiteralIn by Term


  (= 
    (substitute_in_literals  $Var $Term () ()) True)
  (= 
    (substitute-in-literals $Var $Term 
      (Cons  $Lit1 $Lits) 
      (Cons  $Lit1new $Litsnew)) 
    ( (set-det) 
      (substitute-in-literal $Var $Term $Lit1 $Lit1new) 
      (substitute-in-literals $Var $Term $Lits $Litsnew)))


  (= 
    (substitute-in-literal $Var $Term $LiteralIn $LiteralOut) 
    ( (=.. $LiteralIn 
        (Cons  $Functor $Vars)) 
      (substitute-args $Var $Term $Vars $Args) 
      (=.. $LiteralOut 
        (Cons  $Functor $Args))))

;
;                                          substitute variables Vars in argument positions by Term if identical to Var


  (= 
    (substitute-args $Var $Term 
      (Cons  $V $Vs) 
      (Cons  $Term $Args)) 
    ( (== $Var $V) 
      (set-det) 
      (substitute-args $Var $Term $Vs $Args)))

  (= 
    (substitute-args $Var $Term 
      (Cons  $V $Vs) 
      (Cons  $Arg $Args)) 
    ( (contains-var $Var $V) 
      (set-det) 
      (=.. $V 
        (Cons  $Functor $SubVars)) 
      (substitute-args $Var $Term $SubVars $SubArgs) 
      (=.. $Arg 
        (Cons  $Functor $SubArgs)) 
      (substitute-args $Var $Term $Vs $Args)))
;                                         ; Var is subterm of V



  (= 
    (substitute-args $Var $Term 
      (Cons  $V $Vs) 
      (Cons  $V $Args)) 
    (substitute-args $Var $Term $Vs $Args))

  (= 
    (substitute_args  $Var $Term () ()) True)

 
;
;                                         *******************************************************************************

;
;                                         *

;
;                                         * predicate: unflatten_clause/2

;
;                                         *

;
;                                         * syntax: unflatten_clause(+FlatClause,-UnFlatClause)

;
;                                         *

;
;                                         * args: FlatClause : flattened clause (either in list or MeTTa notation)

;
;                                         *       UnFlatClause : unflattened clause

;
;                                         *

;
;                                         * description: Algorithm for unflattening: (Rouveirol,91.p131)

;
;                                         *      for each flattened predicate f_p(t1,..,tn,X) in the body of clause C

;
;                                         *          substitute all occurences of X by the functional term f(t1,..tn)

;
;                                         *          & drop f_p(t1,...,tn,X)

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
;                                         *******************************************************************************



  (= 
    (unflatten-clause 
      (= $Head $Body) 
      (= $Head1 $Body1)) 
    ( (list-to-struct $BodyListIn $Body) 
      (unflatten-clause1 $Head Nil $BodyListIn $Head1 $BodyListOut Nil Nil) 
      (list-to-struct $BodyListOut $Body1) 
      (set-det)))


  (= 
    (unflatten-clause $In $Out) 
    ( (= $In 
        (Cons  
          (with_self  $_ 
            (p)) $_)) 
      (set-det) 
      (clist-to-prolog $In $F) 
      (unflatten-clause $F $G) 
      (clist-to-prolog $Out $G) 
      (set-det)))
;                                         ; list notation




;
;                                         *******************************************************************************

;
;                                         *

;
;                                         * predicate: unflatten_clause/3

;
;                                         *

;
;                                         * syntax: unflatten_clause(+FlatClause,?Bg,-UnFlatClause)

;
;                                         *

;
;                                         * args: FlatClause = ( Head:-Body) : flattened clause

;
;                                         *       Bg : optional background facts - not used yet

;
;                                         *       UnFlatClause : unflattened clause

;
;                                         *

;
;                                         * description: Algorithm for unflattening: (Rouveirol,91.p131)

;
;                                         *      for each flattened predicate f_p(t1,..,tn,X) in the body of clause C

;
;                                         *          substitute all occurences of X by the functional term f(t1,..tn)

;
;                                         *          & drop f_p(t1,...,tn,X)

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
;                                         *******************************************************************************


  (= 
    (unflatten-clause 
      (= $Head $Body) $Bg 
      (= $Head1 $Body1)) 
    ( (list-to-struct $BodyListIn $Body) 
      (unflatten-clause1 $Head Nil $BodyListIn $Head1 $BodyListOut Nil $Bg) 
      (list-to-struct $BodyListOut $Body1)))


;
;                                         ****************************************************************

;
;                                         *

;
;                                         * predicate: unflatten_clause1/7

;
;                                         *

;
;                                         * syntax: unflatten_clause1(+HeadIn,+BodyIn1,+BodyIn2,-HeadOut,-BodyOut1,

;
;                                         *                          -BodyOut2,?Bg)

;
;                                         *

;
;                                         * args: +HeadIn     (function free) head of flattened clause

;
;                                         *       +BodyIn1

;
;                                         *       +BodyIn2    difference lists of body literals (flattened)

;
;                                         *       -HeadOut    head of unflattened clause

;
;                                         *       -BodyOut1

;
;                                         *       -BodyOut2   difference lists of body literals (unflattened)

;
;                                         *       ?Bg         optional background knowledge - not used yet

;
;                                         *

;
;                                         * description: unflattens a clause  ;

;
;                                         *              some variables are replaced by functions &

;
;                                         *              certain literals are dumped

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
;                                         ****************************************************************



  (= 
    (unflatten-clause1 $HeadIn $BodyIn1 
      (Cons  $Literal $Rest) $HeadOut $BodyOut1 $BodyOut2 $Bg) 
    ( (=.. $Literal 
        (Cons  $PredFunctor $Args)) 
      (map-function-to-pred $Functor $PredFunctor) 
      (set-det) 
      (append $Fargs 
        (:: $Var) $Args) 
      (=.. $Function 
        (Cons  $Functor $Fargs)) 
      (substitute-in-literal $Var $Function $HeadIn $HeadInt) 
      (substitute-in-literals $Var $Function $BodyIn1 $BodyInt1) 
      (substitute-in-literals $Var $Function $Rest $BodyInt2) 
      (unflatten-clause1 $HeadInt $BodyInt1 $BodyInt2 $HeadOut $BodyOut1 $BodyOut2 $Bg)))
;                                         ; Literal was introduced by flattening

;                                         ; get first n args (Fargs)

;                                         ; substitute Var by Function in whole clause




  (= 
    (unflatten-clause1 $HeadIn $BodyIn1 
      (Cons  $Literal $Rest) $HeadOut $BodyOut1 $BodyOut2 $Bg) 
    ( (set-det) 
      (append $BodyIn1 
        (:: $Literal) $BodyInt1) 
      (unflatten-clause1 $HeadIn $BodyInt1 $Rest $HeadOut $BodyOut1 $BodyOut2 $Bg)))



  (= 
    (unflatten_clause1  $Head $Body () $Head $Body () $Bg) True)


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	map_function_to_pred/2

;
;                                         *

;
;                                         * syntax: map_function_to_pred(+Function_symbol,-PredName)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	constructs a PredName Function_symbol_p for flattening

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
    (map-function-to-pred Nil nil-p) 
    (set-det))                 ;
;                                          [] -> nil

  (= 
    (map-function-to-pred . cons-p) 
    (set-det))               ;
;                                          lists

  (= 
    (map-function-to-pred $Integer $PredName) 
    ( (integer $Integer) 
      (var $PredName) 
      (number-chars $Integer $String) 
      (atom-chars $Atom $String) 
      (concat-atom 
        (:: integer $Atom p) - $PredName) 
      (set-det)))
;                                         ; integers , e.g. 15 -> integer_15_p

;                                         ; spypoint,


  (= 
    (map-function-to-pred $Integer $PredName) 
    ( (var $Integer) 
      (nonvar $PredName) 
      (midstring $PredName $S integer--p 8 $_ 2) 
      (name $S $List) 
      (number-chars $Integer $List) 
      (integer $Integer) 
      (set-det)))
;                                         ; integer_15_p -> 15


  (= 
    (map-function-to-pred $FunctionName $PredName) 
    ( (atom $FunctionName) 
      (var $PredName) 
      (concat-atom 
        (:: $FunctionName -p) $PredName) 
      (set-det)))
;                                         ; function symbols


  (= 
    (map-function-to-pred $FunctionName $PredName) 
    ( (atom $PredName) 
      (var $FunctionName) 
      (midstring $PredName -p $FunctionName $_ 2 0) 
      (set-det)))

