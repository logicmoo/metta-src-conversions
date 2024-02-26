;                                           (convert-to-metta-file  argument_types $_463422 miles/argument_types.pl miles/argument_types.metta)
;
; MODULE argument_types EXPORTS

  !(module argument-types 
    (:: 
      (/ argument-types 0) 
      (/ type-restriction 2) 
      (/ type-equal 2) 
      (/ type-equal 4) 
      (/ replace-t 4) 
      (/ type-sub 2) 
      (/ type-of 3) 
      (/ types-of 3) 
      (/ compare-types 3) 
      (/ define-type 0) 
      (/ verify-types 0)))


;
; IMPORTS
  !(use-module 
    (home kb) 
    (:: 
      (/ get-example 3) 
      (/ get-clause 5) 
      (/ store-clauses 2) 
      (/ store-clause 4) 
      (/ known 6) 
      (/ delete-clause 1)))
  !(use-module 
    (home lgg) 
    (:: (/ set-lgg 2)))
  !(use-module 
    (home div-utils) 
    (:: 
      (/ body2list 2) 
      (/ myforall 2) 
      (/ different-predicates 2) 
      (/ nth-arg 3) 
      (/ remove-v 3) 
      (/ make-unique 2) 
      (/ shares-var 2) 
      (/ mysetof 3)))
  !(use-module 
    (home var-utils) 
    (:: (/ only-vars 2)))
  !(use-module 
    (home td-basic) 
    (:: (/ append-body 3)))
  !(use-module 
    (home interpreter) 
    (:: (/ t-interpreter 2)))
  !(use-module 
    (home show-utils) 
    (:: (/ show-kb-types 0)))
  !(use-module-if-exists 
    (library subsumes) 
    (:: (/ variant 2)))
  !(use-module-if-exists 
    (library occurs) 
    (:: (/ contains-var 2)))
  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
  !(use-module-if-exists 
    (library strings) 
    (:: (/ gensym 2)))

;
; METAPREDICATES
;
; none


  !(dynamic (/ type-restriction 2))

;
; ***********************************************************************
;
; *
;
; * module: argument_types.pl
;
; *
;
; * author: I.Stahl date:12/92
;
; *
;
; * changed:
;
; *
;
; *
;
; * description: algorithm for determining argument types
;
; * results for each predicate p within the pos examples
;
; * in a kb entry
;
; * type_restriction(p(V1,..,Vn),[type1(V1),...,typen(Vn)])
;
; *
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
; * predicate: argument_types/0
;
; *
;
; * syntax: -
;
; *
;
; * args: none
;
; *
;
; *
;
; * description: toplevel predicate for determining argument types
;
; * results for each predicate p within the pos examples
;
; * in a kb entry
;
; * type_restriction(p(V1,..,Vn),[type1(V1),...,typen(Vn)])
;
; *
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
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= argument-types  
    (mysetof $E 
      (^ $I 
        (get-example $I $E +)) $Elist)
    (different-predicates $Elist $Elist1)
    (argument-types $Elist1)
    (set-det))
; ; Elist = [E1,...,En] pos examples
; ; Elist1 = [[E1,..,Em],...]
; ; list of lists of pos examples with
; ; the same predicate symbol


  (= (argument-types  ())  True)
  (= (argument-types (Cons  $E $R))  
    (argument-types $R)
    (arg-types $E))


  (= (arg-types (Cons  $E $R))  
    (functor $E $P $N)
    (functor $P1 $P $N)
    (det-if-then-else 
      (type-restriction $P1 $_) True 
      (add-atom  &self 
        (type-restriction  $P1 ())))
    (arg-types $N 
      (Cons  $E $R) $P $N))
; ; assert a type restriction for the
; ; predicate if not already present




;
; ***********************************************************************
;
; *
;
; * predicate: arg_types/4
;
; *
;
; * syntax: arg_types(+Counter,+Examplelist,+Pred_symbol,+Pred_arity)
;
; *
;
; * args:
;
; *
;
; *
;
; * description:
;
; * for each argument position (1 to Pred_arity) of the predicate Pred_symbol
;
; * the type of the terms occurring at that position is determined.
;
; * If the same type occurred already elsewhere, the old definition is taken
;
; * in order to avoid duplicate type definitions
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
; *
;
; * see also:
;
; *
;
; ***********************************************************************

  (= (arg-types 0 $_ $_ $_)  
    (set-det))
  (= (arg-types $N $EL $P $M)  
    (is $N1 
      (- $N 1))
    (arg-types $N1 $EL $P $M)
    (nth-arg $EL $N $S)
    (gensym type $Type)
    (arg-type $S Nil Nil $CL $Type)
    (minimize-cl $CL $Type $Type1)
    (adapt-type-restriction $M $P $N $Type1))


;
; ***********************************************************************
;
; *
;
; * predicate: arg_type/5
;
; *
;
; * syntax: arg_type(+Set_of_Argterms,+Ancestors,+Clause_list,-Clause_list,+Typename)
;
; *
;
; * args:
;
; *
;
; *
;
; * description:
;
; * Ancestors are all types calling Typename in their definition. Clauselist
;
; * contains all clauses defining Typename
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
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (arg-type $S $Ancestors $CL $CL2 $T)  
    (different-predicates $S $Slist)
    (init-cl $Slist $T $CL0)
    (append $CL $CL0 $CL1)
    (refine-cl $Slist 
      (Cons  $T $Ancestors) $CL0 $CL1 $CL2))
; ; splits the set of Argterms according to
; ; different functors, Slist = [[T1,..,Tm],..]
; ; for each set of Argterms in Slist, generate
; ; a clause head with pred symbol Typename ;
; generate clause bodies


  (= (init-cl  () $_ ())  True)
  (= (init-cl (Cons  $EL $R) $T (Cons  (= $T1  True) $R1))  
    (init-cl $R $T $R1)
    (set-lgg $EL $E)
    (=.. $T1 
      (:: $T $E)))


;
; ***********************************************************************
;
; *
;
; * predicate: refine_cl/5
;
; *
;
; * syntax: refine_cl(+Slist,+Ancestors,+Clauses,+Clauselist,-Clauselist)
;
; *
;
; * args:
;
; *
;
; *
;
; * description:
;
; * for each set of Argterms in Slist and each corresponding clause head
;
; * in Clauses and Clauselist, add a body literal for each variable in the
;
; * clause head. This body literal may be atom(_),atomic(_),number(_),
;
; * typex(_),where typex is in Ancestors, or typez(_), where typez is a
;
; * new type (recursive call of the algorithm)
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
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (refine-cl  () $_ $_ $CL $CL)  True)
  (= (refine-cl (Cons  $S $R) $A (Cons  (= $Head  $_) $R1) $CL $CL2)  
    (refine-cl $R $A $R1 $CL $CL1)
    (arg 1 $Head $E)
    (det-if-then-else 
      (var $E) 
      (test-var-instantiations $E $S $Head $A $CL1 $CL2) 
      (, 
        (functor $E $_ $N) 
        (ref-cl $N $E $S $Head $A $CL1 $CL2))))
; ; if the head argument is a variable, test its instantiations
; ; in S and add the corresponding literal to the body of Head
; ; if the head argument is no variable,
; ; decompose it, test the variables it contains
; ; and add the corresponding literals to the body
; ; of Head


;
; ***********************************************************************
;
; *
;
; * predicate: ref_cl/7
;
; *
;
; * syntax: ref_cl(+Counter,+Argument,+Argterms,+Head,+Ancestors,
;
; * +Clauselist, -Clauselist)
;
; *
;
; * args:
;
; *
;
; * description: decompose the head argument and test the variables it contains;
;
; * add the corresponding literals to the body of Head
;
; *
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
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (ref-cl 0 $_ $_ $_ $_ $CL $CL)  
    (set-det))
  (= (ref-cl $N $E $S $H $A $CL $CL2)  
    (is $N1 
      (- $N 1))
    (ref-cl $N1 $E $S $H $A $CL $CL1)
    (arg $N $E $X)
    (nth-arg $S $N $Sn)
    (det-if-then-else 
      (var $X) 
      (test-var-instantiations $X $Sn $H $A $CL1 $CL2) 
      (, 
        (functor $X $_ $M) 
        (ref-cl $M $X $Sn $H $A $CL1 $CL2))))



;
; ***********************************************************************
;
; *
;
; * predicate: test_var_instantiation/6
;
; *
;
; * syntax: test_var_instantiations(+Var,+Argterms,+Head,+Ancestors,
;
; * +Clauselist,-Clauselist)
;
; *
;
; * args:
;
; *
;
; *
;
; * description:
;
; * Argterms are the instantiations of Var. If all instantiations of Var
;
; * are atoms/number/atomic, the literal atom(Var)/number(Var)/atomic(Var) is
;
; * added to the body of Head in Clauselist. Else if the definition of a typex
;
; * in Ancestors covers all instantiations of Var, typex(Var) is added to the
;
; * body of Head in Clauselist (recursive definition). Else a new symbol typen
;
; * is created, the literal typen(Var) is added to the body of Head in Clauselist
;
; * and a definition of typen is induced by a recursive call of arg_type.
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


  (= (test-var-instantiations $X $S $H $_ $CL $CL1)  
    (myforall $S atom)
    (set-det)
    (=.. $Lit 
      (:: atom $X))
    (add-literal $CL $H $Lit $CL1))

  (= (test-var-instantiations $X $S $H $_ $CL $CL1)  
    (myforall $S number)
    (set-det)
    (=.. $Lit 
      (:: number $X))
    (add-literal $CL $H $Lit $CL1))

  (= (test-var-instantiations $X $S $H $_ $CL $CL1)  
    (myforall $S atomic)
    (set-det)
    (=.. $Lit 
      (:: atomic $X))
    (add-literal $CL $H $Lit $CL1))

  (= (test-var-instantiations $X $S $H $A $CL $CL1)  
    (test-ancestor $S $A $CL $APred)
    (set-det)
    (=.. $Lit 
      (:: $APred $X))
    (add-literal $CL $H $Lit $CL1))

  (= (test-var-instantiations $X $S $H $A $CL $CL1)  
    (gensym type $T)
    (=.. $Lit 
      (:: $T $X))
    (add-literal $CL $H $Lit $CL0)
    (arg-type $S $A $CL0 $CL1 $T))



  (= (test-ancestor $S (Cons  $APred $_) $CL $APred)  
    (myforall-interpreted $S $APred $CL)
    (set-det))
  (= (test-ancestor $S (Cons  $_ $R) $CL $APred)  
    (test-ancestor $S $R $CL $APred))


;
; ***********************************************************************
;
; *
;
; * predicate: myforall_interpreted/3
;
; *
;
; * syntax: myforall_interpreted(+Argterms,+Pred,+Clauselist)
;
; *
;
; * args:
;
; *
;
; *
;
; * description: tests for each argument term T in Argterms whether Pred(T)
;
; * follows from Clauselist. For that purpose, a special interpreter
;
; * t_interpreter is used that works on Clauselist as program instead
;
; * of the knowledge base.
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
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (myforall-interpreted  () $_ $_)  True)
  (= (myforall-interpreted (Cons  $E $R) $Pred $CL)  
    (=.. $C 
      (:: $Pred $E))
    (t-interpreter $C $CL)
    (myforall-interpreted $R $Pred $CL))


;
; ***********************************************************************
;
; *
;
; * predicate: add_literal/4
;
; *
;
; * syntax: add_literal(+Clauselist,+Head,+Lit,-Clauselist1)
;
; *
;
; * args:
;
; *
;
; * description: adds literal Lit to the clause (Head:- B) within Clauselist
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
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (add-literal (Cons  (= $H  $B) $R) $H1 $Lit (Cons  (= $H  
    ($Lit $B)) $R))  
    (== $H $H1)
    (set-det))
  (= (add-literal (Cons  $C $R) $H $Lit (Cons  $C $R1))  
    (add-literal $R $H $Lit $R1))


;
; ***********************************************************************
;
; *
;
; * predicate: adapt_type_restriction/4
;
; *
;
; * syntax: adapt_type_restriction(+Pred_arity,+Pred_name,+A,+Type)
;
; *
;
; * args:
;
; *
;
; * description:
;
; * Type is the type of the Ath Argposition of the predicate Pred_name. The
;
; * type restriction is type_restriction(p(V1,..,Vn),L). If there is not yet
;
; * an entry typex(VA) in L, Type(VA) is added to L. Else let the definition
;
; * of typex be typex(Hx1):- Bx1. and of Type be Type(H1):- B1.
;
; * ... ...
;
; * typex(Hxm):- Bxm. Type(Ho):- Bo.
;
; * Then we add a new type Tnew(VA) to L with the definition
;
; * Tnew(Hx1):- Bx1. Tnew(H1):- B1.
;
; * ... ...
;
; * Tnew(Hxm):- Bxm. Tnew(Ho):- Bo.
;
; * The definitions of typex and Type remain.
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
   

  (= (adapt-type-restriction $M $P $N $T)  
    ( (functor $P1 $P $M) 
      (remove-atom  &self 
        (type-restriction  $P1 $L)) 
      (arg $N $P1 $P1n) 
      (det-if-then-else 
        (, 
          (member $T1 $L) 
          (=.. $T1 
            (:: $T2 $X)) 
          (== $X $P1n)) 
        (, 
          (gensym type $Tnew) 
          (=.. $D 
            (:: $Tnew $P1n)) 
          (remove-v 
            (:: $T1) $L $L1) 
          (add-atom  &self 
            (type-restriction  $P1 
              ([|]  $D $L1))) 
          (adapt-tr $Tnew $T $T2)) 
        (, 
          (=.. $D 
            (:: $T $P1n)) 
          (add-atom  &self 
            (type-restriction  $P1 
              ([|]  $D $L)))))))


  (= (adapt-tr $Tnew $T1 $T2)  
    (functor $HT1 $T1 1)
    (functor $HT2 $T2 1)
    (mysetof 
      (= $HT1  $B1) 
      (^ $I 
        (^ $Clist 
          (get-clause $I $HT1 $B1 $Clist type))) $C1)
    (mysetof 
      (= $HT2  $B2) 
      (^ $I 
        (^ $Clist 
          (get-clause $I $HT2 $B2 $Clist type))) $C2)
    (append $C1 $C2 $C3)
    (adapt-tr1 $C3 $Tnew $C4)
    (make-unique $C4 $C5)
    (store-clauses $C5 type))


  (= (adapt-tr1  () $_ ())  True)
  (= (adapt-tr1 (Cons  (= $H  $B) $R) $T (Cons  (= $H1  $B) $R1))  
    (adapt-tr1 $R $T $R1)
    (=.. $H 
      (Cons  $_ $Arg))
    (=.. $H1 
      (Cons  $T $Arg)))


;
; ***********************************************************************
;
; *
;
; * predicate: minimize_cl/3
;
; *
;
; * syntax: minimize_cl(+CL,+Typename,-Typename)
;
; *
;
; * args:
;
; *
;
; * description: CL is the list of clauses defining the type Typename.
;
; * If CL contains definitions that occur already in the database,
;
; * or if it contains duplicate definitions, it is minimized.
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
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (minimize-cl $CL $Type $Type1)  
    (mysetof 
      (= $H  $B) 
      (^ $I 
        (^ $Clist 
          (get-clause $I $H $B $Clist type))) $Old_types)
    (mysetof $T 
      (^ $H 
        (^ $B 
          (^ $R 
            (, 
              (member 
                (= $H  $B) $Old_types) 
              (=.. $H 
                (Cons  $T $R)))))) $Oldt_names)
    (mysetof $T 
      (^ $H 
        (^ $B 
          (^ $R 
            (, 
              (member 
                (= $H  $B) $CL) 
              (=.. $H 
                (Cons  $T $R)))))) $Newt_names)
    (append $Old_types $CL $Clauses)
    (minimize-cl $Oldt_names $Newt_names $Clauses $CL $Type $Type1))

  (= (minimize-cl Nil $Newt_names $Clauses $CL $Type $Type1)  
    (minim-cl $Newt_names $Clauses $CL $Type $Type1))
  (= (minimize-cl (Cons  $T $R) $Newt_names $Clauses $CL $Type $Type2)  
    (mysetof $T1 
      (, 
        (member $T1 $Newt_names) 
        (type-equal $T $T1 
          (:: (with-self  $T $T1)) $Clauses)) $Tlist)
    (replace-t $CL $Tlist $T $CL1)
    (make-unique $CL1 $CL2)
    (remove-v $Tlist $Newt_names $Newt_names1)
    (det-if-then-else 
      (member $Type $Tlist) 
      (= $Type1 $T) 
      (= $Type1 $Type))
    (minimize-cl $R $Newt_names1 $Clauses $CL2 $Type1 $Type2))


  (= (minim-cl Nil $_ $CL $Type $Type)  
    (min-cl $CL $CL1)
    (store-clauses $CL1 type)) ;
; the remaining (minimized) set of clauses is stored
                            ;
; in the database
  (= (minim-cl (Cons  $T $R) $Clauses $CL $Type $Type2)  
    (mysetof $T1 
      (, 
        (member $T1 $R) 
        (type-equal $T $T1 
          (:: (with-self  $T $T1)) $Clauses)) $Tlist)
    (replace-t $CL $Tlist $T $CL1)
    (make-unique $CL1 $CL2)
    (remove-v $Tlist $R $R1)
    (det-if-then-else 
      (member $Type $Tlist) 
      (= $Type1 $T) 
      (= $Type1 $Type))
    (minim-cl $R1 $Clauses $CL2 $Type1 $Type2))


  (= (min-cl  () ())  True)
  (= (min-cl (Cons  (= $H  $B) $R) (Cons  (= $H  $B1) $R1))  
    (min-cl $R $R1)
    (min-cl1 $B $B1))


  (= (min-cl1 $A $A)  
    (set-det))
  (= (min-cl1 True True)  
    (set-det))
  (= (min-cl1 (, $A $B) (, $A $B1))  
    (min-cl1 $B $B1))


;
; ***********************************************************************
;
; *
;
; * predicate: replace_t/4
;
; *
;
; * syntax: replace_t(+CL,+List_of_typenames,+Typename,-CL)
;
; *
;
; * args:
;
; *
;
; * description: replaces in CL each occurrence of a typename
;
; * in List_of_typenames by Typename
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


  (= (replace-t  () $_ $_ ())  True)
  (= (replace-t (Cons  (= $H  $_) $R) $Tlist $T $R1)  
    (=.. $H 
      (Cons  $T1 $_))
    (member $T1 $Tlist)
    (set-det)
    (replace-t $R $Tlist $T $R1))
  (= (replace-t (Cons  (= $H  $B) $R) $Tlist $T (Cons  (= $H  $B1) $R1))  
    (repl-t $B $Tlist $T $B1)
    (replace-t $R $Tlist $T $R1))


  (= (repl-t (, $A $B) $Tlist $T (, $A1 $B1))  
    (set-det)
    (repl-t $A $Tlist $T $A1)
    (repl-t $B $Tlist $T $B1))
  (= (repl-t $A $Tlist $T $A1)  
    (=.. $A 
      (Cons  $T1 $R))
    (det-if-then-else 
      (member $T1 $Tlist) 
      (=.. $A1 
        (Cons  $T $R)) 
      (= $A1 $A)))


;
; ***********************************************************************************;
;
; *
;
; * predicate: type_equal/2
;
; *
;
; * syntax: type_equal(+Type_name1,+Type_name2)
;
; *
;
; * args:
;
; *
;
; * description: Tests whether the types Type_name1 and Type_name2 are defined
;
; * identically
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
; **********************************************************************************


  (= (type-equal $T $T)  
    (set-det))
  (= (type-equal $T1 $T2)  
    (mysetof 
      (= $H1  $B1) 
      (^ $I 
        (^ $CL 
          (^ $R 
            (, 
              (get-clause $I $H1 $B1 $CL type) 
              (=.. $H1 
                (Cons  $T1 $R)))))) $Clauses1)
    (mysetof 
      (= $H2  $B2) 
      (^ $I 
        (^ $CL 
          (^ $R 
            (, 
              (get-clause $I $H2 $B2 $CL type) 
              (=.. $H2 
                (Cons  $T2 $R)))))) $Clauses2)
    (append $Clauses1 $Clauses2 $Clauses)
    (type-equal $T1 $T2 
      (:: (with-self  $T1 $T2)) $Clauses))

  (= (type-equal $T $T $_ $_)  
    (set-det))
  (= (type-equal $T1 $T2 $Ancestors $Clauses)  
    (mysetof 
      (= $H  $B) 
      (^ $R 
        (, 
          (member 
            (= $H  $B) $Clauses) 
          (=.. $H 
            (Cons  $T1 $R)))) $Clist1)
    (mysetof 
      (= $H  $B) 
      (^ $R 
        (, 
          (member 
            (= $H  $B) $Clauses) 
          (=.. $H 
            (Cons  $T2 $R)))) $Clist2)
    (compare-clauses $Clist1 $Clist2 $Clauses $Ancestors))


;
; ***********************************************************************
;
; *
;
; * predicate: compare_clauses/4
;
; *
;
; * syntax: compare_clauses(+Clist1,+Clist2,+Clauses12,+Ancestors)
;
; *
;
; * args: Clist1 .. clauses defining Type_name1
;
; * Clist2 .. clauses defining Type_name2
;
; * Clauses12 .. all clauses defining Type_name1 and Type_name2
;
; * Ancestors .. types already tested on equality
;
; *
;
; * description: tests whether Type_name1 and Type_name2 are identically
;
; * defined by comparing the defining clauses
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
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (compare-clauses  () () $_ $_)  True)
  (= (compare-clauses (Cons  (= $H1  $B1) $R) $CL2 $Clauses $Ancestors)  
    (find-variant-clause $CL2 $H1 $CL21 
      (= $H2  $B2))
    (arg 1 $H1 $E1)
    (arg 1 $H2 $E2)
    (comp-clauses $E1 $E2 $B1 $B2 $Clauses $Ancestors)
    (compare-clauses $R $CL21 $Clauses $Ancestors))


  (= (comp-clauses $E1 $E2 $B1 $B2 $C $A)  
    (var $E1)
    (set-det)
    (def-literal $E1 $B1 $L1)
    (def-literal $E2 $B2 $L2)
    (=.. $L1 
      (Cons  $T1 $_))
    (=.. $L2 
      (Cons  $T2 $_))
    (c-clauses $T1 $T2 $C $A))
  (= (comp-clauses $E1 $E2 $B1 $B2 $C $A)  
    (functor $E1 $_ $N)
    (comp-clauses $N $E1 $E2 $B1 $B2 $C $A))
  (= (comp-clauses 0 $_ $_ $_ $_ $_ $_)  
    (set-det))
  (= (comp-clauses $N $E1 $E2 $B1 $B2 $C $A)  
    (is $N1 
      (- $N 1))
    (comp-clauses $N1 $E1 $E2 $B1 $B2 $C $A)
    (arg $N $E1 $E1n)
    (arg $N $E2 $E2n)
    (comp-clauses $E1n $E2n $B1 $B2 $C $A))


  (= (c-clauses atom $L $_ $_)  
    (set-det)
    (= $L atom))
  (= (c-clauses number $L $_ $_)  
    (set-det)
    (= $L number))
  (= (c-clauses atomic $L $_ $_)  
    (set-det)
    (= $L atomic))
  (= (c-clauses $_ $L2 $_ $_)  
    ( (or 
        (= $L2 atom) 
        (or 
          (= $L2 number) 
          (= $L2 atomic))) 
      (set-det) fail))
  (= (c-clauses $T1 $T2 $C $A)  
    (det-if-then-else 
      (or 
        (member 
          (with-self  $T1 $T2) $A) 
        (member 
          (with-self  $T2 $T1) $A)) True 
      (type-equal $T1 $T2 
        (Cons  
          (with-self  $T1 $T2) $A) $C)))

;
; ***********************************************************************
;
; *
;
; * predicate: find_variant_clause/4
;
; *
;
; * syntax: find_variant_clause(+CL,+Head,-CL1,-Clause)
;
; *
;
; * args:
;
; *
;
; * description: CL1 is CL - Clause, where the head argument of Head and
;
; * of the head of Clause are variants
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
; *
;
; * see also:
;
; *
;
; ***********************************************************************


  (= (find-variant-clause (Cons  (= $H2  $B2) $R) $H1 $R (= $H2  $B2))  
    (arg 1 $H1 $E1)
    (arg 1 $H2 $E2)
    (variant $E1 $E2))
  (= (find-variant-clause (Cons  $C $R) $H (Cons  $C $R1) $C1)  
    (find-variant-clause $R $H $R1 $C1))

;
; ***********************************************************************
;
; *
;
; * predicate: def_literal/3
;
; *
;
; * syntax: def_literal(+Var,+Body,-Lit)
;
; *
;
; * args:
;
; *
;
; * description: Lit is the literal within Body that defines the type of
;
; * Var
;
; *
;
; * example: def_literal(A,(atom(A),list(B)),atom(A))
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


  (= (def-literal $X (, $A $B) $C)  
    (set-det)
    (det-if-then-else 
      (contains-var $X $A) 
      (= $C $A) 
      (def-literal $X $B $C)))
  (= (def-literal $X $A $A)  
    (contains-var $X $A)
    (set-det))
  (= (def-literal  $X $_ (all  $X))  True)

;
; **********************************************************************************
;
; *
;
; * predicate: type_sub/2
;
; *
;
; * syntax: type_sub(+Gen,+Spec)
;
; *
;
; * args: Gen, Spec: type names or intermediate type definitions
;
; * t_int(H):- B (cf. type_of).
;
; *
;
; * description: succeeds if the type Gen is more general than
;
; * the type Spec.
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
; **********************************************************************************



  (= (type-sub $Gen (= $H  $B))  
    (type-sub1 
      (:: (= $H  $B)) $Gen Nil))
  (= (type-sub $Gen $Spec)  
    (type-sub $Gen $Spec 
      (:: (with-self  $Gen $Spec))))



;
; ***********************************************************************
;
; *
;
; * predicate: type_sub/3
;
; *
;
; * syntax: type_sub(+Gen,+Spec,+Ancestors)
;
; *
;
; * args:
;
; *
;
; * description:
;
; * Ancestors contains the types that have been compared already in order to
;
; * avoid infinite recursion
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


  (= (type-sub all $_ $_)  
    (set-det)) ;
; all is the most general type
  (= (type-sub $T1 all $_)  
    (set-det)
    (= $T1 all))    
  (= (type-sub $T $T1 $_)  
    (== $T $T1)
    (set-det))

  (= (type-sub atomic $T $_)  
    (set-det)
    (or 
      (= $T atom) 
      (or 
        (= $T atomic) 
        (or 
          (= $T number) 
          (, 
            (functor $HT $T 1) 
            (setof 
              (, $HT $B) 
              (^ $ID 
                (^ $CL 
                  (get-clause $ID $HT $B $CL type))) $TL) 
            (all-t-in $TL 
              (:: atom number atomic)))))))
  (= (type-sub atom $T $_)  
    (set-det)
    (or 
      (= $T atom) 
      (, 
        (functor $HT $T 1) 
        (setof 
          (, $HT $B) 
          (^ $ID 
            (^ $CL 
              (get-clause $ID $HT $B $CL type))) $TL) 
        (all-t-in $TL 
          (:: atom))))) 
  (= (type-sub number $T $_)  
    (set-det)
    (or 
      (= $T number) 
      (, 
        (functor $HT $T 1) 
        (setof 
          (, $HT $B) 
          (^ $ID 
            (^ $CL 
              (get-clause $ID $HT $B $CL type))) $TL) 
        (all-t-in $TL 
          (:: number)))))

  (= (type-sub $T atomic $_)  
    (set-det)
    (or 
      (= $T atomic) 
      (or 
        (= $T all) 
        (, 
          (functor $HT $T 1) 
          (setof 
            (, $HT $B) 
            (^ $ID 
              (^ $CL 
                (get-clause $ID $HT $B $CL type))) $TL) 
          (all-t-in $TL 
            (:: atomic))))))
  (= (type-sub $T atom $_)  
    (set-det)
    (or 
      (= $T atom) 
      (or 
        (= $T atomic) 
        (or 
          (= $T all) 
          (, 
            (functor $HT $T 1) 
            (setof 
              (, $HT $B) 
              (^ $ID 
                (^ $CL 
                  (get-clause $ID $HT $B $CL type))) $TL) 
            (all-t-in $TL 
              (:: atom atomic)))))))
  (= (type-sub $T number $_)  
    (set-det)
    (or 
      (= $T number) 
      (or 
        (= $T atomic) 
        (or 
          (= $T all) 
          (, 
            (functor $HT $T 1) 
            (setof 
              (, $HT $B) 
              (^ $ID 
                (^ $CL 
                  (get-clause $ID $HT $B $CL type))) $TL) 
            (all-t-in $TL 
              (:: number atomic)))))))


  (= (type-sub $TG $TS $A)  
    (functor $HTS $TS 1)
    (mysetof 
      (= $HTS  $BS) 
      (^ $ID 
        (^ $CL 
          (get-clause $ID $HTS $BS $CL type))) $CS)
    (type-sub1 $CS $TG $A))

;
; ***********************************************************************
;
; *
;
; * predicate: type_sub1/3
;
; *
;
; * syntax: type_sub1(+SpecClauses,+Gen,+Ancestors)
;
; *
;
; * args:
;
; *
;
; * description:
;
; * for each clause C in SpecClauses defining the more specific type,
;
; * there must be a clause defining Gen that is more general than C
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


  (= (type-sub1  () $_ $_)  True)
  (= (type-sub1 (Cons  (= $H  $B) $R) (= $HG  $BG) $A)  
    (set-det)
    (type-sub1 $R 
      (= $HG  $BG) $A)
    (set-det)
    (=.. $H 
      (:: $_ $Es))
    (=.. $HG 
      (:: $_ $Es))
    (expand-to-type-def $BG $BG1)
    (is-type-definition (= $H  $B))
    (test-type-def $B)
    (is-type-definition (= $HG  $BG1))
    (test-type-def $BG1)
    (only-vars $Es $EsV)
    (type-sub2 $EsV $BG1 $B $A))
  (= (type-sub1 (Cons  (= $H  $B) $R) $TG $A)  
    (type-sub1 $R $TG $A)
    (set-det)
    (=.. $H 
      (:: $_ $Es))
    (=.. $HTG 
      (:: $TG $Es))
    (get-clause $_ $HTG $BG $_ type)
    (expand-to-type-def $BG $BG1)
    (is-type-definition (= $H  $B))
    (test-type-def $B)
    (is-type-definition (= $HTG  $BG1))
    (test-type-def $BG1)
    (only-vars $Es $EsV)
    (type-sub2 $EsV $BG1 $B $A))


;
; ***********************************************************************
;
; *
;
; * predicate: type_sub2/4
;
; *
;
; * syntax: type_sub2(+Varlist,+Genbody,+Specbody,+Ancestors)
;
; *
;
; * args:
;
; *
;
; * description:
;
; * tests for each variable V in Varlist whether the literal defining V in Genbody
;
; * is of a more general type than the literal defining V in Specbody.
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


  (= (type-sub2  () $_ $_ $_)  True)
  (= (type-sub2 (Cons  $X $R) $BG1 $B $A)  
    (type-sub2 $R $BG1 $B $A)
    (set-det)
    (def-literal $X $BG1 $LG)
    (def-literal $X $B $LS)
    (=.. $LS 
      (Cons  $TS $_))
    (=.. $LG 
      (Cons  $TG $_))
    (det-if-then-else 
      (member 
        (with-self  $TG $TS) $A) True 
      (type-sub $TG $TS 
        (Cons  
          (with-self  $TG $TS) $A))))


;
; ***********************************************************************
;
; *
;
; * predicate: expand_to_type_definition/2
;
; *
;
; * syntax: expand_to_type_definition(+Body,-Body1)
;
; *
;
; * args:
;
; *
;
; * description:
;
; * transform Body to a valid type definition (i.e. each literal is of the
;
; * form t(X), were t in atom,atomic,number,typex and X is simple), by expanding
;
; * literals containing complex terms
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


  (= (expand-to-type-def (, $A $B) (, $A $B1))  
    (simple-td $A)
    (expand-to-type-def $B $B1))
  (= (expand-to-type-def (, $A $B) $B1)  
    (not (simple-td $A))
    (get-clause $_ $A $Lits $_ type)
    (append-body $Lits $B $B0)
    (expand-to-type-def $B0 $B1))
  (= (expand-to-type-def $A $A)  
    (simple-td $A))
  (= (expand-to-type-def $A $A1)  
    (not (= $A (, $_ $_)))
    (not (simple-td $A))
    (get-clause $_ $A $Lits $_ type)
    (expand-to-type-def $Lits $A1))

;
; ***********************************************************************
;
; *
;
; * predicate: simple_td/1
;
; *
;
; * syntax: simple_td(+Lit)
;
; *
;
; * args: Lit is a literal within a type definition
;
; *
;
; * description: succeeds if Lit == true, or Lit = t(X), where X is atomic or
;
; * a variable
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


  (= (simple-td True)  
    (set-det))
  (= (simple-td $A)  
    (=.. $A 
      (:: $_ $X))
    (simple $X))

;
; ***********************************************************************
;
; *
;
; * predicate: test_type_definition/1
;
; *
;
; * syntax: test_type_definition(Body)
;
; *
;
; * args:
;
; *
;
; * description: partially evaluates Body.
;
; * Fails if body contains invalid ground literals
;
; * with predicate symbol atom, number or atomic
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


  (= (test-type-def (, $A $B))  
    (set-det)
    (test-type-def $A)
    (test-type-def $B))
  (= (test-type-def $A)  
    (=.. $A 
      (:: $T $X))
    (ground $X)
    (or 
      (= $T atom) 
      (or 
        (= $T atomic) 
        (= $T number)))
    (set-det)
    (call $A))
  (= (test-type-def  $_)  True)


;
; ***********************************************************************
;
; *
;
; * predicate: is_type_definition/1
;
; *
;
; * syntax: is_type_definition(+Clause)
;
; *
;
; * args:
;
; *
;
; * description:
;
; * succeeds if Clause is a syntactically correct type definition, i.e. only
;
; * atom, atomic, number and typex occur as unary predicate in the body, and
;
; * every variable of Clause occurs once in the head and once in the body of Clause
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


  (= (is-type-definition (= $H  $B))  
    (only-vars $H $Vars)
    (is-type-def $Vars $B))


  (= (is-type-def  () $_)  True)
  (= (is-type-def (Cons  $X $R) $B)  
    (def-literal $X $B $Lit)
    (set-det)
    (=.. $Lit 
      (:: $_ $X1))
    (== $X $X1)
    (is-type-def $R $B))


  (= (all-t-in  () $_)  True)
  (= (all-t-in (Cons  (, $H $B) $R) $L)  
    (=.. $H 
      (:: $_ $X))
    (var $X)
    (=.. $B 
      (:: $N $X))
    (member $N $L)
    (all-t-in $R $L))


;
; **********************************************************************************
;
; *
;
; * predicate: type_of/3
;
; *
;
; * syntax: type_of(+Var,+C,-Type)
;
; *
;
; * args:
;
; *
;
; * description: C is a clause or a literal. Returns the most specific type of Var
;
; * within C. If Var does not occur in C or if it occurs at
;
; * positions with incompatible types, type_of returns fail. If Var
;
; * is a term that only partially matches the body of a type
;
; * definition, a intermediate type definition t_int(Var):- B is
;
; * returned
;
; *
;
; * example:
;
; *
;
; * pecularities:
;
; *
;
; **********************************************************************************


  (= (type-of $V (= $H  $B) $Type)  
    (set-det)
    (type-of $V $H $Type1)
    (type-of $V $B $Type2)
    (compare-types $Type1 $Type2 $Type))
  (= (type-of $V (, $A $B) $Type)  
    (set-det)
    (type-of $V $A $Type1)
    (type-of $V $B $Type2)
    (compare-types $Type1 $Type2 $Type))
  (= (type-of $T $Pred $Type)  
    (det-if-then-else 
      (type-restriction $Pred $Plist) 
      (, 
        (mysetof $Ts 
          (, 
            (member $Ts $Plist) 
            (contains-var $T $Ts)) $TsL) 
        (type-of1 $TsL $T $Type)) 
      (= $Type all)))
  (= (type-of  $_ true all)  True)


  (= (type-of1  () $_ all)  True)
  (= (type-of1 (Cons  $Ts $R) $T $Type)  
    (set-det)
    (type-of1 $R $T $Type1)
    (=.. $Ts 
      (:: $Type0 $T2))
    (det-if-then-else 
      (== $T2 $T) 
      (compare-types $Type1 $Type0 $Type) 
      (, 
        (get-clause $_ $Ts $_ 
          (Cons  $_ $CL) type) 
        (mysetof $Ts1 
          (, 
            (member 
              (with-self  $Ts1 $_) $CL) 
            (contains-var $T $Ts1)) $TsL) 
        (det-if-then-else 
          (= $TsL Nil) 
          (, 
            (mysetof $Ts2 
              (, 
                (member $Ts2 $CL) 
                (shares-var $T $Ts2)) $TsL1) 
            (=.. $H_int 
              (:: t-int $T)) 
            (with-self  
              (kb *) 
              (body2list $B_int $TsL1)) 
            (compare-types $Type1 
              (= $H_int  $B_int) $Type)) 
          (, 
            (type-of1 $TsL $T $Type2) 
            (compare-types $Type1 $Type2 $Type))))))



;
; **********************************************************************************
;
; *
;
; * predicate: types_of/3
;
; *
;
; * syntax: types_of(+Varlist,+C,-Typelist)
;
; *
;
; * args:
;
; *
;
; * description: like type_of for each Var in Varlist.
;
; * Varlist = [V1,..,Vn] => Typelist = [V1:T1,..,Vn:Tn]
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
; **********************************************************************************


  (= (types-of  () $_ ())  True)
  (= (types-of (Cons  $V $R) $C (Cons  (with-self  $V $T) $R1))  
    (type-of $V $C $T)
    (types-of $R $C $R1))


;
; ***********************************************************************
;
; *
;
; * predicate: compare_types/3
;
; *
;
; * syntax: compare_types(+Type1,+Type2,-Type)
;
; *
;
; * args: Type1,Type2: types to be compared
;
; * Type: the most specific type among type1 and typ2
;
; *
;
; * description: returns the more specific type
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


  (= (compare-types $Type1 $Type2 $Type)  
    (det-if-then-else 
      (type-sub $Type1 $Type2) 
      (= $Type $Type2) 
      (det-if-then-else 
        (type-sub $Type2 $Type1) 
        (= $Type $Type1) fail)))

;
; ***********************************************************************
;
; *
;
; * predicate: define_type/0
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
; * description: allows to define a type restriction for a predicate
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


  (= define-type  
    (show-kb-types read-type-restriction))


  (= read-type-restriction  
    (repeat nl 
      (write 'Please enter name and arity of the predicate p/n: ') 
      (det-if-then-else 
        (, 
          (read (/ $P $N)) 
          (atom $P) 
          (integer $N)) 
        (, 
          (functor $F $P $N) 
          (=.. $F 
            (Cons  $P $Args)) 
          (read-type-restriction $Args 1 $Alist) 
          (add-atom  &self 
            (type-restriction  $F $Alist))) fail)))

  (= (read-type-restriction  () $_ ())  True)
  (= (read-type-restriction (Cons  $V $R) $N (Cons  $T $R1))  
    (repeat nl 
      (write 'Please enter the type at argument position ') 
      (write $N) 
      (write  : ) 
      (det-if-then-else 
        (, 
          (read $TN) 
          (atom $TN)) 
        (det-if-then-else 
          (or 
            (, 
              (=.. $H 
                (:: $TN $_)) 
              (get-clause $_ $H $_ $_ type)) 
            (member $TN 
              (:: atom number atomic))) 
          (=.. $T 
            (:: $TN $V)) 
          (, 
            (read-type-definition $TN) 
            (=.. $T 
              (:: $TN $V)))) fail) 
      (is $N1 
        (+ $N 1)) 
      (read-type-restriction $R $N1 $R1)))


;
; ***********************************************************************
;
; *
;
; * predicate: read_type_definition/1
;
; *
;
; * syntax: read_type_definition(+TN)
;
; *
;
; * args: TN.. name of a type
;
; *
;
; * description: allows to enter clauses defining the type TN
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


  (= (read-type-definition $TN)  
    (nl 
      (write 'Type ') 
      (write $TN) 
      (write ' is undefined. Enter a definition (y/n)? ') 
      (read $A) 
      (det-if-then-else 
        (== $A y) 
        (read-type-def $TN) 
        (det-if-then-else 
          (== $A n) fail 
          (, nl 
            (write 'Please enter y or n') 
            (read-type-definition $TN))))))


  (= (read-type-def $TN)  
    (nl 
      (write 'Please enter the definition of ') 
      (write $TN) 
      (write ' in clausal form. Stop by entering stop.') nl repeat 
      (read $A) 
      (det-if-then-else 
        (or 
          (, 
            (= $A 
              (= $H  $_)) 
            (=.. $H 
              (:: $_ $_))) 
          (=.. $A 
            (:: $_ $_))) 
        (, 
          (store-clause $A $_ type $_) nl fail) 
        (det-if-then-else 
          (== $A stop) True 
          (, nl 
            (write 'Please enter a clause or stop') fail)))))

;
; ***********************************************************************
;
; *
;
; * predicate: verify_types/0
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
; * description: checks for the type restrictions in the kb whether
;
; * the contain only defined or built-in types. If not,
;
; * the user is asked for replacing or defining the unknown
;
; * types.
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


  (= verify-types  
    (findall 
      (, $M $A) 
      (type-restriction $M $A) $TSet)
    (verify-types $TSet))

  (= (verify-types  ())  True)
  (= (verify-types (Cons  (, $M $A) $R))  
    (verify-types $R)
    (verify-types $A $M $A))

  (= (verify-types  () $_ $_)  True)
  (= (verify-types (Cons  $H $R) $M $A)  
    (verify-types $R $M $A)
    (=.. $H 
      (Cons  $T $_))
    (findall 
      (, $H1 $B1) 
      (, 
        (=.. $H1 
          (:: $T $_)) 
        (known $ID $H1 $B1 $CL $_ $E) 
        (delete-clause $ID) 
        (add-atom  &self 
          (:  kb 
            (known  $ID $H1 $B1 $CL type $E)))) $Tlist)
    (det-if-then-else 
      (or 
        (\== $Tlist Nil) 
        (member $T 
          (:: atom number atomic))) True 
      (, nl 
        (write 'The type ') 
        (write $T) 
        (write ' is undefined in ') 
        (copy-term 
          (, $M $A) 
          (, $M1 $A1)) 
        (numbervars 
          (, $M1 $A1) 0 $_) 
        (write (type-restriction $M1 $A1)) nl show-kb-types repeat nl 
        (write 'Do you want to replace ') 
        (write $T) 
        (write ' in ') 
        (write (type-restriction $M1 $A1)) 
        (write ' (y/n)?') nl 
        (read $An) 
        (det-if-then-else 
          (== $An y) 
          (, 
            (remove-atom  &self 
              (type-restriction  $M $A)) repeat nl 
            (write 'Enter the name of the type replacing ') 
            (write $T) 
            (write : ) 
            (det-if-then-else 
              (, 
                (read $T1) 
                (atom $T1)) 
              (, 
                (=.. $H1 
                  (:: $T1 $_)) 
                (det-if-then-else 
                  (get-clause $_ $H1 $_ $_ $_) 
                  (, 
                    (vrt $A $T $T1 $A2) 
                    (add-atom  &self 
                      (type-restriction  $M $A2))) 
                  (, nl 
                    (write 'The type ') 
                    (write $T1) 
                    (write ' is undefined.') 
                    (vrt1 $T1)))) fail)) 
          (det-if-then-else 
            (== $An n) 
            (, nl 
              (write 'Then you have to define ') 
              (write $T) 
              (read-type-def $T)) 
            (, 
              (write 'Please enter y or n') fail))))))


  (= (vrt  () $_ $_ ())  True)
  (= (vrt (Cons  $H $R) $T $T1 (Cons  $H1 $R1))  
    (vrt $R $T $T1 $R1)
    (=.. $H 
      (:: $T2 $V))
    (det-if-then-else 
      (== $T2 $T) 
      (=.. $H1 
        (:: $T1 $V)) 
      (= $H1 $H)))



  (= (vrt1 $T1)  
    (nl 
      (write 'Do you want to define it (y/n)? ') 
      (read $Bn) 
      (det-if-then-else 
        (== $Bn y) 
        (read-type-def $T1) 
        (det-if-then-else 
          (== $Bn n) fail 
          (, 
            (write 'Please enter y or n') 
            (vrt1 $T1))))))
                            
                   

