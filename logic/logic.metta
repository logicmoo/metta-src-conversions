;                                           (convert-to-metta-file  logic $_469726 logic/logic.pl logic/logic.metta)

  (= (substitution $Term1 $Term2 $Sub_List)  
    (implies $Term1 $Term2)
    (substitute 
      (:: $Term1) 
      (:: $Term2) $Subst1)
    (sort $Subst1 $Subst2)
    (remove-id $Subst2 $Sub_List)
    (set-det))
; /******************************************************************/
; /* LOGIC.PRO Last Modification: Fri Jan 14 19:25:10 1994 */
; /* Differerent logic procedures useful for learning: determination*/
; /* of subsitutions, implies, Plotkin's least general generalisa- */
; /* tion, Buntine's generalized subsumption. */
; /******************************************************************/
; ; ; Copyright (c) 1988 Stephen Muggleton ; ; This program is free software; you can redistribute it and/or ; modify it under the terms of the GNU General Public License ; Version 1 as published by the Free Software Foundation. ; ; This program is distributed in the hope that it will be useful, ; but WITHOUT ANY WARRANTY; without even the implied warranty of ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ; GNU General Public License for more details. ; ; You should have received a copy of the GNU General Public ; License along with this program; if not, write to the Free ; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, ; USA. ;
; /******************************************************************/
; /* impl. by : Stephen Muggleton */
; /* Turing Institute */
; /* George House */
; /* 36 North hanover Street */
; /* Glasgow, G1 2AD */
; /* U.K. */
; /* E-Mail: steve@turing-institute.ac.uk */
; /* 1988 */
; /* */
; /* reference : A note on inductive generalization */
; /* Gordon Plotkin */
; /* in: B. Meltzer, D. Michie (eds) */
; /* Machine Intelligence 5 */
; /* Elsevier North-Holland 1970 */
; /* */
; /* A further note on inductive generalization */
; /* Gordon Plotkin */
; /* in: B. Meltzer, D. Michie (eds) */
; /* Machine Intelligence 6 */
; /* Elsevier North-Holland 1971 */
; /* */
; /* Generalized Subsumption and Its Applications to */
; /* Induction and Redundancy */
; /* Wray Buntine */
; /* Artificial Intelligence 36, 1988. */
; /* */
; /* ES2ML Tutorial Exercise */
; /* Substitution matching and generalisation in */
; /* MeTTa */
; /* Stephen Muggleton */
; /* */
; /* call : see logic_1.pro */
; /* */
; /******************************************************************/
; ; TH Sun May 30 15:12:41 1993 - made some minor modifications
; /******************************************************************/
; /* */
; /* call : subsitution (+Term1,+Term2,-Subst) */
; /* */
; /* arguments : Term1 = first-order logic Term, with variables */
; /* Term2 = first-order logic Term, without */
; /* variables */
; /* Subst = List of minimal-sized substitutions */
; /* */
; /******************************************************************/
; /* In MeTTa (and first-order logic), a term is recursively */
; /* defined as being either a constant (in MeTTa a lower-case */
; /* atom), a variable (in MeTTa a upper-case variable) or a */
; /* function symbol (in MeTTa a relational expression of the form */
; /* 'p(a,b, ..., X)') which takes a number of terms as arguments. */
; /* Substitutions are unique mappings from variables to Terms, in */
; /* the following denoted by S, and written out as sets of */
; /* variable/term pairs, such as {A/b,B/f(X)}. When a substitution */
; /* S is applied to a term t, each variable within t which is an */
; /* element of the domain of S is replaced by the corresponding */
; /* term within S. Thus, letting t = f(a,A) and S = {A/b,B/f(X)}, */
; /* the term tS = f(a,b). */
; /* The following predicate 'substitution', returns the minimal- */
; /* sized substitution S such that t1 S = t2, when such a */
; /* substitution exists, given two terms t1 and t2. */
; /******************************************************************/
       
;                                           (error
;                                             (syntax-error  operator_expected)
;                                             (file  logic/logic.pl 95 8 5563))

     

  (= (skolemize  () $N $N)  True)
  (= (skolemize (Cons  $Head $Tail) $N $M)  
    (= $Head 
      (%var $N))
    (skolemize $Tail 
      (s $N) $M)
    (set-det))
  (= (skolemize (Cons  $Head $Tail) $N $M)  
    (=.. $Head 
      (Cons  $F $Tail1))
    (skolemize $Tail1 $N $O)
    (skolemize $Tail $O $M))
     

  (= (substitute Nil Nil Nil)  
    (set-det))
  (= (substitute (Cons  $Head1 $Tail1) (Cons  $Head2 $Tail2) (Cons  (/ $Head1 $Head2) $Tail3))  
    (var $Head1)
    (substitute $Tail1 $Tail2 $Tail3)
    (set-det))
  (= (substitute (Cons  $Head1 $Tail1) (Cons  $Head2 $Tail2) $Subst)  
    (=.. $Head1 
      (Cons  $F1 $Tail11))
    (=.. $Head2 
      (Cons  $F1 $Tail22))
    (substitute $Tail11 $Tail22 $Tail33)
    (substitute $Tail1 $Tail2 $Tail4)
    (append $Tail33 $Tail4 $Subst)
    (set-det))


  (= (remove-id Nil Nil)  
    (set-det))
  (= (remove-id (:: $X) (:: $X))  
    (set-det))
  (= (remove-id (Cons  $A (Cons  $B $Tail)) $List)  
    (== $A $B)
    (remove-id 
      (Cons  $A $Tail) $List)
    (set-det))
  (= (remove-id (Cons  $Head1 $Tail1) (Cons  $Head1 $Tail2))  
    (remove-id $Tail1 $Tail2)
    (set-det))


  (= (lgg $Term1 $Term2 $Term3)  
    (lgg1 
      (:: $Term1) 
      (:: $Term2) 
      (:: $Term3) Nil $Subst)
    (set-det))
; /******************************************************************/
; /* */
; /* call : lgg (+Term1,+Term2,-Term3) */
; /* */
; /* arguments : Term1 = first-order logic Term */
; /* Term2 = first-order logic Term */
; /* Term3 = generalized first-order logic Term */
; /* */
; /******************************************************************/
; /* We say that term t1 is a 'generalisation of (or subsumes)' t2 */
; /* iff there exists a substitution S such that t1 S = t2. Also */
; /* Term t is said to be a 'common generalisation' of terms u and */
; /* v iff t is a generalisation of u and t is a generalisation of */
; /* v. In paticular t is the 'least-general-generalisation (lgg)' */
; /* of u and v iff t is a common generalisation of u and v, and */
; /* every other common generalisation t' of u and v is also a */
; /* generalisation of t. */
; /******************************************************************/
   

  (= (lgg1  () () () $Subst $Subst)  True)
  (= (lgg1 (Cons  $Head1 $Tail1) (Cons  $Head2 $Tail2) (Cons  $Head3 $Tail3) $Subst1 $Subst3)  
    (=.. $Head1 
      (Cons  $F $Tail11))
    (=.. $Head2 
      (Cons  $F $Tail22))
    (lgg1 $Tail11 $Tail22 $Tail33 $Subst1 $Subst2)
    (=.. $Head3 
      (Cons  $F $Tail33))
    (lgg1 $Tail1 $Tail2 $Tail3 $Subst2 $Subst3)
    (set-det))
  (= (lgg1 (Cons  $Head1 $Tail1) (Cons  $Head2 $Tail2) (Cons  $Head3 $Tail3) $Subst1 $Subst2)  
    (subst-member 
      (/ $Head3 
        (, $Head1 $Head2)) $Subst1)
    (lgg1 $Tail1 $Tail2 $Tail3 $Subst1 $Subst2)
    (set-det))
  (= (lgg1 (Cons  $Head1 $Tail1) (Cons  $Head2 $Tail2) (Cons  $Head3 $Tail3) $Subst1 $Subst2)  
    (lgg1 $Tail1 $Tail2 $Tail3 
      (Cons  
        (/ $Head3 
          (, $Head1 $Head2)) $Subst1) $Subst2)
    (set-det))


  (= (subst-member (/ $A $B) (Cons  (/ $A $C) $_))  
    (== $B $C)
    (set-det))
  (= (subst-member $A (Cons  $_ $B))  
    (subst-member $A $B)
    (set-det))


  (= (covers  () $_)  True)
; /******************************************************************/
; /* */
; /* call : covers(+Goal,+ClauseList) */
; /* */
; /* arguments : Goal = An instance */
; /* ClauseList = Clauses in a special MeTTa-syntax */
; /* */
; /******************************************************************/
; /* An 'atomic formula' is defined as a predicate symbol which */
; /* takes a number of terms as arguments (such as "mem(a,[b,a])"). */
; /* A 'literal' is defined as being either an atomic formula or */
; /* the negation of an atomic formula (such as "not mem(a,[b,c])". */
; /* A 'clause' is a 'disjunction' of literals. Thus a clause */
; /* (L1 \/ L2 \/ ... Ln) can be represented as a set */
; /* {L1,L2, ... Ln} */
; /* Given two clauses C and D we say that C is a 'generalisation */
; /* (or subsumes)' D whenever there is a substitution S such that */
; /* C S is a subset of or equal to D, w.r.t. a logic program P. */
; /******************************************************************/
  (= (covers (Cons  $H1 $T1) $P)  
    (member $C1 $P)
    (copy $C1 
      (= $H1  $B1))
    (covers-body $B1 $P)
    (covers $T1 $P))


  (= (covers-body  true $_)  True)
  (= (covers-body (, $H $B) $P)  
    (set-det)
    (covers 
      (:: $H) $P)
    (covers-body $B $P))
  (= (covers-body $H $P)  
    (covers 
      (:: $H) $P))
    

  (= (copy $A $B)  
    (or 
      (, 
        (add-atom  &self 
          (yap-inst  $A)) 
        (remove-atom  &self 
          (yap-inst  $B)) 
        (set-det)) 
      (, 
        (remove-atom  &self 
          (yap-inst  $_)) fail)))


  (= (psubsumes $_ Nil)  
    (set-det))
; /******************************************************************/
; /* */
; /* call : psubsumes(+PRG1,+PRG2) */
; /* */
; /* arguments : PRG1 = A clause set in a special MeTTa-syntax */
; /* PRG2 = A clause set in a special MeTTa-syntax */
; /* */
; /******************************************************************/
; /* MeTTa programs consist of a restricted form of clause called */
; /* a 'Horn clause'. Horn clauses contains at most one positive (or*/
; /* unnegated) literal. The positive literal is written as the */
; /* 'head' of a MeTTa clause, while the 'body' of a MeTTa clause */
; /* represents the set of negated literals. The 'goal' of a MeTTa */
; /* program is simply a clause containing no positive literals. A */
; /* MeTTa program should be viewed as a conjunction of clauses. */
; /* This might be represented symbolically as: */
; /* (C1 /\ C2 /\ ... Cn) or in set notation as */
; /* {C1,C2,... Cn} */
; /* Thus an entire MeTTa program can be viewed as a single logical*/
; /* formula. The following is a simplified restatement of */
; /* Herbrand's theorem: */
; /* Given two formulae F1 and F2, F1 is more general than F2 */
; /* iff for every substitution S, (F1 /\ not(F2)) S is false. */
; /******************************************************************/
  (= (psubsumes $P (Cons  $C $T))  
    (bsubsumes $P $C)
    (psubsumes $P $T)
    (set-det))

;                                           (error
;                                             (syntax-error  operator_expected)
;                                             (file  logic/logic.pl 229 7 11814))

	     

  (= (body-units True Nil)  
    (set-det))
  (= (body-units (, $Head1 $Tail1) (Cons  (= $Head1  True) $Tail2))  
    (body-units $Tail1 $Tail2)
    (set-det))
  (= (body-units  $Head ((:-  $Head true)))  True)


  (= (goal-units  () ())  True)
  (= (goal-units (Cons  $Head1 $Tail1) (Cons  (= $Head1  True) $Tail2))  
    (goal-units $Tail1 $Tail2)
    (set-det))


  (= (p-subsumes $ClauseSet1 $Theory $ClauseSet2)  
    (append $ClauseSet1 $Theory $Program)
    (psubsumes $Program $ClauseSet2))
; /******************************************************************/
; /* */
; /* call : p_subsumes(+ClauseSet1,+Theory,+ClauseSet2) */
; /* */
; /* arguments : ClauseSet1 = A set of clauses MeTTa-syntax */
; /* ClauseSet2 = A set of clauses MeTTa-syntax */
; /* Theory = A set of clauses MeTTa-syntax */
; /* */
; /******************************************************************/
; /* This procedure implements the generalized subsumption between */
; /* two clause sets, w.r.t. a background theory. */
; /******************************************************************/
     

  (= (p-equivalent $ClauseSet1 $Theory $ClauseSet2)  
    (append $ClauseSet1 $Theory $Program1)
    (psubsumes $Program1 $ClauseSet2)
    (append $ClauseSet2 $Theory $Program2)
    (psubsumes $Program2 $ClauseSet1))
; /******************************************************************/
; /* */
; /* call : p_subsumes(+ClauseSet1,+Theory,+ClauseSet2) */
; /* */
; /* arguments : ClauseSet1 = A set of clauses MeTTa-syntax */
; /* ClauseSet2 = A set of clauses MeTTa-syntax */
; /* Theory = A set of clauses MeTTa-syntax */
; /* */
; /******************************************************************/
; /* This procedure implements equivalence based on the generalized */
; /* subsumption between two clause sets, w.r.t. a background */
; /* theory. */
; /******************************************************************/
     		    


  (= help  
    ( (write 'Load example calls with command: [Filename].') nl 
      (write 'Call examples with: test1, test2, test3a, test3b,') nl 
      (write '                    test4a, test4b, test4c, test4d, test5') nl))


  !(help *)



