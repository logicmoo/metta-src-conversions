;                                           (convert-to-metta-file  aux $_187446 metagame/misc/aux.pl metagame/misc/aux.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; ============================================================
;
; 
;
; Contains general utility predicates.
;
; Adapted from shared code written by Fernando Pereira,
;
; Martha Pollack, and Barney Pell.
;
; Changes Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;;;;;;;;;;;;;;;;;;;;
;
; ;;;; AUX ;;;;;
;
; ;;;;;;;;;;;;;;;;;;;;



  !(my-use-module (library lists))
  !(use-module (library ordsets))

;
; General auxiliary procedures
;
; Contains:
;
; append/3
;
; average/2
;
; between/3
;
; cgensym/2
;
; concat/3
;
; concat_list/2
;
; cons/3
;
; cull/4
;
; cull_funct/4
;
; extract/3
;
; findall/3
;
; f_cons/3
;
; flatten/2
;
; gensym/2
;
; get_nth/3
;
; increase_term_arity/3
;
; lastsuffix/2
;
; length/2
;
; maplist/3
;
; max/2, max/3
;
; min/2, min/3
;
; mesh/3
;
; member/2
;
; mnl/1
;
; nth/3
;
; nth_letter/2, nth_letter/3
;
; numlist/3
;
; pair_list/3
;
; percolate/3,perc2/4
;
; ppl/1,ppl/2
;
; remove_duplicates/2
;
; remove_test_duplicates/3
;
; reset_gensym/1, reset_gensym/2
;
; rev_append/3
;
; reverse/2,rev2/3
;
; snoc/3
;
; space/0,space/1
;
; split_list/4
;
; split_list_funct/4
;
; stable_sort/2
;
; subset/2
;
; verify/1
;
; whenever/2
;
; ynp/3
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Print mulitple line feeds (arg1 of them).


  (= (mnl 1)  nl)
  (= (mnl $N)  
    ( (> $N 1) nl 
      (is $N1 
        (- $N 1)) 
      (mnl $N1)))


;
; Pretty print the list arg1.
;
; Changed write to print.


  (= (ppl $L)  
    (ppl $L 3))
  (= (ppl  () $Ind)  True)
  (= (ppl (Cons  (Cons  $H $T) $L) $Ind)  
    (is $Ind1 
      (+ $Ind 3))
    (ppl 
      (Cons  $H $T) $Ind1)
    (ppl $L $Ind)
    (set-det))
  (= (ppl (Cons  $H $L) $Ind)  
    ( (space $Ind) 
      (print $H) nl 
      (ppl $L $Ind) 
      (set-det)))
  (= (ppl $A $Ind)  
    ( (space $Ind) 
      (print $A) nl))


;
; Pretty print the list arg1.
;
; Changed write to print.


  (= (pwl $L)  
    (pwl $L 3))
  (= (pwl  () $Ind)  True)
  (= (pwl (Cons  (Cons  $H $T) $L) $Ind)  
    (is $Ind1 
      (+ $Ind 3))
    (pwl 
      (Cons  $H $T) $Ind1)
    (pwl $L $Ind)
    (set-det))
  (= (pwl (Cons  $H $L) $Ind)  
    ( (space $Ind) 
      (write $H) nl 
      (pwl $L $Ind) 
      (set-det)))
  (= (pwl $A $Ind)  
    ( (space $Ind) 
      (write $A) nl))



;
; Print arg1 blank spaces.
;
; Using the predicate name 'space' causes the compiler to crash --Barney


  (= space  
    (write ' '))
  (= (space  0)  True)
  (= (space 1)  space)
  (= (space $N)  
    ( (> $N 1) space 
      (is $N1 
        (- $N 1)) 
      (space $N1)))

;
; Cons an element into a list.


  (= (cons  $E () ($E))  True)
  (= (cons  $E ([|]  $H $T) ([|]  $E ([|]  $H $T)))  True)

;
; "Snoc" an element into the end of a list.


  (= (snoc  $E () ($E))  True)
  (= (snoc $E $L0 $L1)  
    (reverse $L0 $Lr)
    (cons $E $Lr $Lr1)
    (reverse $Lr1 $L1))

;
; Cons an element into a list, unless the element is itself a null list.


  (= (f-cons  () $L $L)  True)
  (= (f-cons  $E ([|]  $H $T) ([|]  $E ([|]  $H $T)))  True)

 


;
; Append the reverse of arg1 to arg2 to give arg3.


  (= (rev-append  () $L $L)  True)
  (= (rev-append (Cons  $H $T) $L $R)  
    (rev-append $T 
      (Cons  $H $L) $R))


;
; Extract arg1 from the list in arg2 to give arg3.
;
; Fails if arg1 is not a member of arg2.


  (= (extract  $Elt ([|]  $Elt $Tail) $Tail)  True)
  (= (extract $Elt (Cons  $Head $Tail) (Cons  $Head $List))  
    (extract $Elt $Tail $List))

;
; Cull all the members of arg2 that match the pattern in arg1.
;
; Culled entities go into arg3; everything else goes into arg4.


  (= (cull  $Pattern () () ())  True)
  (= (cull $Pattern (Cons  $Pattern $T0) (Cons  $Pattern $T1) $R)  
    (cull $Pattern $T0 $T1 $R))
  (= (cull $Pattern (Cons  $H $T0) $C (Cons  $H $T1))  
    (cull $Pattern $T0 $C $T1))

;
; Cull all the members of arg2 whose functor is arg1.
;
; Culled entities go into arg3; everything else goes into arg4.


  (= (cull-funct  $Funct () () ())  True)
  (= (cull-funct $Funct (Cons  $H0 $T0) (Cons  $H0 $T1) $R)  
    (functor $H0 $Funct $_)
    (cull-funct $Funct $T0 $T1 $R))
  (= (cull-funct $Funct (Cons  $H0 $T0) $C (Cons  $H0 $T1))  
    (cull-funct $Funct $T0 $C $T1))

;
; Split_list finds arg1 in arg2, putting everything to the left into arg3 and
;
; everything to the right in arg4. E.g.:
;
; split_list(c,[a,b,c,d,e],[a,b],[d,e]).


  (= (split-list  $Elt ([|]  $Elt $Tail) () $Tail)  True)
  (= (split-list $Elt (Cons  $Head $Tail0) (Cons  $Head $Tail1) $Tail)  
    (split-list $Elt $Tail0 $Tail1 $Tail))

;
; Split_list_funct finds the element with functor arg1 in arg2, putting
;
; everything to the left into arg3 and everything to the right into arg4.


  (= (split-list-funct $Funct (Cons  $Elt $Tail) Nil $Tail)  
    (functor $Elt $Funct $_))
  (= (split-list-funct $Funct (Cons  $H0 $T0) (Cons  $H0 $T1) $T)  
    (split-list-funct $Funct $T0 $T1 $T))


;
; get_nth is like nth, but can be used with arg2 uninstantiated


  (= (get-nth  1 ([|]  $Head $_) $Head)  True)
  (= (get-nth $P (Cons  $_ $Tail) $Elt)  
    (get-nth $P1 $Tail $Elt)
    (is $P 
      (+ $P1 1)))


;
; Letter is the Nth (lowercase) letter.


  (= (nth-letter $N $Letter)  
    (nth-letter-after $N a $Letter)
    (> $N 0)
    (=< $N 26))

;
; Letter is the Nth letter, starting at Letter0.


  (= (nth-letter-after $N $Letter0 $Letter)  
    (atom $Letter)
    (set-det)
    (name $Letter0 
      (:: $A))
    (name $Letter 
      (:: $L))
    (is $N 
      (+ 
        (- $L $A) 1)))
  (= (nth-letter-after $N $Letter0 $Letter)  
    (integer $N)
    (name $Letter0 
      (:: $A))
    (is $L 
      (- 
        (+ $N $A) 1))
    (name $Letter 
      (:: $L)))


;
; Percolate arg1 to the head of arg2 to give arg3.


  (= (percolate $M $L1 $L2)  
    (perc2 $M $L1 Nil $L2))


  (= (perc2 $M (Cons  $M $Lt) $Lp (Cons  $M $L))  
    (reverse $Lp $Lpr)
    (append $Lpr $Lt $L))
  (= (perc2 $M (Cons  $Lh $Lt) $Lp $L)  
    (perc2 $M $Lt 
      (Cons  $Lh $Lp) $L))

;
; Interleaving elements of arg1 and arg2, preserving order within each list
;
; gives arg3.


  (= (mesh  () $L $L)  True)
  (= (mesh  $L () $L)  True)
  (= (mesh (Cons  $A $As) (Cons  $B $Bs) (Cons  $A $Rest))  
    (mesh $As 
      (Cons  $B $Bs) $Rest))
  (= (mesh (Cons  $A $As) (Cons  $B $Bs) (Cons  $B $Rest))  
    (mesh 
      (Cons  $A $As) $Bs $Rest))


;
; flatten(Tree, List)
;
; flattens a Tree of cons cells into a List.
;
; This from Quintus Library Flatten.

  (= (flatten $Tree $List)  
    (flatten $Tree $List Nil))


  (= (-->  (flatten  ()) !)  True)
  (= (-->  (flatten  ([|]  $Head $Tail)) (,  ! (,  (flatten  $Head) (flatten  $Tail))))  True)
  (= (-->  (flatten  $Other) ($Other))  True)
; ; { Other ~= [], Other ~= [_|_] },



;
; Generate in arg2 a new symbol with prefix arg1, and a unique suffix.


  (= (gensym $Prefix $V)  
    ( (var $V) 
      (atomic $Prefix) 
      (lastsuffix $Prefix $M) 
      (is $N 
        (+ $M 1)) 
      (add-atom  &self 
        (flag  
          (gensym  $Prefix) $N)) 
      (concat $Prefix $N $V) 
      (set-det)))

;
; Like gensym, but if V is instantiated it will be left alone.


  (= (cgensym $Prefix $V)  
    (var $V)
    (set-det)
    (gensym $Prefix $V))
  (= (cgensym  $_ $_)  True) 



  (= (lastsuffix $Prefix $M)  
    ( (remove-atom  &self 
        (flag  
          (gensym  $Prefix) $M)) (set-det)))
  (= (lastsuffix  $Prefix 0)  True)


;
; Set all gensym suffixes back to 1.


  (= reset-gensym  
    (remove-all-atoms  &self 
      (flag  
        (gensym  $Prefix) $M)))

  (= (reset-gensym $Prefix)  
    (remove-all-atoms  &self 
      (flag  
        (gensym  $Prefix) $M)))


;
; BI_NAME(?Atom,?List)
;
; A version of name/2 facilitating bidirectional programs,
;
; where both args can be unbound at the time it is called, and they
;
; will be frozen until one is bound.


  (= (bi-name $Atom $List)  
    (when 
      (or 
        (ground $Atom) 
        (ground $List)) 
      (name $Atom $List)))


;
; Form an atom in arg3 that is the concatenation of arg1 and arg2.


  (= (concat $N1 $N2 $N3)  
    (name $N1 $Ls1)
    (name $N2 $Ls2)
    (append $Ls1 $Ls2 $Ls3)
    (name $N3 $Ls3))


  (= (concat-list (:: $A) $A)  
    (set-det))
  (= (concat-list (Cons  $A $Bs) $C)  
    (concat-list $Bs $Bconc)
    (concat $A $Bconc $C))



  (= (append-list (:: $A) $A)  
    (set-det))
  (= (append-list (Cons  $A $Bs) $C)  
    (append-list $Bs $Bconc)
    (append $A $Bconc $C))


;
; BI_CONCAT(?N1, ?N2, ?N3)
;
; A bidirectional version of concat/3.
;
; This is also provided in quintus string_append/3 procedure.

  (= (bi-concat $N1 $N2 $N3)  
    (det-if-then-else 
      (atom $N3) 
      (, 
        (name $N3 $L3) 
        (append $L1 $L2 $L3) 
        (name $N2 $L2) 
        (name $N1 $L1)) 
      (det-if-then-else 
        (atom $N1) 
        (concat $N1 $N2 $N3) 
        (, 
          (format "Error, uninstantiated args in bi_concat~n" Nil) fail))))


  (= (bi-concat-list (:: $A) $A)  
    (set-det))
; /* This is sicstus-specific, so not used. bi_concat(N1, N2, N3) :- bi_name(N1, Ls1), bi_name(N2, Ls2), append(Ls1, Ls2, Ls3), bi_name(N3, Ls3). */
; ; BI_CONCAT_LIST(List,Conc) ; Bi-directional verion of concat_list/3.
  (= (bi-concat-list (Cons  $A $Bs) $C)  
    (bi-concat-list $Bs $Bconc)
    (bi-concat $A $Bconc $C))


;
; Guarantee that a response designates either yes or no.


  (= (ynp y y $Goal)  
    (set-det))
  (= (ynp yes y $Goal)  
    (set-det))
  (= (ynp n n $Goal)  
    (set-det))
  (= (ynp no n $Goal)  
    (set-det))
  (= (ynp $Resp $RespVal $Goal)  
    ( (write 'Please respond with y or n.') nl 
      (call $Goal)))


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; count_bagof and count_setof
;
; Measures the set returned by the base procedure.
;
; Will not backtrack, so A,B, better instantiate all the variables
;
; in needs!

  (= (count-bagof $A $B $C)  
    (det-if-then-else 
      (bagof $A $B $C1) 
      (length $C1 $C) 
      (= $C 0)))


  (= (count-setof $A $B $C)  
    (det-if-then-else 
      (setof $A $B $C1) 
      (length $C1 $C) 
      (= $C 0)))


  (= (count-findall $A $B $C)  
    (findall $A $B $C1)
    (length $C1 $C))

;
; maplist(P,ListIn,ListOut).
;
; Apply the predicate P to each element of a list L to form a new list M.
;
; We assume that P has two arguments, where the first is the input, and the
;
; second the output.
;
; From CLOCKSIN & MELLISH, p. 173.


  (= (maplist  $_ () ())  True)
  (= (maplist $P (Cons  $X $L) (Cons  $Y $M))  
    (=.. $Q 
      (:: $P $X $Y))
    (call $Q)
    (maplist $P $L $M))



;
; For each instance of generator, call goal.


  (= (whenever $Generator $Goal)  
    (or 
      (, 
        (call $Generator) 
        (call $Goal) fail) True))




  (= (verify $Goal)  
    (not (not $Goal)))



  (= (remove-test-duplicates $List $Test $Clean)  
    (remove-test-duplicates $List $Test Nil $Clean))
; /* In sicstus lists library, but not in Quintus, so now put in quintus-version. ; remove_duplicates(+List, ?Pruned) ; is true when Pruned is like List but with all *identical* duplicate ; elements removed. remove_duplicates([], []). remove_duplicates([Head|Tail1], [Head|Tail2]) :- delete(Tail1, Head, Residue), remove_duplicates(Residue, Tail2). */
; ;;; remove_test_duplicates(List,Test,Clean) ;;; Test is of the form: ;;; test(CALL,Pat1,Pat2), ;;; where CALL uses Pat1 & 2.. ;;; ;;; EX: ;;; remove_test_duplicates([f(a,b),f(b,c),f(a,d),f(g,a),f(b,a)], ;;; test((Pat1 = f(A,_),Pat2 = f(A,_)),Pat1,Pat2),Clean). ;;; Pat1 = f(a,d), ;;; A = a, ;;; Pat2 = f(a,b), ;;; Clean = [f(a,b),f(b,c),f(g,a),f(b,a)]

  (= (remove-test-duplicates  () $_ $_ ())  True)
  (= (remove-test-duplicates (Cons  $H $T) $Test_Call $Seen $Clean)  
    (= $Test_Call 
      (test $Test $Pattern1 $Pattern2))
    (member $Pattern2 $Seen)
    (= $H $Pattern1)
    (call $Test)
    (set-det)
    (remove-test-duplicates $T $Test_Call $Seen $Clean))
  (= (remove-test-duplicates (Cons  $H $T) $Test_Call $Seen (Cons  $H $Clean))  
    (remove-test-duplicates $T $Test_Call 
      (Cons  $H $Seen) $Clean))

;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
; Arg2 is the average of Arg1, 0 if empty.


  (= (average $List $Avg)  
    (length $List $N)
    (det-if-then-else 
      (> $N 0) 
      (, 
        (sum-list $List $Sum) 
        (is $Avg 
          (/ $Sum $N))) 
      (= $Avg 0)))
 
;
; Arg2 is the maximum number in Arg1.


  (= (max (Cons  $A $Rest) $Val)  
    (max1 $Rest $A $Val))


  (= (max1  () $A $A)  True)
  (= (max1 (Cons  $H $T) $Old $Val)  
    (max $H $Old $New)
    (max1 $T $New $Val))

;
; Arg3 is the maximum of arg1 and arg2.

  (= (max $A $B $A)  
    (> $A $B)
    (set-det))
  (= (max  $A $B $B)  True)


;
; Arg2 is the minimum number in Arg1.

  (= (min (Cons  $A $Rest) $Val)  
    (min1 $Rest $A $Val))


  (= (min1  () $A $A)  True)
  (= (min1 (Cons  $H $T) $Old $Val)  
    (min $H $Old $New)
    (min1 $T $New $Val))

;
; Arg3 is the minimum of arg1 and arg2.

  (= (min $A $B $A)  
    (< $A $B)
    (set-det))
  (= (min  $A $B $B)  True)


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (writeln $Arg)  
    ( (write $Arg) nl))


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (= (reduce-list (Cons  $A $B) $R)  
    (set-det)
    (reduce-list $A $A1)
    (reduce-list $B $B1)
    (append $A1 $B1 $R))
  (= (reduce-list Nil Nil)  
    (set-det))
  (= (reduce-list  $X ($X))  True)


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (increase-term-arity $TermIn $NewArg $TermOut)  
    (functor $TermIn $Func $N)
    (is $N1 
      (+ $N 1))
    (functor $TermOut $Func $N1)
    (arg $N1 $TermOut $NewArg)
    (copy-args $N $TermIn $TermOut))


  (= (copy-args 0 $TermIn $TermOut)  
    (set-det))
  (= (copy-args $N $TermIn $TermOut)  
    (arg $N $TermIn $Arg)
    (arg $N $TermOut $Arg)
    (is $N1 
      (- $N 1))
    (copy-args $N1 $TermIn $TermOut))


;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (portray-clauses $Clauses)  
    (whenever 
      (member $C $Clauses) 
      (portray-clause $C)))

;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (= (dotimes 0 $_)  
    (set-det))
  (= (dotimes $N $Call)  
    ($Call 
      (is $N1 
        (- $N 1)) 
      (dotimes $N1 $Call)))

;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; STABLE_SORT(+In,?Out)
;
; This is a stable version of built-in sort/2, which does
;
; not merge duplicate entries.
;
; It is based on the stable built-in keysort/2, so we first
;
; convert the list into pairs. It would be nice if MeTTa
;
; provided this built-in also!

  (= (stable-sort $In $Out)  
    (pair-list $In $_ $Keyed)
    (keysort $Keyed $SortKeyed)
    (pair-list $Out $_ $SortKeyed))

;
; PAIR_LIST(?A,?B,?C)
;
; C is a list whose Nth member is A(N)-B(N).
;
; pair_list([1,2,3],[a,b,c],[1-a,2-b,3-c])

  (= (pair-list  () () ())  True)
  (= (pair-list (Cons  $A $As) (Cons  $B $Bs) (Cons  (- $A $B) $Rest))  
    (pair-list $As $Bs $Rest))


  (= (rev-pair-list  () ())  True)
  (= (rev-pair-list (Cons  (- $A $AV) $RestA) (Cons  (- $AV $A) $RestB))  
    (rev-pair-list $RestA $RestB))


;
; ============================================================================
;
; String Pattern Matching
;
; ============================================================================

;
; CONTAINS(Symbol,Text) :- con(Text, ([],Symbol)).
;
; True if Symbol is contained in Text
;
; Both are lists of characters.
;
; Will backtrack over all possibilities.
;
; From Sahlin's phd thesis.


  (= (contains $Symbol $Text)  
    (con $Text $Symbol))


  (= (con  $_ (,  $_ ()))  True)
  (= (con (Cons  $C $Rtext) $SymbInfo)  
    (new $C $SymbInfo $SymbInfoNew)
    (con $Rtext $SymbInfoNew))


  (= (new $C (, $Prefix $C $RestPostfix) (, $PrefixNew $RestPostfix))  
    (append $Prefix 
      (:: $C) $PrefixNew))
  (= (new $C (, $Prefix $D $RestPostfix) (, $PrefixNew $PostfixNew))  
    (\== $C $D)
    (append $Prefix 
      (:: $C) $H)
    (append $PrefixNew $Rest $Prefix)
    (append $_ $PrefixNew $H)
    (append $Rest 
      (Cons  $D $RestPostfix) $PostfixNew))


;
; FOUND(Symbol)
;
; 
;
; Read a sequence of chars until a pattern is found which matches each
;
; char. Uses Sahlin's routines above.
	

  (= (found $Symbol)  
    (found1 $Symbol))


  (= (found1  (,  $_ ()))  True)
  (= (found1 $SymbInfo)  
    (get0 $C)
    (new $C $SymbInfo $SymbInfoNew)
    (found1 $SymbInfoNew))

;
; ============================================================================
;
; between(+Lower, +Upper, ?Number)
;
; is true when Lower, Upper, and Number are integers,
;
; and Lower =< Number =< Upper. If Lower and Upper are given,
;
; Number can be tested or enumerated. If either Lower or Upper
;
; is absent, there is not enough information to find it, and an
;
; error will be reported.
;
; From shared code by Richard O'Keefe.


  (= (between $Lower $Upper $Point)  
    (integer $Lower)
    (integer $Upper)
    (or 
      (, 
        (integer $Point) 
        (set-det) 
        (=< $Lower $Point) 
        (=< $Point $Upper)) 
      (, 
        (var $Point) 
        (set-det) 
        (=< $Lower $Upper) 
        (between1 $Lower $Upper $Point))))
; ; These cuts must be cuts;
; ; they can't be arrows.

;
; ; between1(Lower, Upper, Point)
;
; enumerates values of Point satisfying Lower =< Point =< Upper,
;
; where it is already known that Lower =< Upper and Point was a
;
; variable. A purer version of this is left as a comment.


  (= (between1 $L $L $L)  
    (set-det))
  (= (between1  $L $_ $L)  True)		;
; between1(L, U, L) :- L =< U.
  (= (between1 $L $U $N)  
    (is $M 
      (+ $L 1))
    (between1 $M $U $N))
; ; between1(L, U, N) :- L < U,
; ; M is L+1,	;
; between1(M, U, N).
;
; ============================================================================



  (= (numlist $Min $Max Nil)  
    (> $Min $Max)
    (set-det))
  (= (numlist $Min $Max (Cons  $Min $Rest))  
    (is $Min1 
      (+ $Min 1))
    (numlist $Min1 $Max $Rest))



;
; Suceeds on the unique occurrence for A if
;
; it is constant, otherwise backtracks over B.

  (= (member1 $A $B)  
    (det-if-then-else 
      (var $A) 
      (member $A $B) 
      (memberchk $A $B)))
	
;
; ============================================================================
;
; Association Lists
;
; ============================================================================


  (= (member1-pair (- $H $T) $B)  
    (det-if-then-else 
      (var $H) 
      (member 
        (- $H $T) $B) 
      (memberchk 
        (- $H $T) $B)))



  (= (assoc $List $Prop $Val)  
    (member1-pair 
      (- $Prop $Val) $List))


  (= (set-assoc $List1 $Param $Val $New)  
    (ensure-list $List1 $List2)
    (delete-all-assoc $Param $List2 $List)
    (cons 
      (- $Param $Val) $List $New)) 


  (= (ensure-list $List1 $List1)  
    (is-list $List1)
    (set-det))
  (= (ensure-list  $_ ())  True)


  (= (delete-all-assoc $P $L $L2)  
    (member 
      (- $P $V1) $L)
    (set-det)
    (delete $L 
      (- $P $V1) $L1)
    (delete-all-assoc $P $L1 $L2))
  (= (delete-all-assoc  $_ $L $L)  True)

;
; ============================================================================
;
; Regions
;
; ============================================================================

;
; in_region([a-0.7,b-0.2,c-0.1],0.89,Choice).

  (= (in-region (:: (- $Choice $Prob)) $_ $Choice)  
    (set-det))
  (= (in-region (Cons  (- $C $P) $Choices) $Prob $C)  
    (< $Prob $P)
    (set-det))
  (= (in-region (Cons  (- $C $P) $Choices) $Prob $Choice)  
    (is $PRest 
      (- $Prob $P))
    (in-region $Choices $PRest $Choice))



