;
;                      MODULE div_utils  EXPORTS



  !(module div-utils 
    (:: 
      (/ remove 3) 
      (/ sort-by-length 3) 
      (/ mysetof 3) 
      (/ sum 2) 
      (/ efface 3) 
      (/ buildpar2 3) 
      (/ neg 2) 
      (/ contains-duplicates 1) 
      (/ contains-identicals 1) 
      (/ identical-member 2) 
      (/ convert-to-horn-clause 3) 
      (/ extract-body 2) 
      (/ list-to-struct 2) 
      (/ clist-to-prolog 2) 
      (/ append-all 2) 
      (/ maximum 2) 
      (/ myforall 2) 
      (/ identical-make-unique 2) 
      (/ remove-v 3) 
      (/ remove-variant 3) 
      (/ make-unique 2) 
      (/ variant-mem 2) 
      (/ different-predicates 2) 
      (/ nth-arg 3) 
      (/ split-examples 4) 
      (/ shares-var 2) 
      (/ body2list 2) 
      (/ insert-unique 3) 
      (/ insert-unique 4) 
      (/ effaceall 3) 
      (/ genterm-test 2) 
      (/ subset-chk 2) 
      (/ best 2) 
      (/ subterm-at-position 4) 
      (/ part-of-clause 2) 
      (/ fak 2) 
      (/ fak1 3) 
      (/ nueberk 3) 
      (/ log2 2) 
      (/ log2nueberk 3) 
      (/ sum-of-logs 3)))


;
;                       METAPREDICATES

  !(meta-predicate (mysetof + : -))


;
;                        IMPORTS


  !(use-module-if-exists 
    (library basics) 
    (:: (/ member 2)))
  !(use-module-if-exists 
    (library subsumes) 
    (:: (/ variant 2)))
  !(use-module-if-exists 
    (library occurs) 
    (:: 
      (/ sub-term 2) 
      (/ contains-var 2)))
  !(use-module-if-exists 
    (library lists) 
    (:: (/ rev 2)))
  !(use-module-if-exists 
    (library math) 
    (:: (/ log 2)))

;
;                         ***********************************************************************

;
;                          *

;
;                          * module: div_utils.pl

;
;                          *

;
;                          * author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92

;
;                          *

;
;                           * changed:

;
;                           *

;
;                           * description:	small auxiliary procedures

;
;                           *

;
;                           * see also:

;
;                            *

;
;                            ***********************************************************************



;
;                            ***********************************************************************

;
;                             *

;
;                             * predicate:	remove/3

;
;                             *

;
;                             * syntax: remove(+I,+L,-L)

;
;                             *

;
;                              * args: I .. number, L ... list of numbers

;
;                              *

;
;                              * description:	removes I from L (I occurs at most once in L)

;
;                              *

;
;                              * example:

;
;                               *

;
;                               * peculiarities:	none

;
;                               *

;
;                               * see also:

;
;                               *

;
;                                ***********************************************************************



  (= 
    (remove  $_ () ()) True)
  (= 
    (remove $I 
      (Cons  $I $R) $R) 
    (set-det))
  (= 
    (remove $I 
      (Cons  $J $R) 
      (Cons  $J $R1)) 
    (remove $I $R $R1))

;
;                                 ***********************************************************************

;
;                                  *

;
;                                  * predicate:	sort_by_length/3

;
;                                  *

;
;                                  * syntax: sort_by_length(+L,+Accu,-Accu)

;
;                                  *

;
;                                   * args: L ... list of lists

;
;                                   *       Accu ... L sorted increasingly according to the length of sublists

;
;                                   *

;
;                                   * description:	sorts a list of lists increasingly according to

;
;                                   *              the length of sublists

;
;                                    *

;
;                                    * example:

;
;                                    *

;
;                                    * peculiarities:	none

;
;                                    *

;
;                                     * see also:

;
;                                     *

;
;                                     ***********************************************************************



  (= 
    (sort_by_length  () $L $L) True)
  (= 
    (sort-by-length 
      (Cons  $IXS $R) $L $L2) 
    ( (insert-by-length $IXS $L $L1) (sort-by-length $R $L1 $L2)))


  (= 
    (insert-by-length $X 
      (Cons  $Y $R) 
      (Cons  $Y $R1)) 
    ( (length $X $N) 
      (length $Y $N1) 
      (> $N $N1) 
      (set-det) 
      (insert-by-length $X $R $R1)))
  (= 
    (insert_by_length  $X $L 
      (Cons  $X $L)) True)


;
;                                        ***********************************************************************

;
;                                        *

;
;                                        * predicate:	mysetof/3

;
;                                        *

;
;                                        * syntax: mysetof(+Template,+Generator,-Set)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	as setof/3, but succeeds with Set = [], if Generator

;
;                                         *		fails

;
;                                         *

;
;                                         * example: setof(X, append([1,2,3],X,[4,5]),Set) -> fail

;
;                                         *          mysetof(X, append([1,2,3],X,[4,5]),Set) -> Set = []

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
    (mysetof $A $B $C) 
    ( (setof $A $B $C) (set-det)))
  (= 
    (mysetof  $_ $_ ()) True)


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	sum/2

;
;                                         *

;
;                                         * syntax: sum(+LN,-S)

;
;                                         *

;
;                                         * args: LN .. list of numbers, S number

;
;                                         *

;
;                                         * description:	if LN = [I1,..,In], then S = I1 + ... + In

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
    (sum  
      ($I) $I) True)
  (= 
    (sum 
      (Cons  $I $More) $C) 
    ( (sum $More $J) (is $C (+ $I $J))))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	efface/3 (by Clocksin/Mellish)

;
;                                         *

;
;                                         * syntax: efface(+E,+L,-L)

;
;                                         *

;
;                                         * args: E .. element of list L

;
;                                         *

;
;                                         * description:	removes the first element of L that is unifiable with E

;
;                                         *              from L.

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
    (efface $A 
      (Cons  $A $L) $L) 
    (set-det))
  (= 
    (efface $A 
      (Cons  $B $L) 
      (Cons  $B $M)) 
    (efface $A $L $M))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: effaceall/3

;
;                                         *

;
;                                         * syntax: effacell(+E,+L,-L)

;
;                                         *

;
;                                         * args: E .. element of list L

;
;                                         *

;
;                                         * description:	as efface, but allows backtracking

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
    (effaceall  $A 
      (Cons  $A $L) $L) True)
  (= 
    (effaceall $A 
      (Cons  $B $L) 
      (Cons  $B $M)) 
    (effaceall $A $L $M))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	best/2

;
;                                         *

;
;                                         * syntax: best(+List,-Elem)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	returns the first element of List, on backtracking

;
;                                         *              the second etc.

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
    (best  
      (Cons  $X $_) $X) True)
  (= 
    (best 
      (Cons  $_ $R) $X) 
    (best $R $X))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	buildpar2/3

;
;                                         *

;
;                                         * syntax: buildpar2(+Lit:M,+CL,-CL1)

;
;                                         *

;
;                                         * args: Lit .. literal, M in {p,n,r}, CL and CL1 clauses in list representation

;
;                                         *

;
;                                         * description:	if M = p then CL1 = [Lit:p|CL]

;
;                                         *		else CL1 results from CL by adding Lit:M at the end

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
    (buildpar2  
      (:  $Elem1 p) $List2 
      (Cons  
        (:  $Elem1 p) $List2)) True)
  (= 
    (buildpar2 $ResLit $List2 $Parent2) 
    (append $List2 
      (:: $ResLit) $Parent2))


;
;                                         ************************************************************************

;
;                                         *

;
;                                         * predicate: neg/2

;
;                                         *

;
;                                         * syntax: neg(+Lit:M,-Lit:M1)

;
;                                         *

;
;                                         * args: Lit .... literal, M in {p,n,r}

;
;                                         *

;
;                                         * description: switches the mark of the literal, i.e. if M = p then

;
;                                         *              M1 = n and vice versa

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
;                                         * see also:

;
;                                         *

;
;                                         ************************************************************************



  (= 
    (neg  
      (:  $F p) 
      (:  $F n)) True)
  (= 
    (neg  
      (:  $F n) 
      (:  $F p)) True)


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	contains_duplicates/1

;
;                                         *

;
;                                         * syntax: contains_duplicates(+L)

;
;                                         *

;
;                                         * args: L ... list

;
;                                         *

;
;                                         * description:	succeeds if L contains two unifiable elements

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
    (contains-duplicates (Cons  $H $T)) 
    (member $H $T))
  (= 
    (contains-duplicates (Cons  $_ $T)) 
    (contains-duplicates $T))



;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	contains_identicals/1

;
;                                         *

;
;                                         * syntax: contains_identicals(+L)

;
;                                         *

;
;                                         * args: L ... list

;
;                                         *

;
;                                         * description:	succeeds if L contains two identical (==) elements

;
;                                         *

;
;                                         * example:

;
;                                         *

;
;                                         ***********************************************************************



  (= 
    (contains-identicals (Cons  $H $T)) 
    (contains-var $H $T))
  (= 
    (contains-identicals (Cons  $_ $T)) 
    (contains-identicals $T))



;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: identical_member/2

;
;                                         *

;
;                                         * syntax: identical_member(+Elem,+List)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description: succeeds if Elem is identically (==) contained  in List

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
    (identical-member $A 
      (Cons  $A1 $_)) 
    (== $A $A1))    
  (= 
    (identical-member $A 
      (Cons  $_ $R)) 
    (identical-member $A $R)) 


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	convert_to_horn_clause/3

;
;                                         *

;
;                                         * syntax: convert_to_horn_clause(+PHead,+CL,-HCL)

;
;                                         *

;
;                                         * args: PHead ... preferred head

;
;                                         *       CL ... general clause in list representation

;
;                                         *       HCL ... horn clause in list representation

;
;                                         *

;
;                                         * description:	if CL = [H1:p,..,Hn:p,L1:M1,..,Lm:Mm] where Mi in {p,r}

;
;                                         *              then HCL = [Hj:p,L1:M1,...,Lm:Mm], where Hj is the first

;
;                                         *                   head in CL unifiable with PHead (if one exists), else

;
;                                         *                   the first head in CL

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
    (convert-to-horn-clause $PrefHead $GenClause $HornClause) 
    ( (extract-body $GenClause $Body) 
      (set-det) 
      (det-if-then-else 
        (member 
          (with_self  $PrefHead 
            (p)) $GenClause) 
        (= $Head $PrefHead) 
        (member 
          (with_self  $Head 
            (p)) $GenClause)) 
      (= $HornClause 
        (Cons  
          (with_self  $Head 
            (p)) $Body))))
;                                         ; if preferred head is among

;                                         ; candidates, select it.

;                                         ; Else select first candidate.





;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	extract_body/2

;
;                                         *

;
;                                         * syntax: extract_body(+CL,-CL1)

;
;                                         *

;
;                                         * args: CL .. clause in list representation

;
;                                         *       CL1 = [...,L:M,...] where M in {p,n} and L in CL

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
;                                         * peculiarities:

;
;                                         *

;
;                                         * see also:

;
;                                         *

;
;                                         ***********************************************************************




  (= 
    (extract_body  () ()) True)
  (= 
    (extract-body 
      (Cons  
        (with_self  $L 
          (n)) $Rest) 
      (Cons  
        (with_self  $L 
          (n)) $Rest1)) 
    (extract-body $Rest $Rest1))
  (= 
    (extract-body 
      (Cons  
        (with_self  $L 
          (r)) $Rest) 
      (Cons  
        (with_self  $L 
          (r)) $Rest1)) 
    (extract-body $Rest $Rest1))
  (= 
    (extract-body 
      (Cons  
        (with_self  $_ 
          (p)) $Rest) $Rest1) 
    (extract-body $Rest $Rest1))



;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: 	list_to_struct/2

;
;                                         *

;
;                                         * syntax: list_to_struct(+L,-C)

;
;                                         *

;
;                                         * args: L ... list, C ... conjunction of elements of L

;
;                                         *

;
;                                         * description:	if L = [E1,...,En] then C = (E1,..,En)

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
    (list-to-struct 
      (Cons  $A 
        (Cons  $B $Rest)) 
      (, $A $Rest1)) 
    (list-to-struct 
      (Cons  $B $Rest) $Rest1))
  (= 
    (list_to_struct  
      ($A) $A) True)
  (= 
    (list_to_struct  () true) True)


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: clist_to_MeTTa/2

;
;                                         *

;
;                                         * syntax: clist_to_MeTTa(+CL,-C)

;
;                                         *

;
;                                         * args: CL .. Horn clause in list representation

;
;                                         *       C ... Horn clause in MeTTa format

;
;                                         *

;
;                                         * description:	convert list format to clause format and vice versa

;
;                                         *              (should use body2list!!)

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
    (clist-to-prolog 
      (Cons  
        (with_self  $A 
          (p)) 
        (Cons  $B $Rest)) 
      (= $A $Rest1)) 
    ( (set-det) (clist-to-prolog (Cons  $B $Rest) $Rest1)))
  (= 
    (clist-to-prolog 
      (:: (with_self  $A (p))) 
      (= $A True)) 
    (set-det))
  (= 
    (clist-to-prolog 
      (Cons  
        (with_self  $A 
          (n)) 
        (Cons  $B $Rest)) 
      (, $A $Rest1)) 
    ( (set-det) (clist-to-prolog (Cons  $B $Rest) $Rest1)))
  (= 
    (clist-to-prolog 
      (:: (with_self  $A (n))) $A) 
    (set-det))
  (= 
    (clist-to-prolog 
      (Cons  
        (with_self  $A 
          (r)) 
        (Cons  $B $Rest)) 
      (, $A $Rest1)) 
    (clist-to-prolog 
      (Cons  $B $Rest) $Rest1))
  (= 
    (clist_to_prolog  
      ( (:  $A r)) $A) True)


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	append_all/2

;
;                                         *

;
;                                         * syntax: append_all(+LL,-L)

;
;                                         *

;
;                                         * args: LL .. list of lists, L .. list

;
;                                         *

;
;                                         * description:	appends all lists in LL -> L

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
    (append_all  () ()) True)
  (= 
    (append-all 
      (Cons  $P $R) $R2) 
    ( (append-all $R $R1) (append $P $R1 $R2)))



;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	maximum/2

;
;                                         *

;
;                                         * syntax: maximum(+L,-M)

;
;                                         *

;
;                                         * args: L .. list of numbers, M number

;
;                                         *

;
;                                         * description:	M is the maximum element of L

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
    (maximum  
      ($I) $I) True)

  (= 
    (maximum 
      (Cons  $I $Rest) $I) 
    ( (maximum $Rest $J) 
      (>= $I $J) 
      (set-det)))

  (= 
    (maximum 
      (Cons  $_ $Rest) $J) 
    ( (maximum $Rest $J) (set-det))) 

									
;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	myforall/2

;
;                                         *

;
;                                         * syntax: myforall(+E,+Pred)

;
;                                         *

;
;                                         * args: E ... argument terms, Pred .. type predicate

;
;                                         *

;
;                                         * description:	calls Pred(e) for each e in E, and succeeds only if

;
;                                         *              every call succeeds

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
    (myforall  () $_) True)
  (= 
    (myforall 
      (Cons  $E $R) $Pred) 
    ( (=.. $C 
        (:: $Pred $E)) 
      (call $C) 
      (myforall $R $Pred)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	identical_make_unique/2

;
;                                         *

;
;                                         * syntax: identical_make_unique(+L,-L1)

;
;                                         *

;
;                                         * args: L,L1 ... lists

;
;                                         *

;
;                                         * description: removes all identical duplicates (==) from L

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
    (identical_make_unique  () ()) True)
  (= 
    (identical-make-unique 
      (Cons  $X $R) $R1) 
    ( (contains-var $X $R) 
      (set-det) 
      (identical-make-unique $R $R1)))   
  (= 
    (identical-make-unique 
      (Cons  $X $R) 
      (Cons  $X $R1)) 
    (identical-make-unique $R $R1)) 


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: remove_v/3

;
;                                         *

;
;                                         * syntax: remove_v(+L0,+L,-L1)

;
;                                         *

;
;                                         * args: L0,L,L1 lists

;
;                                         *

;
;                                         * description:	removes each E in L0  from L if E is identically (==)

;
;                                         *              contained  in  L

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
    (remove_v  $_ () ()) True)
  (= 
    (remove-v $T 
      (Cons  $T1 $R) $R1) 
    ( (identical-member $T1 $T) 
      (set-det) 
      (remove-v $T $R $R1)))
  (= 
    (remove-v $T 
      (Cons  $T1 $R) 
      (Cons  $T1 $R1)) 
    (remove-v $T $R $R1))



;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: remove_variant/3

;
;                                         *

;
;                                         * syntax: remove_variant(+L0,+L,-L1)

;
;                                         *

;
;                                         * args: L0,L,L1 lists

;
;                                         *

;
;                                         * description:	removes each E in L0  from L if E is

;
;                                         *              contained as variant in  L

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
    (remove_variant  $_ () ()) True)
  (= 
    (remove-variant $T 
      (Cons  $T1 $R) $R1) 
    ( (variant-mem $T1 $T) 
      (set-det) 
      (remove-variant $T $R $R1)))
  (= 
    (remove-variant $T 
      (Cons  $T1 $R) 
      (Cons  $T1 $R1)) 
    (remove-variant $T $R $R1))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: make_unique/2

;
;                                         *

;
;                                         * syntax: make_unique(+L,-L1)

;
;                                         *

;
;                                         * args: L,L1 .. lists

;
;                                         *

;
;                                         * description:	removes all duplicates (variant) from L

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
    (make_unique  () ()) True)
  (= 
    (make-unique 
      (Cons  $X $R) $R1) 
    ( (variant-mem $X $R) 
      (set-det) 
      (make-unique $R $R1)))
  (= 
    (make-unique 
      (Cons  $X $R) 
      (Cons  $X $R1)) 
    (make-unique $R $R1))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: variant_mem/2

;
;                                         *

;
;                                         * syntax: variant_mem(+Elem,+List)

;
;                                         *

;
;                                         * args:

;
;                                         *

;
;                                         * description:	succeeds if an alphabetical variant of Elem is

;
;                                         *              contained in List

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
    (variant-mem $T 
      (Cons  $T1 $_)) 
    ( (variant $T $T1) (set-det)))
  (= 
    (variant-mem $T 
      (Cons  $_ $R)) 
    (variant-mem $T $R))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: different_predicates/2

;
;                                         *

;
;                                         * syntax: different_predicates(+L,-LL)

;
;                                         *

;
;                                         * args: L .. list of terms, LL list of lists of terms

;
;                                         *

;
;                                         * description:	for each functor f/n occuring in L, LL contains a list Lf

;
;                                         *      consisting of all terms in L with principal functor f/n

;
;                                         *

;
;                                         * example: L = [f(a,b),f(c,d),h(g)] LL = [[f(a,b),f(c,d)],[h(g)]]

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
    (different_predicates  () ()) True)
  (= 
    (different-predicates 
      (Cons  $E $R) 
      (Cons  
        (Cons  $E $Es) $R2)) 
    ( (functor $E $F $N) 
      (diff-predicates $R $R1 $Es $F $N) 
      (different-predicates $R1 $R2)))


  (= 
    (diff_predicates  () () () $_ $_) True)
  (= 
    (diff-predicates 
      (Cons  $E $R) $R2 $Es2 $_ 0) 
    ( (set-det) 
      (diff-predicates $R $R1 $Es1 $_ 0) 
      (det-if-then-else 
        (functor $E $_ 0) 
        (, 
          (= $R2 $R1) 
          (= $Es2 
            (Cons  $E $Es1))) 
        (, 
          (= $R2 
            (Cons  $E $R1)) 
          (= $Es2 $Es1)))))
  (= 
    (diff-predicates 
      (Cons  $E $R) $R2 $Es2 $F $N) 
    ( (diff-predicates $R $R1 $Es1 $F $N) (det-if-then-else (functor $E $F $N) (, (= $R2 $R1) (= $Es2 (Cons  $E $Es1))) (, (= $R2 (Cons  $E $R1)) (= $Es2 $Es1)))))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: nth_arg/3

;
;                                         *

;
;                                         * syntax: nth_arg(+E,+N,-Args)

;
;                                         *

;
;                                         * args: E ... list of terms with principal functor p/n

;
;                                         *       N =< n	argument position

;
;                                         *       Args ... list of argument terms

;
;                                         *

;
;                                         * description:	Args = {A | arg(N,P,A) and P in E}

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
    (nth_arg  () $_ ()) True)
  (= 
    (nth-arg 
      (Cons  $F $R) $N 
      (Cons  $Argn $R1)) 
    ( (arg $N $F $Argn) (nth-arg $R $N $R1)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: split_examples/4

;
;                                         *

;
;                                         * syntax: split_examples(+E,+Term,-P,-N)

;
;                                         *

;
;                                         * args: E,P,N ... list of terms

;
;                                         *

;
;                                         * description: P = {e in E | Term, e unifiable}

;
;                                         *              N = E - P

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
    (split-examples 
      (Cons  $E1 $R) $Lgg $P 
      (Cons  $E1 $M1)) 
    ( (not (= $E1 $Lgg)) 
      (split-examples $R $Lgg $P $M1) 
      (set-det)))

  (= 
    (split-examples 
      (Cons  $E1 $R) $Lgg 
      (Cons  $E1 $P) $M1) 
    ( (split-examples $R $Lgg $P $M1) (set-det)))

  (= 
    (split-examples Nil $_ Nil Nil) 
    (set-det))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	shares_var/2

;
;                                         *

;
;                                         * syntax:	shares_var(+T,+Ts)

;
;                                         *

;
;                                         * args:	T: a term or a clause,Ts: a list of terms or clauses

;
;                                         *

;
;                                         * description:	tests if T shares at least one variable

;
;                                         *		with the terms in t

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
    (shares-var $T $Ts) 
    ( (sub-term $V $T) 
      (var $V) 
      (contains-var $V $Ts)))



;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	body2list/2

;
;                                         *

;
;                                         * syntax:	body2list(?B,?BList)

;
;                                         *

;
;                                         * args:	B: Body of a clause (L1,...,Ln)

;
;                                         *		BList: [L1:x,...,Ln:x] where x is in {r,n}

;
;                                         *

;
;                                         * description:	transforms a clause body to a list of its literals

;
;                                         *		where each literal is augmented by :n (i.e. negative clause literal)

;
;                                         *		or :r (i.e. recursive goal in the clause body).

;
;                                         *              works in both directions

;
;                                         *

;
;                                         * example:	(p(x,y),q(z,w)),[(p(x,y):r,q(z,w):r]

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
    (body2list $B 
      (Cons  
        (with_self  $L1 
          (n)) $RestL)) 
    ( (functor $B , 2) 
      (arg 1 $B $L1) 
      (arg 2 $B $RestB) 
      (body2list $RestB $RestL)))
  (= 
    (body2list $B 
      (:: (with_self  $B (n)))) 
    (set-det))
  (= 
    (body2list $B 
      (Cons  
        (with_self  $L1 
          (r)) $RestL)) 
    ( (functor $B , 2) 
      (arg 1 $B $L1) 
      (arg 2 $B $RestB) 
      (body2list $RestB $RestL)))
  (= 
    (body2list $B 
      (:: (with_self  $B (r)))) 
    (set-det))



;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	insert_unique/3

;
;                                         *

;
;                                         * syntax: insert_unique(+N,+L,-L1)

;
;                                         *

;
;                                         * args: N .. number, L,L1 sorted lists of numbers

;
;                                         *

;
;                                         * description:	inserts N uniquely in the ascendingly sorted list L

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
    (insert-unique $I 
      (Cons  $I $R) 
      (Cons  $I $R)) 
    (set-det))
  (= 
    (insert-unique $I 
      (Cons  $J $R) 
      (Cons  $J $R1)) 
    ( (> $I $J) 
      (set-det) 
      (insert-unique $I $R $R1)))
  (= 
    (insert_unique  $I $L 
      (Cons  $I $L)) True)



;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	insert_unique/4

;
;                                         *

;
;                                         * syntax: insert_unique(+ID,+A,+L,-L1)

;
;                                         *

;
;                                         * args: ID,A .. numbers, L,L1 = [...,ID:List,...]

;
;                                         *

;
;                                         * description:	inserts A in the sublist identified by ID in L

;
;                                         *

;
;                                         * example: insert_unique(2,5,[1:[5,6],2:[4],3:[9,8]],

;
;                                         *                            [1:[5,6],2:[5,4],3:[9,8]])

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
    (insert-unique $I $A Nil 
      (:: (with_self  $I (:: $A)))) 
    (set-det))
  (= 
    (insert-unique $I $A 
      (Cons  
        (with_self  $I $A1) $R) 
      (Cons  
        (with_self  $I 
          (Cons  $A $A1)) $R)) 
    (set-det))
  (= 
    (insert-unique $I $A 
      (Cons  $J $R) 
      (Cons  $J $R1)) 
    (insert-unique $I $A $R $R1))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: genterm_test/3

;
;                                         *

;
;                                         * syntax: genterm_test(+X/T,+Subst)

;
;                                         *

;
;                                         * args: X/T element of a substitution, Subst substitution

;
;                                         *

;
;                                         * description: succeeds if Subst contains a tuple Y/T1 such that

;
;                                         *      T1 == T. In that case, X and Y are unified.

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
    (genterm-test 
      (/ $X $T1) 
      (Cons  
        (/ $X $T2) $_)) 
    ( (== $T1 $T2) (set-det)))
  (= 
    (genterm-test $S 
      (Cons  $_ $Rest)) 
    (genterm-test $S $Rest))



;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: subset_chk/2

;
;                                         *

;
;                                         * syntax: subset_chk(+L,+L1)

;
;                                         *

;
;                                         * args: L, L1 .. lists

;
;                                         *

;
;                                         * description:	succeeds, if L is a subset of L1 (without unification)

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



;
;                                          subset check uses library 'basics'


  (= 
    (subset-chk Nil $_) 
    (set-det))
  (= 
    (subset-chk 
      (Cons  $Elem1 $Rest1) $List2) 
    ( (identical-member $Elem1 $List2) 
      (set-det) 
      (subset-chk $Rest1 $List2)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate:	subterm_at_position/4

;
;                                         *

;
;                                         * syntax: subterm_at_position(+Term,-Sub,+Pos,-Pos)

;
;                                         *

;
;                                         * args: Term, Sub: MeTTa terms

;
;                                         *       Pos: position of Sub within Term (a list of numbers)

;
;                                         *

;
;                                         * description:	returns a subterm of Term and its position, on backtracking

;
;                                         *              further subterms

;
;                                         *

;
;                                         * example: ?- subterm_at_position(p(a,[a]),S,[],P).

;
;                                         *          S = p(a,[a]), P = [];

;
;                                         *          S = a         P = [1];

;
;                                         *          S = [a]       P = [2];

;
;                                         *          S = a         P = [2,1];

;
;                                         *          S = []        P = [2,2]

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
    (subterm-at-position $T $T $P $P1) 
    (rev $P $P1))
  (= 
    (subterm-at-position $T $S $P $P1) 
    ( (nonvar $T) 
      (functor $T $_ $N) 
      (> $N 0) 
      (subterm-at-position $N $T $S $P $P1)))

  (= 
    (subterm-at-position $N $T $S $P $P1) 
    ( (> $N 0) 
      (arg $N $T $Tn) 
      (subterm-at-position $Tn $S 
        (Cons  $N $P) $P1)))
  (= 
    (subterm-at-position $N $T $S $P $P1) 
    ( (> $N 0) 
      (is $N1 
        (- $N 1)) 
      (subterm-at-position $N1 $T $S $P $P1)))


;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: part_of_clause/2

;
;                                         *

;
;                                         * syntax: part_of_clause(+Term,+Clause)

;
;                                         *

;
;                                         * args: Term: a MeTTa term, Clause: a MeTTa clause

;
;                                         *

;
;                                         * description:	succeeds if Term is a literal within clause, a part

;
;                                         *              of the clause body or the clause itself

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
    (part-of-clause $S $B) 
    (== $S $B))
  (= 
    (part-of-clause $S 
      (= $H $B)) 
    ( (set-det) (det-if-then-else (or (== $S $H) (== $S $B)) True (part-of-clause $S $B))))
  (= 
    (part-of-clause $S 
      (, $H $B)) 
    ( (set-det) (det-if-then-else (or (== $S $H) (== $S $B)) True (part-of-clause $S $B))))



;
;                                         ***********************************************************************

;
;                                         *

;
;                                         * predicate: several arithmetic predicates

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
;                                         * description:	arithmetic predicates used in heuristic measures

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
    (fak $X 1) 
    ( (=:= $X 0) (set-det)))
  (= 
    (fak $N $NF) 
    ( (is $N1 
        (- $N 1)) 
      (fak $N1 $N1F) 
      (is $NF 
        (* $N1F $N))))


  (= 
    (fak1 $N $N 1) 
    (set-det))
  (= 
    (fak1 $A $B $C) 
    ( (is $A1 
        (+ $A 1)) 
      (fak1 $A1 $B $C1) 
      (is $C 
        (* $C1 $A1))))


  (= 
    (nueberk $N $K $NUK) 
    ( (is $NK 
        (- $N $K)) 
      (fak1 $NK $N $NKF) 
      (fak $K $KF) 
      (is $NUK 
        (/ $NKF $KF))))


  (= 
    (log2 $X $LX) 
    ( (log $X $LNX) 
      (log 2 $LN2) 
      (is $LX 
        (/ $LNX $LN2))))


  (= 
    (log2nueberk $_ 0.0 0.0) 
    (set-det))
  (= 
    (log2nueberk $N 1.0 $LN) 
    ( (log2 $N $LN) (set-det)))
  (= 
    (log2nueberk $N $N 0.0) 
    (set-det))
  (= 
    (log2nueberk $N $K $L) 
    ( (is $N1 
        (+ 
          (- $N $K) 1)) 
      (sum-of-logs $N1 $N $L1) 
      (sum-of-logs 1.0 $K $L2) 
      (is $L 
        (- $L1 $L2))))


  (= 
    (sum-of-logs $O $O $LO) 
    ( (log2 $O $LO) (set-det)))
  (= 
    (sum-of-logs $U $O $L) 
    ( (< $U $O) 
      (set-det) 
      (is $U1 
        (+ $U 1)) 
      (sum-of-logs $U1 $O $L1) 
      (log2 $U $LU) 
      (is $L 
        (+ $L1 $LU))))
  (= 
    (sum-of-logs $U $O $_) 
    ( (> $U $O) 
      (set-det) 
      (fail)))
