;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 




  !(my-use-module (library lists))
; 

  !(use-module (library ordsets))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 



  (= 
    (mnl 1) 
    (nl))
; 

  (= 
    (mnl $N) 
    ( (> $N 1) 
      (nl) 
      (is $N1 
        (- $N 1)) 
      (mnl $N1)))
; 



;
; 

;
; 



  (= 
    (ppl $L) 
    (ppl $L 3))
; 

  (= 
    (ppl  () $Ind) True)
; 

  (= 
    (ppl 
      (Cons  
        (Cons  $H $T) $L) $Ind) 
    ( (is $Ind1 
        (+ $Ind 3)) 
      (ppl 
        (Cons  $H $T) $Ind1) 
      (ppl $L $Ind) 
      (set-det)))
; 

  (= 
    (ppl 
      (Cons  $H $L) $Ind) 
    ( (space $Ind) 
      (print $H) 
      (nl) 
      (ppl $L $Ind) 
      (set-det)))
; 

  (= 
    (ppl $A $Ind) 
    ( (space $Ind) 
      (print $A) 
      (nl)))
; 



;
; 

;
; 



  (= 
    (pwl $L) 
    (pwl $L 3))
; 

  (= 
    (pwl  () $Ind) True)
; 

  (= 
    (pwl 
      (Cons  
        (Cons  $H $T) $L) $Ind) 
    ( (is $Ind1 
        (+ $Ind 3)) 
      (pwl 
        (Cons  $H $T) $Ind1) 
      (pwl $L $Ind) 
      (set-det)))
; 

  (= 
    (pwl 
      (Cons  $H $L) $Ind) 
    ( (space $Ind) 
      (write $H) 
      (nl) 
      (pwl $L $Ind) 
      (set-det)))
; 

  (= 
    (pwl $A $Ind) 
    ( (space $Ind) 
      (write $A) 
      (nl)))
; 




;
; 

;
; 



  (= 
    (space) 
    (write ' '))
; 

  (= 
    (space  0) True)
; 

  (= 
    (space 1) 
    (space))
; 

  (= 
    (space $N) 
    ( (> $N 1) 
      (space) 
      (is $N1 
        (- $N 1)) 
      (space $N1)))
; 


;
; 



  (= 
    (cons  $E () 
      ($E)) True)
; 

  (= 
    (cons  $E 
      (Cons  $H $T) 
      (Cons  $E 
        (Cons  $H $T))) True)
; 


;
; 



  (= 
    (snoc  $E () 
      ($E)) True)
; 

  (= 
    (snoc $E $L0 $L1) 
    ( (reverse $L0 $Lr) 
      (cons $E $Lr $Lr1) 
      (reverse $Lr1 $L1)))
; 


;
; 



  (= 
    (f_cons  () $L $L) True)
; 

  (= 
    (f_cons  $E 
      (Cons  $H $T) 
      (Cons  $E 
        (Cons  $H $T))) True)
; 


 


;
; 



  (= 
    (rev_append  () $L $L) True)
; 

  (= 
    (rev-append 
      (Cons  $H $T) $L $R) 
    (rev-append $T 
      (Cons  $H $L) $R))
; 



;
; 

;
; 



  (= 
    (extract  $Elt 
      (Cons  $Elt $Tail) $Tail) True)
; 

  (= 
    (extract $Elt 
      (Cons  $Head $Tail) 
      (Cons  $Head $List)) 
    (extract $Elt $Tail $List))
; 


;
; 

;
; 



  (= 
    (cull  $Pattern () () ()) True)
; 

  (= 
    (cull $Pattern 
      (Cons  $Pattern $T0) 
      (Cons  $Pattern $T1) $R) 
    (cull $Pattern $T0 $T1 $R))
; 

  (= 
    (cull $Pattern 
      (Cons  $H $T0) $C 
      (Cons  $H $T1)) 
    (cull $Pattern $T0 $C $T1))
; 


;
; 

;
; 



  (= 
    (cull_funct  $Funct () () ()) True)
; 

  (= 
    (cull-funct $Funct 
      (Cons  $H0 $T0) 
      (Cons  $H0 $T1) $R) 
    ( (functor $H0 $Funct $_) (cull-funct $Funct $T0 $T1 $R)))
; 

  (= 
    (cull-funct $Funct 
      (Cons  $H0 $T0) $C 
      (Cons  $H0 $T1)) 
    (cull-funct $Funct $T0 $C $T1))
; 


;
; 

;
; 

;
; 



  (= 
    (split_list  $Elt 
      (Cons  $Elt $Tail) () $Tail) True)
; 

  (= 
    (split-list $Elt 
      (Cons  $Head $Tail0) 
      (Cons  $Head $Tail1) $Tail) 
    (split-list $Elt $Tail0 $Tail1 $Tail))
; 


;
; 

;
; 



  (= 
    (split-list-funct $Funct 
      (Cons  $Elt $Tail) Nil $Tail) 
    (functor $Elt $Funct $_))
; 

  (= 
    (split-list-funct $Funct 
      (Cons  $H0 $T0) 
      (Cons  $H0 $T1) $T) 
    (split-list-funct $Funct $T0 $T1 $T))
; 



;
; 



  (= 
    (get_nth  1 
      (Cons  $Head $_) $Head) True)
; 

  (= 
    (get-nth $P 
      (Cons  $_ $Tail) $Elt) 
    ( (get-nth $P1 $Tail $Elt) (is $P (+ $P1 1))))
; 



;
; 



  (= 
    (nth-letter $N $Letter) 
    ( (nth-letter-after $N a $Letter) 
      (> $N 0) 
      (=< $N 26)))
; 


;
; 



  (= 
    (nth-letter-after $N $Letter0 $Letter) 
    ( (atom $Letter) 
      (set-det) 
      (name $Letter0 
        (:: $A)) 
      (name $Letter 
        (:: $L)) 
      (is $N 
        (+ 
          (- $L $A) 1))))
; 

  (= 
    (nth-letter-after $N $Letter0 $Letter) 
    ( (integer $N) 
      (name $Letter0 
        (:: $A)) 
      (is $L 
        (- 
          (+ $N $A) 1)) 
      (name $Letter 
        (:: $L))))
; 



;
; 



  (= 
    (percolate $M $L1 $L2) 
    (perc2 $M $L1 Nil $L2))
; 



  (= 
    (perc2 $M 
      (Cons  $M $Lt) $Lp 
      (Cons  $M $L)) 
    ( (reverse $Lp $Lpr) (append $Lpr $Lt $L)))
; 

  (= 
    (perc2 $M 
      (Cons  $Lh $Lt) $Lp $L) 
    (perc2 $M $Lt 
      (Cons  $Lh $Lp) $L))
; 


;
; 

;
; 



  (= 
    (mesh  () $L $L) True)
; 

  (= 
    (mesh  $L () $L) True)
; 

  (= 
    (mesh 
      (Cons  $A $As) 
      (Cons  $B $Bs) 
      (Cons  $A $Rest)) 
    (mesh $As 
      (Cons  $B $Bs) $Rest))
; 

  (= 
    (mesh 
      (Cons  $A $As) 
      (Cons  $B $Bs) 
      (Cons  $B $Rest)) 
    (mesh 
      (Cons  $A $As) $Bs $Rest))
; 



;
; 

;
; 

;
; 


  (= 
    (flatten $Tree $List) 
    (flatten $Tree $List Nil))
; 



  (= 
    (-->  
      (flatten  ()) !) True)
; 

  (= 
    (-->  
      (flatten  
        (Cons  $Head $Tail)) 
      (,  ! 
        (,  
          (flatten  $Head) 
          (flatten  $Tail)))) True)
; 

  (= 
    (-->  
      (flatten  $Other) 
      ($Other)) True)
; 




;
; 



  (= 
    (gensym $Prefix $V) 
    ( (var $V) 
      (atomic $Prefix) 
      (lastsuffix $Prefix $M) 
      (is $N 
        (+ $M 1)) 
      (add-symbol  &self 
        (flag  
          (gensym  $Prefix) $N)) 
      (concat $Prefix $N $V) 
      (set-det)))
; 


;
; 



  (= 
    (cgensym $Prefix $V) 
    ( (var $V) 
      (set-det) 
      (gensym $Prefix $V)))
; 

  (= 
    (cgensym  $_ $_) True)
; 
 



  (= 
    (lastsuffix $Prefix $M) 
    ( (remove-symbol  &self 
        (flag  
          (gensym  $Prefix) $M)) (set-det)))
; 

  (= 
    (lastsuffix  $Prefix 0) True)
; 



;
; 



  (= 
    (reset-gensym) 
    (remove-all-symbols  &self 
      (flag  
        (gensym  $Prefix) $M)))
; 


  (= 
    (reset-gensym $Prefix) 
    (remove-all-symbols  &self 
      (flag  
        (gensym  $Prefix) $M)))
; 



;
; 

;
; 

;
; 

;
; 



  (= 
    (bi-name $Atom $List) 
    (when 
      (or 
        (ground $Atom) 
        (ground $List)) 
      (name $Atom $List)))
; 



;
; 



  (= 
    (concat $N1 $N2 $N3) 
    ( (name $N1 $Ls1) 
      (name $N2 $Ls2) 
      (append $Ls1 $Ls2 $Ls3) 
      (name $N3 $Ls3)))
; 



  (= 
    (concat-list 
      (:: $A) $A) 
    (set-det))
; 

  (= 
    (concat-list 
      (Cons  $A $Bs) $C) 
    ( (concat-list $Bs $Bconc) (concat $A $Bconc $C)))
; 




  (= 
    (append-list 
      (:: $A) $A) 
    (set-det))
; 

  (= 
    (append-list 
      (Cons  $A $Bs) $C) 
    ( (append-list $Bs $Bconc) (append $A $Bconc $C)))
; 



;
; 

;
; 

;
; 


  (= 
    (bi-concat $N1 $N2 $N3) 
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
          (format "Error, uninstantiated args in bi_concat~n" Nil) 
          (fail)))))
; 



  (= 
    (bi-concat-list 
      (:: $A) $A) 
    (set-det))
; 

  (= 
    (bi-concat-list 
      (Cons  $A $Bs) $C) 
    ( (bi-concat-list $Bs $Bconc) (bi-concat $A $Bconc $C)))
; 



;
; 



  (= 
    (ynp y y $Goal) 
    (set-det))
; 

  (= 
    (ynp yes y $Goal) 
    (set-det))
; 

  (= 
    (ynp n n $Goal) 
    (set-det))
; 

  (= 
    (ynp no n $Goal) 
    (set-det))
; 

  (= 
    (ynp $Resp $RespVal $Goal) 
    ( (write 'Please respond with y or n.') 
      (nl) 
      (call $Goal)))
; 



;
; 


;
; 

;
; 

;
; 

;
; 


  (= 
    (count-bagof $A $B $C) 
    (det-if-then-else 
      (bagof $A $B $C1) 
      (length $C1 $C) 
      (= $C 0)))
; 



  (= 
    (count-setof $A $B $C) 
    (det-if-then-else 
      (setof $A $B $C1) 
      (length $C1 $C) 
      (= $C 0)))
; 



  (= 
    (count-findall $A $B $C) 
    ( (findall $A $B $C1) (length $C1 $C)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (maplist  $_ () ()) True)
; 

  (= 
    (maplist $P 
      (Cons  $X $L) 
      (Cons  $Y $M)) 
    ( (=.. $Q 
        (:: $P $X $Y)) 
      (call $Q) 
      (maplist $P $L $M)))
; 




;
; 



  (= 
    (whenever $Generator $Goal) 
    (or 
      (, 
        (call $Generator) 
        (call $Goal) 
        (fail)) True))
; 





  (= 
    (verify $Goal) 
    (not (not $Goal)))
; 




  (= 
    (remove-test-duplicates $List $Test $Clean) 
    (remove-test-duplicates $List $Test Nil $Clean))
; 


  (= 
    (remove_test_duplicates  () $_ $_ ()) True)
; 

  (= 
    (remove-test-duplicates 
      (Cons  $H $T) $Test_Call $Seen $Clean) 
    ( (= $Test_Call 
        (test $Test $Pattern1 $Pattern2)) 
      (member $Pattern2 $Seen) 
      (= $H $Pattern1) 
      (call $Test) 
      (set-det) 
      (remove-test-duplicates $T $Test_Call $Seen $Clean)))
; 

  (= 
    (remove-test-duplicates 
      (Cons  $H $T) $Test_Call $Seen 
      (Cons  $H $Clean)) 
    (remove-test-duplicates $T $Test_Call 
      (Cons  $H $Seen) $Clean))
; 


;
; 



;
; 



  (= 
    (average $List $Avg) 
    ( (length $List $N) (det-if-then-else (> $N 0) (, (sum-list $List $Sum) (is $Avg (/ $Sum $N))) (= $Avg 0))))
; 

 
;
; 



  (= 
    (max 
      (Cons  $A $Rest) $Val) 
    (max1 $Rest $A $Val))
; 



  (= 
    (max1  () $A $A) True)
; 

  (= 
    (max1 
      (Cons  $H $T) $Old $Val) 
    ( (max $H $Old $New) (max1 $T $New $Val)))
; 


;
; 


  (= 
    (max $A $B $A) 
    ( (> $A $B) (set-det)))
; 

  (= 
    (max  $A $B $B) True)
; 



;
; 


  (= 
    (min 
      (Cons  $A $Rest) $Val) 
    (min1 $Rest $A $Val))
; 



  (= 
    (min1  () $A $A) True)
; 

  (= 
    (min1 
      (Cons  $H $T) $Old $Val) 
    ( (min $H $Old $New) (min1 $T $New $Val)))
; 


;
; 


  (= 
    (min $A $B $A) 
    ( (< $A $B) (set-det)))
; 

  (= 
    (min  $A $B $B) True)
; 



;
; 



  (= 
    (writeln $Arg) 
    ( (write $Arg) (nl)))
; 



;
; 


  (= 
    (reduce-list 
      (Cons  $A $B) $R) 
    ( (set-det) 
      (reduce-list $A $A1) 
      (reduce-list $B $B1) 
      (append $A1 $B1 $R)))
; 

  (= 
    (reduce-list Nil Nil) 
    (set-det))
; 

  (= 
    (reduce_list  $X 
      ($X)) True)
; 



;
; 



  (= 
    (increase-term-arity $TermIn $NewArg $TermOut) 
    ( (functor $TermIn $Func $N) 
      (is $N1 
        (+ $N 1)) 
      (functor $TermOut $Func $N1) 
      (arg $N1 $TermOut $NewArg) 
      (copy-args $N $TermIn $TermOut)))
; 



  (= 
    (copy-args 0 $TermIn $TermOut) 
    (set-det))
; 

  (= 
    (copy-args $N $TermIn $TermOut) 
    ( (arg $N $TermIn $Arg) 
      (arg $N $TermOut $Arg) 
      (is $N1 
        (- $N 1)) 
      (copy-args $N1 $TermIn $TermOut)))
; 



;
; 



  (= 
    (portray-clauses $Clauses) 
    (whenever 
      (member $C $Clauses) 
      (portray-clause $C)))
; 


;
; 



  (= 
    (dotimes 0 $_) 
    (set-det))
; 

  (= 
    (dotimes $N $Call) 
    ($Call 
      (is $N1 
        (- $N 1)) 
      (dotimes $N1 $Call)))
; 


;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


  (= 
    (stable-sort $In $Out) 
    ( (pair-list $In $_ $Keyed) 
      (keysort $Keyed $SortKeyed) 
      (pair-list $Out $_ $SortKeyed)))
; 


;
; 

;
; 

;
; 


  (= 
    (pair_list  () () ()) True)
; 

  (= 
    (pair-list 
      (Cons  $A $As) 
      (Cons  $B $Bs) 
      (Cons  
        (- $A $B) $Rest)) 
    (pair-list $As $Bs $Rest))
; 



  (= 
    (rev_pair_list  () ()) True)
; 

  (= 
    (rev-pair-list 
      (Cons  
        (- $A $AV) $RestA) 
      (Cons  
        (- $AV $A) $RestB)) 
    (rev-pair-list $RestA $RestB))
; 



;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (contains $Symbol $Text) 
    (con $Text $Symbol))
; 



  (= 
    (con  $_ 
      (,  $_ ())) True)
; 

  (= 
    (con 
      (Cons  $C $Rtext) $SymbInfo) 
    ( (new $C $SymbInfo $SymbInfoNew) (con $Rtext $SymbInfoNew)))
; 



  (= 
    (new $C 
      (, $Prefix $C $RestPostfix) 
      (, $PrefixNew $RestPostfix)) 
    (append $Prefix 
      (:: $C) $PrefixNew))
; 

  (= 
    (new $C 
      (, $Prefix $D $RestPostfix) 
      (, $PrefixNew $PostfixNew)) 
    ( (\== $C $D) 
      (append $Prefix 
        (:: $C) $H) 
      (append $PrefixNew $Rest $Prefix) 
      (append $_ $PrefixNew $H) 
      (append $Rest 
        (Cons  $D $RestPostfix) $PostfixNew)))
; 



;
; 

;
; 

;
; 

;
; 

	

  (= 
    (found $Symbol) 
    (found1 $Symbol))
; 



  (= 
    (found1  
      (,  $_ ())) True)
; 

  (= 
    (found1 $SymbInfo) 
    ( (get0 $C) 
      (new $C $SymbInfo $SymbInfoNew) 
      (found1 $SymbInfoNew)))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (between $Lower $Upper $Point) 
    ( (integer $Lower) 
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
          (between1 $Lower $Upper $Point)))))
; 


;
; 

;
; 

;
; 

;
; 



  (= 
    (between1 $L $L $L) 
    (set-det))
; 

  (= 
    (between1  $L $_ $L) True)
; 
		;
; 

  (= 
    (between1 $L $U $N) 
    ( (is $M 
        (+ $L 1)) (between1 $M $U $N)))
; 
	;
; 

;
; 




  (= 
    (numlist $Min $Max Nil) 
    ( (> $Min $Max) (set-det)))
; 

  (= 
    (numlist $Min $Max 
      (Cons  $Min $Rest)) 
    ( (is $Min1 
        (+ $Min 1)) (numlist $Min1 $Max $Rest)))
; 




;
; 

;
; 


  (= 
    (member1 $A $B) 
    (det-if-then-else 
      (var $A) 
      (member $A $B) 
      (memberchk $A $B)))
; 

	
;
; 

;
; 

;
; 



  (= 
    (member1-pair 
      (- $H $T) $B) 
    (det-if-then-else 
      (var $H) 
      (member 
        (- $H $T) $B) 
      (memberchk 
        (- $H $T) $B)))
; 




  (= 
    (assoc $List $Prop $Val) 
    (member1-pair 
      (- $Prop $Val) $List))
; 



  (= 
    (set-assoc $List1 $Param $Val $New) 
    ( (ensure-list $List1 $List2) 
      (delete-all-assoc $Param $List2 $List) 
      (cons 
        (- $Param $Val) $List $New)))
; 
 


  (= 
    (ensure-list $List1 $List1) 
    ( (is-list $List1) (set-det)))
; 

  (= 
    (ensure_list  $_ ()) True)
; 



  (= 
    (delete-all-assoc $P $L $L2) 
    ( (member 
        (- $P $V1) $L) 
      (set-det) 
      (delete $L 
        (- $P $V1) $L1) 
      (delete-all-assoc $P $L1 $L2)))
; 

  (= 
    (delete_all_assoc  $_ $L $L) True)
; 


;
; 

;
; 

;
; 


;
; 


  (= 
    (in-region 
      (:: (- $Choice $Prob)) $_ $Choice) 
    (set-det))
; 

  (= 
    (in-region 
      (Cons  
        (- $C $P) $Choices) $Prob $C) 
    ( (< $Prob $P) (set-det)))
; 

  (= 
    (in-region 
      (Cons  
        (- $C $P) $Choices) $Prob $Choice) 
    ( (is $PRest 
        (- $Prob $P)) (in-region $Choices $PRest $Choice)))
; 




