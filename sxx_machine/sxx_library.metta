;                                           (convert-to-metta-file  sxx_library $_221086 sxx_machine/sxx_library.pl sxx_machine/sxx_library.metta)

  (= toplevel  
    (top fail))
  (= toplevel  toplevel)


  (= top  
    ( (write ?- ) 
      (vread $Goal $Vars) 
      !$Goal 
      (writevars $Vars) 
      (write Yes) nl notmore 
      (set-det) fail))

  (= top  top)


  (= notmore  
    ( (write 'more? ') 
      (get0 $X) 
      (noteq $X 10) 
      (set-det) 
      (untilend $X) fail))
  (= notmore  True)


  (= (noteq $X $X)  
    ( (set-det) fail))
  (= (noteq  $_ $_)  True)


  (= (untilend 10)  
    (set-det))
  (= (untilend $_)  
    (get0 $X)
    (untilend $X))


  (= (writevars  ())  True)
  (= (writevars (Cons  (= $Var $Name) $R))  
    ( (write $Name) 
      (write  = ) 
      (write $Var) nl fail))
  (= (writevars (Cons  $_ $R))  
    (writevars $R))



  (= (nrev Nil Nil)  
    (set-det))
  (= (nrev (Cons  $A $B) $O)  
    (nrev $B $C)
    (append $C 
      (:: $A) $O))


  (= (append  () $L $L)  True)
  (= (append (Cons  $A $L1) $L2 (Cons  $A $L3))  
    (append $L1 $L2 $L3))


  (= (max $A $B $B)  
    (smallerthan $A $B))
  (= (max $A $B $A)  
    (smallerthan $B $A))
  (= (max  $A $A $A)  True)



  (= (sort Nil Nil)  
    (set-det))
  (= (sort (Cons  $X $R) $Out)  
    (split $X $R $S $G)
    (sort $S $SS)
    (sort $G $GS)
    (append $SS 
      (Cons  $X $GS) $Out))


  (= (split $X Nil Nil Nil)  
    (set-det))
  (= (split $X (Cons  $A $R) (Cons  $A $S) $G)  
    (smallerthan $A $X)
    (set-det)
    (split $X $R $S $G))
  (= (split $X (Cons  $A $R) $S (Cons  $A $G))  
    (split $X $R $S $G))


  (= (time $Goal)  
    ( (cputime $T1) 
      !$Goal 
      (cputime $T2) 
      (is $T 
        (- $T2 $T1)) 
      (write $T) nl))



  (= !$G  
    ( (var $G) 
      (set-det) fail))
  (= !(, $G $B)  
    (set-det)
    !$G
    !$B)
  (= !(det-if-then $If $Then)  
    (set-det)
    (execdisj 
      (det-if-then $If $Then) True))
  (= !(or $B1 $B2)  
    (set-det)
    (execdisj $B1 $B2))
  (= !$G  
    (specialgoal $G $G1)
    (set-det)
    !$G1)
  (= !$G  
    (call $G))


  (= (execdisj (det-if-then $If $Then) $Else)  
    !$If
    (set-det)
    !$Then)
  (= (execdisj (det-if-then $If $Then) $Else)  
    (set-det)
    !$Else)
  (= (execdisj $B1 $B2)  
    !$B1)
  (= (execdisj $B1 $B2)  
    !$B2)



  (= (loop 0)  
    (set-det))
  (= (loop $N)  
    (is $M 
      (- $N 1))
    (loop $M))


  (= (or $X $_)  
    !$X)
  (= (or $_ $Y)  
    !$Y)


  (= (findall $X $Goal $L)  
    (initfindall $Handle)
    (findall2 $X $Goal $L $Handle))

  (= (findall2 $X $Goal $L $Handle)  
    ( !$Goal 
      (addfindall $X $Handle) fail))
  (= (findall2 $X $Goal $L $Handle)  
    (retrievefindall $L $Handle))


  (= (copy-term $T1 $T2)  
    (findall $X 
      (= $X $T1) 
      (:: $T2)))


  (= (var $X)  
    (type-of $X var))


  (= (atomic $X)  
    (type-of $X $A)
    (atomic2 $A))

  (= (atomic2 atom)  
    (set-det))
  (= (atomic2  integer)  True)


  (= (atom $X)  
    (type-of $X atom))


  (= (integer $X)  
    (type-of $X integer))



  (= (assumeduring $G $F)  
    (assume (- $F $Tag))
    !$G
    (= $Tag 1))


  (= (assumed $F)  
    (allassumed $L)
    (m1 $F $L))


  (= (m1 $X (Cons  (- $X $Tag) $_))  
    (var $Tag))
  (= (m1 $X (Cons  $_ $R))  
    (m1 $X $R))


  (= (statistics runtime (:: $T $_))  
    (cputime $T))


;
; the following are also needed in the compiler at the moment


  (= (specialgoal (< $X $Y) (smallerthan $X $Y))  
    (set-det))
  (= (specialgoal (=< $X $Y) (smallerorequal $X $Y))  
    (set-det))
  (= (specialgoal (> $X $Y) (smallerthan $Y $X))  
    (set-det))
  (= (specialgoal (>= $X $Y) (smallerorequal $Y $X))  
    (set-det))
  (= (specialgoal (=:= $X $Y) (arithequal $Y $X))  
    (set-det))
  (= (specialgoal (= $X $Y) (unify $Y $X))  
    (set-det))
  (= (specialgoal (or $X $Y) (or $X $Y))  
    (set-det))
  (= (specialgoal (@< $X $Y) (termsmallerthan $X $Y))  
    (set-det))
  (= (specialgoal (@> $X $Y) (termgreaterthan $X $Y))  
    (set-det))
  (= (specialgoal (@=< $X $Y) (termsmallerequal $X $Y))  
    (set-det))
  (= (specialgoal (@>= $X $Y) (termgreaterequal $X $Y))  
    (set-det))
  (= (specialgoal (== $X $Y) (termequal $X $Y))  
    (set-det))


  (= (termsmallerthan $X $Y)  
    (compare < $X $Y))

  (= (termgreaterthan $X $Y)  
    (compare > $X $Y))


  (= (termsmallerequal $X $Y)  
    (compare $C $X $Y)
    (smeq $C))

  (= (smeq <)  
    (set-det))
  (= (smeq  =)  True)


  (= (termgreaterequal $X $Y)  
    (compare $C $X $Y)
    (smgr $C))

  (= (smgr >)  
    (set-det))
  (= (smgr  =)  True)


  (= (termequal $X $Y)  
    (compare = $X $Y))



  (= (not $G)  
    ( !$G 
      (set-det) fail))
  (= (not  $_)  True)



  (= (freeze $X $G)  
    (var $X)
    (set-det)
    (freeze-internal $X $G))
  (= (freeze $X $G)  
    !$G)


  (= (execpendinggoals Nil)  execcontinuation)
  (= (execpendinggoals (Cons  $G $R))  
    !$G
    (execpendinggoals $R))

