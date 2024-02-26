;                                           (convert_to_metta_file  index $_209758 index/index.pl index/index.metta)

  !(unknown $_ fail)
  !(no-style-check all)

  !(compile (library basics))
  !(compile (library lists))
  !(compile (library sets))
  !(compile (library not))
;
; :-compile(library(strings)).

  !(dynamic (/ switched-on 1))
  !(dynamic (/ level-set 2))


  (= (switched_on  cwa)  True)
  (= (switched_on  eval)  True)
  (= (switched_on  debug)  True)


  (= (level_set  acc (+  0.8))  True)
  (= (level_set  conf (+  2))  True)
  (= (level_set  split (/  0.5 0.1))  True)


  (= (run)  
    (commands Nil Nil Nil))
; /* modules */
; /* :-compile(utils),compile(char),compile(eval),compile(hooks), compile(object),compile(sort),compile(decomp). :-compile(inter),compile(commands). */



  (= (listdiff  $L () $L)  True)
; /* utilities */
  (= (listdiff $L (Cons  $H $T) $V)  
    (remove $H $L $L1)
    (listdiff $L1 $T $V))


  (= (remove  $_ () ())  True)
  (= (remove $H (Cons  $H $T) $L)  
    (remove $H $T $L))
  (= (remove $X (Cons  $H $T) (Cons  $H $L))  
    (remove $X $T $L)
    (\= $X $H))


  (= (select-two (Cons  $H $T) $H $Y)  
    (member $Y $T))
  (= (select-two (Cons  $H $T) $X $Y)  
    (select-two $T $X $Y))


  (= (forall $Goal $Condition)  
    (bagof0 $Condition $Goal $List)
    (forall1 $List))


  (= (forall1  ())  True)
  (= (forall1 (Cons  $H $T))  
    (call $H)
    (forall1 $T))


  (= (bagof0 $T $G $L)  
    (bagof $T $G $L)
    (set-det))
  (= (bagof0  $T $G ())  True)


  (= (setof0 $T $G $L)  
    (setof $T $G $L)
    (set-det))
  (= (setof0  $T $G ())  True)

;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  index/index.pl 67 4 1074))



  (= (add-if $X $Ys $Ys)  
    (member $X $Ys)
    (set-det))
  (= (add_if  $X $Ys (Cons  $X $Ys))  True)


  (= (flatten $Xs $Ys)  
    (flatten-dl $Xs Nil $Ys))


  (= (flatten-dl Nil $Ys $Ys)  
    (set-det))
  (= (flatten-dl (Cons  $X $Xs) $Ys0 $Ys)  
    (set-det)
    (flatten-dl $Xs $Ys0 $Ys1)
    (flatten-dl $X $Ys1 $Ys))
  (= (flatten_dl  $X $Xs (Cons  $X $Xs))  True)

  (= (char $InICs $PosIn $NegIn $OutICs $PosOut $NegOut)  
    (char1 $InICs $PosIn $NegIn Nil $TmpICs $PosOut $NegOut)
    (cleanup $TmpICs Nil $OutICs))
; /* characterisation */


  (= (char1  () $P $N $Out $Out $P $N)  True)
  (= (char1 (Cons  $IC $ICs) $P0 $N0 $Acc $Out $P $N)  
    (write-debug (:: 'evaluating ' $IC))
    (evaluate $P0 $N0 $IC $Tuples $P1 $N1 $Answer)
    (det-if-then-else 
      (= $Answer refine) 
      (, 
        (refinements $IC $Tuples $Spec) 
        (append $ICs $Spec $NewICs) 
        (= $NewAcc $Acc)) 
      (det-if-then-else 
        (= $Answer keep) 
        (, 
          (= $NewICs $ICs) 
          (= $NewAcc 
            (Cons  $IC $Acc))) 
        (det-if-then-else 
          (= $Answer 
            (ignore $E)) 
          (, 
            (= $NewICs $ICs) 
            (= $NewAcc $Acc)) 
          (det-if-then 
            (= $Answer 
              (keep $E)) 
            (, 
              (= $NewICs $ICs) 
              (insert-ic $Acc $IC $E $NewAcc))))))
    (write-debug (:: '	result: ' $Answer))
    (set-det)
    (char1 $NewICs $P1 $N1 $NewAcc $Out $P $N))


  (= (evaluate $P0 $N0 $IC $Tuples $P $N $Answer)  
    (det-if-then-else 
      (evaluate1 $P0 $IC $Answer) 
      (, 
        (= $P $P0) 
        (= $N $N0) 
        (= $Tuples Nil)) 
      (det-if-then otherwise 
        (, 
          (contr $P0 $N0 $IC $T $A) 
          (det-if-then-else 
            (= $A yes) 
            (, 
              (= $Answer refine) 
              (= $P $P0) 
              (= $N $N0) 
              (= $Tuples $T)) 
            (det-if-then-else 
              (= $A no) 
              (, 
                (= $Answer keep) 
                (= $P $P0) 
                (= $N $N0) 
                (= $Tuples $T)) 
              (det-if-then 
                (= $A possibly) 
                (, 
                  (queries $P0 $N0 $T $P1 $N1) 
                  (evaluate $P1 $N1 $IC $Tuples $P $N $Answer)))))))))
; ; write_debug(['contr...']),


  (= (contr $P $N $IC $Tuples $Answer)  
    (horn $IC $HornIC)
    (incons $HornIC $P $N $Tuples $Answer))


  (= (queries  $P $N () $P $N)  True)
  (= (queries $P $N (Cons  $T $Ts) $P1 $N1)  
    (query $P $N $T $P2 $N2)
    (queries $P2 $N2 $Ts $P1 $N1))

;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  index/index.pl 122 4 2503))

;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  index/index.pl 124 4 2590))


  (= (query $P $N (- $T) $P (Cons  $T $N))  
    (switched-on cwa)
    (write (- $T))
    (nl))
  (= (query  $P $N (+  $T) $P $N)  True)


  (= (cleanup (Cons  $X $In) $Acc $Out)  
    (member $IC $In)
    (subsumed $X $IC)
    (set-det)
    (cleanup $In $Acc $Out))
  (= (cleanup (Cons  $X $In) $Acc $Out)  
    (member $IC $Acc)
    (subsumed $X $IC)
    (set-det)
    (cleanup $In $Acc $Out))
  (= (cleanup (Cons  $X $In) $Acc $Out)  
    (cleanup $In 
      (Cons  $X $Acc) $Out))
  (= (cleanup  () $Out $Out)  True)


  (= (incons (= $Head  $Body) $P $N $Tuples $Answer)  
    (satisfied $Body $P $N $TuplesB)
    (falsified $Head $P $N $TuplesH)
    (set-det)
    (= $Answer yes)
    (append $TuplesB $TuplesH $Tuples))
  (= (incons (= $Head  $Body) $P $N $Tuples $Answer)  
    (satisfied $Body $P $N $TuplesB)
    (unsatisfied $Head $P $N $TuplesH)
    (set-det)
    (= $Answer possibly)
    (append $TuplesB $TuplesH $Tuples))
  (= (incons (= $Head  $Body) $P $N Nil $Answer)  
    (= $Answer no))


  (= (satisfied (, $A $B) $P $N $Tuples)  
    (satisfied $A $P $N $TuplesA)
    (satisfied $B $P $N $TuplesB)
    (append $TuplesA $TuplesB $Tuples))
  (= (satisfied (or $A $B) $P $N $Tuples)  
    (or 
      (satisfied $A $P $N $Tuples) 
      (satisfied $B $P $N $Tuples)))
  (= (satisfied $A $P $N (:: (+ $A)))  
    (member $A $P))
  (= (satisfied  (= $A  $A) $P $N ())  True)	;
; fds only
  (= (satisfied $A $P $N Nil)  
    (proc $Rel $Proc)
    (exec-proc $A $Proc))


  (= (exec-proc $Goal (, $P1 $P2))  
    (or 
      (exec-proc $Goal $P1) 
      (exec-proc $Goal $P2)))
  (= (exec-proc $Goal (= $Goal  $Body))  
    (call $Body))


  (= (falsified (, $A $B) $P $N $Tuples)  
    (or 
      (, 
        (set-det) 
        (falsified $A $P $N $Tuples)) 
      (falsified $B $P $N $Tuples)))
  (= (falsified $A $P $N (:: (- $A)))  
    (member $A $N))
  (= (falsified (= $A $B) $P $N Nil)  
    (\= $A $B))
; ; fds only


  (= (unsatisfied (, $A $B) $P $N $Tuples)  
    (or 
      (, 
        (set-det) 
        (unsatisfied $A $P $N $Tuples)) 
      (unsatisfied $B $P $N $Tuples)))
  (= (unsatisfied $A $P $N (:: (- $A)))  
    (not (satisfied $A $P $N $_))
    (not (falsified $A $P $N $_)))

  (= (evaluate1 $P $IC $Answer)  
    (switched-on eval)
    (dep $IC $Type $Rel $From $To)
    (filter $P $Rel $P1 $Rest)
    (splitsort $P1 $IC $SplitP $NumbersC)
    (calc-conf $NumbersC $Conf)
    (det-if-then-else 
      (compare (conf $Conf)) 
      (, 
        (divides $SplitP $IC $NewP $NumbersA) 
        (calc-acc $NumbersA $Acc) 
        (det-if-then-else 
          (compare (acc $Acc)) 
          (= $Answer 
            (keep (acc $Acc))) 
          (det-if-then 
            (compare (split $Acc)) 
            (= $Answer 
              (keep (split $Acc)))))) 
      (det-if-then otherwise 
        (= $Answer 
          (ignore (conf $Conf))))))
; ; write_debug(['splitsort...']),
; ; write_debug(['divides...']),


  (= (calc-conf (, $NDivs $NTuples) $Confirmation)  
    (is $Confirmation 
      (/ $NTuples $NDivs)))


  (= (calc-acc $Numbers $Accuracy)  
    (calc-acc1 $Numbers 0 $NTuples 0 $NLarge 0 $MaxNP)
    (is $Accuracy 
      (- 1 
        (/ 
          (* 
            (- $MaxNP 1) 
            (- $NTuples $NLarge)) $NTuples))))


  (= (calc_acc1  () $NT $NT $NL $NL $MaxNP $MaxNP)  True)
  (= (calc-acc1 (Cons  $D $Ds) $NT0 $NT $NL0 $NL $MaxNP0 $MaxNP)  
    (eval2 $D 0 $Sum 0 $Largest 0 $NParts)
    (is $NT1 
      (+ $NT0 $Sum))
    (is $NL1 
      (+ $NL0 $Largest))
    (det-if-then-else 
      (> $NParts $MaxNP0) 
      (= $MaxNP1 $NParts) 
      (det-if-then otherwise 
        (= $MaxNP1 $MaxNP0)))
    (calc-acc1 $Ds $NT1 $NT $NL1 $NL $MaxNP1 $MaxNP))


  (= (eval2  () $S $S $L $L $M $M)  True)
  (= (eval2 (Cons  $N $Ns) $S0 $S $L0 $L $M0 $M)  
    (is $S1 
      (+ $S0 $N))
    (det-if-then-else 
      (> $N $L0) 
      (= $L1 $N) 
      (det-if-then otherwise 
        (= $L1 $L0)))
    (is $M1 
      (+ $M0 1))
    (eval2 $Ns $S1 $S $L1 $L $M1 $M))


  (= (insert_ic  $L $IC $E (Cons  $IC $L))  True)

  !(op 900 xfx :)
  !(op 800 xfx -->)
  !(op 800 xfx ->->)
  !(op 800 xfx ><)
  !(op 800 xfx <)


  (= (init-ICs $Rel $ICs)  
    (rel $Rel $AList)
    (choose-list 'Which dependencies' $AList $AttrList)
    (bagof0 
      (mvd $Rel Nil 
        (:: $A)) 
      (member $A $AttrList) $IC1)
    (bagof0 
      (fd $Rel Nil 
        (:: $A)) 
      (member $A $AttrList) $IC2)
    (append $IC1 $IC2 $ICs))
; /* initialisation */


  (= (horn (fd $Rel $FromList (:: $To)) (= (= $A1 $A2)  
    ($Tuple1 $Tuple2)))  
    (values $Rel $FromList $ValueList $Tuple1)
    (values $Rel $FromList $ValueList $Tuple2)
    (value $Rel $To $A1 $Tuple1)
    (value $Rel $To $A2 $Tuple2))
; /* translation to Horn form */
  (= (horn (mvd $Rel $FromList $ToList) (= $Tuple1  
    ($Tuple2 $Tuple3)))  
    (rel $Rel $AttrList)
    (listdiff $AttrList $FromList $List)
    (listdiff $List $ToList $RestList)
    (values $Rel $FromList $FromValues $Tuple1)
    (values $Rel $FromList $FromValues $Tuple2)
    (values $Rel $FromList $FromValues $Tuple3)
    (values $Rel $ToList $ToValues $Tuple1)
    (values $Rel $ToList $ToValues $Tuple2)
    (values $Rel $RestList $RestValues $Tuple1)
    (values $Rel $RestList $RestValues $Tuple3))
  (= (horn (join $Rel $R1 $R2) (= $Tuple1  
    ($Tuple2 $Tuple3)))  
    (rel $R1 $R1Attrs)
    (rel $R2 $R2Attrs)
    (values $Rel $R1Attrs $R1Values $Tuple1)
    (values $Rel $R2Attrs $R2Values $Tuple1)
    (values $R1 $R1Attrs $R1Values $Tuple2)
    (values $R2 $R2Attrs $R2Values $Tuple3))
  (= (horn (plus $Rel $List) (= $Tuple  $Body))  
    (rel $Rel $RelAttrs)
    (values $Rel $RelAttrs $Values $Tuple)
    (make-body $List $Values $Body))
  (= (horn (proc $Rel) $Proc)  
    (proc $Rel $Proc))	;
; works only for one-clause procs


  (= (make-body (:: $R) $Values $L)  
    (set-det)
    (rel $R $Attrs)
    (values $R $Attrs $Values $L))
  (= (make-body (Cons  $R $Rs) $Values (or $L $Ls))  
    (rel $R $Attrs)
    (values $R $Attrs $Values $L)
    (make-body $Rs $Values $Ls))


  (= (refinements (fd $Rel $From (:: $To)) (:: (+ $T1) (+ $T2)) $ICs)  
    (set-det)
    (diff $T1 $T2 $Diff)
    (bagof0 
      (fd $Rel 
        (Cons  $Attr $From) 
        (:: $To)) 
      (fd-spec $To $Diff $Attr) $ICs))
; /* refinements on the meta-level */
  (= (refinements (mvd $Rel $From $To) (:: (+ $T1) (+ $T2) (- $T3)) $ICs)  
    (set-det)
    (diff $T1 $T3 $D13)
    (diff $T2 $T3 $D23)
    (bagof0 
      (mvd $Rel 
        (Cons  $Attr $From) $ST) 
      (mvd-spec $Rel $From $To $D13 $D23 $Attr $ST) $ICs))
  (= (refinements $IC $Tuples $ICs)  
    (display $IC $DisplayIC)
    (write $DisplayIC)
    (show-list ' is contradicted by' $Tuples)
    (ic-spec $IC $ICs))


  (= (fd-spec $To $Diff $Attr)  
    (member $Attr $Diff)
    (\= $Attr $To))


  (= (mvd-spec $Rel $From $To $Diff $_ $Attr $ST)  
    (member $Attr $Diff)
    (remove $Attr $To $ST)
    (\= $ST Nil)
    (compl 
      (mvd $Rel 
        (Cons  $Attr $From) $ST) 
      (mvd $Rel 
        (Cons  $Attr $From) $SCT))
    (\= $SCT Nil))
  (= (mvd-spec $Rel $From $To $_ $Diff $Attr $ST)  
    (member $Attr $Diff)
    (remove $Attr $To $ST)
    (\= $ST Nil)
    (compl 
      (mvd $Rel 
        (Cons  $Attr $From) $ST) 
      (mvd $Rel 
        (Cons  $Attr $From) $SCT))
    (\= $SCT Nil))


  (= (ic-spec $IC (Cons  $R $Rs))  
    (prompt-read refinement $DR)
    (set-det)
    (display $R $DR)
    (ic-spec $IC $Rs))
  (= (ic_spec  $IC ())  True)


  (= (subsumed (fd $Rel $From1 $To) (fd $Rel $From2 $To))  
    (subset $From2 $From1))
; /* subsumption test */
  (= (subsumed (mvd $Rel $From1 $To) (mvd $Rel $From2 $To))  
    (set-det)
    (subset $From2 $From1))
  (= (subsumed (mvd $Rel $From1 $To1) (mvd $Rel $From2 $To2))  
    (subset $From2 $From1)
    (compl 
      (mvd $Rel $From1 $To1) 
      (mvd $Rel $From1 $To2)))


  (= (compl (mvd $Rel $From $To) (mvd $Rel $From $CTo))  
    (rel $Rel $AttrList)
    (listdiff $AttrList $From $AL1)
    (listdiff $AL1 $To $CTo))


  (= (compress $In $Out)  
    (select 
      (fd $Rel $From $To1) $In $In1)
    (select 
      (fd $Rel $From $To2) $In1 $In2)
    (set-det)
    (append $To1 $To2 $To)
    (compress 
      (Cons  
        (fd $Rel $From $To) $In2) $Out))
; /* compression */
  (= (compress  $ICs $ICs)  True)


  (= (dep  (fd  $Rel $From $To) fd $Rel $From $To)  True)
; /* dependencies only */
  (= (dep  (mvd  $Rel $From $To) mvd $Rel $From $To)  True)


  (= (display (fd $Rel $From $To) (with_self  $Rel (--> $From $To)))  
    (set-det))
; /* display form */
  (= (display (mvd $Rel $From $To) (with_self  $Rel (->-> $From $To)))  
    (set-det))
  (= (display (join $Rel $R1 $R2) (>< (= $Rel $R1) $R2))  
    (set-det))
  (= (display (plus $Rel $List) (= $Rel $List))  
    (set-det))
  (= (display (proc $Rel) (with_self  (calculated *) $Rel))  
    (set-det))
  (= (display  $X $X)  True)


  (= (template all $X)  
    (set-det))
; /* templates */
  (= (template fd (fd $Rel $From $To))  
    (set-det))
  (= (template mvd (mvd $Rel $From $To))  
    (set-det))
  (= (template join (join $Rel $R1 $R2))  
    (set-det))
  (= (template plus (plus $Rel $List))  
    (set-det))
  (= (template proc (proc $Rel))  
    (set-det))
  (= (template $R $Tuple)  
    (rel $R $A)
    (set-det)
    (values $R $A $V $Tuple))
  (= (template  $X $X)  True)

  (= (diff $Tuple1 $Tuple2 $Diff)  
    (diff $Tuple1 $Tuple2 $Rel $Equal $Diff))
; /* object level <--> meta level */
; ; old diff

  (= (diff $Tuple1 $Tuple2 $Rel $Equal $Diff)  
    (=.. $Tuple1 
      (Cons  $Rel $Values1))
    (=.. $Tuple2 
      (Cons  $Rel $Values2))
    (rel $Rel $AttrList)
    (diff1 $Values1 $Values2 $AttrList $Equal $Diff))
; ; new diff (24-2-93)


  (= (diff1  () () () () ())  True)			;
; new diff1 (24-2-93)
  (= (diff1 (Cons  $V $Values1) (Cons  $V $Values2) (Cons  $A $AttrList) (Cons  $A $Equal) $Diff)  
    (diff1 $Values1 $Values2 $AttrList $Equal $Diff))
  (= (diff1 (Cons  $V1 $Values1) (Cons  $V2 $Values2) (Cons  $A $AttrList) $Equal (Cons  $A $Diff))  
    (\= $V1 $V2)
    (diff1 $Values1 $Values2 $AttrList $Equal $Diff))


  (= (values  $_ () () $_)  True)
  (= (values $Rel (Cons  $Attr $AttrList) (Cons  $Value $ValueList) $Tuple)  
    (value $Rel $Attr $Value $Tuple)
    (values $Rel $AttrList $ValueList $Tuple))


  (= (value $Rel $Attr $Value $Tuple)  
    (rel $Rel $AttrList)
    (al2vl $Attr $AttrList $Value $ValueList)
    (=.. $Tuple 
      (Cons  $Rel $ValueList)))


  (= (al2vl  $_ () $_ ())  True)
  (= (al2vl $Attr (Cons  $Attr $AttrList) $Value (Cons  $Value $ValueList))  
    (al2vl $Attr $AttrList $Value $ValueList))
  (= (al2vl $Attr (Cons  $A $AttrList) $Value (Cons  $V $ValueList))  
    (or 
      (\= $Attr $A) 
      (\= $Value $V))
    (al2vl $Attr $AttrList $Value $ValueList))

  (= (my-sort $Rel $SortAttrs $Tuples $Sorted)  
    (rel $Rel $Attrs)
    (listdiff $Attrs $SortAttrs $RestAttrs)
    (append $SortAttrs $RestAttrs $NewAttrs)
    (reorder $Tuples $NewAttrs $NewTuples)
    (setof0 $T 
      (member $T $NewTuples) $TmpSorted)
    (reorder $TmpSorted $Attrs $Sorted))
; /* text of window: sort */
; ; quick & dirty


  (= (reorder  () $Attrs ())  True)
  (= (reorder (Cons  $T $Ts) $Attrs (Cons  $NewT $NewTs))  
    (values $Rel $Attrs $Values $T)
    (=.. $NewT 
      (Cons  $Rel $Values))
    (reorder $Ts $Attrs $NewTs))


  (= (splitsort $Ts $Dep $SortedTs)  
    (splitsort $Ts $Dep $SortedTs 0 $N 0 $M))

  (= (splitsort $Ts $Dep $SortedTs (, $N $M))  
    (splitsort $Ts $Dep $SortedTs 0 $N 0 $M))

  (= (splitsort  () $Dep () $N $N $M $M)  True)
  (= (splitsort (Cons  $Tuple $Tuples) $Dep (Cons  $Equals $SortedUnEquals) $N0 $N $M0 $M)  
    (partition $Tuples $Tuple $Dep $Equals $UnEquals $M0 $M1)
    (is $N1 
      (+ $N0 1))
    (set-det)
    (splitsort $UnEquals $Dep $SortedUnEquals $N1 $N $M1 $M))


  (= (partition Nil $Tuple $Dep (:: $Tuple) Nil $M0 $M)  
    (is $M 
      (+ $M0 1)))
  (= (partition (Cons  $T $Ts) $Tuple $Dep $Es $UnEs $M0 $M)  
    (horn $Dep 
      (= $Head  
        ($Tuple $Tuple2)))
    (det-if-then-else 
      (= $Tuple2 $T) 
      (, 
        (= $Es 
          (Cons  $T $Es1)) 
        (= $UnEs $UnEs1) 
        (is $M1 
          (+ $M0 1))) 
      (det-if-then otherwise 
        (, 
          (= $Es $Es1) 
          (= $UnEs 
            (Cons  $T $UnEs1)) 
          (= $M1 $M0))))
    (set-det)
    (partition $Ts $Tuple $Dep $Es1 $UnEs1 $M1 $M))


  (= (joinsort $Parts $Dep $NotContr $Contr)  
    (joinsort $Parts $Dep Nil $NotContr Nil $Contr))

  (= (joinsort  () $Dep $NotContr $NotContr $Contr $Contr)  True)
  (= (joinsort (Cons  $Part $Parts) $Dep $NC0 $NC $C0 $C)  
    (contr $Part Nil $Dep $Tuples $Answer)
    (det-if-then-else 
      (= $Answer no) 
      (, 
        (append $NC0 $Part $NC1) 
        (= $C1 $C0)) 
      (det-if-then otherwise 
        (, 
          (= $NC1 $NC0) 
          (divide1 $Part Nil $Dep $DPart) 
          (= $C1 
            (Cons  $DPart $C0)))))
    (set-det)
    (joinsort $Parts $Dep $NC1 $NC $C1 $C))

  (= (decompose $DisplayIC $PosIn $PosOut $InICs $OutICs)  
    (display $IC $DisplayIC)
    (contr $PosIn Nil $IC $Tuples $Answer)
    (det-if-then-else 
      (= $Answer no) 
      (split $IC $PosIn $PosOut $InICs $OutICs) 
      (det-if-then otherwise 
        (divide $PosIn $IC $PosOut $InICs $OutICs))))
; /* text of window: decomp */


  (= (split $Dep $PosIn $PosOut $ICs (Cons  $Join $ICs))  
    (dep $Dep $Type $Rel $From $To)
    (split1 $Rel $From $To $PosIn $PosOut $Join))


  (= (split1 $Rel $From $To $PosIn $PosOut (join $Rel $R1 $R2))  
    (append $From $To $In1)
    (new-name $In1 $Out1 $R1)
    (rel $Rel $AttrList)
    (listdiff $AttrList $To $In2)
    (new-name $In2 $Out2 $R2)
    (splits $Rel $R1 $R2 $Out1 $Out2 $PosIn $PosTmp)
    (remove-dups $PosTmp $PosOut))


  (= (splits  $Rel $R1 $R2 $AList1 $AList2 () ())  True)
  (= (splits $Rel $R1 $R2 $AList1 $AList2 (Cons  $T $Ts) (Cons  $T1 (Cons  $T2 $Rest)))  
    (values $Rel $AList1 $ValueList1 $T)
    (set-det)
    (=.. $T1 
      (Cons  $R1 $ValueList1))
    (values $Rel $AList2 $ValueList2 $T)
    (=.. $T2 
      (Cons  $R2 $ValueList2))
    (splits $Rel $R1 $R2 $AList1 $AList2 $Ts $Rest))
  (= (splits $Rel $R1 $R2 $AList1 $AList2 (Cons  $T $Ts) (Cons  $T $Rest))  
    (splits $Rel $R1 $R2 $AList1 $AList2 $Ts $Rest))
; ; T not in Rel


  (= (new-name $AListIn $AListOut $R)  
    (show-list attributes $AListIn)
    (prompt-read 'relation name' $Answer)
    (det-if-then-else 
      (= $Answer -) 
      (, 
        (concat-atom $AListIn $R) 
        (= $AListOut $AListIn)) 
      (det-if-then-else 
        (= $Answer +) 
        (, 
          (prompt-read 'rel(Name,AttrList)' 
            (rel $R $AListOut)) 
          (permutation $AListOut $AListIn)) 
        (det-if-then otherwise 
          (, 
            (= $R $Answer) 
            (= $AListOut $AListIn)))))
    (new-rel $R $AListOut))


  (= (exec-rule $DisplayRule $PosIn $PosOut)  
    (display $Rule $DisplayRule)
    (horn $Rule 
      (= $Tuple  $Body))
    (setof0 $Tuple 
      (^ $Ts 
        (satisfied $Body $PosIn Nil $Ts)) $PosOut))


  (= (divide $PosIn $Dep $PosOut $InICs (Cons  (plus $Rel $Names) $OutICs))  
    (dep $Dep $Type $Rel $From $To)
    (filter $PosIn $Rel $PosFiltered $Rest)
    (splitsort $PosFiltered $Dep $SplitPos)
    (joinsort $SplitPos $Dep $NotContr $Contr)
    (find-division $Rel $NotContr $Contr $Numbers $NewPos1)
    (new-preds $Rel $NewPos1 Nil $PosTmp1 Nil $Names)
    (append $PosTmp1 $Rest $PosTmp2)
    (decomp-again $Names $Dep $PosTmp2 $PosOut $InICs $OutICs))
; ; divides(SplitPos,Dep,NewPos,Numbers),


  (= (decomp_again  () $Dep $Pos $Pos $ICs $ICs)  True)
  (= (decomp-again (Cons  $Name $Names) $Dep $PosIn $PosOut $InICs $OutICs)  
    (det-if-then-else 
      (yesno (:: 'Decompose ' $Name ? )) 
      (, 
        (dep $Dep $Type $R $From $To) 
        (dep $NewDep $Type $Name $From $To) 
        (display $NewDep $DDep) 
        (decompose $DDep $PosIn $PosTmp $InICs $TmpICs)) 
      (det-if-then otherwise 
        (, 
          (= $PosTmp $PosIn) 
          (= $TmpICs $InICs))))
    (decomp-again $Names $Dep $PosTmp $PosOut $TmpICs $OutICs))


  (= (divides  () $Dep () ())  True)
  (= (divides (Cons  $H $T) $Dep (Cons  $NewH $NewT) (Cons  $NsH $NsT))  
    (length $H $LH)
    (divide1 $H $LH $_ Nil 0 $_ $Dep $NewH $NsH)
    (set-det)
    (divides $T $Dep $NewT $NsT))


  (= (divide1 $Pos1 $Pos2 $IC $PosOut)  
    (divide1 $Pos1 0 $_ $Pos2 0 $_ $IC $PosOut $_))

  (= (divide1 Nil $K $K Nil $L $L $IC Nil Nil)  
    (set-det))
  (= (divide1 $Pos1 $K0 $K $Pos2 $L0 $L $IC $PosOut $M)  
    (contr $Pos1 Nil $IC $Tuples $Answer)
    (det-if-then-else 
      (or 
        (= $Answer yes) 
        (= $Answer possibly)) 
      (, 
        (= $Tuples 
          (Cons  
            (+ $T1) 
            (Cons  
              (+ $T2) $N))) 
        (remove $T2 $Pos1 $NewPos1) 
        (is $K1 
          (- $K0 1)) 
        (= $NewPos2 
          (Cons  $T2 $Pos2)) 
        (is $L1 
          (+ $L0 1)) 
        (= $PosOut $NewPosOut) 
        (= $M $NewM)) 
      (det-if-then 
        (= $Answer no) 
        (, 
          (= $NewPos1 $Pos2) 
          (= $K1 $L0) 
          (= $NewPos2 Nil) 
          (= $L1 0) 
          (= $PosOut 
            (Cons  $Pos1 $NewPosOut)) 
          (= $M 
            (Cons  $K0 $NewM)))))
    (set-det)
    (divide1 $NewPos1 $K1 $K $NewPos2 $L1 $L $IC $NewPosOut $NewM))


  (= (find-division $Rel $NotContr $Contr $Numbers $NewPos)  
    (det-if-then-else 
      (= $NotContr Nil) 
      (= $NewPos $Contr) 
      (det-if-then otherwise 
        (= $NewPos 
          (Cons  
            (:: $NotContr) $Contr))))
    (show-lists $Rel 
      (:: segment part) $NewPos)
    (yesno (:: 'Proceed? ')))


  (= (new_preds  $Rel () $New $New $Names $Names)  True)
  (= (new-preds $Rel (Cons  $H $T) $New0 $New $Names0 $Names)  
    (new-preds1 $Rel $H $New0 $New1 $Names0 $Names1)
    (new-preds $Rel $T $New1 $New $Names1 $Names))


  (= (new_preds1  $Rel () $New $New $Names $Names)  True)
  (= (new-preds1 $Rel (Cons  $R $Rs) $New0 $New $Names0 $Names)  
    (show-list $Rel 'partial relation' $R)
    (prompt-read 'relation name' $N)
    (rel $Rel $Attrs)
    (new-rel $N $Attrs)
    (new-relation $Rel $R $N $NewR)
    (append $New0 $NewR $New1)
    (add-if $N $Names0 $Names1)
    (new-preds1 $Rel $Rs $New1 $New $Names1 $Names))


  (= (new-relation $Rel $R $N $NewR)  
    (bagof0 $NewT 
      (^ $T 
        (, 
          (member $T $R) 
          (new-tuple $Rel $T $N $NewT))) $NewR))


  (= (new-tuple $Rel $T $N $NewT)  
    (=.. $T 
      (Cons  $Rel $Args))
    (set-det)
    (=.. $NewT 
      (Cons  $N $Args)))
  (= (new_tuple  $Rel $T $N $T)  True)	;
; tuple from other relation

  (= (show-lists $Filter $Texts $Lists)  
    (show-lists $Filter $Texts 1 Nil $Lists))
; /* interaction */

  (= (show_lists  $Filter $Words $N $Text ())  True)
  (= (show-lists $Filter Nil $N $Text (Cons  $H $T))  
    (show-list $Filter $Text 
      (Cons  $H $T)))
  (= (show-lists $Filter (Cons  $Word $Words) 1 $Text (:: $H))  
    (set-det)
    (show-lists $Filter $Words 1 $Text $H))
  (= (show-lists $Filter (Cons  $Word $Words) $N $Text (Cons  $H $T))  
    (append $Text 
      (:: $Word $N  --- ) $NewText)
    (show-lists $Filter $Words 1 $NewText $H)
    (is $N1 
      (+ $N 1))
    (show-lists $Filter 
      (Cons  $Word $Words) $N1 $Text $T))


  (= (show-list $Filter $Text $List)  
    (filter $List $Filter $Filtered)
    (show-list $Text $Filtered))

  (= (show-list $Text Nil)  
    (set-det)
    (write-list (:: 'There are no ' $Text .))
    (nl))
  (= (show-list $Text $List)  
    (write-list (:: $Text :))
    (display-list $List $List1)
    (show-items $List1)
    (nl))


  (= (choose-list $Text $List $Sel)  
    (display-list $List $List1)
    (choose-items 1 $List1)
    (write-list (:: $Text ? ))
    (read $Ns)
    (det-if-then-else 
      (nths $List $Ns Nil $Sel) True 
      (det-if-then otherwise 
        (, 
          (write 'Wrong number! Try again.') 
          (nl) 
          (choose-list $Text $List $Sel)))))


  (= (nths $List all Nil $List)  
    (set-det))
  (= (nths $In (- $A $A) $Tmp $Out)  
    (set-det)
    (nths $In $A $Tmp $Out))
  (= (nths $In (- $A $B) $Tmp $Out)  
    (set-det)
    (< $A $B)
    (is $A1 
      (+ $A 1))
    (nths $In $A $Tmp $Tmp1)
    (nths $In 
      (- $A1 $B) $Tmp1 $Out))
  (= (nths $In (, $N $Ns) $Tmp $Out)  
    (set-det)
    (nths $In $N $Tmp $Tmp1)
    (nths $In $Ns $Tmp1 $Out))
;
; nths([],1,Tmp,[X|Tmp]):-!,
;
; write('Which one? '),read(X).
  (= (nths (Cons  $X $R) 1 $Tmp (Cons  $X $Tmp))  
    (set-det))
  (= (nths (Cons  $X $R) $N $Tmp $Out)  
    (is $N1 
      (- $N 1))
    (nths $R $N1 $Tmp $Out))

;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  index/index.pl 607 6 16533))

;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  index/index.pl 611 6 16631))



  (= (choose-items $N Nil)  
    (nl))
; ; nl,write_list([' ',N,'. Other...']),
  (= (choose-items $N (Cons  $H $T))  
    (nl)
    (write-list (::     $N .  $H))
    (is $N1 
      (+ $N 1))
    (choose-items $N1 $T))


  (= (display_list  () ())  True)
  (= (display-list (Cons  $H $T) (Cons  $DH $DT))  
    (det-if-then-else 
      (switched-on horn) 
      (displayhorn $H $DH) 
      (det-if-then otherwise 
        (display $H $DH)))
    (display-list $T $DT))


  (= (displayhorn $X $HX)  
    (horn $X $HX)
    (set-det))
  (= (displayhorn  $X $X)  True)


  (= (prompt-read $Question $Answer)  
    (write-list (:: $Question ? ))
    (read $Answer)
    (det-if-then-else 
      (= $Answer stop) fail 
      (det-if-then otherwise True)))


  (= (yesno $Question)  
    (write-list $Question)
    (read $Answer)
    (det-if-then-else 
      (= $Answer yes) True 
      (det-if-then-else 
        (= $Answer no) fail 
        (det-if-then otherwise 
          (, 
            (call $Answer) 
            (yesno $Question))))))

;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  index/index.pl 647 4 17368))


  (= (write-debug $Message)  
    (switched-on debug)
    (write 	| )
    (write-list $Message)
    (nl))


  (= (write-list $List)  
    (flatten $List $FList)
    (write-list1 $FList))


  (= (write_list1  ())  True)
  (= (write-list1 (Cons  $H $T))  
    (write $H)
    (write-list1 $T))


  (= (quit)  
    (abort))

  (= (keyword1  save '   	save in Prolog database')  True)
; /* commands */
  (= (keyword1  get '   	get from Prolog database')  True)
  (= (keyword1  show '   	show current')  True)
  (= (keyword1  count '   	count current')  True)
  (= (keyword1  del '   	delete')  True)
  (= (keyword1  add '   	add new')  True)
  (= (keyword1  init '   	initialise constraints')  True)
  (= (keyword1  find '   	find constraints')  True)
  (= (keyword1  check '   	check validity')  True)
  (= (keyword1  decomp '   	decompose relation')  True)
  (= (keyword1  comp '   	compose relations')  True)
  (= (keyword1  switch '   	switch on or off')  True)
  (= (keyword1  set '   	set level')  True)
  (= (keyword1  help '   	get help')  True)


  (= (keyword2  pos '   	positive tuples')  True)
  (= (keyword2  neg '   	negative tuples')  True)
  (= (keyword2  ics '   	integrity constraints')  True)
  (= (keyword2  calc '   	calculated relations')  True)


  (= (switch  cwa '    	Closed-World Assumption')  True)
  (= (switch  horn '    	display in Horn form')  True)
  (= (switch  eval '   	evaluate constraints')  True)
  (= (switch  debug '   	show debugging information')  True)


  (= (level  conf '    	confirmation level of constraint')  True)
  (= (level  acc '     	accuracy of constraint')  True)
  (= (level  split '     	splitting level of constraint')  True)


  !(forall 
    (^ $T 
      (keyword1 $W $T)) 
    (op 1200 fx $W))
  !(forall 
    (^ $T 
      (keyword2 $W $T)) 
    (op 1100 fx $W))
  !(forall 
    (^ $T 
      (level $W $T)) 
    (op 1100 fx $W))


  (= (commands $InICs $PosIn $NegIn)  
    (prompt-read '' $Command)
    (do-command $Command $InICs $PosIn $NegIn $OutICs $PosOut $NegOut)
    (set-det)
    (commands $OutICs $PosOut $NegOut))
  (= (commands $InICs $PosIn $NegIn)  
    (write no)
    (nl)
    (set-det)
    (commands $InICs $PosIn $NegIn))


  (= (do-command (save $X) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (set-det)
    (save-command $X $ICs $Pos $Neg))
  (= (do-command (get $X) $InICs $PosIn $NegIn $OutICs $PosOut $NegOut)  
    (set-det)
    (get-command $X $InICs $PosIn $NegIn $OutICs $PosOut $NegOut))
  (= (do-command (show $X) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (set-det)
    (show-command $X $ICs $Pos $Neg))
  (= (do-command (count $X) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (set-det)
    (count-command $X $ICs $Pos $Neg))
  (= (do-command (del $X) $InICs $PosIn $NegIn $OutICs $PosOut $NegOut)  
    (set-det)
    (del-command $X $InICs $PosIn $NegIn $OutICs $PosOut $NegOut))
  (= (do-command (add $X) $InICs $PosIn $NegIn $OutICs $PosOut $NegOut)  
    (set-det)
    (add-command $X $InICs $PosIn $NegIn $OutICs $PosOut $NegOut))
  (= (do-command (init $X) $InICs $Pos $Neg $OutICs $Pos $Neg)  
    (set-det)
    (init-command $X $InICs $OutICs))
  (= (do-command (find $X) $InICs $PosIn $NegIn $OutICs $PosOut $NegOut)  
    (set-det)
    (find-command $X $InICs $PosIn $NegIn $OutICs $PosOut $NegOut))
  (= (do-command (check $X) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (set-det)
    (check-command $X $ICs $Pos $Neg))
  (= (do-command (decomp $IC) $InICs $PosIn $Neg $OutICs $PosOut $Neg)  
    (set-det)
    (decompose $IC $PosIn $PosOut $InICs $OutICs))
  (= (do-command (comp $Rule) $ICs $PosIn $Neg $ICs $PosOut $Neg)  
    (set-det)
    (exec-rule $Rule $PosIn $PosNew)
    (append $PosIn $PosNew $PosOut))
  (= (do-command (switch $X) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (set-det)
    (switch $X))
  (= (do-command (set $X) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (set-det)
    (set $X))
  (= (do-command (help $Topic) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (set-det)
    (help-command $Topic))
  (= (do-command $Command $ICs $Pos $Neg $ICs $Pos $Neg)  
    (call $Command))
  (= (do-command $Command $ICs $Pos $Neg $ICs $Pos $Neg)  
    (write ?))


  (= (save-command (pos $Rel) $ICs $Pos $Neg)  
    (save-pos $Rel $Pos))
  (= (save-command (neg $Rel) $ICs $Pos $Neg)  
    (save-neg $Rel $Neg))
  (= (save-command (ics $T) $ICs $Pos $Neg)  
    (save-ics $T $ICs))


  (= (get-command (pos $Rel) $ICs $PosIn $Neg $ICs $PosOut $Neg)  
    (get-pos $Rel $Pos)
    (append $PosIn $Pos $PosOut))
  (= (get-command (neg $Rel) $ICs $Pos $NegIn $ICs $Pos $NegOut)  
    (get-neg $Rel $Neg)
    (append $NegIn $Neg $NegOut))
  (= (get-command (ics $T) $InICs $Pos $Neg $OutICs $Pos $Neg)  
    (get-ics $T $ICs)
    (append $InICs $ICs $OutICs))


  (= (show-command all $ICs $Pos $Neg)  
    (set-det)
    (show-list 'positive tuples' $Pos)
    (show-list 'negative tuples' $Neg)
    (show-list 'integrity constraints' $ICs))
  (= (show-command rel $ICs $Pos $Neg)  
    (set-det)
    (bagof0 $R 
      (^ $A 
        (rel $R $A)) $Rels)
    (show-list relations $Rels))
  (= (show-command $Other $ICs $Pos $Neg)  
    (get-list $Other $ICs $Pos $Neg $List $Text)
    (set-det)
    (show-list $Text $List))
  (= (show-command $Wrong $ICs $Pos $Neg)  
    (show-list choices 
      (:: 'show all' 'show rel' 'show pos all' 'show pos Rel' 'show neg all' 'show neg Rel' 'show ics all' 'show ics Rel')))


  (= (count-command $Filter $ICs $Pos $Neg)  
    (get-list $Filter $ICs $Pos $Neg $List $Text)
    (length $List $N)
    (write-list (:: 'There are ' $N ' ' $Text .))
    (nl))


  (= (del_command  all $ICs $Pos $Neg () () ())  True)
  (= (del-command (ics $F) $InICs $Pos $Neg $OutICs $Pos $Neg)  
    (filter $InICs $F $Deleted $OutICs))
  (= (del-command (pos $Rel) $ICs $PosIn $Neg $ICs $PosOut $Neg)  
    (filter $PosIn $Rel $Deleted $PosOut))
  (= (del-command (neg $Rel) $ICs $Pos $NegIn $ICs $Pos $NegOut)  
    (filter $NegIn $Rel $Deleted $NegOut))
  (= (del-command (rel $R) $ICs $PosIn $NegIn $ICs $PosOut $NegOut)  
    ( (remove-is-symbol  &self 
        (rel  $R $AList)) 
      (filter $PosIn $Rel $_ $PosOut) 
      (filter $NegIn $Rel $_ $NegOut)))
  (= (del-command (calc $Rel) $InICs $Pos $Neg $OutICs $Pos $Neg)  
    ( (remove-all-atoms  &self 
        (proc  $Rel $Proc)) (remove (proc $Rel) $InICs $OutICs)))


  (= (add-command (ics $DisplayIC) $ICs $Pos $Neg (Cons  $IC $ICs) $Pos $Neg)  
    (display $IC $DisplayIC))
  (= (add_command  (pos  $Tuple) $ICs $Pos $Neg $ICs (Cons  $Tuple $Pos) $Neg)  True)
  (= (add_command  (neg  $Tuple) $ICs $Pos $Neg $ICs $Pos (Cons  $Tuple $Neg))  True)
  (= (add-command (rel $R $AList) $ICs $Pos $Neg $ICs $Pos $Neg)  
    (new-rel $R $AList))
  (= (add-command (calc $Rel) $InICs $PosIn $Neg (Cons  (proc $Rel) $OutICs) $PosOut $Neg)  
    (ask-proc $Rel)
    (filter $PosIn $Rel $RelTuples $_)
    (setof0 $T 
      (, 
        (member $T $RelTuples) 
        (satisfied $T Nil Nil Nil)) $SatTuples)
    (listdiff $PosIn $SatTuples $PosOut)
    (remove 
      (proc $Rel) $InICs $OutICs))
 


  (= (init-command (ics $Rel) $InICs $OutICs)  
    (init-ICs $Rel $ICs)
    (append $InICs $ICs $OutICs))


  (= (find-command ics $InICs $PosIn $NegIn $OutICs $PosOut $NegOut)  
    (char $InICs $PosIn $NegIn $OutICs $PosOut $NegOut))


  (= (check-command (ics $DisplayIC) $ICs $Pos $Neg)  
    (display $IC $DisplayIC)
    (check-ics $IC $Pos $Neg))
  (= (check-command (pos $Tuple) $ICs $Pos $Neg)  
    (set-det)
    (forall 
      (member $IC $ICs) 
      (check-ics $IC 
        (Cons  $Tuple $Pos) $Neg)))


  (= (help-command commands)  
    (set-det)
    (bagof0 
      (with_self  $W $T) 
      (keyword1 $W $T) $L)
    (show-list Commands $L))
  (= (help-command filters)  
    (set-det)
    (bagof0 
      (with_self  $W $T) 
      (keyword2 $W $T) $L)
    (show-list Filters $L))
  (= (help-command switches)  
    (set-det)
    (bagof0 
      (with_self  $W $T) 
      (switch $W $T) $L)
    (show-list Switches $L))
  (= (help-command levels)  
    (set-det)
    (bagof0 
      (with_self  $W $T) 
      (level $W $T) $L)
    (show-list Levels $L))
  (= (help-command $_)  
    (help-command commands)
    (help-command filters)
    (help-command switches)
    (help-command levels))


  (= (get-list (pos $Filter) $ICs $Pos $Neg $List 'positive tuples')  
    (filter $Pos $Filter $List))
  (= (get-list (neg $Filter) $ICs $Pos $Neg $List 'negative tuples')  
    (filter $Neg $Filter $List))
  (= (get-list (ics $Filter) $ICs $Pos $Neg $List 'integrity constraints')  
    (filter $ICs $Filter $Tmp)
    (compress $Tmp $List))


  (= (filter $In $F $Out)  
    (template $F $Template)
    (setof0 $Template 
      (member $Template $In) $Out))

  (= (filter $In $F $Out $Rest)  
    (filter $In $F $Out)
    (listdiff $In $Out $Rest))


  (= (new-rel $R $Attrs)  
    (rel $R $Attrs)
    (set-det))
  (= (new-rel $R $Attrs)  
    (rel $R $A)
    (set-det)
    (write 'Error: relation name already in use')
    (nl)
    (fail))
  (= (new-rel $R $Attrs)  
    (add-is-symbol  &self 
      (rel  $R $Attrs)))


  (= (ask-proc $Rel)  
    ( (prompt-read clause $Clause) 
      (add-is-symbol  &self 
        (proc  $Rel $Clause)) 
      (set-det) 
      (ask-proc $Rel)))
  (= (ask_proc  $Rel)  True)


  (= (get-pos $Rel $Pos)  
    (template $Rel $Tuple)
    (bagof0 $Tuple 
      (pos-tuple $Tuple) $Pos))


  (= (get-neg $Rel $Neg)  
    (template $Rel $Tuple)
    (bagof0 $Tuple 
      (neg-tuple $Tuple) $Neg))


  (= (get-ics $T $ICs)  
    (template $T $Templ)
    (bagof0 $Templ 
      (constraint $Templ) $ICs))


  (= (save-pos $Rel $Pos)  
    (template $Rel $T)
    (forall 
      (member $T $Pos) 
      (myassert (pos-tuple $T))))


  (= (save-neg $Rel $Neg)  
    (template $Rel $T)
    (forall 
      (member $T $Neg) 
      (myassert (neg-tuple $T))))


  (= (save-ics $T $ICs)  
    (template $T $Templ)
    (forall 
      (member $Templ $ICs) 
      (myassert (constraint $Templ))))


  (= (check-ics $IC $Pos $Neg)  
    (display $IC $DisplayIC)
    (evaluate $Pos $Neg $IC $Tuples $PosOut $NegOut $Answer)
    (write $DisplayIC)
    (det-if-then-else 
      (= $Answer refine) 
      (show-list ' is contradicted by' $Tuples) 
      (det-if-then-else 
        (= $Answer keep) 
        (, 
          (write ' is satisfied') 
          (nl)) 
        (det-if-then-else 
          (= $Answer 
            (keep $E)) 
          (, 
            (write-list (:: ' looks promising: ' $E)) 
            (nl)) 
          (det-if-then 
            (= $Answer 
              (ignore $E)) 
            (, 
              (write-list (:: ' has low confirmation: ' $E)) 
              (nl)))))))


  (= (switch $X)  
    ( (switch $X $T) 
      (remove-is-symbol  &self 
        (switched_on  $X)) 
      (set-det) 
      (write-list (:: $X ' is now off.')) 
      (nl)))
  (= (switch $X)  
    ( (switch $X $T) 
      (set-det) 
      (add-is-symbol  &self 
        (switched_on  $X)) 
      (write-list (:: $X ' is now on.')) 
      (nl)))
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  index/index.pl 912 28 25372))



  (= (set $X)  
    (=.. $X 
      (Cons  $Level $Rest))
    (level $Level $T)
    (set-det)
    (det-if-then-else 
      (= $Rest 
        (:: $Value)) 
      (, 
        (or 
          (, 
            (remove-is-symbol  &self 
              (level_set  $Level $V)) 
            (set-det)) True) 
        (add-is-symbol  &self 
          (level_set  $Level $Value))) 
      (det-if-then 
        (= $Rest Nil) 
        (, 
          (get-level $Level $L) 
          (write (= $Level $L)) 
          (nl)))))
  (= (set $_)  
    (setof0 
      (= $L $V) 
      (^ $T 
        (, 
          (level $L $T) 
          (get-level $L $V))) $L)
    (show-list levels $L))


  (= (get-level $Level $Value)  
    (level $Level $T)
    (or 
      (, 
        (level-set $Level $Value) 
        (set-det)) 
      (= $Value 0)))


  (= (compare $X)  
    (=.. $X 
      (:: $Level $Value))
    (get-level $Level $L)
    (det-if-then-else 
      (= $L 
        (+ $V)) 
      (>= $Value $V) 
      (det-if-then-else 
        (= $L 
          (- $V)) 
        (=< $Value $V) 
        (det-if-then-else 
          (= $L 
            (/ $V $A)) 
          (, 
            (is $Upper 
              (+ $V $A)) 
            (=< $Value $Upper) 
            (is $Lower 
              (- $V $A)) 
            (>= $Value $Lower)) 
          (det-if-then otherwise 
            (, 
              (write-list (:: 'Wrong level: ' (= $Level $L))) 
              (nl) 
              (break)))))))



  !(run *)

