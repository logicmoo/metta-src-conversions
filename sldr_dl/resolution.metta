;                                           (convert_to_metta_file  resolution $_192350 sldr_dl/resolution.pl sldr_dl/resolution.metta)

  !(:: (nnet *))



  (= (dnn-heuristic $Nnet $G $SList (method $_ (input $TreeBreadth $TreeDepth) $_) $Res)  
    (pred-to-vec $G $SList $TreeBreadth $TreeDepth $InVec)
    (nnet-forward $Nnet 
      (:: $InVec) 
      (:: $OutVec))
    (label-vec $OutVec 1 $OVL)
    (sort $OVL $OVLS)
    (reverse $OVLS $Res)
    (set-det))



  (= (comp-indim $TreeBreadth $TreeDepth $NumWord $Dim)  
    (get-partial-tree a $TreeBreadth $TreeDepth $T)
    (flatten $T $FT)
    (length $FT $P)
    (is $Dim 
      (* $P $NumWord))
    (set-det))



  (= (pred-to-vec $Pred (:: $SList $MaxN) $TreeBreadth $TreeDepth $Vec)  
    (copy-term $Pred $GT)
    (vble-fill $GT $GTF)
    (get-partial-tree $GTF $TreeBreadth $TreeDepth $PT)
    (flatten $PT $PTF)
    (symlist-to-numlist $PTF $SList $NL)
    (numlist-to-vec $NL $MaxN $Vec)
    (set-det))


  (= (label-vec Nil $_ Nil)  
    (set-det))
  (= (label-vec (Cons  $Elem $L) $Num (Cons  (:: $Elem $Num) $LT))  
    (is $N1 
      (+ $Num 1))
    (label-vec $L $N1 $LT)
    (set-det))
  


  (= (dnn-train $Nnet $G $SList $AxNum (method (learning $NumEpoch $LRate) (input $TreeBreadth $TreeDepth) (output $OutDim)))  
    (pred-to-vec $G $SList $TreeBreadth $TreeDepth $InVec)
    (axnum-to-vec $AxNum $OutDim $TgtVec)
    (nnet-train $Nnet 
      (:: $InVec) 
      (:: $TgtVec) $NumEpoch $LRate)
    (set-det))


  (= (axnum-to-vec $AxNum $Dim $Vec)  
    (> $AxNum $Dim)
    (print (:: 'Warning: AxNum is bigger than Dim: ' $AxNum $Dim))
    (nl)
    (copy-n-times 0 $Dim $Vec)
    (set-det))

  (= (axnum-to-vec $AxNum $Dim $Vec)  
    (is $D1 
      (- $AxNum 1))
    (copy-n-times 0 $D1 $Vec1)
    (is $D2 
      (- $Dim $AxNum))
    (copy-n-times 0 $D2 $Vec2)
    (append $Vec1 
      (Cons  1 $Vec2) $Vec)
    (set-det))



  !(dynamic (/ search-time 1))

  (= (init-search-time)  
    ( (remove-all-atoms  &self 
        (search_time  $_)) (add-is-symbol  &self (search_time  0))))

  (= (add-search-time)  
    ( (search-time $N) 
      (remove-all-atoms  &self 
        (search_time  $_)) 
      (is $N1 
        (+ $N 1)) 
      (add-is-symbol  &self 
        (search_time  $N1)) 
      (set-det)))


  (= (dnn-sl-resolution $A $B $C $D $E $F $G)  
    (dnn-sl-resolution $A $B $C standard $D $E $F $G))

  (= (dnn_sl_resolution  () $_ $_ $_ $_ $_ $_ ())  True)
  (= (dnn-sl-resolution $GList (:: $AList $NumA) $SList $StatModName $Nnet $Mtd $Depth $Path)  
    (> $Depth 0)
    (is $D1 
      (- $Depth 1))
    (= $GList 
      (Cons  
        (- $OrgG) $GLT))
    (copy-term $GList $PreGList)
    (static-module $StatModName $OrgG $G)
    (copy-term $G $GTemp)
    (dnn-heuristic $Nnet $G $SList $Mtd $SAL)
    (member 
      (:: $_ $AxNum) $SAL)
    (member 
      (:: $AxNum $AxName $AxRule) $AList)
    (add-search-time)
    (copy-term $AxRule $Ax)
    (append $GN 
      (:: (+ $G)) $Ax)
    (append $GN $GLT $GListNew)
    (dnn-sl-resolution $GListNew 
      (:: $AList $NumA) $SList $StatModName $Nnet $Mtd $D1 $PathNew)
    (= $Path 
      (Cons  
        (:: $PreGList $AxName) $PathNew))
    (or 
      (, 
        (= $Mtd 
          (method 
            (learning $_ $_) $_ $_)) 
        (dnn-train $Nnet $GTemp $SList $AxNum $Mtd)) 
      (= $Mtd 
        (method reasoning $_ $_))))
; ;SAL = AList,
; ;Ax = [+G|GN],


  (= (print_by_line  ())  True)
  (= (print-by-line (Cons  $X $L))  
    (print $X)
    (nl)
    (print-by-line $L))




  !(dynamic (/ num-vble 1))
  ! (remove-all-atoms  &self 
      (num_vble  $_))
  ! (add-is-symbol  &self 
      (num_vble  0))


  (= (vble-fill $X $X)  
    (ground $X)
    (set-det))
  (= (vble-fill $X $X)  
    ( (var $X) 
      (num-vble $N1) 
      (is $N 
        (+ $N1 1)) 
      (remove-all-atoms  &self 
        (num_vble  $_)) 
      (add-is-symbol  &self 
        (num_vble  $N)) 
      (= $X 
        (vble $N)) 
      (set-det)))
  (= (vble-fill Nil Nil)  
    (set-det))
  (= (vble-fill (Cons  $X $L) (Cons  $XT $LT))  
    (vble-fill $X $XT)
    (vble-fill $L $LT)
    (set-det))
 


  (= (copy-n-times $_ 0 Nil)  
    (set-det))
  (= (copy-n-times $X $N (Cons  $X $L))  
    (> $N 0)
    (is $N1 
      (- $N 1))
    (copy-n-times $X $N1 $L)
    (set-det))


  (= (produce-empty-tree $_ 0 novalue)  
    (set-det))
  (= (produce-empty-tree $B $D (Cons  novalue $L))  
    (> $D 0)
    (is $D1 
      (- $D 1))
    (produce-empty-tree $B $D1 $Res1)
    (copy-n-times $Res1 $B $L)
    (set-det))


  (= (get-partial-tree (Cons  $X $_) $_ 0 $X)  
    (set-det))
  (= (get-partial-tree $X $_ 0 $X)  
    (not (is-list $X))
    (set-det))
  (= (get-partial-tree (Cons  $X $L) $Breadth $Depth (Cons  $X $LT))  
    (> $Depth 0)
    (is $D1 
      (- $Depth 1))
    (get-partial-tree2 $L $Breadth $D1 $LT)
    (set-det))
  (= (get-partial-tree $X $Breadth $Depth (Cons  $X $LT))  
    (not (is-list $X))
    (> $Depth 0)
    (is $D1 
      (- $Depth 1))
    (get-partial-tree2 Nil $Breadth $D1 $LT)
    (set-det))


  (= (get-partial-tree2 $L $Breadth $Depth $Res)  
    (findall $XT 
      (, 
        (member $X $L) 
        (get-partial-tree $X $Breadth $Depth $XT)) $Res1)
    (length $L $LenL)
    (length $XT $LenL)
    (is $N1 
      (- $Breadth $LenL))
    (or 
      (, 
        (>= $N1 0) 
        (produce-empty-tree $Breadth $Depth $TEmpty) 
        (copy-n-times $TEmpty $N1 $Res2) 
        (append $Res1 $Res2 $Res)) 
      (, 
        (< $N1 0) 
        (get-first-element $Res1 $Breadth $Res)))
    (set-det))
; ; Check if the lengths agree.


  (= (get_first_element  $_ 0 ())  True)
  (= (get-first-element Nil $N (Cons  norule $Res1))  
    (> $N 0)
    (is $N1 
      (- $N 1))
    (get-first-element Nil $N1 $Res1))
  (= (get-first-element (Cons  $X $L) $N (Cons  $X $Res1))  
    (> $N 0)
    (is $N1 
      (- $N 1))
    (get-first-element $L $N1 $Res1))


  (= (symlist-to-numlist Nil $_ Nil)  
    (set-det))
  (= (symlist-to-numlist (Cons  $X $L) $SList (Cons  $XT $LT))  
    (or 
      (, 
        (= $X 
          (vble $_)) 
        (= $XT 1)) 
      (or 
        (, 
          (= $X novalue) 
          (= $XT -1)) 
        (member 
          (:: $XT $X) $SList)))
    (symlist-to-numlist $L $SList $LT)
    (set-det))


  (= (num-to-vec -1 $Dim $Vec)  
    (copy-n-times 0 $Dim $Vec)
    (set-det))
  (= (num-to-vec $Num $Dim $Vec)  
    (> $Num $Dim)
    (print (:: 'Warning: Num is bigger than Dim: ' $Num $Dim))
    (nl)
    (copy-n-times 0 $Dim $Vec)
    (set-det))

  (= (num-to-vec $Num $Dim $Vec)  
    (is $D1 
      (- $Num 1))
    (copy-n-times 0 $D1 $Vec1)
    (is $D2 
      (- $Dim $Num))
    (copy-n-times 0 $D2 $Vec2)
    (append $Vec1 
      (Cons  1 $Vec2) $Vec)
    (set-det))




  (= (numlist-to-vec Nil $_ Nil)  
    (set-det))
  (= (numlist-to-vec (Cons  $X $L) $Dim $Res)  
    (num-to-vec $X $Dim $XT)
    (numlist-to-vec $L $Dim $LT)
    (append $XT $LT $Res)
    (set-det))


  (= (for $K $P $Q)  
    (=< $P $Q)
    (= $K $P))
  (= (for $K $P $Q)  
    (< $P $Q)
    (is $P1 
      (+ $P 1))
    (for $K $P1 $Q))


  (= (static_module  standard $G $G)  True)



