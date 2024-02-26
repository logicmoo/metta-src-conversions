
  (= 
    (my-sort $Rel $SortAttrs $Tuples $Sorted) 
    ( (rel $Rel $Attrs) 
      (listdiff $Attrs $SortAttrs $RestAttrs) 
      (append $SortAttrs $RestAttrs $NewAttrs) 
      (reorder $Tuples $NewAttrs $NewTuples) 
      (setof0 $T 
        (member $T $NewTuples) $TmpSorted) 
      (reorder $TmpSorted $Attrs $Sorted)))
; 



  (= 
    (reorder  () $Attrs ()) True)
; 

  (= 
    (reorder 
      (Cons  $T $Ts) $Attrs 
      (Cons  $NewT $NewTs)) 
    ( (values $Rel $Attrs $Values $T) 
      (=.. $NewT 
        (Cons  $Rel $Values)) 
      (reorder $Ts $Attrs $NewTs)))
; 



  (= 
    (splitsort $Ts $Dep $SortedTs) 
    (splitsort $Ts $Dep $SortedTs 0 $N 0 $M))
; 


  (= 
    (splitsort $Ts $Dep $SortedTs 
      (, $N $M)) 
    (splitsort $Ts $Dep $SortedTs 0 $N 0 $M))
; 


  (= 
    (splitsort  () $Dep () $N $N $M $M) True)
; 

  (= 
    (splitsort 
      (Cons  $Tuple $Tuples) $Dep 
      (Cons  $Equals $SortedUnEquals) $N0 $N $M0 $M) 
    ( (partition $Tuples $Tuple $Dep $Equals $UnEquals $M0 $M1) 
      (is $N1 
        (+ $N0 1)) 
      (set-det) 
      (splitsort $UnEquals $Dep $SortedUnEquals $N1 $N $M1 $M)))
; 



  (= 
    (partition Nil $Tuple $Dep 
      (:: $Tuple) Nil $M0 $M) 
    (is $M 
      (+ $M0 1)))
; 

  (= 
    (partition 
      (Cons  $T $Ts) $Tuple $Dep $Es $UnEs $M0 $M) 
    ( (horn $Dep 
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
      (partition $Ts $Tuple $Dep $Es1 $UnEs1 $M1 $M)))
; 



  (= 
    (joinsort $Parts $Dep $NotContr $Contr) 
    (joinsort $Parts $Dep Nil $NotContr Nil $Contr))
; 


  (= 
    (joinsort  () $Dep $NotContr $NotContr $Contr $Contr) True)
; 

  (= 
    (joinsort 
      (Cons  $Part $Parts) $Dep $NC0 $NC $C0 $C) 
    ( (contr $Part Nil $Dep $Tuples $Answer) 
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
      (joinsort $Parts $Dep $NC1 $NC $C1 $C)))
; 


