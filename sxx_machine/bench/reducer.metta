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
    (top) 
    ( (try 
        (fac 3) $ans1) (try (quick (:: 3 1 2)) $ans2)))
; 

;
; 



  (= 
    (try $inpexpr $anslist) 
    ( (listify $inpexpr $list) 
      (curry $list $curry) 
      (t-reduce $curry $ans) 
      (make-list $ans $anslist)))
; 


;
; 



  (= 
    (end $X) 
    ( (atom $X) (set-det)))
; 

  (= 
    (end $X) 
    (== $X Nil))
; 



  (= 
    (list-functor-name $Name) 
    (functor 
      (Cons  $_ $_) $Name $_))
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
    (t_def  fac 
      ($N) 
      (cond  
        (= $N 0) 1 
        (*  $N 
          (fac  
            (-  $N 1))))) True)
; 


;
; 

  (= 
    (t_def  quick 
      ($l) 
      (cond  
        (= $l ()) () 
        (cond  
          (= 
            (tl  $l) ()) $l 
          (quick2  
            (split  
              (hd  $l) 
              (tl  $l)))))) True)
; 

  (= 
    (t_def  quick2 
      ($l) 
      (append  
        (quick  
          (hd  $l)) 
        (quick  
          (tl  $l)))) True)
; 


  (= 
    (t_def  split 
      ($e $l) 
      (cond  
        (= $l ()) 
        ( ($e)) 
        (cond  
          (=<  
            (hd  $l) $e) 
          (inserthead  
            (hd  $l) 
            (split  $e 
              (tl  $l))) 
          (inserttail  
            (hd  $l) 
            (split  $e 
              (tl  $l)))))) True)
; 

  (= 
    (t_def  inserthead 
      ($e $l) 
      (Cons  
        (Cons  $e 
          (hd  $l)) 
        (tl  $l))) True)
; 

  (= 
    (t_def  inserttail 
      ($e $l) 
      (Cons  
        (hd  $l) 
        (Cons  $e 
          (tl  $l)))) True)
; 


  (= 
    (t_def  append 
      ($a $b) 
      (cond  
        (= $a ()) $b 
        (Cons  
          (hd  $a) 
          (append  
            (tl  $a) $b)))) True)
; 


;
; 


;
; 

;
; 



  (= 
    (t-reduce $expr $ans) 
    ( (atomic $expr) 
      (set-det) 
      (= $ans $expr)))
; 

;
; 

  (= 
    (t-reduce 
      (Cons  $y 
        (Cons  $x $LF)) 
      (Cons  $yr 
        (Cons  $xr $LF))) 
    ( (list-functor-name $LF) 
      (t-reduce $x $xr) 
      (set-det) 
      (t-reduce $y $yr) 
      (set-det)))
; 

  (= 
    (t-reduce $expr $ans) 
    ( (t-append $next $red $form $expr) 
      (t-redex $form $red) 
      (set-det) 
      (t-reduce $next $ans) 
      (set-det)))
; 



  (= 
    (t_append  $link $link $l $l) True)
; 

  (= 
    (t-append 
      (Cons  $a $l1) $link $l2 
      (Cons  $a $l3)) 
    (t-append $l1 $link $l2 $l3))
; 


;
; 


;
; 


  (= 
    (t-redex 
      (Cons  $x 
        (Cons  $g 
          (Cons  $f 
            (Cons  $k sp)))) 
      (Cons  
        (Cons  $xr $g) 
        (Cons  
          (Cons  $xr $f) $k))) 
    (t-reduce $x $xr))
; 

  (= 
    (t_redex  
      (Cons  $x 
        (Cons  $g 
          (Cons  $f 
            (Cons  $k bp)))) 
      (Cons  
        (Cons  $x $g) 
        (Cons  $f $k))) True)
; 

  (= 
    (t_redex  
      (Cons  $x 
        (Cons  $g 
          (Cons  $f 
            (Cons  $k cp)))) 
      (Cons  $g 
        (Cons  
          (Cons  $x $f) $k))) True)
; 

  (= 
    (t-redex 
      (Cons  $x 
        (Cons  $g 
          (Cons  $f s))) 
      (Cons  
        (Cons  $xr $g) 
        (Cons  $xr $f))) 
    (t-reduce $x $xr))
; 

  (= 
    (t_redex  
      (Cons  $x 
        (Cons  $g 
          (Cons  $f b))) 
      (Cons  
        (Cons  $x $g) $f)) True)
; 

  (= 
    (t_redex  
      (Cons  $x 
        (Cons  $g 
          (Cons  $f c))) 
      (Cons  $g 
        (Cons  $x $f))) True)
; 

  (= 
    (t_redex  
      (Cons  $y 
        (Cons  $x k)) $x) True)
; 

  (= 
    (t_redex  
      (Cons  $x i) $x) True)
; 


;
; 

  (= 
    (t-redex 
      (Cons  $elsepart 
        (Cons  $ifpart 
          (Cons  $cond cond))) $ifpart) 
    ( (t-reduce $cond $bool) 
      (= $bool True) 
      (set-det)))
; 

	;
; 

  (= 
    (t_redex  
      (Cons  $elsepart 
        (Cons  $ifpart 
          (Cons  $cond cond))) $elsepart) True)
; 


;
; 

  (= 
    (t-redex 
      (Cons  $f apply) $fr) 
    (t-reduce $f $fr))
; 


;
; 

  (= 
    (t-redex 
      (Cons  $arg hd) $x) 
    ( (list-functor-name $LF) (t-reduce $arg (Cons  $y (Cons  $x $LF)))))
; 

  (= 
    (t-redex 
      (Cons  $arg tl) $y) 
    ( (list-functor-name $LF) (t-reduce $arg (Cons  $y (Cons  $x $LF)))))
; 


;
; 

  (= 
    (t-redex 
      (Cons  $y 
        (Cons  $x $op)) $res) 
    ( (end $op) 
      (member $op 
        (:: + - * // mod)) 
      (t-reduce $x $xres) 
      (t-reduce $y $yres) 
      (number $xres) 
      (number $yres) 
      (eval $op $res $xres $yres)))
; 


;
; 

  (= 
    (t-redex 
      (Cons  $y 
        (Cons  $x $test)) $res) 
    ( (end $test) 
      (member $test 
        (:: < > =< >= =\= =:=)) 
      (t-reduce $x $xres) 
      (t-reduce $y $yres) 
      (number $xres) 
      (number $yres) 
      (det-if-then-else 
        (relop $test $xres $yres) 
        (= $res True) 
        (= $res False)) 
      (set-det)))
; 


;
; 

  (= 
    (t-redex 
      (Cons  $y 
        (Cons  $x =)) $res) 
    ( (t-reduce $x $xres) 
      (t-reduce $y $yres) 
      (det-if-then-else 
        (= $xres $yres) 
        (= $res True) 
        (= $res False)) 
      (set-det)))
; 


;
; 

  (= 
    (t-redex 
      (Cons  $x $op) $res) 
    ( (end $op) 
      (member $op 
        (:: -)) 
      (t-reduce $x $xres) 
      (number $xres) 
      (eval1 $op $t $xres)))
; 


;
; 

;
; 

;
; 

  (= 
    (t-redex $in $out) 
    ( (append $par $func $in) 
      (end $func) 
      (t-def $func $args $expr) 
      (t $args $expr $def) 
      (append $par $def $out)))
; 


;
; 



  (= 
    (eval + $C $A $B) 
    (is $C 
      (+ $A $B)))
; 

  (= 
    (eval - $C $A $B) 
    (is $C 
      (- $A $B)))
; 

  (= 
    (eval * $C $A $B) 
    (is $C 
      (* $A $B)))
; 

  (= 
    (eval // $C $A $B) 
    (is $C 
      (// $A $B)))
; 

  (= 
    (eval mod $C $A $B) 
    (is $C 
      (mod $A $B)))
; 



  (= 
    (eval1 - $C $A) 
    (is $C 
      (- $A)))
; 



  (= 
    (relop < $A $B) 
    (< $A $B))
; 

  (= 
    (relop > $A $B) 
    (> $A $B))
; 

  (= 
    (relop =< $A $B) 
    (=< $A $B))
; 

  (= 
    (relop >= $A $B) 
    (>= $A $B))
; 

  (= 
    (relop =\= $A $B) 
    (=\= $A $B))
; 

  (= 
    (relop =:= $A $B) 
    (=:= $A $B))
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
    (t $argvars $expr $trans) 
    ( (listify $expr $list) 
      (curry $list $curry) 
      (t-argvars $argvars $curry $trans) 
      (set-det)))
; 



  (= 
    (t_argvars  () $trans $trans) True)
; 

  (= 
    (t-argvars 
      (Cons  $x $argvars) $in $trans) 
    ( (t-argvars $argvars $in $mid) 
      (t-vars $mid $vars) 
      (t-trans $x $mid $vars $trans)))
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
    (curry $a $a) 
    ( (or 
        (var $a) 
        (atomic $a)) (set-det)))
; 

  (= 
    (curry 
      (Cons  $func $args) $cargs) 
    (currylist $args $cargs $func))
; 


;
; 


  (= 
    (currylist Nil $link $link) 
    (set-det))
; 

  (= 
    (currylist 
      (Cons  $a $args) $cargs $link) 
    ( (curry $a $c) (currylist $args $cargs (Cons  $c $link))))
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
    (t-vars $v 
      (:: (:: $v))) 
    ( (var $v) (set-det)))
; 

  (= 
    (t-vars $a 
      (:: Nil)) 
    ( (atomic $a) (set-det)))
; 

  (= 
    (t-vars 
      (:: $func) 
      (:: Nil)) 
    ( (atomic $func) (set-det)))
; 

  (= 
    (t-vars 
      (Cons  $arg $func) 
      (:: $g 
        (Cons  $g1 $af1) 
        (Cons  $g2 $af2))) 
    ( (t-vars $arg 
        (Cons  $g1 $af1)) 
      (t-vars $func 
        (Cons  $g2 $af2)) 
      (unionv $g1 $g2 $g)))
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
    (t-trans $x $a $_ 
      (Cons  $a k)) 
    ( (or 
        (atomic $a) 
        (, 
          (var $a) 
          (\== $a $x))) (set-det)))
; 

  (= 
    (t-trans $x $y $_ i) 
    ( (== $x $y) (set-det)))
; 

  (= 
    (t-trans $x $e 
      (Cons  $ve $_) 
      (Cons  $e k)) 
    (notinv $x $ve))
; 

  (= 
    (t-trans $x 
      (Cons  $f $e) 
      (:: $vef $sf $se) $res) 
    ( (= $sf 
        (Cons  $vf $_)) 
      (= $se 
        (Cons  $ve $other)) 
      (or 
        (end $e) 
        (, 
          (= $other 
            (:: $_ 
              (Cons  $ve1 $_))) 
          (\== $ve1 Nil))) 
      (t-rule1 $x $e $ve $se $f $vf $sf $res)))
; 

  (= 
    (t-trans $x 
      (Cons  $g 
        (Cons  $f $e)) 
      (:: $vefg $sg $sef) $res) 
    ( (= $sg 
        (Cons  $vg $_)) 
      (= $sef 
        (:: $vef $sf $se)) 
      (= $se 
        (Cons  $ve $_)) 
      (= $sf 
        (Cons  $vf $_)) 
      (t-rule2 $x $e $f $vf $sf $g $vg $sg $res)))
; 


;
; 


  (= 
    (t-rule1 $x $e $ve $se $f $vf $sf $e) 
    ( (notinv $x $ve) 
      (== $x $f) 
      (set-det)))
; 

  (= 
    (t-rule1 $x $e $ve $se $f $vf $sf 
      (Cons  $resf 
        (Cons  $e b))) 
    ( (notinv $x $ve) 
      (inv $x $vf) 
      (\== $x $f) 
      (set-det) 
      (t-trans $x $f $sf $resf)))
; 

  (= 
    (t-rule1 $x $e $ve $se $f $vf $sf 
      (Cons  $f 
        (Cons  $rese c))) 
    ( (notinv $x $vf) 
      (set-det) 
      (t-trans $x $e $se $rese)))
; 

  (= 
    (t-rule1 $x $e $ve $se $f $vf $sf 
      (Cons  $resf 
        (Cons  $rese s))) 
    ( (t-trans $x $e $se $rese) (t-trans $x $f $sf $resf)))
; 


;
; 


  (= 
    (t-rule2 $x $e $f $vf $sf $g $vg $sg 
      (Cons  $g 
        (Cons  $e c))) 
    ( (== $x $f) 
      (notinv $x $vg) 
      (set-det)))
; 

  (= 
    (t-rule2 $x $e $f $vf $sf $g $vg $sg 
      (Cons  $resg 
        (Cons  $e s))) 
    ( (== $x $f) 
      (set-det) 
      (t-trans $x $g $sg $resg)))
; 

  (= 
    (t-rule2 $x $e $f $vf $sf $g $vg $sg 
      (Cons  $g 
        (Cons  $resf 
          (Cons  $e cp)))) 
    ( (inv $x $vf) 
      (notinv $x $vg) 
      (set-det) 
      (t-trans $x $f $sf $resf)))
; 

  (= 
    (t-rule2 $x $e $f $vf $sf $g $vg $sg 
      (Cons  $resg 
        (Cons  $resf 
          (Cons  $e sp)))) 
    ( (inv $x $vf) 
      (set-det) 
      (t-trans $x $f $sf $resf) 
      (t-trans $x $g $sg $resg)))
; 

  (= 
    (t-rule2 $x $e $f $vf $sf $g $vg $sg 
      (Cons  $f $e)) 
    ( (== $x $g) (set-det)))
; 

  (= 
    (t-rule2 $x $e $f $vf $sf $g $vg $sg 
      (Cons  $resg 
        (Cons  $f 
          (Cons  $e bp)))) 
    (t-trans $x $g $sg $resg))
; 


;
; 


;
; 


;
; 


  (= 
    (make-list $a $a) 
    (atomic $a))
; 

  (= 
    (make-list 
      (Cons  $b 
        (Cons  $a $LF)) 
      (Cons  $a $rb)) 
    ( (list-functor-name $LF) (make-list $b $rb)))
; 



  (= 
    (listify $X $X) 
    ( (or 
        (var $X) 
        (atomic $X)) (set-det)))
; 

  (= 
    (listify $Expr 
      (Cons  $Op $LArgs)) 
    ( (functor $Expr $Op $N) (listify-list 1 $N $Expr $LArgs)))
; 



  (= 
    (listify-list $I $N $_ Nil) 
    ( (> $I $N) (set-det)))
; 

  (= 
    (listify-list $I $N $Expr 
      (Cons  $LA $LArgs)) 
    ( (=< $I $N) 
      (set-det) 
      (arg $I $Expr $A) 
      (listify $A $LA) 
      (is $I1 
        (+ $I 1)) 
      (listify-list $I1 $N $Expr $LArgs)))
; 



  (= 
    (member  $X 
      (Cons  $X $_)) True)
; 

  (= 
    (member $X 
      (Cons  $_ $L)) 
    (member $X $L))
; 



  (= 
    (append  () $L $L) True)
; 

  (= 
    (append 
      (Cons  $X $L1) $L2 
      (Cons  $X $L3)) 
    (append $L1 $L2 $L3))
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
    (intersectv  () $_ ()) True)
; 

  (= 
    (intersectv 
      (Cons  $A $S1) $S2 $S) 
    (intersectv-2 $S2 $A $S1 $S))
; 



  (= 
    (intersectv_2  () $_ $_ ()) True)
; 

  (= 
    (intersectv-2 
      (Cons  $B $S2) $A $S1 $S) 
    ( (compare $Order $A $B) (intersectv-3 $Order $A $S1 $B $S2 $S)))
; 



  (= 
    (intersectv-3 < $_ $S1 $B $S2 $S) 
    (intersectv-2 $S1 $B $S2 $S))
; 

  (= 
    (intersectv-3 = $A $S1 $_ $S2 
      (Cons  $A $S)) 
    (intersectv $S1 $S2 $S))
; 

  (= 
    (intersectv-3 > $A $S1 $_ $S2 $S) 
    (intersectv-2 $S2 $A $S1 $S))
; 



  (= 
    (intersectv_list  () ()) True)
; 

  (= 
    (intersectv-list 
      (Cons  $InS $Sets) $OutS) 
    (intersectv-list $Sets $InS $OutS))
; 



  (= 
    (-->  
      (intersectv_list  ()) ()) True)
; 

  (= 
    (-->  
      (intersectv_list  
        (Cons  $S $Sets)) 
      (,  
        (intersectv  $S) 
        (intersectv_list  $Sets))) True)
; 


;
; 


  (= 
    (diffv  () $_ ()) True)
; 

  (= 
    (diffv 
      (Cons  $A $S1) $S2 $S) 
    (diffv-2 $S2 $A $S1 $S))
; 



  (= 
    (diffv_2  () $A $S1 
      (Cons  $A $S1)) True)
; 

  (= 
    (diffv-2 
      (Cons  $B $S2) $A $S1 $S) 
    ( (compare $Order $A $B) (diffv-3 $Order $A $S1 $B $S2 $S)))
; 



  (= 
    (diffv-3 < $A $S1 $B $S2 
      (Cons  $A $S)) 
    (diffv $S1 
      (Cons  $B $S2) $S))
; 

  (= 
    (diffv-3 = $A $S1 $_ $S2 $S) 
    (diffv $S1 $S2 $S))
; 

  (= 
    (diffv-3 > $A $S1 $_ $S2 $S) 
    (diffv-2 $S2 $A $S1 $S))
; 


;
; 


  (= 
    (unionv  () $S2 $S2) True)
; 

  (= 
    (unionv 
      (Cons  $A $S1) $S2 $S) 
    (unionv-2 $S2 $A $S1 $S))
; 



  (= 
    (unionv_2  () $A $S1 
      (Cons  $A $S1)) True)
; 

  (= 
    (unionv-2 
      (Cons  $B $S2) $A $S1 $S) 
    ( (compare $Order $A $B) (unionv-3 $Order $A $S1 $B $S2 $S)))
; 



  (= 
    (unionv-3 < $A $S1 $B $S2 
      (Cons  $A $S)) 
    (unionv-2 $S1 $B $S2 $S))
; 

  (= 
    (unionv-3 = $A $S1 $_ $S2 
      (Cons  $A $S)) 
    (unionv $S1 $S2 $S))
; 

  (= 
    (unionv-3 > $A $S1 $B $S2 
      (Cons  $B $S)) 
    (unionv-2 $S2 $A $S1 $S))
; 


;
; 


  (= 
    (subsetv  () $_) True)
; 

  (= 
    (subsetv 
      (Cons  $A $S1) 
      (Cons  $B $S2)) 
    ( (compare $Order $A $B) (subsetv-2 $Order $A $S1 $S2)))
; 



  (= 
    (subsetv-2 = $_ $S1 $S2) 
    (subsetv $S1 $S2))
; 

  (= 
    (subsetv-2 > $A $S1 $S2) 
    (subsetv 
      (Cons  $A $S1) $S2))
; 


;
; 


  (= 
    (small_subsetv  () $_) True)
; 

  (= 
    (small-subsetv 
      (Cons  $A $S1) $S2) 
    ( (inv $A $S2) (small-subsetv $S1 $S2)))
; 


;
; 


  (= 
    (inv $A 
      (Cons  $B $S)) 
    ( (compare $Order $A $B) (inv-2 $Order $A $S)))
; 



  (= 
    (inv_2  = $_ $_) True)
; 

  (= 
    (inv-2 > $A $S) 
    (inv $A $S))
; 


;
; 


  (= 
    (notinv $A $S) 
    (notinv-2 $S $A))
; 



  (= 
    (notinv_2  () $_) True)
; 

  (= 
    (notinv-2 
      (Cons  $B $S) $A) 
    ( (compare $Order $A $B) (notinv-3 $Order $A $S)))
; 



  (= 
    (notinv_3  < $_ $_) True)
; 

  (= 
    (notinv-3 > $A $S) 
    (notinv-2 $S $A))
; 


;
; 


