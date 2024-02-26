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
    (s-to-d-graph Nil Nil) 
    (set-det))
; 

  (= 
    (s-to-d-graph 
      (Cons  
        (- $Vertex $Neibs1) $G) 
      (Cons  
        (- $Vertex $Neibs2) $D)) 
    ( (init-dists $Neibs1 $Neibs2) (s-to-d-graph $G $D)))
; 



  (= 
    (init_dists  () ()) True)
; 

  (= 
    (init-dists 
      (Cons  $H $T) 
      (Cons  
        (- $H 1) $Ts)) 
    (init-dists $T $Ts))
; 

	

  (= 
    (d-to-s-graph Nil Nil) 
    (set-det))
; 

  (= 
    (d-to-s-graph 
      (Cons  
        (- $Vertex $Neibs1) $G) 
      (Cons  
        (- $Vertex $Neibs2) $D)) 
    ( (strip-dists $Neibs1 $Neibs2) (d-to-s-graph $G $D)))
; 



  (= 
    (strip_dists  () ()) True)
; 

  (= 
    (strip-dists 
      (Cons  
        (- $H $_) $T) 
      (Cons  $H $Ts)) 
    (strip-dists $T $Ts))
; 




  (= 
    (s-floyd $S_Graph $Closure) 
    ( (s-to-d-graph $S_Graph $Graph) (floyd $Graph $Closure)))
; 


;
; 


  (= 
    (zero-self-d-graph Nil Nil) 
    (set-det))
; 

  (= 
    (zero-self-d-graph 
      (Cons  
        (- $Vertex $Neibs1) $G) 
      (Cons  
        (- $Vertex $Neibs2) $D)) 
    ( (ord-min-union 
        (:: (- $Vertex 0)) $Neibs1 $Neibs2) (zero-self-d-graph $G $D)))
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
    (floyd $Graph $Closure) 
    ( (zero-self-d-graph $Graph $Init) (floyd $Init $Init $Closure)))
; 


  (= 
    (floyd Nil $Closure $Closure) 
    (set-det))
; 

  (= 
    (floyd 
      (Cons  
        (- $V $_) $G) $E $Closure) 
    ( (memberchk 
        (- $V $Y) $E) 
      (floyd $E $V $Y $NewE) 
      (floyd $G $NewE $Closure)))
; 


  (= 
    (floyd 
      (Cons  
        (- $X $Neibs) $G) $V $Y 
      (Cons  
        (- $X $NewNeibs) $NewG)) 
    ( (memberchk 
        (- $V $VDist) $Neibs) 
      (set-det) 
      (increment-dists $Y $VDist $YInc) 
      (ord-min-union $Neibs $YInc $NewNeibs) 
      (floyd $G $V $Y $NewG)))
; 

  (= 
    (floyd 
      (Cons  
        (- $X $Neibs) $G) $V $Y 
      (Cons  
        (- $X $Neibs) $NewG)) 
    ( (set-det) (floyd $G $V $Y $NewG)))
; 

  (= 
    (floyd  () $_ $_ ()) True)
; 



  (= 
    (increment_dists  () $_ ()) True)
; 

  (= 
    (increment-dists 
      (Cons  
        (- $H $D) $T) $N 
      (Cons  
        (- $H $D1) $T1)) 
    ( (is $D1 
        (+ $D $N)) (increment-dists $T $N $T1)))
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
    (ord_min_union  () $Set2 $Set2) True)
; 

  (= 
    (ord-min-union 
      (Cons  $Head1 $Tail1) $Set2 $Union) 
    (ord-min-union-1 $Set2 $Head1 $Tail1 $Union))
; 



  (= 
    (ord_min_union_1  () $Head1 $Tail1 
      (Cons  $Head1 $Tail1)) True)
; 

  (= 
    (ord-min-union-1 
      (Cons  $Head2 $Tail2) $Head1 $Tail1 $Union) 
    ( (weighted-compare $Order $Head1 $Head2) (ord-min-union-1 $Order $Head1 $Tail1 $Head2 $Tail2 $Union)))
; 


  (= 
    (ord-min-union-1 < $Head1 $Tail1 $Head2 $Tail2 
      (Cons  $Head1 $Union)) 
    (ord-min-union-1 $Tail1 $Head2 $Tail2 $Union))
; 

  (= 
    (ord-min-union-1 > $Head1 $Tail1 $Head2 $Tail2 
      (Cons  $Head2 $Union)) 
    (ord-min-union-1 $Tail2 $Head1 $Tail1 $Union))
; 

  (= 
    (ord-min-union-1 = 
      (- $Head1 $N1) $Tail1 
      (- $Head1 $N2) $Tail2 
      (Cons  
        (- $Head1 $N) $Union)) 
    ( (min $N1 $N2 $N) (ord-min-union $Tail1 $Tail2 $Union)))
; 



  (= 
    (weighted-compare $Order 
      (- $Head1 $N1) 
      (- $Head2 $N2)) 
    (compare $Order $Head1 $Head2))
; 





  (= 
    (time-floyd $P $N) 
    ( (random-graph $P $N $G) 
      (s-to-d-graph $G $Z) 
      (runtime (floyd $Z $ZLast))))
; 



