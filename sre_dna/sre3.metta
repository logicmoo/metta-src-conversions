
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
    (<:>  
      (::=  expr 
        (^^  iter_expr $A)) 
      (,  
        (::-  
          (construct  $E) 
          (^^  $A 
            (construct  $E))) 
        (,  
          (::-  
            (raw_generate  $S $SL1 $SL2) 
            (^^  $A 
              (raw_generate  $S $SL1 $SL2))) 
          (::-  
            (recognize  $S $S2 $PrSoFar $Pr) 
            (,  
              (check_prob  $PrSoFar) 
              (^^  $A 
                (recognize  $S $S2 $PrSoFar $Pr))))))) True)
; 


  (= 
    (<:>  
      (::=  expr 
        (^^  noniter_expr $A)) 
      (,  
        (::-  
          (construct  $E) 
          (^^  $A 
            (construct  $E))) 
        (,  
          (::-  
            (raw_generate  $S $SL1 $SL2) 
            (^^  $A 
              (raw_generate  $S $SL1 $SL2))) 
          (::-  
            (recognize  $S $S2 $PrSoFar $Pr) 
            (,  
              (check_prob  $PrSoFar) 
              (^^  $A 
                (recognize  $S $S2 $PrSoFar $Pr))))))) True)
; 


;
; 


  (= 
    (<:>  
      (::=  noniter_expr 
        (a)) 
      (,  
        (construct  a) 
        (,  
          (::-  
            (raw_generate  
              (a) $SL1 $SL2) 
            (is  $SL2 
              (+  $SL1 1))) 
          (::-  
            (recognize  
              (Cons  a $T) $T $PrSoFar $PrSoFar) 
            (check_prob  $PrSoFar))))) True)
; 


  (= 
    (<:>  
      (::=  noniter_expr 
        (b)) 
      (,  
        (construct  b) 
        (,  
          (::-  
            (raw_generate  
              (b) $SL1 $SL2) 
            (is  $SL2 
              (+  $SL1 1))) 
          (::-  
            (recognize  
              (Cons  b $T) $T $PrSoFar $PrSoFar) 
            (check_prob  $PrSoFar))))) True)
; 


;
; 


  (= 
    (<:>  
      (::=  noniter_expr 
        (,  
          (^^  guardedexpr_a $A1) 
          (,  
            (^^  intval $B1) 
            (,  
              (^^  guardedexpr_b $A2) 
              (^^  intval $B2))))) 
      (,  
        (::-  
          (construct  
            ( (,  $E1 $N1) (,  $E2 $N2))) 
          (,  
            (^^  $A1 
              (construct  $E1)) 
            (,  
              (^^  $B1 
                (construct  $N1)) 
              (,  
                (^^  $A2 
                  (construct  $E2)) 
                (^^  $B2 
                  (construct  $N2)))))) 
        (,  
          (::-  
            (raw_generate  $S $SL1 $SL2) 
            (,  
              (^^  $B1 
                (construct  $N1)) 
              (,  
                (^^  $B2 
                  (construct  $N2)) 
                (;  
                  (->  
                    (raw_select_term  
                      ($N1 $N2) 1) 
                    (^^  $A1 
                      (raw_generate  $S $SL1 $SL2))) 
                  (^^  $A2 
                    (raw_generate  $S $SL1 $SL2)))))) 
          (,  
            (::-  
              (recognize  $S $S2 $PrSoFar $Pr) 
              (,  
                (^^  $B1 
                  (construct  $Val1)) 
                (,  
                  (^^  $B2 
                    (construct  $Val2)) 
                  (,  
                    (is  $Pr2 
                      (*  $PrSoFar 
                        (/  $Val1 
                          (+  $Val1 $Val2)))) 
                    (,  
                      (check_prob  $Pr2) 
                      (^^  $A1 
                        (recognize  $S $S2 $Pr2 $Pr))))))) 
            (::-  
              (recognize  $S $S2 $PrSoFar $Pr) 
              (,  
                (^^  $B1 
                  (construct  $Val1)) 
                (,  
                  (^^  $B2 
                    (construct  $Val2)) 
                  (,  
                    (is  $Pr2 
                      (*  $PrSoFar 
                        (/  $Val2 
                          (+  $Val1 $Val2)))) 
                    (,  
                      (check_prob  $Pr2) 
                      (^^  $A2 
                        (recognize  $S $S2 $Pr2 $Pr))))))))))) True)
; 


;
; 


  (= 
    (<:>  
      (::=  noniter_expr 
        (,  
          (^^  expr $A) 
          (^^  expr $B))) 
      (,  
        (::-  
          (construct  
            (:  $E $F)) 
          (,  
            (^^  $A 
              (construct  $E)) 
            (^^  $B 
              (construct  $F)))) 
        (,  
          (::-  
            (raw_generate  $S $SL1 $SL2) 
            (,  
              (^^  $A 
                (raw_generate  $S1 $SL1 $SL3)) 
              (,  
                (^^  $B 
                  (raw_generate  $S2 $SL3 $SL2)) 
                (append  $S1 $S2 $S)))) 
          (::-  
            (recognize  $S $S2 $PrSoFar $Pr) 
            (,  
              (check_prob  $PrSoFar) 
              (,  
                (^^  $A 
                  (recognize  $S $S3 $PrSoFar $Pr1)) 
                (,  
                  (check_prob  $Pr1) 
                  (^^  $B 
                    (recognize  $S3 $S2 $Pr1 $Pr))))))))) True)
; 


;
; 


  (= 
    (<:>  
      (::=  iter_expr 
        (,  
          (^^  noniter_expr $A) 
          (^^  probval $B))) 
      (,  
        (::-  
          (construct  
            (*  $E $P)) 
          (,  
            (^^  $A 
              (construct  $E)) 
            (^^  $B 
              (construct  $P)))) 
        (,  
          (::-  
            (raw_generate  $S $SL1 $SL2) 
            (,  
              (^^  $B 
                (construct  $P)) 
              (,  
                (max_string_length_P  $MaxL) 
                (raw_gen_loop  $A $P $MaxL $S $SL1 $SL2)))) 
          (::-  
            (recognize  $S $S2 $PrSoFar $Pr) 
            (,  
              (check_prob  $PrSoFar) 
              (,  
                (^^  $B 
                  (construct  $Pr1)) 
                (recognize_loop  $A $Pr1 $S $S2 $PrSoFar $Pr))))))) True)
; 


;
; 


  (= 
    (<:>  
      (::=  iter_expr 
        (,  
          (^^  noniter_expr $A) 
          (^^  probval $B))) 
      (,  
        (::-  
          (construct  
            (+  $E $P)) 
          (,  
            (^^  $A 
              (construct  $E)) 
            (^^  $B 
              (construct  $P)))) 
        (,  
          (::-  
            (raw_generate  $S $SL1 $SL2) 
            (,  
              (^^  $A 
                (raw_generate  $S1 $SL1 $SL3)) 
              (,  
                (^^  $B 
                  (construct  $P)) 
                (,  
                  (max_string_length_P  $MaxL) 
                  (,  
                    (raw_gen_loop  $A $P $MaxL $S2 $SL3 $SL2) 
                    (,  
                      (append  $S1 $S2 $S) !)))))) 
          (::-  
            (recognize  $S $S2 $PrSoFar $Pr) 
            (,  
              (check_prob  $PrSoFar) 
              (,  
                (^^  $A 
                  (recognize  $S $S3 $PrSoFar $Pr1)) 
                (,  
                  (\+  
                    (= $S $S3)) 
                  (,  
                    (check_prob  $Pr1) 
                    (,  
                      (^^  $B 
                        (construct  $Pr2)) 
                      (recognize_loop  $A $Pr2 $S3 $S2 $Pr1 $Pr)))))))))) True)
; 


;
; 


  (= 
    (<:>  
      (::=  guardedexpr_a 
        (a)) 
      (,  
        (construct  a) 
        (,  
          (::-  
            (raw_generate  
              (a) $SL1 $SL2) 
            (is  $SL2 
              (+  $SL1 1))) 
          (::-  
            (recognize  
              (Cons  a $T) $T $PrSoFar $PrSoFar) 
            (check_prob  $PrSoFar))))) True)
; 


  (= 
    (<:>  
      (::=  guardedexpr_a 
        (,  
          (a) 
          (^^  expr $A))) 
      (,  
        (::-  
          (construct  
            (:  a $E)) 
          (^^  $A 
            (construct  $E))) 
        (,  
          (::-  
            (raw_generate  
              (Cons  a $S) $SL1 $SL2) 
            (,  
              (^^  $A 
                (raw_generate  $S $SL1 $SL3)) 
              (is  $SL2 
                (+  $SL3 1)))) 
          (::-  
            (recognize  
              (Cons  a $S) $S2 $PrSoFar $Pr) 
            (,  
              (check_prob  $PrSoFar) 
              (^^  $A 
                (recognize  $S $S2 $PrSoFar $Pr))))))) True)
; 


  (= 
    (<:>  
      (::=  guardedexpr_b 
        (b)) 
      (,  
        (construct  b) 
        (,  
          (::-  
            (raw_generate  
              (b) $SL1 $SL2) 
            (is  $SL2 
              (+  $SL1 1))) 
          (::-  
            (recognize  
              (Cons  b $T) $T $PrSoFar $PrSoFar) 
            (check_prob  $PrSoFar))))) True)
; 


  (= 
    (<:>  
      (::=  guardedexpr_b 
        (,  
          (b) 
          (^^  expr $A))) 
      (,  
        (::-  
          (construct  
            (:  b $E)) 
          (^^  $A 
            (construct  $E))) 
        (,  
          (::-  
            (raw_generate  
              (Cons  b $S) $SL1 $SL2) 
            (,  
              (^^  $A 
                (raw_generate  $S $SL1 $SL3)) 
              (is  $SL2 
                (+  $SL3 1)))) 
          (::-  
            (recognize  
              (Cons  b $S) $S2 $PrSoFar $Pr) 
            (,  
              (check_prob  $PrSoFar) 
              (^^  $A 
                (recognize  $S $S2 $PrSoFar $Pr))))))) True)
; 



;
; 


  (= 
    (<:>  
      (::=  intval 
        (,  
          ($N) 
          { (is_an_integer  $N) })) 
      (construct  $N)) True)
; 


  (= 
    (<:>  
      (::=  probval 
        (,  
          ($R) 
          { (is_a_probability  $R) })) 
      (construct  $R)) True)
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
    (is-an-integer $N) 
    ( (integer $N) (set-det)))
; 

  (= 
    (is-an-integer $N) 
    ( (int-range $Low $High) (random $Low $High $N)))
; 



  (= 
    (int_range  0 1000) True)
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
    (is-a-probability $R) 
    ( (float $R) (set-det)))
; 

  (= 
    (is-a-probability $R) 
    ( (random $T) (is $R (/ (truncate (* $T 100)) 100))))
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
    (raw-select-term $L $K) 
    ( (sumlist $L $SL 0 $Sum) 
      (random 0 $Sum $X) 
      (select-kth-term $SL $X 1 $K $_) 
      (set-det)))
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
    (sumlist  () () $Sum $Sum) True)
; 

  (= 
    (sumlist 
      (Cons  $N $R) 
      (Cons  $NewSum $SumList2) $LastSum $Sum) 
    ( (is $NewSum 
        (+ $LastSum $N)) (sumlist $R $SumList2 $NewSum $Sum)))
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
    (select-kth-term 
      (:: $Val) $_ $K $K $Val) 
    (set-det))
; 
  
  (= 
    (select-kth-term 
      (Cons  $Val $_) $X $K $K $Val) 
    ( (>= $Val $X) (set-det)))
; 

  (= 
    (select-kth-term 
      (Cons  $_ $R) $X $K $K2 $Val) 
    ( (is $K3 
        (+ $K 1)) (select-kth-term $R $X $K3 $K2 $Val)))
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
    (raw-gen-loop $Tree $Pr $MaxL $S $SL1 $SL2) 
    ( (< $SL1 $MaxL) 
      (maybe $Pr) 
      (^^ $Tree 
        (raw-generate $S1 $SL1 $SL3)) 
      (raw-gen-loop $Tree $Pr $MaxL $S2 $SL3 $SL2) 
      (append $S1 $S2 $S) 
      (set-det)))
; 

  (= 
    (raw-gen-loop $Tree $Pr $_ Nil $SL $SL) 
    (set-det))
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
    (recognize-loop $_ $Pr Nil Nil $PrSoFar $FinalPr) 
    ( (set-det) 
      (is $FinalPr 
        (* $PrSoFar 
          (- 1.0 $Pr))) 
      (check-prob $FinalPr)))
; 

  (= 
    (recognize-loop $T $Pr $S $S $PrSoFar $FinalPr) 
    ( (is $FinalPr 
        (* $PrSoFar 
          (- 1.0 $Pr))) (check-prob $FinalPr)))
; 

  (= 
    (recognize-loop $Tree $Pr $S $S2 $PrSoFar $FinalPr) 
    ( (is $Pr3 
        (* $PrSoFar $Pr)) 
      (check-prob $Pr3) 
      (^^ $Tree 
        (recognize $S $S3 $Pr3 $Pr1)) 
      (not (= $S $S3)) 
      (check-prob $Pr1) 
      (recognize-loop $Tree $Pr $S3 $S2 $Pr1 $FinalPr)))
; 



  (= 
    (check-prob $P) 
    ( (min-grammar-prob-P $E) 
      (> $P $E) 
      (set-det)))
; 


;
; 

;
; 



  (= 
    (sre $Type $Expr $String $SL) 
    ( (repeat) 
      (or 
        (= $Type full) 
        (= $Type grow)) 
      (generate-tree expr $Type 12 $_ $Tree $_) 
      (^^ $Tree 
        (construct $Expr)) 
      (^^ $Tree 
        (raw-generate $String 0 $SL)) 
      (nl) 
      (sre-pp $Expr) 
      (nl) 
      (write 'tree ') 
      (write $Tree) 
      (nl) 
      (tree-depth $Tree $Depth) 
      (write 'Depth = ') 
      (write $Depth) 
      (nl)))
; 



  (= 
    (sre2 $Type $Expr $Input) 
    ( (repeat) 
      (or 
        (= $Type full) 
        (= $Type grow)) 
      (generate-tree expr $Type 12 $_ $Tree $_) 
      (^^ $Tree 
        (construct $Expr)) 
      (nl) 
      (write $Type) 
      (nl) 
      (sre-pp $Expr) 
      (nl) 
      (bagof 
        (, $Leftover $Pr) 
        (^^ $Tree 
          (recognize $Input $Leftover 1.0 $Pr)) $Rlist) 
      (write 'Recog list: ') 
      (nl) 
      (writelist $Rlist) 
      (nl)))
; 



  (= 
    (sre2c $Type $Expr $Input) 
    ( (repeat) 
      (or 
        (= $Type full) 
        (= $Type grow)) 
      (generate-tree expr $Type 12 $_ $Tree $_) 
      (^^ $Tree 
        (construct $Expr)) 
      (nl) 
      (write $Type) 
      (nl) 
      (sre-pp $Expr) 
      (nl) 
      (bagof $Pr 
        (^^ $Tree 
          (recognize $Input Nil 1.0 $Pr)) $Prlist) 
      (write 'Pr list: ') 
      (nl) 
      (writelist $Prlist) 
      (nl)))
; 



  (= 
    (sre2b $Input) 
    ( (generate-tree expr grow 8 $_ $Tree $_) 
      (^^ $Tree 
        (construct $Expr)) 
      (^^ $Tree 
        (recognize $Input $Leftover 1.0 $Pr)) 
      (nl) 
      (sre-pp $Expr) 
      (nl) 
      (write 'Prob = ') 
      (write $Pr) 
      (nl) 
      (write 'Leftover = ') 
      (write $Leftover) 
      (nl)))
; 


;
; 

;
; 



  (= 
    (sre-pp (* $E $R)) 
    ( (write () 
      (sre-pp $E) 
      (write )*) 
      (write $R) 
      (set-det)))
; 

  (= 
    (sre-pp (+ $E $R)) 
    ( (write () 
      (sre-pp $E) 
      (write )+) 
      (write $R) 
      (set-det)))
; 

  (= 
    (sre-pp (with_self  $E $F)) 
    ( (sre-pp $E) 
      (write :) 
      (sre-pp $F) 
      (set-det)))
; 

  (= 
    (sre-pp (Cons  $A $T)) 
    ( (sre-pp-l (Cons  $A $T)) (set-det)))
; 

  (= 
    (sre-pp (, $A $B)) 
    ( (write () 
      (sre-pp $A) 
      (write ,) 
      (write $B) 
      (write )) 
      (set-det)))
; 

  (= 
    (sre-pp $X) 
    (write $X))
; 



  (= 
    (sre-pp-l (:: $A)) 
    ( (sre-pp $A) (set-det)))
; 

  (= 
    (sre-pp-l (Cons  $A $T)) 
    ( (write [) 
      (sre-pp $A) 
      (write +) 
      (sre-pp-l $T) 
      (write ]) 
      (set-det)))
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

