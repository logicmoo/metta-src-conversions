;                                           (convert_to_metta_file  ex3 $_203698 miles/examples/ex3.pl miles/examples/ex3.metta)
;
; grammar rules


  (= (s (:: hans trifft susi) Nil)  
    (pn 
      (:: hans trifft susi) 
      (:: trifft susi))
    (v-t 
      (:: trifft susi) 
      (:: susi))
    (pn 
      (:: susi) Nil))

  (= (s (:: martha schlaeft) Nil)  
    (pn 
      (:: martha schlaeft) 
      (:: schlaeft))
    (v-i 
      (:: schlaeft) Nil))



  (= (vp $A $B)  
    (v-t $A $C)
    (np $C $B))
  (= (vp (:: sieht den mann) Nil)  
    (v-t 
      (:: sieht den mann) 
      (:: den mann))
    (det 
      (:: den mann) 
      (:: mann))
    (n 
      (:: mann) Nil))
  (= (vp (:: hilft karl) Nil)  
    (v-t 
      (:: hilft karl) 
      (:: karl))
    (pn 
      (:: karl) Nil))


  (= (min1 $D (Cons  (s $D) $E))  
    (min1 $D $E))
  (= (min1 $F (Cons  (s (s $F)) $G))  
    (min1 $F $G))

; 
; 
; | ?- clear_kb,do_full_kb('examples/ex3.pl').
; 
; Try:
; | ?- clear_kb, init_kb('examples/ex3.pl').
; | ?- intra_construct1(1,2,A,B,C),show_clauses([1,2,A,B,C]).
; | ?- g2_op(1,2,A,B,C). ;; stellt Fragen
; | ?- show_kb.
; | ?- identify(4,3,J), show_clause(J).
; | ?- identify(5,I,J), show_clause(J).
; | ?- apply_g2([4,5,10],A,BB),show_kb.
