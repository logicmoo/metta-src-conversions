
  (= 
    (test1) 
    ( (substitution 
        (f 
          (g $A) $B) 
        (f 
          (g (h a)) 
          (i b)) $ERG) 
      (write $ERG) 
      (nl)))
; 



  (= 
    (test2) 
    ( (lgg 
        (f 
          (g 3) 3 
          (j 6)) 
        (f 
          (g 2) 2 
          (j (h 6))) $ERG) 
      (write $ERG) 
      (nl)))
; 



  (= 
    (test3a) 
    (covers 
      (:: (mem 3 (:: 4 3))) 
      (:: 
        (= 
          (mem  $A 
            (Cons  $A $_)) True) 
        (= 
          (mem $A 
            (Cons  $_ $B)) 
          (mem $A $B)))))
; 



  (= 
    (test3b) 
    (covers 
      (:: (mem 3 (:: 4 5))) 
      (:: 
        (= 
          (mem  $A 
            (Cons  $A $_)) True) 
        (= 
          (mem $A 
            (Cons  $_ $B)) 
          (mem $A $B)))))
; 



  (= 
    (test4a) 
    (psubsumes 
      (:: 
        (= 
          (mem  $A 
            (Cons  $A $_)) True) 
        (= 
          (mem $B 
            (Cons  $_ $C)) 
          (mem $B $C))) 
      (:: (= (mem $D (Cons  $_ (Cons  $_ $E))) (mem $D $E)))))
; 



  (= 
    (test4b) 
    (psubsumes 
      (:: 
        (= 
          (mem  $A 
            (Cons  $A $_)) True) 
        (= 
          (mem $B 
            (Cons  $_ $C)) 
          (mem $B $C))) 
      (:: 
        (= 
          (mem $D 
            (Cons  $_ 
              (Cons  $_ $E))) 
          (mem $D $E)) 
        (= 
          (mem $D 
            (Cons  $_ 
              (Cons  $_ 
                (Cons  $_ $E)))) 
          (mem $D $E)))))
; 



  (= 
    (test4c) 
    (psubsumes 
      (:: 
        (= 
          (mem  $A 
            (Cons  $A $_)) True) 
        (= 
          (mem $B 
            (Cons  $_ $C)) 
          (mem $B $C))) 
      (:: (= (mem $D (Cons  $_ (Cons  $_ $E))) (mem $X $E)))))
; 



  (= 
    (test4d) 
    (psubsumes 
      (:: (= (mem $B (Cons  $_ $C)) (mem $B $C))) 
      (:: (= (mem $D (Cons  $_ (Cons  $_ $E))) (mem $D $E)))))
; 



  (= 
    (test5) 
    (p-subsumes 
      (:: (= (cuddly-pet $X) ((small $X) (fluffy $X) (pet $X)))) 
      (:: 
        (= 
          (pet $X) 
          (cat $X)) 
        (= 
          (pet $X) 
          (dog $X)) 
        (= 
          (small $X) 
          (cat $X))) 
      (:: 
        (= 
          (cuddly-pet $X) 
          ( (small $X) 
            (fluffy $X) 
            (dog $X))) 
        (= 
          (cuddly-pet $X) 
          ( (fluffy $X) (cat $X))))))
; 



