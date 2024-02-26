;                                           (convert_to_metta_file  ccs_utils $_453856 sre_dna/ccs_utils.pl sre_dna/ccs_utils.metta)

  (= (trace-append $R $S $T)  
    (tappend $R $S $T))
  (= (trace-append $R $S $T)  
    (tappend $S $R $T))


  (= (tappend  () $X $X)  True)
  (= (tappend (Cons  $A $R) (Cons  $B $S) $T)  
    (opposite $A $B)
    (set-det)
    (trace-append $R $S $T))
  (= (tappend (Cons  $A $R) (Cons  $B $S) (Cons  $A $T))  
    (trace-append $R 
      (Cons  $B $S) $T))


  (= (opposite  $X (-  $X))  True)
  (= (opposite  (-  $X) $X)  True)



  (= (stripped (- $A) $A)  
    (set-det))
  (= (stripped  $A $A)  True)

