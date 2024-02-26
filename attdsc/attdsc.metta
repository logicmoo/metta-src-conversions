
  !(op 300 xfx <==)
; 


  !(dynamic (/ <== 2))
; 



  (= 
    (learn $Class) 
    ( (bagof 
        (example $ClassX $Obj) 
        (example $ClassX $Obj) $Examples) 
      (learn $Examples $Class $Description) 
      (nl) 
      (write $Class) 
      (write <==) 
      (nl) 
      (writelist $Description) 
      (add-symbol  &self 
        (<==  $Class $Description))))
; 


  (= 
    (learn $Examples $Class Nil) 
    (not (member (example $Class $_) $Examples)))
; 

  (= 
    (learn $Examples $Class 
      (Cons  $Conj $Conjs)) 
    ( (learn-conj $Examples $Class $Conj) 
      (remove $Examples $Conj $RestExamples) 
      (learn $RestExamples $Class $Conjs)))
; 

	

  (= 
    (learn-conj $Examples $Class Nil) 
    ( (not (, (member (example $ClassX $_) $Examples) (not (== $ClassX $Class)))) (set-det)))
; 

  (= 
    (learn-conj $Examples $Class 
      (Cons  $Cond $Conds)) 
    ( (choose-cond $Examples $Class $Cond) 
      (filter $Examples 
        (:: $Cond) $Examples1) 
      (learn-conj $Examples1 $Class $Conds)))
; 

	

  (= 
    (choose-cond $Examples $Class $AttVal) 
    ( (findall 
        (/ $AV $Score) 
        (score $Examples $Class $AV $Score) $AVs) (best $AVs $AttVal)))
; 

	

  (= 
    (best  
      ( (/  $AttVal $_)) $AttVal) True)
; 

  (= 
    (best 
      (Cons  
        (/ $AV0 $S0) 
        (Cons  
          (/ $AV1 $S1) $AVSlist)) $AttVal) 
    (or 
      (, 
        (> $S1 $S0) 
        (set-det) 
        (best 
          (Cons  
            (/ $AV1 $S1) $AVSlist) $AttVal)) 
      (best 
        (Cons  
          (/ $AV0 $S0) $AVSlist) $AttVal)))
; 

	

  (= 
    (score $Examples $Class $AttVal $Score) 
    ( (candidate $Examples $Class $AttVal) 
      (filter $Examples 
        (:: $AttVal) $Examples1) 
      (length $Examples1 $N1) 
      (count-pos $Examples1 $Class $NPos1) 
      (> $NPos1 0) 
      (is $Score 
        (- 
          (* 2 $NPos1) $N1))))
; 

	

  (= 
    (candidate $Examples $Class 
      (= $Att $Val)) 
    ( (get-symbols  &self 
        (= 
          (attribute  $Att $Values) true)) 
      (member $Val $Values) 
      (suitable 
        (= $Att $Val) $Examples $Class)))
; 

	

  (= 
    (suitable $AttVal $Examples $Class) 
    ( (member 
        (example $ClassX $ObjX) $Examples) 
      (not (== $ClassX $Class)) 
      (not (satisfy $ObjX (:: $AttVal))) 
      (set-det)))
; 

	

  (= 
    (count_pos  () $_ 0) True)
; 

  (= 
    (count-pos 
      (Cons  
        (example $ClassX $_) $Examples) $Class $N) 
    ( (count-pos $Examples $Class $N1) (or (, (= $ClassX $Class) (set-det) (is $N (+ $N1 1))) (= $N $N1))))
; 

	

  (= 
    (filter $Examples $Cond $Examples1) 
    (findall 
      (example $Class $Obj) 
      (, 
        (member 
          (example $Class $Obj) $Examples) 
        (satisfy $Obj $Cond)) $Examples1))
; 

		

  (= 
    (remove  () $_ ()) True)
; 

  (= 
    (remove 
      (Cons  
        (example $Class $Obj) $Es) $Conj $Es1) 
    ( (satisfy $Obj $Conj) 
      (set-det) 
      (remove $Es $Conj $Es1)))
; 

  (= 
    (remove 
      (Cons  $E $Es) $Conj 
      (Cons  $E $Es1)) 
    (remove $Es $Conj $Es1))
; 

	

  (= 
    (satisfy $Object $Conj) 
    (not (, (member (= $Att $Val) $Conj) (member (= $Att $ValX) $Object) (not (== $ValX $Val)))))
; 

	    

  (= 
    (match $Object $Description) 
    ( (member $Conj $Description) (satisfy $Object $Conj)))
; 

	

  (= 
    (writelist  ()) True)
; 

  (= 
    (writelist (Cons  $X $L)) 
    ( (tab 2) 
      (write $X) 
      (nl) 
      (writelist $L)))
; 



  (= 
    (help) 
    ( (write 'Load data set and start learning with command: [Filename].') (nl)))
; 



  !(help *)
; 


