
  !(dynamic (/ :: 2))
; 


  !(op 150 yfx ::)
; 

  !(op 145 xfx <-)
; 

  !(op 140 xfy &)
; 

  !(op 135 xfx :=)
; 



  (= 
    (derivation 
      (<- $P $C) $TYP) 
    ( (name a1 $NAME) 
      (add-context $NAME $C) 
      (generate-goal-ids $P $ID1 1 $I1) 
      (expand-derivation $P $P2 $ID1 $ID2 
        (:: $I1 $ID1) 
        (:: $I2 $DERIVATION)) 
      (add-symbol  &self 
        (::  $TYP $DERIVATION)) 
      (write (:: $TYP $DERIVATION)) 
      (nl) 
      (fail)))
; 

  (= 
    (derivation $_ $_) 
    ( (name a1 
        (:: $N1 $N2)) (del-context (:: $N1 $_))))
; 



  (= 
    (add-context 
      (:: $N1 $N2) 
      (& $P1 $P2)) 
    ( (set-det) 
      (name $C 
        (:: $N1 $N2)) 
      (add-symbol  &self 
        (::  $C 
          (<-  $P1 true))) 
      (is $N3 
        (+ $N2 1)) 
      (add-context 
        (:: $N1 $N3) $P2)))
; 

  (= 
    (add-context 
      (:: $N1 $N2) $P1) 
    ( (name $C 
        (:: $N1 $N2)) (add-symbol  &self (::  $C (<-  $P1 true)))))
; 



  (= 
    (del-context (:: $N1 $N2)) 
    ( (:: $C 
        (<- $P1 True)) 
      (name $C 
        (:: $N1 $_)) 
      (remove-symbol  &self 
        (::  $C 
          (<-  $P1 true))) 
      (fail)))
; 

  (= 
    (del_context  $_) True)
; 



  (= 
    (generate-goal-ids 
      (& $P1 $P2) 
      (& $I1 $I2) $I1 $I4) 
    ( (set-det) 
      (is $I3 
        (+ $I1 1)) 
      (generate-goal-ids $P2 $I2 $I3 $I4)))
; 

  (= 
    (generate-goal-ids $P1 $I1 $I1 $I4) 
    (is $I4 
      (+ $I1 1)))
; 



  (= 
    (expand-derivation True True $ID1 $ID1 $D1 $D1) 
    (set-det))
; 

  (= 
    (expand-derivation 
      (& True $P3) $P3 
      (& $ID1 $ID3) $ID3 $D1 $D1) 
    (set-det))
; 

  (= 
    (expand-derivation 
      (& $P1 $P3) $P5 
      (& $ID1 $ID3) $ID5 $D1 $D3) 
    ( (expand-derivation- $P1 $P2 $ID1 $ID2 $D1 $D2) 
      (join-goals 
        (& $P2 $P3) $P4 
        (& $ID2 $ID3) $ID4) 
      (expand-derivation $P4 $P5 $ID4 $ID5 $D2 $D3)))
; 

;                                 (error
;                                   (syntax_error  operator_expected)
;                                   (file  discr/discr.pl 201 8 11767))



  (= 
    (expand-derivation- $P1 $P2 $ID1 $ID2 
      (:: $I1 $D1) 
      (:: $I2 $D2)) 
    ( (:: $C 
        (<- $P1 $P2)) 
      (generate-goal-ids $P2 $ID2 $I1 $I2) 
      (= $D2 
        (:: 
          (:: $D1 $C) 
          (<- $ID1 $ID2)))))
; 



  (= 
    (join-goals 
      (& $P1 
        (& $P2 $P3)) 
      (& $P1 $P5) 
      (& $ID1 
        (& $ID3 $ID3)) 
      (& $ID1 $ID5)) 
    ( (set-det) (join-goals (& $P2 $P3) $P5 (& $ID2 $ID3) $ID5)))
; 

  (= 
    (join-goals 
      (& True $P3) $P3 
      (& $ID1 $ID3) $ID3) 
    (set-det))
; 

  (= 
    (join_goals  $P1 $P1 $ID1 $ID1) True)
; 


;                                    (error
;                                      (syntax_error  operator_expected)
;                                      (file  discr/discr.pl 248 8 14466))



  (= 
    (spec $T1 $T2) 
    ( (ground $T2 1 $_) (= $T1 $T2)))
; 



  (= 
    (ground 
      (skolem-function $N1) $N1 $N2) 
    ( (set-det) (is $N2 (+ $N1 1))))
; 

  (= 
    (ground $T $N1 $N2) 
    ( (=.. $T 
        (Cons  $_ $TS)) 
      (== $TS Nil) 
      (set-det)))
; 

  (= 
    (ground $T $N1 $N2) 
    ( (=.. $T 
        (Cons  $_ $TS)) (grounds $TS $N1 $N2)))
; 



  (= 
    (grounds 
      (Cons  $T $TS) $N1 $N3) 
    ( (ground $T $N1 $N2) (grounds $TS $N1 $N2)))
; 

  (= 
    (grounds  () $N1 $N1) True)
; 



  (= 
    (generate-discriminants $P $PA $PR) 
    ( (generate-goal-ids $P $ID 1 $_) 
      (determine-discriminant 
        (:: 
          (:: 
            (:: $P $ID) $P) $ID) 
        (:: 
          (:: 
            (:: $PA $IA) $PR) $IR)) 
      (add-symbol  &self 
        (::  disc $PA)) 
      (write (:: disc $PA)) 
      (nl) 
      (fail)))
; 

  (= 
    (generate_discriminants  $_ $_ $_) True)
; 


;                                         (error
;                                           (syntax_error  operator_expected)
;                                           (file  discr/discr.pl 293 8 16547))


  (= 
    (determine-discriminant 
      (:: 
        (:: 
          (:: 
            (& True $PA3) 
            (& $_ $IA3)) 
          (& True $PR3)) 
        (& $_ $IR3)) $P3) 
    ( (set-det) (= $P3 (:: (:: (:: $PA3 $IA3) $PR3) $IR3))))
; 

  (= 
    (determine-discriminant 
      (:: 
        (:: 
          (:: 
            (& $PA1 $PA3) 
            (& $IA1 $IA3)) 
          (& $PR1 $PR3)) 
        (& $IR1 $IR3)) $P3) 
    ( (determine-discriminant 
        (:: 
          (:: 
            (:: $PA1 $IA1) $PR1) $IR1) 
        (:: 
          (:: 
            (:: $PA2 $IA2) $PR2) $IR2)) 
      (join-goals 
        (& $PA2 $PA3) $PA5 
        (& $IA2 $IA3) $IA5) 
      (join-goals 
        (& $PR2 $PR3) $PR5 
        (& $IR2 $IR3) $IR5) 
      (= $P3 
        (:: 
          (:: 
            (:: $PA5 $IA5) $PR5) $IR5))))
; 

  (= 
    (determine-discriminant 
      (:: 
        (:: 
          (:: 
            (& $PA1 $PA3) 
            (& $IA1 $IA3)) 
          (& $PR1 $PR3)) 
        (& $IR1 $IR3)) $P3) 
    ( (determine-discriminant 
        (:: 
          (:: 
            (:: $PA3 $IA3) $PR3) $IR3) 
        (:: 
          (:: 
            (:: $PA4 $IA4) $PR4) $IR4)) (= $P3 (:: (:: (:: (& $PA1 $PA4) (& $IA1 $IA4)) (& $PR1 $PR4)) (& $IR1 $PR4)))))
; 

;                                          (error
;                                            (syntax_error  operator_expected)
;                                            (file  discr/discr.pl 305 8 17158))



  (= 
    (determine-discriminant- $P1 $P3) 
    ( (= $P1 
        (:: 
          (:: 
            (:: $PA1 $IA1) $PR1) $IR1)) 
      (:: $CA 
        (<- $PA1 $PA2)) 
      (:: app $DA) 
      (in-derivation-p 
        (:: $CA 
          (<- $IA1 $IA2)) $DA) 
      (:: $CR 
        (<- $PR1 $PR2)) 
      (:: rej $DR) 
      (in-derivation-p 
        (:: $CR 
          (<- $IR1 $IR2)) $DR) 
      (= $P3 
        (:: 
          (:: 
            (:: $PA2 $IA2) $PR2) $IR2))))
; 



  (= 
    (in-derivation-p 
      (:: $X $C) 
      (:: 
        (:: $DER $X) $C)) 
    (set-det))
; 

  (= 
    (in-derivation-p 
      (:: $X $C) 
      (:: $DER $_)) 
    (in-derivation-p 
      (:: $X $C) $DER))
; 



  (= 
    (help) 
    ( (write 'Load data set with command: [Filename].') (nl)))
; 



  !(help *)
; 


