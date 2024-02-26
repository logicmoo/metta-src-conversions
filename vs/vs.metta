
  (= 
    (learn) 
    ( (writeln 'First positive example ?') 
      (read $POS_EX) 
      (nl) 
      (initialize $POS_EX $G $S) 
      (versionspace $G $S)))
; 

 

  (= 
    (versionspace Nil $_) 
    ( (writeln 'There is no consistent concept description in this language !') (set-det)))
; 

  (= 
    (versionspace $_ Nil) 
    ( (writeln 'There is no consistent concept description in this language !') (set-det)))
; 

  (= 
    (versionspace 
      (:: $CONCEPT) 
      (:: $CONCEPT)) 
    ( (set-det) 
      (writeln 'The consistent generalization is : ') 
      (writeln $CONCEPT)))
; 

  (= 
    (versionspace $G $S) 
    ( (writeln 'The G-set is : ') 
      (writeln $G) 
      (writeln 'The S-set is : ') 
      (writeln $S) 
      (nl) 
      (writeln 'Next example :') 
      (generate-ex $G $S $NEXT_EX) 
      (set-det) 
      (writeln $NEXT_EX) 
      (writeln 'Classification of the example ? [p/n]') 
      (read $CLASS) 
      (nl) 
      (adjust-versionspace $CLASS $NEXT_EX $G $S $NG $NS) 
      (versionspace $NG $NS)))
; 

  (= 
    (versionspace $G $S) 
    (writeln 'Impossible to generate relevant examples'))
; 

  

  (= 
    (adjust-versionspace p $EX $G $S $NG $NS) 
    ( (retain-g $G $NG $EX) 
      (generalize-s $S $S1 $NG $EX) 
      (prune-s $S1 $NS)))
; 

  (= 
    (adjust-versionspace n $EX $G $S $NG $NS) 
    ( (retain-s $S $NS $EX) 
      (specialize-g $G $G1 $NS $EX) 
      (prune-g $G1 $NG)))
; 

 

  (= 
    (retain-g Nil Nil $_) 
    (set-det))
; 

  (= 
    (retain-g 
      (Cons  $CONCEPT $G) 
      (Cons  $CONCEPT $NG) $EX) 
    ( (covers $CONCEPT $EX) 
      (set-det) 
      (retain-g $G $NG $EX)))
; 

  (= 
    (retain-g 
      (Cons  $CONCEPT $G) $NG $EX) 
    (retain-g $G $NG $EX))
; 

 

  (= 
    (retain-s Nil Nil $_) 
    (set-det))
; 

;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  vs/vs.pl 164 11 8582))

  (= 
    (retain-s 
      (Cons  $CONCEPT $S) $NS $EX) 
    (retain-s $S $NS $EX))
; 

 

  (= 
    (generalize-s $S $NS $NG $EX) 
    (setofnil $NCON 
      (^ $CON 
        (, 
          (member $CON $S) 
          (valid-least-generalization $CON $EX $NCON $NG))) $NS))
; 

 

  (= 
    (specialize-g $G $NG $NS $EX) 
    (setofnil $NCONCEPT 
      (^ $CONCEPT 
        (, 
          (member $CONCEPT $G) 
          (valid-greatest-specialization $CONCEPT $EX $NCONCEPT $NS))) $NG))
; 

 

  (= 
    (valid-least-generalization $CONCEPT $EX $NCONCEPT $NG) 
    ( (least-generalization $CONCEPT $EX $NCONCEPT) 
      (member $GENERAL $NG) 
      (more-general $GENERAL $NCONCEPT)))
; 

 

  (= 
    (valid-greatest-specialization $CONCEPT $EX $NCONCEPT $NS) 
    ( (greatest-specialization $CONCEPT $EX $NCONCEPT) 
      (member $SPECIFIC $NS) 
      (more-general $NCONCEPT $SPECIFIC)))
; 



  (= 
    (prune-s $S $NS) 
    (prune-s-acc $S $S $NS))
; 


  (= 
    (prune-s-acc Nil $_ Nil) 
    (set-det))
; 

  (= 
    (prune-s-acc 
      (Cons  $SPECIFIC $S) $ACC $NS) 
    ( (member $SPECIFIC1 $ACC) 
      (not (== $SPECIFIC1 $SPECIFIC)) 
      (more-general $SPECIFIC $SPECIFIC1) 
      (set-det) 
      (prune-s-acc $S $ACC $NS)))
; 

  (= 
    (prune-s-acc 
      (Cons  $SPECIFIC $S) $ACC 
      (Cons  $SPECIFIC $NS)) 
    (prune-s-acc $S $ACC $NS))
; 

 

  (= 
    (prune-g $G $NG) 
    (prune-g-acc $G $G $NG))
; 


  (= 
    (prune-g-acc Nil $_ Nil) 
    (set-det))
; 

  (= 
    (prune-g-acc 
      (Cons  $GENERAL $G) $ACC $NG) 
    ( (member $GENERAL1 $ACC) 
      (not (== $GENERAL $GENERAL1)) 
      (more-general $GENERAL1 $GENERAL) 
      (set-det) 
      (prune-g-acc $G $ACC $NG)))
; 

  (= 
    (prune-g-acc 
      (Cons  $GENERAL $G) $ACC 
      (Cons  $GENERAL $NG)) 
    (prune-g-acc $G $ACC $NG))
; 

 

  (= 
    (allcovers Nil $_) 
    (set-det))
; 

  (= 
    (allcovers 
      (Cons  $CON $REST) $EX) 
    ( (covers $CON $EX) (allcovers $REST $EX)))
; 

 
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  vs/vs.pl 334 11 18121))


  (= 
    (generate-ex 
      (Cons  $GENERAL $G) $S $EX) 
    (generate-ex $G $S $EX))
; 

 

  (= 
    (find-ex Nil Nil) 
    (set-det))
; 

  (= 
    (find-ex 
      (Cons  $GENERAL $G) 
      (Cons  $LEAF $EX)) 
    ( (isa $LEAF $GENERAL) 
      (leaf $LEAF) 
      (find-ex $G $EX)))
; 

 

  (= 
    (initialize $POS_EX 
      (:: $TOP) 
      (:: $POS_EX)) 
    (max $TOP $POS_EX))
; 

 

  (= 
    (covers  () ()) True)
; 

  (= 
    (covers 
      (Cons  $C $CONCEPT) 
      (Cons  $E $EXAMPLE)) 
    ( (isa $E $C) (covers $CONCEPT $EXAMPLE)))
; 

 

  (= 
    (least_generalization  () () ()) True)
; 

  (= 
    (least-generalization 
      (Cons  $CONCEPT $C) 
      (Cons  $EX $E) 
      (Cons  $NCONCEPT $N)) 
    ( (lge $CONCEPT $EX $NCONCEPT) (least-generalization $C $E $N)))
; 

 

  (= 
    (greatest-specialization 
      (Cons  $CONCEPT $C) 
      (Cons  $EX $E) 
      (Cons  $NCONCEPT $C)) 
    (gsp $CONCEPT $EX $NCONCEPT))
; 

  (= 
    (greatest-specialization 
      (Cons  $CONCEPT $C) 
      (Cons  $EX $E) 
      (Cons  $CONCEPT $N)) 
    (greatest-specialization $C $E $N))
; 



  (= 
    (more-general $CONCEPT1 $CONCEPT2) 
    (covers $CONCEPT1 $CONCEPT2))
; 

 

  (= 
    (max Nil Nil) 
    (set-det))
; 

  (= 
    (max 
      (Cons  $TOP $T) 
      (Cons  $EX $E)) 
    ( (top $TOP $EX) (max $T $E)))
; 

 
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  vs/vs.pl 468 11 25683))

 
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  vs/vs.pl 480 12 26336))



  (= 
    (isa  $X $X) True)
; 

  (= 
    (isa $X $Y) 
    ( (son $X $Z) (isa $Z $Y)))
; 

 

  (= 
    (lge $X1 $X2 $X1) 
    ( (isa $X2 $X1) (set-det)))
; 

 
  (= 
    (lge $X1 $X2 $L) 
    ( (son $X1 $F) (lge $F $X2 $L)))
; 

 
;                                           (error
;                                             (syntax_error  operator_expected)
;                                             (file  vs/vs.pl 530 11 28892))

 

  (= 
    (gsp $X1 $X2 $G) 
    ( (son $S $X1) (gsp $S $X2 $G)))
; 

 

  (= 
    (writeln $X) 
    ( (display $X) (nl)))
; 

 

  (= 
    (member  $X 
      (Cons  $X $Y)) True)
; 

  (= 
    (member $X 
      (Cons  $Y $Z)) 
    (member $X $Z))
; 

 

  (= 
    (append  () $X $X) True)
; 

  (= 
    (append 
      (Cons  $X $Y) $Z 
      (Cons  $X $W)) 
    (append $Y $Z $W))
; 

 

  (= 
    (setofnil $X $Y $Z) 
    ( (setof $X $Y $Z) (set-det)))
; 

  (= 
    (setofnil  $X $Y ()) True)
; 

 

  (= 
    (help) 
    ( (write ' Start VS with command: learn.') (nl)))
; 



  !(help *)
; 


