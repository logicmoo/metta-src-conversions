;                                           (convert-to-metta-file  teacher $_278198 multagnt/teacher.pl multagnt/teacher.metta)
;
; The knowledge of the teacher about different topics

;
; Fact theory


  (= (db-entry  (:  teacher gilchrist_family) (parent  euan warren) ())  True)
  (= (db-entry  (:  teacher gilchrist_family) (parent  berenice warren) ())  True)
  (= (db-entry  (:  teacher gilchrist_family) (parent  warren catherine) ())  True)
  (= (db-entry  (:  teacher gilchrist_family) (parent  warren charles) ())  True)
  (= (db-entry  (:  teacher gilchrist_family) (parent  warren david) ())  True)
  (= (db-entry  (:  teacher gilchrist_family) (parent  warren julia) ())  True)
  (= (db-entry  (:  teacher gilchrist_family) (parent  kate catherine) ())  True)
  (= (db-entry  (:  teacher gilchrist_family) (parent  kate charles) ())  True)
  (= (db-entry  (:  teacher gilchrist_family) (parent  kate david) ())  True)
  (= (db-entry  (:  teacher gilchrist_family) (parent  kate julia) ())  True)
  (= (db-entry  (:  teacher gilchrist_family) (parent  charles lucinda) ())  True)

;
; Fact and Rule Theory


  (= (def-theory  (:  teacher entropy) ((:  teacher warm) (:  teacher cold) (:  teacher door)))  True)

  (= (db-entry  (:  teacher warm) (warm  kitchen) ())  True)
  (= (db-entry  (:  teacher warm) (warm  living_room) ())  True)
  (= (db-entry  (:  teacher cold) (cold  fridge) ())  True)
  (= (db-entry  (:  teacher cold) (cold  street) ())  True)
  (= (db-entry  (:  teacher cold) (cold  garden) ())  True)
  (= (db-entry  (:  teacher door) (door  living_room kitchen) ())  True)
  (= (db-entry  (:  teacher door) (door  living_room street) ())  True)
  (= (db-entry  (:  teacher door) (door  living_room garden) ())  True)
  (= (db-entry  (:  teacher door) (door  kitchen garden) ())  True)
  (= (db-entry  (:  teacher door) (door  kitchen fridge) ())  True)
  (= (db-entry  (:  teacher door) (door  street garden) ())  True)
  (= (db-entry  (:  teacher entropy) (entropy-increases  $A $B) ((warm  $A) (cold  $B) (door  $A $B)))  True)
  (= (db-entry  (:  teacher entropy) (entropy-increases  $A $B) ((cold  $A) (warm  $B) (door  $A $B)))  True)
  (= (db-entry  (:  teacher entropy) (entropy-increases  $A $B) ((warm  $A) (cold  $B) (door  $B $A)))  True)
  (= (db-entry  (:  teacher entropy) (entropy-increases  $A $B) ((cold  $A) (warm  $B) (door  $B $A)))  True)

;
; Recusive Theory

  (= (db-entry  (:  teacher t_member) (member  $A ([|]  $A $_)) ())  True)
  (= (db-entry  (:  teacher t_member) (member  $A ([|]  $_ $B)) ((member  $A $B)))  True)

  (= (db-entry  (:  teacher t_append) (append  () $List $List) ())  True)
  (= (db-entry  (:  teacher t_append) (append  ([|]  $First $Rest) $List ([|]  $First $TempList)) ((append  $Rest $List $TempList)))  True)


  (= (def-theory  (:  teacher t_reverse) ((:  teacher t_append)))  True)

  (= (db-entry  (:  teacher t_reverse) (reverse  () ()) ())  True)
  (= (db-entry  (:  teacher t_reverse) (reverse  ([|]  $X $Y) $Z) ((reverse  $Y $Y1) (append  $Y1 ($X) $Z)))  True)


  (= (def-theory  (:  teacher qsort) ((:  teacher partition) (:  teacher t_append)))  True)

  (= (db-entry  (:  teacher qsort) (qsort  () ()) ())  True)
  (= (db-entry  (:  teacher qsort) (qsort  ([|]  $X $L) $L5) ((partition  $L $X $L1 $L2) (qsort  $L1 $L3) (qsort  $L2 $L4) (append  $L3 ([|]  $X $L4) $L5)))  True)

  (= (db-entry  (:  teacher partition) (partition  () $_ () ()) ())  True)
  (= (db-entry  (:  teacher partition) (partition  ([|]  $X $L) $Y ([|]  $X $L1) $L2) ((<  $X $Y) (partition  $L $Y $L1 $L2)))  True)
  (= (db-entry  (:  teacher partition) (partition  ([|]  $X $L) $Y $L1 ([|]  $X $L2)) ((>=  $X $Y) (partition  $L $Y $L1 $L2)))  True)

