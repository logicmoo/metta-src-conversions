;                                           (convert-to-metta-file  learner1 $_118902 multagnt/learner1.pl multagnt/learner1.metta)
;
; Right knowledge of a learner about different topics

;
; Fact theory


  (= (db-entry  (:  learner gilchrist_family) (parent  euan warren) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  berenice warren) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  warren catherine) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  warren charles) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  warren david) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  warren julia) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  kate catherine) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  kate charles) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  kate david) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  kate julia) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  charles lucinda) ())  True)

;
; Fact and Rule Theory


  (= (def-theory  (:  learner entropy) ((:  learner warm) (:  learner cold) (:  learner door)))  True)

  (= (db-entry  (:  learner warm) (warm  kitchen) ())  True)
  (= (db-entry  (:  learner warm) (warm  living_room) ())  True)
  (= (db-entry  (:  learner cold) (cold  fridge) ())  True)
  (= (db-entry  (:  learner cold) (cold  street) ())  True)
  (= (db-entry  (:  learner cold) (cold  garden) ())  True)
  (= (db-entry  (:  learner door) (door  living_room kitchen) ())  True)
  (= (db-entry  (:  learner door) (door  living_room street) ())  True)
  (= (db-entry  (:  learner door) (door  living_room garden) ())  True)
  (= (db-entry  (:  learner door) (door  kitchen garden) ())  True)
  (= (db-entry  (:  learner door) (door  kitchen fridge) ())  True)
  (= (db-entry  (:  learner door) (door  street garden) ())  True)
  (= (db-entry  (:  learner entropy) (entropy-increases  $A $B) ((warm  $A) (cold  $B) (door  $A $B)))  True)
  (= (db-entry  (:  learner entropy) (entropy-increases  $A $B) ((cold  $A) (warm  $B) (door  $A $B)))  True)
  (= (db-entry  (:  learner entropy) (entropy-increases  $A $B) ((warm  $A) (cold  $B) (door  $B $A)))  True)
  (= (db-entry  (:  learner entropy) (entropy-increases  $A $B) ((cold  $A) (warm  $B) (door  $B $A)))  True)

;
; Recusive Theory

  (= (db-entry  (:  learner t_member) (member  $A ([|]  $A $_)) ())  True)
  (= (db-entry  (:  learner t_member) (member  $A ([|]  $_ $B)) ((member  $A $B)))  True)

  (= (db-entry  (:  learner t_append) (append  () $List $List) ())  True)
  (= (db-entry  (:  learner t_append) (append  ([|]  $First $Rest) $List ([|]  $First $TempList)) ((append  $Rest $List $TempList)))  True)


  (= (def-theory  (:  learner t_reverse) ((:  learner t_append)))  True)

  (= (db-entry  (:  learner t_reverse) (reverse  () ()) ())  True)
  (= (db-entry  (:  learner t_reverse) (reverse  ([|]  $X $Y) $Z) ((reverse  $Y $Y1) (append  $Y1 ($X) $Z)))  True)


  (= (def-theory  (:  learner qsort) ((:  learner partition) (:  learner t_append)))  True)

  (= (db-entry  (:  learner qsort) (qsort  () ()) ())  True)
  (= (db-entry  (:  learner qsort) (qsort  ([|]  $X $L) $L5) ((partition  $L $X $L1 $L2) (qsort  $L1 $L3) (qsort  $L2 $L4) (append  $L3 ([|]  $X $L4) $L5)))  True)

  (= (db-entry  (:  learner partition) (partition  () $_ () ()) ())  True)
  (= (db-entry  (:  learner partition) (partition  ([|]  $X $L) $Y ([|]  $X $L1) $L2) ((<  $X $Y) (partition  $L $Y $L1 $L2)))  True)
  (= (db-entry  (:  learner partition) (partition  ([|]  $X $L) $Y $L1 ([|]  $X $L2)) ((>=  $X $Y) (partition  $L $Y $L1 $L2)))  True)

