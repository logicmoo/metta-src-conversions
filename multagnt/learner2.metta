;                                           (convert-to-metta-file  learner2 $_205064 multagnt/learner2.pl multagnt/learner2.metta)
;
; Wrong knowledge of a learner about different topics
;
; Differences to Learner1 are enclosed in comments

;
; Fact theory

  (= (db-entry  (:  learner gilchrist_family) (parent  euan warren) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  berenice warren) ())  True)
;
; 
;
; db_entry(learner:gilchrist_family,parent(warren,catherine),[]).
;
; 
  (= (db-entry  (:  learner gilchrist_family) (parent  warren charles) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  warren david) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  warren julia) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  kate catherine) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  kate charles) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  kate david) ())  True)
  (= (db-entry  (:  learner gilchrist_family) (parent  kate julia) ())  True)
;
; 
;
; db_entry(learner:gilchrist_family,parent(charles,lucinda),[]).
;
; 

;
; Fact and Rule Theory


  (= (def-theory  (:  learner entropy) ((:  learner warm) (:  learner cold) (:  learner door)))  True)

  (= (db-entry  (:  learner warm) (warm  living_room) ())  True)
  (= (db-entry  (:  learner warm) (warm  kitchen) ())  True)
  (= (db-entry  (:  learner cold) (cold  street) ())  True)
  (= (db-entry  (:  learner cold) (cold  garden) ())  True)
  (= (db-entry  (:  learner cold) (cold  fridge) ())  True)
;
; 
;
; The following entries are incorrect
;
; 
  (= (db-entry  (:  learner door) (door  a_living_room a_kitchen) ())  True)
  (= (db-entry  (:  learner door) (door  a_living_room a_street) ())  True)
  (= (db-entry  (:  learner door) (door  a_living_room a_garden) ())  True)
  (= (db-entry  (:  learner door) (door  a_kitchen a_garden) ())  True)
  (= (db-entry  (:  learner door) (door  a_kitchen a_fridge) ())  True)
  (= (db-entry  (:  learner door) (door  a_street a_garden) ())  True)
;
; 
;
; Now the theory continues correctly
;
; 
  (= (db-entry  (:  learner entropy) (entropy-increases  $A $B) ((warm  $A) (cold  $B) (door  $A $B)))  True)
  (= (db-entry  (:  learner entropy) (entropy-increases  $A $B) ((cold  $A) (warm  $B) (door  $A $B)))  True)
  (= (db-entry  (:  learner entropy) (entropy-increases  $A $B) ((warm  $A) (cold  $B) (door  $B $A)))  True)
  (= (db-entry  (:  learner entropy) (entropy-increases  $A $B) ((cold  $A) (warm  $B) (door  $B $A)))  True)

;
; Recusive Theory

;
; incorrect t_member
  (= (db-entry  (:  learner t_member) (member  $A ()) ())  True)
  (= (db-entry  (:  learner t_member) (member  $A ([|]  $B $T)) ((not  (= $A  $B)) (member  $A $T)))  True)
;
; | |
;
; +--------+ this is a fault !
;
; 

  (= (db-entry  (:  learner t_append) (append  () $List ()) ())  True)
;
; ||
;
; ++ this is a fault !
  (= (db-entry  (:  learner t_append) (append  ([|]  $First $Rest) $List ([|]  $First $TempList)) ((append  $Rest $List $TempList)))  True)


  (= (def-theory  (:  learner t_reverse) ((:  learner t_append)))  True)

  (= (db-entry  (:  learner t_reverse) (reverse  () ()) ())  True)
  (= (db-entry  (:  learner t_reverse) (reverse  ([|]  $X $Y) $Z) ((reverse  $Y $Y1) (append  $Y1 ($X) $Z)))  True)


  (= (def-theory  (:  learner qsort) ((:  learner partition) (:  learner t_append1)))  True)

  (= (db-entry  (:  learner qsort) (qsort  () ()) ())  True)
  (= (db-entry  (:  learner qsort) (qsort  ([|]  $X $L) $L5) ((partition  $L $X $L1 $L2) (qsort  $L1 $L3) (qsort  $L2 $L4) (append1  $L3 ([|]  $X $L4) $L5)))  True)

  (= (db-entry  (:  learner partition) (partition  ([|]  $X $L) $Y ($L1) $L2) ((<  $X $Y) (partition  $L $Y $L1 $L2)))  True)
; ; | | ; +--+ this is a fault !
  (= (db-entry  (:  learner partition) (partition  ([|]  $X $L) $Y $L1 ([|]  $X $L2)) ((>=  $X $Y) (partition  $L $Y $L1 $L2)))  True)
  (= (db-entry  (:  learner partition) (partition  () $_ () ()) ())  True)

  (= (db-entry  (:  learner t_append1) (append1  () $List $List) ())  True)
  (= (db-entry  (:  learner t_append1) (append1  ([|]  $First $Rest) $List ([|]  $First $TempList)) ((append1  $Rest $List $TempList)))  True)

