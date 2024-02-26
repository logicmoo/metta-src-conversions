;
;                          Wrong knowledge of a learner about different topics

;
;                          Differences to Learner1 are enclosed in comments


;
;                           Fact theory


  (= 
    (db_entry  
      (:  learner gilchrist_family) 
      (parent  euan warren) ()) True)
  (= 
    (db_entry  
      (:  learner gilchrist_family) 
      (parent  berenice warren) ()) True)
;
; 

;
;                            db_entry(learner:gilchrist_family,parent(warren,catherine),[]).

;
; 

  (= 
    (db_entry  
      (:  learner gilchrist_family) 
      (parent  warren charles) ()) True)
  (= 
    (db_entry  
      (:  learner gilchrist_family) 
      (parent  warren david) ()) True)
  (= 
    (db_entry  
      (:  learner gilchrist_family) 
      (parent  warren julia) ()) True)
  (= 
    (db_entry  
      (:  learner gilchrist_family) 
      (parent  kate catherine) ()) True)
  (= 
    (db_entry  
      (:  learner gilchrist_family) 
      (parent  kate charles) ()) True)
  (= 
    (db_entry  
      (:  learner gilchrist_family) 
      (parent  kate david) ()) True)
  (= 
    (db_entry  
      (:  learner gilchrist_family) 
      (parent  kate julia) ()) True)
;
; 

;
;                                db_entry(learner:gilchrist_family,parent(charles,lucinda),[]).

;
; 


;
;                                Fact and Rule Theory



  (= 
    (def_theory  
      (:  learner entropy) 
      ( (:  learner warm) 
        (:  learner cold) 
        (:  learner door))) True)

  (= 
    (db_entry  
      (:  learner warm) 
      (warm  living_room) ()) True)
  (= 
    (db_entry  
      (:  learner warm) 
      (warm  kitchen) ()) True)
  (= 
    (db_entry  
      (:  learner cold) 
      (cold  street) ()) True)
  (= 
    (db_entry  
      (:  learner cold) 
      (cold  garden) ()) True)
  (= 
    (db_entry  
      (:  learner cold) 
      (cold  fridge) ()) True)
;
; 

;
;                                   The following entries are incorrect

;
; 

  (= 
    (db_entry  
      (:  learner door) 
      (door  a_living_room a_kitchen) ()) True)
  (= 
    (db_entry  
      (:  learner door) 
      (door  a_living_room a_street) ()) True)
  (= 
    (db_entry  
      (:  learner door) 
      (door  a_living_room a_garden) ()) True)
  (= 
    (db_entry  
      (:  learner door) 
      (door  a_kitchen a_garden) ()) True)
  (= 
    (db_entry  
      (:  learner door) 
      (door  a_kitchen a_fridge) ()) True)
  (= 
    (db_entry  
      (:  learner door) 
      (door  a_street a_garden) ()) True)
;
; 

;
;                                      Now the theory continues correctly

;
; 

  (= 
    (db_entry  
      (:  learner entropy) 
      (entropy_increases  $A $B) 
      ( (warm  $A) 
        (cold  $B) 
        (door  $A $B))) True)
  (= 
    (db_entry  
      (:  learner entropy) 
      (entropy_increases  $A $B) 
      ( (cold  $A) 
        (warm  $B) 
        (door  $A $B))) True)
  (= 
    (db_entry  
      (:  learner entropy) 
      (entropy_increases  $A $B) 
      ( (warm  $A) 
        (cold  $B) 
        (door  $B $A))) True)
  (= 
    (db_entry  
      (:  learner entropy) 
      (entropy_increases  $A $B) 
      ( (cold  $A) 
        (warm  $B) 
        (door  $B $A))) True)

;
;                                        Recusive Theory


;
;                                         incorrect t_member

  (= 
    (db_entry  
      (:  learner t_member) 
      (member  $A ()) ()) True)
  (= 
    (db_entry  
      (:  learner t_member) 
      (member  $A 
        (Cons  $B $T)) 
      ( (not  
          (= $A $B)) (member  $A $T))) True)
;
;                                         					   |        |

;
;                                         					   +--------+ this is a fault !

;
; 


  (= 
    (db_entry  
      (:  learner t_append) 
      (append  () $List ()) ()) True)
;
;                                         					  ||

;
;                                         					  ++ this is a fault !

  (= 
    (db_entry  
      (:  learner t_append) 
      (append  
        (Cons  $First $Rest) $List 
        (Cons  $First $TempList)) 
      ( (append  $Rest $List $TempList))) True)


  (= 
    (def_theory  
      (:  learner t_reverse) 
      ( (:  learner t_append))) True)

  (= 
    (db_entry  
      (:  learner t_reverse) 
      (reverse  () ()) ()) True)
  (= 
    (db_entry  
      (:  learner t_reverse) 
      (reverse  
        (Cons  $X $Y) $Z) 
      ( (reverse  $Y $Y1) (append  $Y1 ($X) $Z))) True)


  (= 
    (def_theory  
      (:  learner qsort) 
      ( (:  learner partition) (:  learner t_append1))) True)

  (= 
    (db_entry  
      (:  learner qsort) 
      (qsort  () ()) ()) True)
  (= 
    (db_entry  
      (:  learner qsort) 
      (qsort  
        (Cons  $X $L) $L5) 
      ( (partition  $L $X $L1 $L2) 
        (qsort  $L1 $L3) 
        (qsort  $L2 $L4) 
        (append1  $L3 
          (Cons  $X $L4) $L5))) True)

  (= 
    (db_entry  
      (:  learner partition) 
      (partition  
        (Cons  $X $L) $Y 
        ($L1) $L2) 
      ( (<  $X $Y) (partition  $L $Y $L1 $L2))) True)
;                                         ;					     |  |
;                                         ;					     +--+ this is a fault !


  (= 
    (db_entry  
      (:  learner partition) 
      (partition  
        (Cons  $X $L) $Y $L1 
        (Cons  $X $L2)) 
      ( (>=  $X $Y) (partition  $L $Y $L1 $L2))) True)
  (= 
    (db_entry  
      (:  learner partition) 
      (partition  () $_ () ()) ()) True)

  (= 
    (db_entry  
      (:  learner t_append1) 
      (append1  () $List $List) ()) True)
  (= 
    (db_entry  
      (:  learner t_append1) 
      (append1  
        (Cons  $First $Rest) $List 
        (Cons  $First $TempList)) 
      ( (append1  $Rest $List $TempList))) True)

