;
; 


;
; 



  (= 
    (db_entry  
      (:  teacher gilchrist_family) 
      (parent  euan warren) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher gilchrist_family) 
      (parent  berenice warren) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher gilchrist_family) 
      (parent  warren catherine) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher gilchrist_family) 
      (parent  warren charles) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher gilchrist_family) 
      (parent  warren david) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher gilchrist_family) 
      (parent  warren julia) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher gilchrist_family) 
      (parent  kate catherine) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher gilchrist_family) 
      (parent  kate charles) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher gilchrist_family) 
      (parent  kate david) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher gilchrist_family) 
      (parent  kate julia) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher gilchrist_family) 
      (parent  charles lucinda) ()) True)
; 


;
; 



  (= 
    (def_theory  
      (:  teacher entropy) 
      ( (:  teacher warm) 
        (:  teacher cold) 
        (:  teacher door))) True)
; 


  (= 
    (db_entry  
      (:  teacher warm) 
      (warm  kitchen) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher warm) 
      (warm  living_room) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher cold) 
      (cold  fridge) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher cold) 
      (cold  street) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher cold) 
      (cold  garden) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher door) 
      (door  living_room kitchen) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher door) 
      (door  living_room street) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher door) 
      (door  living_room garden) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher door) 
      (door  kitchen garden) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher door) 
      (door  kitchen fridge) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher door) 
      (door  street garden) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher entropy) 
      (entropy_increases  $A $B) 
      ( (warm  $A) 
        (cold  $B) 
        (door  $A $B))) True)
; 

  (= 
    (db_entry  
      (:  teacher entropy) 
      (entropy_increases  $A $B) 
      ( (cold  $A) 
        (warm  $B) 
        (door  $A $B))) True)
; 

  (= 
    (db_entry  
      (:  teacher entropy) 
      (entropy_increases  $A $B) 
      ( (warm  $A) 
        (cold  $B) 
        (door  $B $A))) True)
; 

  (= 
    (db_entry  
      (:  teacher entropy) 
      (entropy_increases  $A $B) 
      ( (cold  $A) 
        (warm  $B) 
        (door  $B $A))) True)
; 


;
; 


  (= 
    (db_entry  
      (:  teacher t_member) 
      (member  $A 
        (Cons  $A $_)) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher t_member) 
      (member  $A 
        (Cons  $_ $B)) 
      ( (member  $A $B))) True)
; 


  (= 
    (db_entry  
      (:  teacher t_append) 
      (append  () $List $List) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher t_append) 
      (append  
        (Cons  $First $Rest) $List 
        (Cons  $First $TempList)) 
      ( (append  $Rest $List $TempList))) True)
; 



  (= 
    (def_theory  
      (:  teacher t_reverse) 
      ( (:  teacher t_append))) True)
; 


  (= 
    (db_entry  
      (:  teacher t_reverse) 
      (reverse  () ()) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher t_reverse) 
      (reverse  
        (Cons  $X $Y) $Z) 
      ( (reverse  $Y $Y1) (append  $Y1 ($X) $Z))) True)
; 



  (= 
    (def_theory  
      (:  teacher qsort) 
      ( (:  teacher partition) (:  teacher t_append))) True)
; 


  (= 
    (db_entry  
      (:  teacher qsort) 
      (qsort  () ()) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher qsort) 
      (qsort  
        (Cons  $X $L) $L5) 
      ( (partition  $L $X $L1 $L2) 
        (qsort  $L1 $L3) 
        (qsort  $L2 $L4) 
        (append  $L3 
          (Cons  $X $L4) $L5))) True)
; 


  (= 
    (db_entry  
      (:  teacher partition) 
      (partition  () $_ () ()) ()) True)
; 

  (= 
    (db_entry  
      (:  teacher partition) 
      (partition  
        (Cons  $X $L) $Y 
        (Cons  $X $L1) $L2) 
      ( (<  $X $Y) (partition  $L $Y $L1 $L2))) True)
; 

  (= 
    (db_entry  
      (:  teacher partition) 
      (partition  
        (Cons  $X $L) $Y $L1 
        (Cons  $X $L2)) 
      ( (>=  $X $Y) (partition  $L $Y $L1 $L2))) True)
; 


