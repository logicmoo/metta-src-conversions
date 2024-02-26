;
; 



  (= 
    (column $X) 
    ( (brick $X) 
      (standing $X) 
      (is-on $X $Y) 
      (ground $Y)))
; 


  (= 
    (column $X) 
    ( (brick $X) 
      (standing $X) 
      (is-on $X $Y) 
      (column $Y)))
; 



  (= 
    (same-height $X $Y) 
    ( (ground $X) (ground $Y)))
; 


  (= 
    (same-height $X $Y) 
    ( (brick $X) 
      (standing $X) 
      (brick $Y) 
      (standing $Y) 
      (is-on $X $X1) 
      (is-on $Y $Y1) 
      (same-height $X1 $Y1)))
; 



;
; 



  (= 
    (arch $X) 
    ( (part-of $A $X) 
      (part-of $B $X) 
      (part-of $C $X) 
      (is-on $A $B) 
      (is-on $A $C) 
      (is-on $B $D) 
      (is-on $C $E) 
      (ground $D) 
      (ground $E) 
      (left-of $B $C) 
      (does-not-touch $B $C) 
      (lying $A) 
      (wedge $A) 
      (standing $B) 
      (standing $C) 
      (brick $B) 
      (brick $C)))
; 



  (= 
    (arch $X) 
    ( (part-of $A $X) 
      (part-of $B $X) 
      (part-of $C $X) 
      (is-on $A $B) 
      (is-on $A $C) 
      (is-on $B $D) 
      (is-on $C $E) 
      (left-of $B $C) 
      (does-not-touch $B $C) 
      (lying $A) 
      (wedge $A) 
      (standing $B) 
      (standing $C) 
      (brick $B) 
      (brick $C) 
      (brick $D) 
      (brick $E) 
      (does-not-touch $D $E) 
      (standing $D) 
      (standing $E) 
      (is-on $D $F) 
      (is-on $E $G) 
      (ground $G) 
      (ground $F)))
; 



;
; 


  (= 
    (arch $X) 
    ( (part-of $A $X) 
      (part-of $B $X) 
      (part-of $C $X) 
      (is-on $A $B) 
      (is-on $A $C) 
      (is-on $B $D) 
      (is-on $C $E) 
      (ground $D) 
      (ground $E) 
      (left-of $B $C) 
      (does-not-touch $B $C) 
      (lying $A) 
      (wedge $A) 
      (standing $B) 
      (standing $C) 
      (brick $B) 
      (brick $C) 
      (red $B) 
      (green $C)))
; 


  (= 
    (arch $X) 
    ( (part-of $A $X) 
      (part-of $B $X) 
      (part-of $C $X) 
      (is-on $A $B) 
      (is-on $A $C) 
      (is-on $B $D) 
      (is-on $C $E) 
      (ground $D) 
      (ground $E) 
      (left-of $B $C) 
      (does-not-touch $B $C) 
      (lying $A) 
      (wedge $A) 
      (standing $B) 
      (standing $C) 
      (brick $B) 
      (brick $C) 
      (green $B) 
      (red $C)))
; 


  (= 
    (arch $X) 
    ( (part-of $A $X) 
      (part-of $B $X) 
      (part-of $C $X) 
      (is-on $A $B) 
      (is-on $A $C) 
      (is-on $B $D) 
      (is-on $C $E) 
      (ground $D) 
      (ground $E) 
      (left-of $B $C) 
      (does-not-touch $B $C) 
      (lying $A) 
      (wedge $A) 
      (standing $B) 
      (standing $C) 
      (brick $B) 
      (brick $C) 
      (blue $B) 
      (red $C)))
; 



;
; 



  (= 
    (column $X) 
    ( (brick $X) 
      (standing $X) 
      (is-on $X $Y) 
      (table $Y)))
; 


  (= 
    (column $X) 
    ( (block $X) 
      (standing $X) 
      (is-on $X $Y) 
      (ground $Y)))
; 


  (= 
    (column $X) 
    ( (block $X) 
      (standing $X) 
      (is-on $X $Y) 
      (column $Y)))
; 



; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 

