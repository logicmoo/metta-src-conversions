
  (= 
    (classes  
      (accept reject ask)) True)
; 


  (= 
    (attributes  
      (account cash employed)) True)
; 


  (= 
    (example  1 accept 
      ( (= account bank) 
        (= employed yes) 
        (= cash 300))) True)
; 

  (= 
    (example  2 accept 
      ( (= account bank) 
        (= employed yes) 
        (= cash 300))) True)
; 

  (= 
    (example  3 accept 
      ( (= account bank) 
        (= employed no) 
        (= cash 300))) True)
; 

  (= 
    (example  4 accept 
      ( (= account bank) 
        (= employed no) 
        (= cash 4000))) True)
; 

  (= 
    (example  5 accept 
      ( (= account none) 
        (= employed yes) 
        (= cash 4000))) True)
; 

  (= 
    (example  6 reject 
      ( (= account none) 
        (= employed yes) 
        (= cash 300))) True)
; 

  (= 
    (example  7 reject 
      ( (= account none) 
        (= employed no) 
        (= cash 300))) True)
; 

  (= 
    (example  8 reject 
      ( (= account none) 
        (= employed no) 
        (= cash 4000))) True)
; 

  (= 
    (example  9 reject 
      ( (= account none) 
        (= employed no) 
        (= cash 4000))) True)
; 

  (= 
    (example  10 ask 
      ( (= account others) 
        (= employed no) 
        (= cash 300))) True)
; 

  (= 
    (example  11 ask 
      ( (= account others) 
        (= employed no) 
        (= cash 4000))) True)
; 

  (= 
    (example  12 ask 
      ( (= account others) 
        (= employed no) 
        (= cash 300))) True)
; 


