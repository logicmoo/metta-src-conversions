;
; 

;
; 



  (= 
    (matomic $X) 
    (atomic $X))
; 


  (= 
    (matom $X) 
    (atom $X))
; 


  (= 
    (mnumber $X) 
    (number $X))
; 


  (= 
    (ex  
      (t  nil) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 0 nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 0 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 0 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 0 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 0 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  0) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  0) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  0) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  0) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  0) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  
            (s  0)) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  
            (s  0)) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  
            (s  0)) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  
            (s  0)) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  
            (s  0)) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  
            (s  
              (s  0))) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  
            (s  
              (s  0))) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  nil 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 0 nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 0 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 0 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 0 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 0 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  0) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  0) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  0) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  0) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  0) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  
            (s  0)) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  
            (s  0)) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  
            (s  0)) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  
            (s  0)) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  
            (s  0)) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  
            (s  
              (s  0))) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 0 nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 0 nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 0 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 0 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 0 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 0 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  0) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  0) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  0) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  0) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  0) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  
            (s  0)) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  
            (s  0)) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  
            (s  0)) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  
            (s  0)) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  
            (s  0)) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  
            (s  
              (s  0))) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  0) nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 0 nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 0 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 0 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 0 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 0 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  0) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  0) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  0) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  0) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  0) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  
            (s  0)) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  
            (s  0)) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  
            (s  0)) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  
            (s  0)) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  
            (s  0)) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  
            (s  
              (s  0))) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  0)) nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 0 nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 0 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 0 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 0 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 0 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  0) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  0) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  0) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  0) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  0) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  
            (s  0)) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  
            (s  0)) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  
            (s  0)) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  
            (s  0)) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  
            (s  0)) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  
            (s  
              (s  0))) nil)) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 0 nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  0) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  
              (s  0)) nil))) +) True)
; 

  (= 
    (ex  
      (t  
        (tree  
          (tree  nil 
            (s  
              (s  
                (s  0))) nil) 
          (s  
            (s  
              (s  0))) 
          (tree  nil 
            (s  
              (s  
                (s  0))) nil))) +) True)
; 


