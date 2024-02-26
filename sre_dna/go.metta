
;
; 

;
; 

;
; 



  (= 
    (?-  
      (use_module  
        (library  random))) True)
; 

  (= 
    (?-  
      (use_module  
        (library  system))) True)
; 

  (= 
    (?-  
      (use_module  
        (library  lists))) True)
; 


  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna dynamics)))) True)
; 

  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna dctg)))) True)
; 

  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna parameters_P)))) True)
; 

  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna operators)))) True)
; 

  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna dctg_pp)))) True)
; 

  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna utils)))) True)
; 


  (= 
    (?-  
      (,  
        (dctg_file_P  $FileDCTG) 
        (,  
          (grammar  $FileDCTG) make_grammar_table))) True)
; 


  (= 
    (regen) 
    ( (tell compile-file.pl) 
      (write '?- use-module(library(lists)).
        ') 
      (nl) 
      (listing) 
      (told)))
; 


  !(regen *)
; 
 




  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna ccs_utils)))) True)
; 

  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna dctg_gen)))) True)
; 

  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna dctg_reprod)))) True)
; 

  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna dctg_utils)))) True)
; 

  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna generate)))) True)
; 

  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna gp_engine)))) True)
; 

  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna lamarckian)))) True)
; 

  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna evaluation)))) True)
; 

  (= 
    (?-  
      (consult  
        (library  
          (/  sre_dna file_stats)))) True)
; 


;
; 

;
; 


  (= 
    (?-  
      (,  
        (fitness_func_P  $File) 
        (consult  $File))) True)
; 
 

  (= 
    (?-  
      (:  fast 
        (consult  compile_file))) True)
; 

  (= 
    (?-  clean_up) True)
; 



