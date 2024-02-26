

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
      (compile  dynamics)) True)
; 

  (= 
    (?-  
      (consult  dctg)) True)
; 

  (= 
    (?-  
      (consult  parameters_P)) True)
; 

  (= 
    (?-  
      (compile  operators)) True)
; 

  (= 
    (?-  
      (consult  dctg_pp)) True)
; 

  (= 
    (?-  
      (compile  utils)) True)
; 


  (= 
    (?-  
      (,  
        (dctg_file_P  $FileDCTG) 
        (,  
          (grammar  $FileDCTG) make_grammar_table))) True)
; 

  (= 
    (?-  
      (,  
        (tell  compile_file.pl) 
        (,  
          (write  '
?- use_module(library(lists)).
        ') 
          (,  nl 
            (,  listing told))))) True)
; 


  (= 
    (?-  
      (compile  ccs_utils)) True)
; 

  (= 
    (?-  
      (compile  dctg_gen)) True)
; 

  (= 
    (?-  
      (compile  dctg_reprod)) True)
; 

  (= 
    (?-  
      (compile  dctg_utils)) True)
; 

  (= 
    (?-  
      (compile  generate)) True)
; 

  (= 
    (?-  
      (compile  gp_engine)) True)
; 

  (= 
    (?-  
      (compile  lamarckian)) True)
; 

  (= 
    (?-  
      (compile  evaluation)) True)
; 

  (= 
    (?-  
      (compile  file_stats)) True)
; 


;
; 


  (= 
    (?-  
      (,  
        (fitness_func_P  $File) 
        (compile  $File))) True)
; 
 

  (= 
    (?-  
      (:  fast 
        (compile  compile_file.pl))) True)
; 

  (= 
    (?-  clean_up) True)
; 



