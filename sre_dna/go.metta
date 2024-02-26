;                                           (convert-to-metta-file  go $_334428 sre_dna/go.pl sre_dna/go.metta)

;
; ?- load_files(library(random)).
;
; ?- load_files(library(date)).
;
; ?- load_files(library(strings)).


  (= (?-  (use-module  (library  random)))  True)
  (= (?-  (use-module  (library  system)))  True)
  (= (?-  (use-module  (library  lists)))  True)

  (= (?-  (consult  (library  (/  sre_dna dynamics))))  True)
  (= (?-  (consult  (library  (/  sre_dna dctg))))  True)
  (= (?-  (consult  (library  (/  sre_dna parameters_P))))  True)
  (= (?-  (consult  (library  (/  sre_dna operators))))  True)
  (= (?-  (consult  (library  (/  sre_dna dctg_pp))))  True)
  (= (?-  (consult  (library  (/  sre_dna utils))))  True)

  (= (?-  (,  (dctg-file-P  $FileDCTG) (,  (grammar  $FileDCTG) make_grammar_table)))  True)

  (= regen  
    ( (tell compile-file.pl) 
      (write '?- use-module(library(lists)).
        ') nl listing told))
; ; fast: new

  !(regen *) 




  (= (?-  (consult  (library  (/  sre_dna ccs_utils))))  True)
  (= (?-  (consult  (library  (/  sre_dna dctg_gen))))  True)
  (= (?-  (consult  (library  (/  sre_dna dctg_reprod))))  True)
  (= (?-  (consult  (library  (/  sre_dna dctg_utils))))  True)
  (= (?-  (consult  (library  (/  sre_dna generate))))  True)
  (= (?-  (consult  (library  (/  sre_dna gp_engine))))  True)
  (= (?-  (consult  (library  (/  sre_dna lamarckian))))  True)
  (= (?-  (consult  (library  (/  sre_dna evaluation))))  True)
  (= (?-  (consult  (library  (/  sre_dna file_stats))))  True)

;
; following must follow 'parameters_P' above.
;
; Convenient to put here, as interactive debugging of DCTG-GP is easier.

  (= (?-  (,  (fitness-func-P  $File) (consult  $File)))  True)
; /* ?- consult(library(sre_dna/sre_mutation3)). ?- consult(library(sre_dna/sre_crossover3a)). ?- consult(library(sre_dna/dna_proc)). ?- consult(library(sre_dna/mask_optimizer)). ?- dna_file_P(DNA_file), consult(DNA_file). */ 

  (= (?-  (:  fast (consult  compile_file)))  True)
  (= (?-  clean_up)  True)


