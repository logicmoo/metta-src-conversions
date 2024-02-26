;
; 

;
; 

;
; 

;
; 



  (= 
    (?-  
      (dynamic  
        (/  seed_P 2))) True)
; 

  (= 
    (?-  
      (dynamic  
        (/  recog_flag 1))) True)
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


  !(dynamic (/ debug-set-P 1))
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




  (= 
    (wd_P  /pack/narsese/prolog/sre_dna/) True)
; 



  (= 
    (fitness_func_P  reg_gram_1.pl) True)
; 


  (= 
    (dctg_file_P  sre3.pl) True)
; 



  (= 
    (population_size_P  750 500) True)
; 
	;
; 


  (= 
    (cull_method_P  elite) True)
; 
		;
; 


  (= 
    (max_runs_P  1 solution 50) True)
; 
	;
; 


  (= 
    (prob_grow_P  0.5) True)
; 
		;
; 


  (= 
    (prob_crossover_P  0.9) True)
; 
        ;
; 


  (= 
    (reprod_P  3) True)
; 
 			;
; 


  (= 
    (prob_internal_crossover_P  0.9) True)
; 
 ;
; 


  (= 
    (prob_terminal_mutation_P  0.75) True)
; 
 ;
; 


  (= 
    (max_depth_P  10 17) True)
; 
		;
; 


  (= 
    (error_tolerance_P  0) True)
; 
		;
; 


  (= 
    (tournament_size_P  4 4) True)
; 
	;
; 


  (= 
    (lamarckian_P  0.0 10 best 0.1) True)
; 
 ;
; 

;
; 


  (= 
    (unique_population_P  yes) True)
; 
	;
; 


  (= 
    (trace_limit_P  0 0) True)
; 
		;
; 


  (= 
    (rep_limit_P  2) True)
; 
			;
; 


  (= 
    (max_string_length_P  20) True)
; 
	;
; 


  (= 
    (seed_P  random 
      (,  $_ 
        (,  $_ $_))) True)
; 
	;
; 


  (= 
    (popn_dump_P  no) True)
; 
			;
; 


  (= 
    (gen_type_P  steadystate) True)
; 
	;
; 


  (= 
    (evaluator_reset_P  generate_testset 100) True)
; 
	;
; 


  (= 
    (reprod_verif_P  no) True)
; 
		;
; 


  (= 
    (user_args_P  ()) True)
; 
			;
; 


  (= 
    (dctg_root_P  expr) True)
; 
		;
; 


  (= 
    (dctg_override_P  () ()) True)
; 
	;
; 


  (= 
    (mutation_range_P  0.1) True)
; 
 		;
; 


  (= 
    (sre_mintestcnt_P  2) True)
; 
		;
; 


  (= 
    (gen_set_size_P  1000) True)
; 
		;
; 


  (= 
    (min_grammar_prob_P  0.0001) True)
; 
	;
; 


  (= 
    (min_skip_prob_P  0.0001) True)
; 
	;
; 


  (= 
    (unique_guards_P  no) True)
; 
		;
; 


  (= 
    (elite_migrate_P  0 no) True)
; 
		;
; 


  (= 
    (negsetsize_P  30) True)
; 
			;
; 


  (= 
    (eval_with_ID_P  no) True)
; 
		;
; 



