;                                           (convert-to-metta-file  parameters_P $_322066 sre_dna/parameters_P.pl sre_dna/parameters_P.metta)
;
; ------------------------------------------------
;
; October 2001
;
; Author: Brian Ross
;
; Dept. of Computer Science, Brock University


  (= (?-  (dynamic  (/  seed_P 2)))  True)
  (= (?-  (dynamic  (/  recog_flag 1)))  True)

;
; GP Control Parameters
;
; ---------------------------
;
; fitness_func_P(F) - F is file name containing fitness function.
;
; Should have a function called evaluator/2. It sets
;
; standardized fitness scores.
;
; dctg_file_P(F) - F is file name containing DCTG grammar
;
; population_size_P(I,N) - size (initial, final) of GP population
;
; - final is culled from initial
;
; cull_method_P(T) - T=tournament or elite
;
; max_runs_P(R,Type,Gen) - total number R of runs; max generations Gen per run
;
; Type = limit: run to R, even if soln found
;
; Type = solution: run to R, but stop when soln
;
; prob_grow_P - during ramped init population gen, this is
;
; probability a 'grow' tree is attempted
;
; prob_crossover_P - probability of crossover mating
;
; prob mutation = 1 - prob cross
;
; reprod_P(T) - # tries for crossover
;
; prob_internal_crossover_P - probability that crossover happens on an internal
;
; node in 1 parent; set to 'no' otherwise
;
; prob_terminal_mutation_P - probability that mutation is on a terminal node;
;
; set to 'no' otherwise
;
; max_depth_P(I,C) - I = max depth of initial generated indiv's,
;
; C = maximum depth permitted in offspring
;
; Make sure I is large enough, or else generation will stall.
;
; error_tolerance_P - wrt evaluation, tolerance for correct solns.
;
; tournament_size_P(S,R) - # entries in tournament selection S and
;
; replacement R
;
; lamarckian_P(P, K, Select, ProbCross) - do Lamarkian evol on
;
; P ; of population,
;
; K = # iterations for best-first search
;
; Select = {best, worst, random} selection
;
; If P=1.0, then Select is irrelevant.
;
; ProbCross is prob crossover (vs mutation)
;
; unique_population_P(no) - no/yes: children added to popn should be unique
;
; (doesn't affect genesis population however)
;
; trace_limit_P(U,T) - stops interpretation when this many unique/total
;
; traces have been obtained (turn off = (0,0) )
;
; if arg 1 = deterministic, then only 1 soln
;
; rep_limit_P(X) - if >0, X is max recursion depth for ! repetition
;
; 0 = unlimited repetition
;
; simplify_P(yes) - if 'yes', then offspring are simplified
;
; before added to population
;
; seed_P(X,Y) - if X = default, then default random cycle
;
; if X = random, then system timer used to initialize
;
; (Y set by program to seeds used, for reinit later)
;
; X=manual means Y = (A, B, C) are seed integers 1...30,000

  !(dynamic (/ debug-set-P 1))
;
; debug_set_P(yes): for additional debug printing, if implemented
;
; popn_dump_P(no) - if yes, then population dumped at end of each gen
;
; gen_type_P(T) - generation type: T = steadystate, separate
;
; evaluator_reset_P(P, G)- if G=no, ignore; else call P every G-th generation
;
; reprod_verif_P(T) - if yes, then each reproduced Tree has its DCTG code
;
; executed, to verify it wrt embedded MeTTa code;
;
; user_args_P(_) - list of user args for executing dctg calls
;
; make sure it has # members of what DCTG expects!
;
; dctg_root_P(Root) - root nonterminal of DCTG
;
; dctg_override_P(Term, Nonterm). - user override of terminal, nonterminal
;
; designation for rules; done for entire nonterm set
;
; mutation_range_P(R) - range to mutate SRE numeric values
;
; sre_mintestcnt_P(M) - minimum count for test set entries
;
; gen_set_size_P(S) - initial size of generated grammar test set
;
; min_grammar_prob_P(P) - minimum prob for grammar recognition to continue
;
; min_skip_prob_P(P). - min prob for skipping to continue
;
; unique_guards_P(yes) - if yes, guards in choice are unique; otherwise not.
;
; elite_migrate_P(N, R) - if gen_type=separate, then N best individuals
;
; migrate to next generation; if R=yes, then reeval fitness
;
; negsetsize_P(S) - number negative examples to generate
;
; eval_with_ID_P(T) - T=yes then include expr ID in call to evaluator
;
; else don't include



  (= (wd-P  /pack/narsese/prolog/sre_dna/)  True)


  (= (fitness-func-P  reg_gram_1.pl)  True)

  (= (dctg-file-P  sre3.pl)  True)


  (= (population-size-P  750 500)  True)	;
; <-- 750, 500

  (= (cull-method-P  elite)  True)		;
; <-- tournament

  (= (max-runs-P  1 solution 50)  True)	;
; <-- 5, solution, 35

  (= (prob-grow-P  0.5)  True)		;
; <-- 0.25

  (= (prob-crossover-P  0.9)  True)        ;
; <-- 0.90

  (= (reprod-P  3)  True) 			;
; <-- 3

  (= (prob-internal-crossover-P  0.9)  True) ;
; <-- 0.90 or no

  (= (prob-terminal-mutation-P  0.75)  True) ;
; <-- 0.75 or no

  (= (max-depth-P  10 17)  True)		;
; <-- 6, 17

  (= (error-tolerance-P  0)  True)		;
; <-- 0.000001

  (= (tournament-size-P  4 4)  True)	;
; <-- 2, 3

  (= (lamarckian-P  0.0 10 best 0.1)  True) ;
; <-- 0.25, 10, best, 0.20; (0.0,...) = off
;
; lamarckian_P(0.25, 10, best, 0.20).

  (= (unique-population-P  yes)  True)	;
; <-- no

  (= (trace-limit-P  0 0)  True)		;
; <-- (40, 90)

  (= (rep-limit-P  2)  True)			;
; <-- 3

  (= (max-string-length-P  20)  True)	;
; <-- 10

  (= (seed-P  random (,  $_ (,  $_ $_)))  True)	;
; <-- random, (_,_,_)

  (= (popn-dump-P  no)  True)			;
; <-- no

  (= (gen-type-P  steadystate)  True)	;
; <-- steadystate

  (= (evaluator-reset-P  generate_testset 100)  True)	;
; <-- no

  (= (reprod-verif-P  no)  True)		;
; <-- yes

  (= (user-args-P  ())  True)			;
; <-- eg. [], [_] or [_|_] if arity 0, 1 or 2

  (= (dctg-root-P  expr)  True)		;
; 

  (= (dctg-override-P  () ())  True)	;
; <-- [], []

  (= (mutation-range-P  0.1)  True) 		;
; <-- was 0.025

  (= (sre-mintestcnt-P  2)  True)		;
; <-- 2

  (= (gen-set-size-P  1000)  True)		;
; <-- 250

  (= (min-grammar-prob-P  0.0001)  True)	;
; <-- 1.0e-4

  (= (min-skip-prob-P  0.0001)  True)	;
; <-- 1.0e-4

  (= (unique-guards-P  no)  True)		;
; <-- yes

  (= (elite-migrate-P  0 no)  True)		;
; <-- 10

  (= (negsetsize-P  30)  True)			;
; <-- 75

  (= (eval-with-ID-P  no)  True)		;
; <-- no


