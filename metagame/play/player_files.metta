;                                           (convert-to-metta-file  player_files $_491110 metagame/play/player_files.pl metagame/play/player_files.metta)
;
; player_files.pl


  (= (system-files  play (param ops parse1 parse2 controller history help help_advisors interface setup gen_menu start_menu local invert matches notation stat thread statify_theory efficient_state compile_syms mobility alphabeta value advisors))  True)
; ; comms,
; ; experiment,

  (= (system-files  fullplay (param ops parse1 parse2 controller menus interface start_menu local comms invert matches notation stat compile_state compile_syms index_preds))  True)


;
; Files defined by generator system

  (= (system-files  generator (genstructs piece_names gen tokenizer grammar gen_parameters))  True)

  (= (system-files  library (aux tracing timing dynamic_load shells args randoms theoryl menus))  True)

  (= (system-files  analysis (struct paths analysis mygraphs floyd tables arrive prom possess exclude dominate group step flight global potent tourney))  True)

;
; ----------------------------------------
;
; The following are used in statify_theory.pl:


  (= (theory-files  (dynamic_preds parse boards print_boards legal goals))  True)


