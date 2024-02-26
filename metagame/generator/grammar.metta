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
    (-->  
      (game  $Game) 
      (,  
        { (game  $Game $Name $Board $Pieces $Goals $Constraints) } 
        (,  
          (game) 
          (,  
            (game_name  $Name) 
            (,  line 
              (,  
                (goal_defs  $Goals) 
                (,  line 
                  (,  
                    (board  $Board) 
                    (,  line 
                      (,  
                        (opt_constraints  $Constraints) 
                        (,  line 
                          (,  
                            (piece_defs  $Pieces) 
                            (,  
                              (end game) period))))))))))))) True)
; 



  (= 
    (-->  
      (game_name  $Game) 
      (,  
        ($Game) 
        { (is-symbol  $Game) })) True)
; 


  (= 
    (-->  
      (board  $B) 
      (,  
        { (,  
            (board  $B) 
            (,  
              (board_size  $B $XMax $YMax) 
              (,  
                (board_type  $B $Type) 
                (,  
                  (board_promote_rows  $B $Promote) 
                  (,  
                    (board_inversion  $B $Inversion) 
                    (board_assignments  $B $Assignments)))))) } 
        (,  
          (board_size) 
          (,  
            (number  $XMax) 
            (,  
              (by) 
              (,  
                (number  $YMax) 
                (,  line 
                  (,  
                    (board_type) 
                    (,  
                      (board_type  $Type) 
                      (,  line 
                        (,  
                          (opt_inversion  $Inversion) 
                          (,  
                            (promote_rank) 
                            (,  
                              (number  $Promote) 
                              (,  line 
                                (,  
                                  (setup) 
                                  (,  line 
                                    (assignment_list  $Assignments))))))))))))))))) True)
; 


  (= 
    (-->  
      (board_type  planar) 
      (planar)) True)
; 

  (= 
    (-->  
      (board_type  vertical_cylinder) 
      (vertical_cylinder)) True)
; 



  (= 
    (inversion_type  forward) True)
; 

  (= 
    (inversion_type  diagonal) True)
; 





  (= 
    (-->  
      (assignment_list  $A) 
      (|  
        (assignment_decision  $A) 
        (assignments  $A))) True)
; 



  (= 
    (-->  
      (assignments  
        ($A)) 
      (assignment  $A)) True)
; 

  (= 
    (-->  
      (assignments  
        (Cons  $A $As)) 
      (,  
        (assignment  $A) 
        (,  line 
          (assignments  $As)))) True)
; 


  (= 
    (-->  
      (assignment  
        (= $A $S)) 
      (,  
        (tab  5) 
        (,  
          (piece_name  $A) 
          (,  
            (at) 
            (square_list  $S))))) True)
; 


  (= 
    (-->  
      (assignment_decision  $D) 
      (,  
        { (,  
            (decision  $D) 
            (,  
              (decision_chooser  $D $C) 
              (,  
                (decision_options  $D $O) 
                (decision_constraints  $D $Con)))) } 
        (,  
          (tab  3) 
          (,  
            (decision) 
            (,  
              (assigner  $C) 
              (,  
                (assigns) 
                (,  
                  (piece_names  $O) 
                  (,  line 
                    (,  
                      (tab  5) 
                      (,  
                        (to) 
                        (,  
                          (square_list  $Con) 
                          (,  line 
                            (,  
                              (tab  3) 
                              (end decision)))))))))))))) True)
; 


  (= 
    (-->  
      (assigner  $C) 
      (player  $C)) True)
; 

  (= 
    (-->  
      (assigner  random) 
      (random)) True)
; 



;
; 

  (= 
    (-->  
      (piece_defs  ()) ()) True)
; 

  (= 
    (-->  
      (piece_defs  
        (Cons  $D $Defs)) 
      (,  
        (piece_def  $D) 
        (,  line 
          (piece_defs  $Defs)))) True)
; 



  (= 
    (-->  
      (piece_def  $Def) 
      (,  
        { (piece_definition  $Def $Name $Movement $Capture $Promote $Con) } 
        (,  
          (define) 
          (,  
            (piece_name  $Name) 
            (,  line 
              (,  
                (opt_moving  $Movement) 
                (,  
                  (opt_capturing  $Capture) 
                  (,  
                    (opt_promoting  $Name $Promote) 
                    (,  
                      (opt_constraints  $Con) 
                      (,  
                        (end define) line)))))))))) True)
; 



  (= 
    (-->  
      (opt_moving  ()) ()) True)
; 

  (= 
    (-->  
      (opt_moving  $Movement) 
      (,  
        (tab  3) 
        (,  
          (moving) 
          (,  line 
            (,  
              (movement_def  $Movement) 
              (,  line 
                (,  
                  (tab  3) 
                  (,  
                    (end moving) line)))))))) True)
; 
 

  (= 
    (-->  
      (opt_capturing  ()) ()) True)
; 

  (= 
    (-->  
      (opt_capturing  $Capture) 
      (,  
        (tab  3) 
        (,  
          (capturing) 
          (,  line 
            (,  
              (capture_def  $Capture) 
              (,  line 
                (,  
                  (tab  3) 
                  (,  
                    (end capturing) line)))))))) True)
; 
 

;
; 

  (= 
    (-->  
      (opt_promoting  $Name 
        (promote  $Name)) ()) True)
; 

  (= 
    (-->  
      (opt_promoting  $_ $Promote) 
      (,  
        (tab  3) 
        (,  
          (promoting) 
          (,  
            (promote_def  $Promote) 
            (,  line 
              (,  
                (tab  3) 
                (,  
                  (end promoting) line))))))) True)
; 
 



  (= 
    (-->  
      (opt_constraints  $Constraint) 
      (,  
        { (,  
            (constraint  $Constraint) 
            (,  
              (constraint_must_capture  $Constraint no) 
              (constraint_continue_captures  $Constraint no))) } ())) True)
; 
	
  (= 
    (-->  
      (opt_constraints  $Constraint) 
      (,  
        (tab  3) 
        (,  
          (constraints) 
          (,  
            (constraint_def  $Constraint) line)))) True)
; 
 


;
; 

  (= 
    (-->  
      (opt_inversion  diagonal) ()) True)
; 
	
  (= 
    (-->  
      (opt_inversion  $Inversion) 
      (,  
        (inversion) 
        (,  
          (inversion_def  $Inversion) line))) True)
; 
 


  (= 
    (-->  
      (inversion_def  $Inversion) 
      (,  
        { (inversion_type  $Inversion) } 
        ($Inversion))) True)
; 




  (= 
    (-->  
      (movement_def  
        ($M)) 
      (simple_movement  $M)) True)
; 

  (= 
    (-->  
      (movement_def  
        (Cons  $M $Ms)) 
      (,  
        (simple_movement  $M) 
        (,  line 
          (,  line 
            (movement_def  $Ms))))) True)
; 


  (= 
    (-->  
      (simple_movement  $M) 
      (,  
        { (,  
            (movement  $M) 
            (,  
              (movement_type  $M $T) 
              (,  
                (movement_dir  $M $D) 
                (movement_sym  $M $S)))) } 
        (,  
          (tab  15) 
          (,  
            (movement) 
            (,  line 
              (,  
                (movement_type  $T) 
                (,  line 
                  (,  
                    (gdirection  $D) 
                    (,  
                      (syms  $S) 
                      (,  line 
                        (,  
                          (tab  15) 
                          (end movement)))))))))))) True)
; 



  (= 
    (-->  
      (movement_type  $T) 
      (leaper  $T)) True)
; 

  (= 
    (-->  
      (movement_type  $T) 
      (rider  $T)) True)
; 

  (= 
    (-->  
      (movement_type  $T) 
      (hopper  $T)) True)
; 


  (= 
    (-->  
      (leaper  $L) 
      (,  
        { (leaper  $L) } 
        (,  
          (tab  20) 
          (leap)))) True)
; 


  (= 
    (-->  
      (rider  $R) 
      (,  
        { (,  
            (rider  $R) 
            (,  
              (rider_must  $R $Must) 
              (,  
                (rider_min  $R $Min) 
                (rider_max  $R $Max)))) } 
        (,  
          (tab  20) 
          (,  
            (ride) 
            (,  
              (min_dist  $Min) 
              (,  
                (max_dist  $Max) 
                (longest  $Must))))))) True)
; 


  (= 
    (-->  
      (min_dist  1) ()) True)
; 

  (= 
    (-->  
      (min_dist  $Min) 
      (,  
        (min) 
        (number  $Min))) True)
; 


  (= 
    (-->  
      (max_dist  any) ()) True)
; 
 
  (= 
    (-->  
      (max_dist  $N) 
      (,  
        (max) 
        (number  $N))) True)
; 


  (= 
    (-->  
      (longest  no) ()) True)
; 

  (= 
    (-->  
      (longest  yes) 
      (longest)) True)
; 



  (= 
    (-->  
      (hopper  $H) 
      (,  
        { (,  
            (hopper  $H) 
            (,  
              (hopper_before  $H $B) 
              (,  
                (hopper_over  $H $O) 
                (,  
                  (hopper_after  $H $A) 
                  (hopper_type  $H $R))))) } 
        (,  
          (tab  20) 
          (,  
            (hop) 
            (,  
              (before) 
              (,  
                (compare_eq  $B) 
                (,  
                  (over) 
                  (,  
                    (compare_eq  $O) 
                    (,  
                      (after) 
                      (,  
                        (compare_eq  $A) 
                        (,  line 
                          (,  
                            (tab  20) 
                            (,  
                              (hop_over) 
                              (description  $R)))))))))))))) True)
; 



;
; 

;
; 

;
; 


  (= 
    (-->  
      (syms  $Sym) 
      (,  
        { (symmetry  $Sym) } 
        (,  
          (symmetry) 
          (symmetry_set  $Sym)))) True)
; 


;
; 

  (= 
    (-->  
      (symmetry_set  $Sym) 
      (,  
        { (,  
            (sym_forward  $Sym yes) 
            (,  
              (sym_side  $Sym yes) 
              (sym_rotation  $Sym yes))) } 
        (all_symmetry))) True)
; 

  (= 
    (-->  
      (symmetry_set  $Sym) 
      (,  openbrace 
        (,  
          (sym_set  $Sym) closebrace))) True)
; 


  (= 
    (-->  
      (sym_set  $Sym) 
      (,  
        (forward  $Sym) 
        (,  
          (side  $Sym) 
          (rotation  $Sym)))) True)
; 



  (= 
    (-->  
      (forward  $Sym) 
      (,  () 
        { (sym_forward  $Sym no) })) True)
; 

  (= 
    (-->  
      (forward  $Sym) 
      (,  
        (forward) 
        { (sym_forward  $Sym yes) })) True)
; 


  (= 
    (-->  
      (side  $Sym) 
      (,  () 
        { (sym_side  $Sym no) })) True)
; 

  (= 
    (-->  
      (side  $Sym) 
      (,  
        (side) 
        { (sym_side  $Sym yes) })) True)
; 


  (= 
    (-->  
      (rotation  $Sym) 
      (,  
        (rotation) 
        { (sym_rotation  $Sym yes) })) True)
; 

  (= 
    (-->  
      (rotation  $Sym) 
      (,  
        { (sym_rotation  $Sym no) } ())) True)
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
    (-->  
      (compare_eq  $C) 
      (,  
        { (,  
            (comparison  $C $Comp $Number) 
            (,  
              (comparison_comp  $C $Comp) 
              (comparison_num  $C $Number))) } 
        (,  openb 
          (,  
            (x) 
            (,  
              (comparative  $Comp) 
              (,  
                (delta  $Number) closeb)))))) True)
; 


;
; 


  (= 
    (-->  
      (comparative  geq) 
      (>=)) True)
; 

  (= 
    (-->  
      (comparative  eq) 
      (=)) True)
; 

  (= 
    (-->  
      (comparative  leq) 
      (<=)) True)
; 



;
; 

;
; 

;
; 


  (= 
    (-->  
      (gdirection  $Dir) 
      (,  
        { (direction  $Dir $X $Y) } 
        (,  
          (tab  20) 
          (,  
            (<) 
            (,  
              (delta  $X) 
              (,  comma 
                (,  
                  (delta  $Y) 
                  (>)))))))) True)
; 
  

  (= 
    (-->  
      (square_list  $Squares) 
      (,  openbrace 
        (,  
          (squares  $Squares) closebrace))) True)
; 


  (= 
    (-->  
      (squares  
        ($H)) 
      (gsquare  $H)) True)
; 

  (= 
    (-->  
      (squares  
        (Cons  $H $T)) 
      (,  
        (gsquare  $H) 
        (squares  $T))) True)
; 


  (= 
    (-->  
      (gsquare  $Sq) 
      (,  
        { (,  
            (square  $Sq $X $Y) 
            (alpha_squares_mode  on)) } 
        (,  
          (() 
          (,  
            ($Col) 
            (,  
              { (nth_letter  $X $Col) } 
              (,  comma 
                (,  
                  (number  $Y) 
                  ())))))))) True)
; 
 

  (= 
    (-->  
      (gsquare  $Sq) 
      (,  
        { (,  
            (square  $Sq $X $Y) 
            (alpha_squares_mode  off)) } 
        (,  
          (() 
          (,  
            (number  $X) 
            (,  comma 
              (,  
                (number  $Y) 
                ()))))))) True)
; 
 


  (= 
    (-->  
      (delta  $Delta) 
      (number  $Delta)) True)
; 


  (= 
    (-->  
      (number  $N) 
      (,  
        ($N) 
        { (number  $N) })) True)
; 



;
; 

;
; 

;
; 


  (= 
    (-->  
      (capture_def  
        ($C)) 
      (simple_capture  $C)) True)
; 

  (= 
    (-->  
      (capture_def  
        (Cons  $C $Cs)) 
      (,  
        (simple_capture  $C) 
        (,  line 
          (,  line 
            (capture_def  $Cs))))) True)
; 


  (= 
    (-->  
      (simple_capture  $C) 
      (,  
        { (,  
            (capture  $C) 
            (,  
              (capture_movement  $C $M) 
              (,  
                (capture_methods  $C $T) 
                (,  
                  (capture_type  $C $R) 
                  (capture_effect  $C $E))))) } 
        (,  
          (tab  5) 
          (,  
            (capture by) 
            (,  
              (capture_methods  $T) 
              (,  line 
                (,  
                  (tab  14) 
                  (,  
                    (type) 
                    (,  
                      (description  $R) 
                      (,  line 
                        (,  
                          (tab  14) 
                          (,  
                            (effect) 
                            (,  
                              (effect  $E) 
                              (,  line 
                                (,  
                                  (movement_def  $M) 
                                  (,  line 
                                    (,  
                                      (tab  5) 
                                      (end capture)))))))))))))))))) True)
; 


  (= 
    (-->  
      (capture_methods  $M) 
      (,  
        { (method  $M) } 
        (,  openbrace 
          (,  
            (retrieve  $M) 
            (,  
              (clobber  $M) 
              (,  
                (hop  $M) closebrace)))))) True)
; 



  (= 
    (-->  
      (retrieve  $Method) 
      (,  () 
        { (method_retrieve  $Method no) })) True)
; 

  (= 
    (-->  
      (retrieve  $Method) 
      (,  
        (retrieve) 
        { (method_retrieve  $Method yes) })) True)
; 


  (= 
    (-->  
      (clobber  $Method) 
      (,  () 
        { (method_clobber  $Method no) })) True)
; 

  (= 
    (-->  
      (clobber  $Method) 
      (,  
        (clobber) 
        { (method_clobber  $Method yes) })) True)
; 


  (= 
    (-->  
      (hop  $Method) 
      (,  
        (hop) 
        { (method_hop  $Method yes) })) True)
; 

  (= 
    (-->  
      (hop  $Method) 
      (,  
        { (method_hop  $Method no) } ())) True)
; 
 


;
; 

;
; 

  (= 
    (-->  
      (effect  remove) 
      (remove)) True)
; 

  (= 
    (-->  
      (effect  
        (possess  $Player)) 
      ($Player possesses)) True)
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
    (-->  
      (goal_defs  $Goals) 
      (,  
        (goals) 
        (goals  $Goals))) True)
; 


  (= 
    (-->  
      (goals  ()) ()) True)
; 

  (= 
    (-->  
      (goals  
        (Cons  $G $Goals)) 
      (,  
        (simple_goal  $G) 
        (,  line 
          (,  
            (tab  5) 
            (goals  $Goals))))) True)
; 


  (= 
    (-->  
      (simple_goal  $Arrive) 
      (,  
        { (arrive_goal  $Arrive $Desc $Squares) } 
        (,  
          (arrive) 
          (,  
            (description  $Desc) 
            (,  
              (at) 
              (square_list  $Squares)))))) True)
; 


  (= 
    (-->  
      (simple_goal  $Eradicate) 
      (,  
        { (eradicate_goal  $Eradicate $Desc) } 
        (,  
          (eradicate) 
          (description  $Desc)))) True)
; 

	               
  (= 
    (-->  
      (simple_goal  $Stalemate) 
      (,  
        { (stalemate_goal  $Stalemate $Player) } 
        (,  
          (stalemate) 
          (player  $Player)))) True)
; 


;
; 

;
; 

;
; 


  (= 
    (-->  
      (description  $Desc) 
      (,  
        { (piece_description  $Desc $Player $Pieces) } 
        (,  openb 
          (,  
            (player_gen  $Player) 
            (,  
              (piece_names  $Pieces) closeb))))) True)
; 


  (= 
    (-->  
      (piece_names  any_piece) 
      (any_piece)) True)
; 

  (= 
    (-->  
      (piece_names  $Pieces) 
      (,  openbrace 
        (,  
          (identifiers  $Pieces) closebrace))) True)
; 


  (= 
    (-->  
      (identifiers  
        ($Piece)) 
      (piece_name  $Piece)) True)
; 

  (= 
    (-->  
      (identifiers  
        (Cons  $P $Pieces)) 
      (,  
        (piece_name  $P) 
        (identifiers  $Pieces))) True)
; 



  (= 
    (-->  
      (piece_name  $Piece) 
      (,  
        ($Piece) 
        { (is-symbol  $Piece) })) True)
; 



  (= 
    (-->  
      (player_gen  $Player) 
      (,  openbrace 
        (,  
          (player  $Player) closebrace))) True)
; 

  (= 
    (-->  
      (player_gen  any_player) 
      (any_player)) True)
; 


  (= 
    (-->  
      (player  player) 
      (player)) True)
; 

  (= 
    (-->  
      (player  opponent) 
      (opponent)) True)
; 



  (= 
    (player  player) True)
; 

  (= 
    (player  opponent) True)
; 



;
; 

;
; 

;
; 



  (= 
    (-->  
      (constraint_defs  $Constraint) 
      (,  
        { (,  
            (constraint  $Constraint) 
            (,  
              (constraint_must_capture  $Constraint no) 
              (constraint_continue_captures  $Constraint no))) } ())) True)
; 
	
  (= 
    (-->  
      (constraint_defs  $Constraint) 
      (,  
        (constraints) 
        (constraint_def  $Constraint))) True)
; 




  (= 
    (-->  
      (constraint_def  $Constraint) 
      (,  
        { (constraint  $Constraint) } 
        (,  
          (must_capture  $Constraint) 
          (continue_captures  $Constraint)))) True)
; 


  (= 
    (-->  
      (must_capture  $Constraint) 
      (,  
        { (constraint_must_capture  $Constraint no) } ())) True)
; 
 
  (= 
    (-->  
      (must_capture  $Constraint) 
      (,  
        (must_capture) 
        { (constraint_must_capture  $Constraint yes) })) True)
; 


  (= 
    (-->  
      (continue_captures  $Constraint) 
      (,  () 
        { (constraint_continue_captures  $Constraint no) })) True)
; 

  (= 
    (-->  
      (continue_captures  $Constraint) 
      (,  
        { (constraint_continue_captures  $Constraint yes) } 
        (continue_captures))) True)
; 




;
; 

;
; 


  (= 
    (-->  
      (promote_def  $Prom) 
      (,  
        { (decision  $Prom) } 
        (promotion_decision  $Prom))) True)
; 

  (= 
    (-->  
      (promote_def  
        (promote  $Prom)) 
      (,  
        (promote_to) 
        (piece_name  $Prom))) True)
; 



;
; 

;
; 


  (= 
    (-->  
      (promotion_decision  $D) 
      (,  
        { (,  
            (decision  $D) 
            (,  
              (decision_chooser  $D $C) 
              (decision_options  $D $O))) } 
        (,  
          (decision) 
          (,  
            (player  $C) 
            (,  line 
              (,  
                (tab  10) 
                (,  
                  (options) 
                  (description  $O)))))))) True)
; 




  (= 
    (-->  line 
      (,  
        { (parsing_mode  printing) } 
        (line))) True)
; 

  (= 
    (-->  line 
      (,  
        { (parsing_mode  parsing) } ())) True)
; 


  (= 
    (-->  
      (tab  $T) 
      (,  
        { (parsing_mode  printing) } 
        ( (tab  $T)))) True)
; 

  (= 
    (-->  
      (tab  $T) 
      (,  
        { (parsing_mode  parsing) } ())) True)
; 


  (= 
    (-->  
      (true  any) ()) True)
; 



  (= 
    (-->  semi 
      (;)) True)
; 


  (= 
    (-->  colon 
      (:)) True)
; 


  (= 
    (-->  comma 
      (,)) True)
; 


  (= 
    (-->  openp 
      (()) True)
; 


  (= 
    (-->  closep 
      ())) True)
; 


  (= 
    (-->  openb 
      ([)) True)
; 


  (= 
    (-->  closeb 
      (])) True)
; 


  (= 
    (-->  openbrace 
      ({)) True)
; 


  (= 
    (-->  closebrace 
      (})) True)
; 


  (= 
    (-->  period 
      (.)) True)
; 



  (= 
    (-->  identifier 
      (,  
        ($X) 
        { (is-symbol  $X) })) True)
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
    (assignments-string $Assignment $String) 
    ( (var $String) 
      (set-det) 
      (assignments $Assignment $Tokens 
        (Cons  . $Rest)) 
      (print-tokens-to-string $Tokens $String)))
; 

  (= 
    (assignments-string $Assignment $String) 
    ( (var $Assignment) 
      (read-tokens-from-string $String $Tokens) 
      (assignments $Assignment $Tokens 
        (Cons  . $Rest))))
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
    (portray-square (square $X $Y)) 
    ( (with-alpha-squares (gsquare (square $X $Y) $S Nil)) (print-tokens $S)))
; 



  (= 
    (portray-player player) 
    (write white))
; 

  (= 
    (portray-player opponent) 
    (write black))
; 



  (= 
    (portray-piece (piece $A $B)) 
    ( (piece 
        (piece $A $B) $S Nil) (print-tokens $S)))
; 



  (= 
    (portray-moving (move $Piece $Player $From $To)) 
    ( (moving 
        (move $Piece $Player $From $To) $S Nil) (print-tokens $S)))
; 

	

  (= 
    (portray-game (game $Name $Board $Pieces $Goals $Constraints)) 
    (format "<Game: ~w>" 
      (:: $Name)))
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
    (gen $L) 
    ( (generate-game $G) (game $G $L $_)))
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
    (random-game-to-file $File) 
    (print-gen-game-to-file $File))
; 

	


  (= 
    (print-gen-game) 
    ( (record-seed) 
      (gen $G) 
      (print-tokens $G)))
; 



  (= 
    (print-gen-game-to-file $File) 
    ( (record-seed) 
      (generate-game $G) 
      (set-printing-mode) 
      (game $G $GameList $_) 
      (print-game-to-file $GameList $File) 
      (set-parsing-mode)))
; 



  (= 
    (print-game-struct $G) 
    ( (set-printing-mode) 
      (game $G $GameList $_) 
      (set-parsing-mode) 
      (print-tokens $GameList)))
; 

	


  (= 
    (prettify-game $GameList $Pretty) 
    ( (set-parsing-mode) 
      (game $G $GameList $_) 
      (set-printing-mode) 
      (game $G $Pretty) 
      (set-parsing-mode)))
; 

	

  (= 
    (pretty-print-game-to-file $GameList $File) 
    ( (prettify-game $GameList $Pretty) (print-game-to-file $Pretty $File)))
; 



;
; 

;
; 


  (= 
    (print-game-to-file $Game $File) 
    ( (sys-suffixed-filename $File game $GameFile) 
      (format "~nWriting game to file: ~w~n" 
        (:: $GameFile)) 
      (tell $GameFile) 
      (write-old-seed) 
      (print-tokens $Game) 
      (told)))
; 


;
; 

;
; 


  (= 
    (write-old-seed) 
    ( (old-seed $Seed) 
      (set-det) 
      (nl) 
      (write '% seed: ') 
      (write $Seed) 
      (nl)))
; 

  (= write_old_seed True)
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
    (print-read-game $File) 
    ( (format "~nReading game from file~n" Nil) 
      (read-game-from-file-to-list $File $Game) 
      (format "~nRead game from file~n" Nil) 
      (set-parsing-mode) 
      (format "~nParsing game in parse mode~n" Nil) 
      (game $G $Game Nil) 
      (set-printing-mode) 
      (format "~nParsing game in print mode~n" Nil) 
      (game $G $Game1 Nil) 
      (format "~nPrinting game~n" Nil) 
      (print-tokens $Game1)))
; 




  (= 
    (read-game-from-file-to-list $File $Game) 
    ( (format "~nReading game from file: ~w~n" 
        (:: $File)) (read-tokens-from-file $File $Game)))
; 



  (= 
    (read-game-from-string-to-list $String $Game) 
    ( (format "~nReading game from string. ~n" Nil) (read-tokens-from-string $String $Game)))
; 




  (= 
    (read-game-to-list $Game) 
    (read-tokens $Game))
; 

	


