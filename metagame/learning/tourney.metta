;                                           (convert_to_metta_file  tourney $_154582 metagame/learning/tourney.pl metagame/learning/tourney.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; tourney.pl
;
; ================================================================================
;
; Players for a tournament
;
; ================================================================================

;
; HOSTTIME_RANDOMIZE
;
; A hacky and expensive way to use the real time
;
; and name of the host to initalize the random seed,
;
; for when experiments are done on multiple machines.

  (= (hosttime-randomize)  
    (hosttime-random-count-mod $X)
    (dotimes $X 
      (not (, (random $_) (fail)))))


  (= (hosttime-random-count $Count)  
    (realtime-seconds $S)
    (shell 
      (:: 'echo %%') $Process)
    (is $Sum 
      (+ $S $Process))
    (random 1 $Sum $Count))


  (= (hosttime-random-count-mod $X)  
    (hosttime-random-count $Count)
    (is $X 
      (mod $Count 200)))

	



		      

  (= (do-tourney $Name $File)  
    (tourney-setting $Name $Set)
    (tourney-to-file $File $Set))


;
; Start with a different random seed for each process running a tourney
;
; to avoid duplication.

  (= (tourney-to-file $File $GameMatches)  
    (hosttime-randomize)
    (with-output-file $File append 
      (tourney $GameMatches $LogFile))
    (format "Tourney Done!~n" Nil))

;
; File 2nd arg not used now. Perhaps for log file later.

  (= (tourney Nil $_)  
    (format "Tourney done!~n" Nil)
    (current-output $O)
    (flush-output $O))
  (= (tourney (Cons  (- $Game $Matches) $Rest) $File)  
    (game-matches $Game $Matches $File)
    (tourney $Rest $File))
	

  (= (game-matches $Game $Matches $File)  
    (setup-game $Game)
    (play-matches $Matches))


  (= (setup-game $Game)  
    (load-game $Game)
    (evalfile-top static)
    (build-tables))
	



  (= (play_matches  ())  True)
  (= (play-matches (Cons  $M $Matches))  
    (play-match $M)
    (play-matches $Matches))



  (= (play-match (match $White $Black $Number))  
    (load-player player $White)
    (load-player opponent $Black)
    (dotimes $Number 
      (contest $White $Black)))


;
; Sets the params and search method for a numbered player.

  (= (load-player $Role $Name)  
    (player-setting $Name $Search $Params)
    (set-role-file $Role $Params)
    (player-color $Role $Color)
    (player-top $Color $Search))
	

  (= (contest $White $Black)  
    (format "<~p> vs. <~p>~n" 
      (:: $White $Black))
    (record-seed)
    (write-old-seed %%)
    (start)
    (process-contest-outcome $White $Black))


  (= (process-contest-outcome $White $Black)  
    (player-current-game $G)
    (game-name $G $GameName)
    (det-if-then-else 
      (recorded-game-outcome $Role) 
      (, 
        (process-contest-outcome $Role $White $Black $Outcome) 
        (format "Outcome of <~p> -- <~p> vs. <~p>: <~p>~n" 
          (:: $GameName $White $Black $Outcome))) 
      (format "Outcome of <~p> -- <~p> vs. <~p>: <~p>~n" 
        (:: $GameName $White $Black NOTHING))))

  (= (process_contest_outcome  player $White $Black $White)  True)
  (= (process_contest_outcome  opponent $White $Black $Black)  True)
  (= (process_contest_outcome  draw $_ $_ draw)  True)
	  
	  




  (= (tourney-test)  
    (tourney-setting test $Set)
    (tourney $Set))



  (= (tourney_details  g1 game1 thesis)  True)
  (= (tourney_details  g2 game2 thesis)  True)
  (= (tourney_details  g3 game3 thesis)  True)
  (= (tourney_details  g4 game4 thesis)  True)
  (= (tourney_details  g5 game5 thesis)  True)

  (= (tourney_details  g123 game1 23)  True)
  (= (tourney_details  g124 game1 24)  True)
  (= (tourney_details  g134 game1 34)  True)


  (= (tourney_details  g101 game1 1)  True)
  (= (tourney_details  g102 game1 2)  True)
  (= (tourney_details  g103 game1 3)  True)
  (= (tourney_details  g104 game1 4)  True)

  (= (tourney_details  g201 game2 1)  True)
  (= (tourney_details  g202 game2 2)  True)
  (= (tourney_details  g203 game2 3)  True)
  (= (tourney_details  g204 game2 4)  True)

  (= (tourney_details  g301 game3 1)  True)
  (= (tourney_details  g302 game3 2)  True)
  (= (tourney_details  g303 game3 3)  True)
  (= (tourney_details  g304 game3 4)  True)

  (= (tourney_details  g401 game4 1)  True)
  (= (tourney_details  g402 game4 2)  True)
  (= (tourney_details  g403 game4 3)  True)
  (= (tourney_details  g404 game4 4)  True)


  (= (tourney_details  g501 game5 1)  True)
  (= (tourney_details  g502 game5 2)  True)
  (= (tourney_details  g503 game5 3)  True)
  (= (tourney_details  g504 game5 4)  True)



  (= (tourney_details  g423 game4 23)  True)
  (= (tourney_details  g424 game4 24)  True)
  (= (tourney_details  g434 game4 34)  True)



;
; playing random player against everyone on every game
;
; separately, 10 games each color.
  (= (tourney_details  g1r0 game1 r0)  True)
  (= (tourney_details  g1r1 game1 r1)  True)
  (= (tourney_details  g1r2 game1 r2)  True)
  (= (tourney_details  g1r3 game1 r3)  True)
  (= (tourney_details  g1r4 game1 r4)  True)

  (= (tourney_details  g2r0 game2 r0)  True)
  (= (tourney_details  g2r1 game2 r1)  True)
  (= (tourney_details  g2r2 game2 r2)  True)
  (= (tourney_details  g2r3 game2 r3)  True)
  (= (tourney_details  g2r4 game2 r4)  True)

  (= (tourney_details  g3r0 game3 r0)  True)
  (= (tourney_details  g3r1 game3 r1)  True)
  (= (tourney_details  g3r2 game3 r2)  True)
  (= (tourney_details  g3r3 game3 r3)  True)
  (= (tourney_details  g3r4 game3 r4)  True)

  (= (tourney_details  g4r0 game4 r0)  True)
  (= (tourney_details  g4r1 game4 r1)  True)
  (= (tourney_details  g4r2 game4 r2)  True)
  (= (tourney_details  g4r3 game4 r3)  True)
  (= (tourney_details  g4r4 game4 r4)  True)

  (= (tourney_details  g5r0 game5 r0)  True)
  (= (tourney_details  g5r1 game5 r1)  True)
  (= (tourney_details  g5r2 game5 r2)  True)
  (= (tourney_details  g5r3 game5 r3)  True)
  (= (tourney_details  g5r4 game5 r4)  True)


  (= (tourney_details  g1r0a game1 r0a)  True)

  (= (tourney_details  g1r1a game1 r1a)  True)

  (= (tourney_details  g1r2a game1 r2a)  True)

  (= (tourney_details  g1r3a game1 r3a)  True)

  (= (tourney_details  g1r4a game1 r4a)  True)

  (= (tourney_details  g2r02 game1 r02)  True)

  (= (tourney_details  g3r4a game3 r4a)  True)

  (= (tourney_details  g5a game5 thesis2)  True)




  (= (matches_for_setting  r02 ((match  r 0 2)))  True)


  (= (matches_for_setting  r0a ((match  r 0 10)))  True)

  (= (matches_for_setting  r1a ((match  r 1 10)))  True)

  (= (matches_for_setting  1 ((match  0 1 10)))  True)
  (= (matches_for_setting  2 ((match  0 2 10)))  True)
  (= (matches_for_setting  3 ((match  0 3 10)))  True)
  (= (matches_for_setting  4 ((match  0 4 10)))  True)


  (= (matches_for_setting  r2a ((match  r 2 10)))  True)


  (= (matches_for_setting  r3a ((match  r 3 10)))  True)


  (= (matches_for_setting  r4a ((match  r 4 10)))  True)


  (= (matches_for_setting  thesis ((match  0 1 1) (match  1 0 1) (match  0 2 1) (match  2 0 1) (match  0 3 1) (match  3 0 1) (match  0 4 1) (match  4 0 1) (match  1 2 1) (match  2 1 1) (match  1 3 1) (match  3 1 1) (match  1 4 1) (match  4 1 1) (match  2 3 1) (match  3 2 1) (match  2 4 1) (match  4 2 1) (match  3 4 1) (match  4 3 1) (match  0 0 2)))  True)


  (= (matches_for_setting  thesis2 ((match  4 3 1) (match  3 4 1) (match  4 2 1) (match  2 4 1) (match  2 3 1) (match  3 2 1) (match  4 1 1) (match  1 4 1)))  True)


  (= (matches_for_setting  23 ((match  2 3 2) (match  3 2 2)))  True)

  (= (matches_for_setting  24 ((match  2 4 2) (match  4 2 2)))  True)


  (= (matches_for_setting  34 ((match  3 4 2) (match  4 3 2)))  True)


  (= (matches_for_setting  r0 ((match  0 r 10) (match  r 0 10)))  True)

  (= (matches_for_setting  r1 ((match  1 r 10) (match  r 1 10)))  True)

  (= (matches_for_setting  r2 ((match  2 r 10) (match  r 2 10)))  True)

  (= (matches_for_setting  r3 ((match  3 r 10) (match  r 3 10)))  True)

  (= (matches_for_setting  r4 ((match  4 r 10) (match  r 4 10)))  True)




  (= (tourney-setting $Tourney (:: (- $Game $Matches)))  
    (tourney-details $Tourney $Game $MatchName)
    (set-det)
    (matches-for-setting $MatchName $Matches))

  (= (tourney_setting  test ((-  checkers ((match  1 0 1) (match  0 0 1))) (-  turncoat_chess ((match  0 0 1)))))  True)



;
; Just a random player.

  (= (player_setting  r random ())  True)


;
; Just a random_aggressive player.
  (= (player_setting  0 random_aggressive ())  True)


;
; Everything, and pthreat.
  (= (player_setting  1 iterate ((-  gmovmob 1) (-  gcapmob 1) (-  pthreat 1) (-  gthreat 0) (-  initprom 1) (-  possess 1) (-  arrive_distance 100) (-  promote_distance 1) (-  eventual_mobility 1) (-  static 1) (-  vital 1) (-  max_static_mob 1) (-  max_eventual_mob 1) (-  eradicate 1) (-  victims 1) (-  immunity 1) (-  giveaway 1) (-  eradicate 1) (-  stalemate 1) (-  arrive 1)))  True)
; ;; statics



;
; Just emob, promdist, arrivedist, and static.
  (= (player_setting  2 iterate ((-  gmovmob 0) (-  gcapmob 0) (-  pthreat 0) (-  gthreat 0) (-  lthreat 0) (-  vital 0) (-  initprom 1) (-  possess 1) (-  arrive_distance 100) (-  promote_distance 1) (-  eventual_mobility 1) (-  static 1) (-  max_static_mob 1) (-  max_eventual_mob 1) (-  eradicate 1) (-  victims 1) (-  immunity 1) (-  giveaway 1) (-  eradicate 1) (-  stalemate 1) (-  arrive 1)))  True)
; ;; statics



;
; Just emob and static.
  (= (player_setting  3 iterate ((-  gmovmob 0) (-  gcapmob 0) (-  pthreat 0) (-  gthreat 0) (-  lthreat 0) (-  vital 0) (-  initprom 0) (-  possess 0) (-  arrive_distance 0) (-  promote_distance 0) (-  eventual_mobility 1) (-  static 1) (-  max_static_mob 1) (-  max_eventual_mob 1) (-  eradicate 1) (-  victims 1) (-  immunity 1) (-  giveaway 1) (-  eradicate 1) (-  stalemate 1) (-  arrive 1)))  True)
; ;; statics



;
; Just promdist and arrivedist
  (= (player_setting  4 iterate ((-  gmovmob 0) (-  gcapmob 0) (-  pthreat 0) (-  gthreat 0) (-  lthreat 0) (-  vital 0) (-  initprom 0) (-  possess 0) (-  arrive_distance 100) (-  promote_distance 1) (-  eventual_mobility 0) (-  static 0) (-  max_static_mob 1) (-  max_eventual_mob 1) (-  eradicate 1) (-  victims 1) (-  immunity 1) (-  giveaway 1) (-  eradicate 1) (-  stalemate 1) (-  arrive 1)))  True)
; ;; statics



;
; Just dynamic mobility.
  (= (player_setting  5 iterate ((-  gmovmob 1) (-  gcapmob 0) (-  pthreat 0) (-  gthreat 0) (-  lthreat 0) (-  vital 0) (-  initprom 0) (-  possess 0) (-  arrive_distance 0) (-  promote_distance 0) (-  eventual_mobility 0) (-  static 0) (-  max_static_mob 1) (-  max_eventual_mob 1) (-  eradicate 1) (-  victims 1) (-  immunity 1) (-  giveaway 1) (-  eradicate 1) (-  stalemate 1) (-  arrive 1)))  True)
; ;; statics




;
; Everything, and gthreat.
  (= (player_setting  6 iterate ((-  gmovmob 1) (-  gcapmob 1) (-  pthreat 0) (-  gthreat 1) (-  initprom 1) (-  possess 1) (-  arrive_distance 100) (-  promote_distance 1) (-  eventual_mobility 1) (-  static 1) (-  vital 1) (-  max_static_mob 1) (-  max_eventual_mob 1) (-  eradicate 1) (-  victims 1) (-  immunity 1) (-  giveaway 1) (-  eradicate 1) (-  stalemate 1) (-  arrive 1)))  True)
; ;; statics


;
; Everything, and lthreat.
  (= (player_setting  7 iterate ((-  gmovmob 1) (-  gcapmob 1) (-  pthreat 0) (-  gthreat 0) (-  lthreat 1) (-  initprom 1) (-  possess 1) (-  arrive_distance 100) (-  promote_distance 1) (-  eventual_mobility 1) (-  static 1) (-  vital 1) (-  max_static_mob 1) (-  max_eventual_mob 1) (-  eradicate 1) (-  victims 1) (-  immunity 1) (-  giveaway 1) (-  eradicate 1) (-  stalemate 1) (-  arrive 1)))  True)
; ;; statics


;
; ==============================================================================
;
; Interface
;
; ==============================================================================


  (= (playernum-top $Color $Name)  
    (player-color $Role $Color)
    (load-player $Role $Name))


  (= (playernums-top $Player $Opp)  
    (playernum-top white $Player)
    (playernum-top black $Opp))	

