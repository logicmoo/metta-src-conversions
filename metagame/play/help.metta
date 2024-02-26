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
    (help-top) 
    ( (help-top-general) 
      (help-tables) 
      (help-system)))
; 



  (= 
    (help-top $F) 
    ( (help-top-entry $F $G) (call $G)))
; 

  (= 
    (help-top help) 
    ( (help-top-topics $Topics) 
      (format "To select a topic, type 'help <Topic>.'~n" Nil) 
      (format "Help in this menu is available on the following topics:~n" Nil) 
      (pwl $Topics)))
; 

	

  (= 
    (help-top-topics $Topics) 
    ( (findall $Topic 
        (help-top-entry $Topic $_) $AllTopics) (sort $AllTopics $Topics)))
; 



;
; 

;
; 


  (= 
    (help_top_entry  player help_player) True)
; 

  (= 
    (help_top_entry  show help_show) True)
; 

  (= 
    (help_top_entry  trace help_trace) True)
; 

  (= 
    (help_top_entry  cd help_cd) True)
; 

  (= 
    (help_top_entry  iterate help_iterate) True)
; 

  (= 
    (help_top_entry  weights help_weights) True)
; 

  (= 
    (help_top_entry  eval_fn help_eval_fn) True)
; 

  (= 
    (help_top_entry  evalfile help_evalfile) True)
; 

  (= 
    (help_top_entry  history help_history) True)
; 
 
  (= 
    (help_top_entry  clock help_clock) True)
; 
 
  (= 
    (help_top_entry  tables help_tables) True)
; 

  (= 
    (help_top_entry  advisors help_advisors) True)
; 

  (= 
    (help_top_entry  playernum help_playernum) True)
; 



;
; 



  (= 
    (help-top-general) 
    (format "\nGenerating and Playing a Game:\n------------------------------\ngen.                 => enters menu for generating random games.\ngames_library.       => show games in library\ngame <file>.         => loads <file>.game as the current game\nevalfile <file>.     => loads <file>.eval as eval tables for current game\nplayer <c> <p>.      => set player <p> to play color <c> ('help player.')\nplayers <w> <b>.     => set player <w> as white, <b> as black.\nplayers.             => shows current players. \nstart.               => start playing current game from initial position\nstart <posname>.     => start playing current game from saved position <posname>\n\t                ('help history')\nclock.               => prints current game clock (help clock)\n" Nil))
; 




  (= 
    (help-tables) 
    (format "\nConstructing and Using Analysis Tables\n--------------------------------------\nbuild.               => builds tables using current advisors. \nshowstatic.          => shows the static table values for all pieces.\nadvice <c> <p>.      => shows static advice for piece <p> of color <c>.\nalladvisors.         => shows current weights for all advisors.\nactive.              => shows weights for only non-zero weighted advisors\nplayernum <c> <n>.   => set numbered player <n> to play color <c> ('help playernum.')\nplayernums <w> <b>.  => set numbered player <w> as white, <b> as black.\nhelp advisors.       => more info on the advisors and what they do. \n" Nil))
; 






  (= 
    (help-system) 
    ( (help-game) (help-state)))
; 



  (= 
    (help-game) 
    (format "\nExamining Game\n--------------\npieces.              => show the names of the pieces in the current game\ndefine <piece>.      => print the definition of <piece> in the current game\ngoals.               => print the goals of the current game\nboard.               => print board definition of the current game\nrules.               => print the full rules of the current game\n" Nil))
; 




  (= 
    (help-state) 
    (format "\t\nChanging System State\n---------------------\nset <p> <v>.         => set parameter <p> to value <v>  (set global default)\nset <c> <p> <v>.     => set parameter <p> to value <v>, for player <c> only\nunset <c>            => clears params specific to player <c> \nset.                 => show parameter settings\nrandomize <N>        => use random seed #<N> (N = 1..10)\nshow {options}       => show configuration data ('help show.') \n(un)trace {options}  => trace some system behavior ('help trace.') \ncd <dir>.            => change current directory to <dir> ('help cd.')\npwd.                 => show current directory name\nls.                  => show contents of current directory\nprolog. (abort)      => abort to prolog\nquit.                => exit session (back to shell)\nhelp help.           => list of all additional help topics. \n" Nil))
; 





  (= 
    (help-playernum) 
    (format "See the file learning/tourney.pl for more info.~n" Nil))
; 
 


  (= 
    (help-player) 
    ( (help-player-color) 
      (help-player-options) 
      (help-player-example)))
; 

	

  (= 
    (help-player-color) 
    (format "\nplayer <color> <player>.\n------------------------\nTells the system how the moves will be chosen for color <color>.\n\n<color> must be one of the following:\n\nwhite           => the first player.\nblack           => the second player.\n" Nil))
; 



  (= 
    (help-player-options) 
    ( (help-player-options-simple) (help-player-options-search)))
; 



  (= 
    (help-player-options-simple) 
    (format "\n<player> must be one of the following:\n\nSimple Players\n--------------\nhuman              => chosen by a human using this interface.\nrandom             => play a random legal move.\ninstant            => play first legal move available (faster)\ncautious           => plays first non-losing move available (faster)\nrandom_cautious    => play random non-losing move \nrandom_aggressive  => wins if can, else plays cautiously\n" Nil))
; 



  (= 
    (help-player-options-search) 
    (format "\nSearching Players\n-----------------\nalpha_beta         => does alpha_beta search down to a fixed depth \n                      (specified by <depth> parameter).  \n                      uses principal continuation heuristic.\n                      Bounded by same time limits and move ordering as\n                      iterate player.\niterate            => iterative deepening alpha-beta search.\n                      uses either fixed or random ordering based on <ordering> \n                      parameter.\n                      ('help iterate' for more info on this)\niterate_random     => iterate player with random move ordering when all equal.\n                      ('help iterate' for more info on this)\niterate_fixed      => iterate player with fixed ordering.\n" Nil))
; 




  (= 
    (help-player-example) 
    (format "\nExample:\n\n     player white human.\n     player black random_aggressive.\n\nBoth players can be specified at once by the command:\n    player <white_player> <black_player>.\n\n" Nil))
; 




  (= 
    (help-cd) 
    (format "\ncd <dir>.          => change current directory to <dir> \n---------\n\n<dir> must be an atom or between single quotes:\nExample:\n\n     cd mygames.\n     cd '~~/metagame/games/'.\n" Nil))
; 



;
; 

;
; 

;
; 



  (= 
    (help-com $_ $_ $_) 
    (help-commands))
; 



  (= 
    (help-com $_ $_ $_ $F) 
    ( (help-com-entry $F $G) (call $G)))
; 

  (= 
    (help-com $_ $_ $_ help) 
    ( (help-com-topics $Topics) 
      (format "To select a topic, type 'help <Topic>.'~n" Nil) 
      (format "Help in this menu is available on the following topics:~n" Nil) 
      (pwl $Topics)))
; 

	

  (= 
    (help-com-topics $Topics) 
    ( (findall $Topic 
        (help-com-entry $Topic $_) $AllTopics) (sort $AllTopics $Topics)))
; 



  (= 
    (help_com_entry  notation help_move) True)
; 

  (= 
    (help_com_entry  move help_move) True)
; 

  (= 
    (help_com_entry  clock help_clock) True)
; 

  (= 
    (help_com_entry  query help_query) True)
; 

  (= 
    (help_com_entry  show help_show) True)
; 

  (= 
    (help_com_entry  trace help_trace) True)
; 

  (= 
    (help_com_entry  cd help_cd) True)
; 

  (= 
    (help_com_entry  iterate_random help_iterate) True)
; 

  (= 
    (help_com_entry  iterate help_iterate) True)
; 

  (= 
    (help_com_entry  advice help_advice) True)
; 

  (= 
    (help_com_entry  advisors help_advisors) True)
; 

  (= 
    (help_com_entry  weights help_weights) True)
; 

  (= 
    (help_com_entry  eval_fn help_eval_fn) True)
; 

  (= 
    (help_com_entry  evalfile help_evalfile) True)
; 

  (= 
    (help_com_entry  history help_history) True)
; 





  (= 
    (help-commands) 
    ( (help-com-entry) 
      (help-com-selecting) 
      (help-system) 
      (help-com-state) 
      (help-syntax)))
; 



  (= 
    (help-com-selecting) 
    (format "\nSPECIAL MOVE SELECTION METHODS:\n-------------------------------\nselect.            => backtrack through available moves\nrandom.            => play a random legal move\nrandom_aggressive  => play a winning, or random non-losing move, in that order\nvictor.            => play a move which wins immediately\nendgame.           => play a move which ends the game immediately\ncautious.          => play a move which blocks opponent's victory, if threatened\nmate.              => play a move which forces a win in 2-ply\nthreaten.          => play a move which threatens victory next move\ninstant.           => play the first move generated (doesn't ask)\nalpha_beta {<n>}.  => does n-ply alpha-beta search (or DEPTH parameter if no n)\niterate.           => does iterative-deepening search until timeout. \nadvice.            => shows comments used in eval fn on current position (help)\nadvice <x> <y>     => shows local advice for piece now at (x,y)\nevaluate.          => evaluate current position using current parameters. \n" Nil))
; 




  (= 
    (help-com-state) 
    (format "\nEXAMINING AND MODIFYING STATE OF GAME\n-------------------------------------\ndisplay.        => print current state\nclock.          => prints current game clock (help clock)\nquery.          => computes some function on the current position (help query)\nsetup.          => enters menu for setting up board positions\npass.           => transfer control to the other player (ie null move)\naccess.         => access state from a command level\ncheckpoint <n>. => record current state under name <n> (for debugging)\nrestore <n>.    => sets current state to that checkpointed as name <n> \nrestart.        => abandon current game, choose new game and players\nevalfile <file>.=> loads <file>.eval as eval tables for current game\n                   (help evalfile)\nnext (prev) <n> => goto next (or previous) <n>th position (help history)\n" Nil))
; 




  (= 
    (help-com-entry) 
    (format "\nBASIC MOVE ENTRY: \n-----------------\n<Move>          => plays move <Move> ('help notation' for more information)\n" Nil))
; 



  (= 
    (help-syntax) 
    (format "\nNote that all keyboard input can contain newlines, and that a period\nsignals the end of the input/command.\n" Nil))
; 





  (= 
    (help-move) 
    ( (help-move-basic) (help-move-completion)))
; 



  (= 
    (help-move-basic) 
    (format "\nBASIC MOVE ENTRY: \n-----------------\nThis follows the grammatical notation for moves, \nillustrated as follows:\n\nBasic Movements, eg:\n     white king (5,1) -> (5,2). \n\nBasic Movements with a removal capture, eg:\n     white king (5,1) -> (5,2) x black rook (4,1).\n\nPossession captures indicate player who will possess:\n     white king (5,1) -> (5,2) x black rook (4,1) (white).\n\nMultiple Captures, eg: (if bug captures <-1,0> by {retrieve clobber})\n     white bug (4,1) -> (3,1) x white fish (3,1) black bug (5,1).\n\nContinued Captures, eg:\n     white checker (3,3) -> (5,5) x black checker (4,4);\n     white checker (5,5) -> (3,7) x black king (4,6).\n\nPlacing a piece from a player's hand, eg:\n     white king (white) -> (5,1).\n\nPromoting a piece which moved to or past the promote_rank.\n  If player promotes, this happens at end of his turn:\n     white pawn (2,7) -> (2,8); promote (2,8) white queen.\n  If opponent promotes, this happens at start of his turn:\n     promote (2,8) black queen; black queen (2,8) -> (2,5).\n\n" Nil))
; 



  (= 
    (help-move-completion) 
    (format "\nMOVE COMPLETION: (when COMPLETIONS parameter is ON)\n----------------\nInstead of the full grammatical move notation, you can enter\na sequence of words which occurs in the complete move notation.\nThis will match all moves containing those words in that order,\npossibly with other words in-between.\n\nUnlike the mode for entry of full grammatical move notations,\nsquares are here refered to in a more convenient form:\n\t(X,Y)\nwhere X is the letter for that column, and Y is the number for the row.\n\nSome examples of moves to be completed are:\n\npawn.            => completes to moves involving a pawn.\nwhite (c,3) ->.  => completes to moves moving a white piece from (c,3).\nx.               => completes to moves which capture something.\nx rook.          => completes to moves which capture a rook.\npromote.         => completes to moves which enter promotion zone.\n.                => completes to any legal moves.\n" Nil))
; 




  (= 
    (help-advice) 
    (format "\nadvice\n------\nPrints the advice regarding the current position, from all active\nadvisors.  This advice will be weighted according to the values of the\nparamters to determine the overall evaluation of the position. \n\nadvice <row> <col>\n------------------\nGives only the local advice for the piece at <row> <col> in the current\nposition.  \n\nExample:\n\tadvice f 4. ==> Will give local advice for whatever piece is there.\n        advice.     ==> Gives all advice, including global advice.  \n\nhelp advisors    ==> Gives more info on what advisors do. \n" Nil))
; 




  (= 
    (help-query) 
    (format "\nquery <function> <arguments>.\n------------------------\nCalls a querying function on its arguments.\n\n<function> must be one of the following:\n\nmobility        => prints number of moves available for each player\nmaterial        => prints number of pieces on board for each player\ngoal            => prints whether a goal has been achieved \n\nThese functions have an optional argument, a <player>:\n\nplayer             => returns value for white only\nopponent           => returns value for black only\n\nExample:\n\n     query mobility player.\n     query material opponent.\n     query goal.\n ~n" Nil))
; 





  (= 
    (help-show) 
    (format "\nshow <function> <arguments>.\n----------------------------\nCalls a showing function on its arguments.\n\n<function> must be one of the following:\n\nweights           => prints weight vectors for eval fns\nweights <player>  => prints weight vector for eval fn for <player>\n                     <player> is {player, opponent, or default}\n\nExample:\n\n     show weights.\t\n     show weights player.\n     show weights default.\n ~n" Nil))
; 




  (= 
    (help-weights) 
    (format "\nAccessing Evaluation Function Weights\n-------------------------------------\nThe weights used by a particular player can be changed by the\ncommand:\n\tweight <P> MATERIAL MOBILITY.   (negative values must be quoted)\nExample:\n\tweight player 5 1.\n\tweight opponent '-1' 0.\nWould make player value pieces and moves positively, \nwith pieces worth five times the value of each move,\nand would make opponent prefer losing material, and not \ncare about mobility.  \n\nNote that if a weight is set to 0, no time is spent in counting\nthat feature in a position.\n\nTo view the weights: \n\tshow weights.            ==> shows weights or both players\n\tshow weights <player>.   ==> shows weights for <player>\n" Nil))
; 



  (= 
    (help-eval-fn) 
    (format "\nEVALUATION FUNCTION\n--------------------\nThe default  evaluation function is based on\nmaterial difference and mobility difference between the two \nplayers, where PLAYER (white) prefers positive differences,\nand OPPONENT (black) prefers negative differences.\n\nThe function is:  \n\tEVAL = EVAL(PLAYER) - EVAL(OPPONENT)\nwhere\n\tEVAL(P) =   WEIGHT(P,MATERIAL)*MATERIAL(P)\n                  - WEIGHT(P,MOBILITY)*MOBILITY(P)\nand\n\tMATERIAL(P) = Number of pieces P has on board\n\tMOBILITY(P) = Number of legal moves P has on board\n\nFor info on viewing and modifying these weights, (help weights)\n" Nil))
; 




  (= 
    (help-evalfile) 
    (format "\nEVALUATION FUNCTION TABLES\n--------------------------\nevalfile <file>.  => loads <file>.eval as eval tables for current game\n\nSome example tables are found in:\n     'Metagame/games/chess.eval'\n     'Metagame/games/turncoat.eval'\n     \nIf you create your own for a specific game, these can be loaded\nin either before starting a game (top-level menu), or\nwhen it is the human's turn to make a move (move command menu).\n\nThe searching players ('help player') make use of whichever table file\nis currently loaded.  \n\nThe following parameters are useful with evaluation tables:\n   <square>: relative weight to give piece-square tables \n   <material>: relative weight to give specific material tables\n   <gen_material>: relative weight to give a player for having\n                   each piece of his color on the board.\n   <piece_attacks>:  (not implemented)\n   <piece_mobility>: (not implemented)\n" Nil))
; 




  (= 
    (help-iterate) 
    (format "\nThe <iterate> player\n---------------------------\nThis player performs an iterative deepening alpha-beta search.\nIt uses the principal continuation heuristic. \nMove ordering is determined by the paramer ORDERING:\n\trandom:  choose a random move when all evaluated equal.\n        fixed:   choose the first move found when all evaluated equal. \n\nThe iterative searcher will end the search after it has run out\nof time, based on the parameter: <move_time_limit>.\nFor example, the command:\n\tset move_time_limit 20000. \nwill force it to stop its search after 20 seconds (20,000 msec).\n\nNote that a player loses the game if it uses more than <game_time_limit>\nmsecs, so set this with the command:\n\tset game_time_limit 3600000.   (for 1 hour time-limit)\n\nMore info is available about the current eval fn (help eval_fn).\n\nSome tracing information is available on this player, (help trace). \n ~n" Nil))
; 




  (= 
    (help-trace) 
    ( (help-trace-general) 
      (help-trace-play) 
      (help-trace-ab) 
      (help-trace-gen)))
; 



  (= 
    (help-trace-general) 
    (format "\ntrace   <module> {<component>}.\nuntrace <module> {<component>}.\n-------------------------------\nEnables or disables tracing some component of a module.\nIf the module does not have any components, no component\nis needed here.\n\nListing traced Modules\n----------------------\nTo list the modules which are currently being traced, do:\n\tlist_tracing.\n\nTraceable Modules\n-----------------\nCurrently, the following tracing modules might be useful:\n ~n" Nil))
; 



  (= 
    (help-trace-play) 
    (format "\nVerbosity when playing games (play) \n-----------------------------------\n\tstate:  print state as moves are chosen by players.\n        move:   print move as moves are chosen by players. \n        clock:  print the clock as moves are played. \n\nThese are all set to ON by default whenever a \nhuman is playing.  \n\nExample:  \n\ttrace play state.      => turns this on\n\tuntrace play clock.    => turns this off\n\n" Nil))
; 

 



  (= 
    (help-trace-ab) 
    (format "\nTracing Alphabeta (ab) Search\n-----------------------------\n\tordering:  info regarding move ordering heuristics\n\tvalue:  info regarding value of moves found during search\n                also traces principal continuations \n\tresources: info regarding resource consumption during search\n        timing: info on timeout checks during search\n\titeration: info on time taken by each iteration of the search\n        eval: show evaluation of each node evaluated.\n        expand: show each node as it is expanded. \n        advice: show influence on evals for each node evaluated.\n        state:  print each state visited.  \n\nExample:  \n\ttrace ab ordering.  => turns this on\n\tuntrace ab value.   => turns this off\n\n" Nil))
; 




  (= 
    (help-trace-gen) 
    (format "\nTracing Game Generation (gen) \n-----------------------------\n\tgoals:  info  goal generation\n\tsimplify:  info on goal simplification\n        subsume:  info on goal redundancy checking and elimination\n\tpieces: info on piece generation.\n\nExample:  \n\ttrace gen pieces.      => turns this on\n\tuntrace gen subsume.   => turns this off\n" Nil))
; 

 

;
; 

;
; 

;
; 



  (= 
    (help-accept) 
    (format "\nYou can accept this choice, backtrack, or abort:\n\nyes.  (y)       => accept this choice\nnext. (n,no)    => reject this choice and consider next (if any)\nabort.          => accept no choices, go back to menu\nhelp.           => show this list\n" Nil))
; 




