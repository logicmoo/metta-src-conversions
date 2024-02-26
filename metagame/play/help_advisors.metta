;
; 

;
; 

;
; 

;
; 



  (= 
    (help-advisors) 
    (print-advisors))
; 




  (= 
    (print-advisors) 
    ( (advisor-herald $H) 
      (format "~s~n" 
        (:: $H)) 
      (whenever 
        (print-advisor $A) 
        (format "~n~n" Nil))))
; 

	


  (= 
    (advisor_herald  "Explanation of ADVISORS\n-----------------------\nFollowing is the list of all advisors with a brief explanation.\nAlso shown (in parens) is the current value for each advisor.\nFor more information, consult the papers or source-code.\n") True)
; 



  (= 
    (print-advisor $A) 
    ( (advisor-weight $A $W) 
      (advhelp $A $Help) 
      (format "<~p>  (~p) ~n~s" 
        (:: $A $W $Help))))
; 




  (= 
    (advhelp  gen_material "Gives 1 point for each white piece, -1 for black.") True)
; 


  (= 
    (advhelp  material "Uses user-defined material function if available (help evalfile).") True)
; 


  (= 
    (advhelp  square "Uses user-defined piece-square table if available (help evalfile).") True)
; 


  (= 
    (advhelp  lthreat "Gives points for each enemy piece a piece can capture.") True)
; 


  (= 
    (advhelp  potent "Like lthreat, but reduces each threat value if defended.") True)
; 


  (= 
    (advhelp  gthreat "Value only for best of all lthreats.") True)
; 


  (= 
    (advhelp  pthreat "Value only for best of all potent threats.") True)
; 


  (= 
    (advhelp  vital "Doesn't like leaving pieces attacked when enemy achieves goal \nby removing them.  Only sensitive when there are VITAL_NUMBER pieces left.") True)
; 


  (= 
    (advhelp  dynamic_mobility "1 point for each move piece makes in current position.") True)
; 


  (= 
    (advhelp  static_mobility "1 point each move piece makes from square on empty board.") True)
; 


  (= 
    (advhelp  eventual_mobility "Points for each square piece can ever reach from square on empty board,\ndiscounted by how many moves it takes piece to get there from square.  The discount\nfunction is controlled by DISCOUNT parameter, either INVERSE or EXPONENT.") True)
; 


  (= 
    (advhelp  gmovmob "Sum of dynamic_mobility for all player's pieces.") True)
; 


  (= 
    (advhelp  gcapmob "1 point for each capturing-move in current position.") True)
; 


  (= 
    (advhelp  arrive_distance "Favors pieces on squares close to achieving arrival goals.") True)
; 


  (= 
    (advhelp  promote_distance "Favors getting pieces close to promotion. \nValue based on cost to get to promotion square, and value of best piece the piece\ncan eventually promote into.") True)
; 


  (= 
    (advhelp  possess "Points for each piece player has in hand.  Generally the value\nis the average or max of the values it would have when placed on the board.") True)
; 

 
  (= 
    (advhelp  initprom "Anticipates points which will follow when player init-promotes a piece\nnext turn.  Value will be value of best choice that player can make.") True)
; 


  (= 
    (advhelp  random "Adds a random noise to position value, in range [RANDOM_MIN,RANDOM_MAX].\nNote that this value will be multiplied by the weight \nattached to this advisor [as is the case for all advisors.]") True)
; 



  (= 
    (advhelp  static "The following advisors are used to build the static tables for each piece. \nThis advisor determines how much to weigh the static values vs. other advisors.") True)
; 


  (= 
    (advhelp  victims "Point for each piece this piece could someday capture.") True)
; 


  (= 
    (advhelp  immunity "Points for each enemy piece which cannot someday capture this piece.") True)
; 


  (= 
    (advhelp  giveaway "Points for each piece we own which can someday capture this piece.") True)
; 


  (= 
    (advhelp  eradicate "Points for pieces which enemy would like to eradicate.") True)
; 


  (= 
    (advhelp  stalemate "Degree to which piece might contribute to stalemate goals.") True)
; 


  (= 
    (advhelp  arrive "Degree to which piece might contribute to arrive goals.") True)
; 


  (= 
    (advhelp  max_static_mob "Maximum static mobility piece has.") True)
; 


  (= 
    (advhelp  max_eventual_mob "Maximum eventual mobility piece has.") True)
; 


  (= 
    (advhelp  avg_static_mob "Average static mobility piece has.") True)
; 


  (= 
    (advhelp  avg_eventual_mob "Average eventual mobility piece has.") True)
; 


  (= 
    (advhelp  dominate "Not used.") True)
; 



