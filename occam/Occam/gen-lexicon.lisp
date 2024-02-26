(setq *word-lis* nil)

(def-word (polity type (country) name (ussr))
	  #'(lambda(&rest ignore) (list "the Sovient Union")))
(def-word (polity type (country) name (us))
	  #'(lambda(&rest ignore) (list "the United States")))
(def-word (polity type (country) name (france))
	  #'(lambda(&rest ignore) (list "France")))
(def-word (polity type (country) name (australia))
	  #'(lambda(&rest ignore) (list "Australia")))
(def-word (polity type (country) name (AFGANISTAN))
	  #'(lambda(&rest ignore) (list "Afganistan")))
(def-word (polity type (country) name (South-africa))
	  #'(lambda(&rest ignore) (list "South Africa")))

(def-word (polity type (country) name (ARGENTINA))
	  #'(lambda(&rest ignore) (list "Argentina")))
(def-word (polity type (country) name (south-korea))
	  #'(lambda(&rest ignore) (list "South Korea")))

(def-word (polity type (country) name (canada))
	  #'(lambda(&rest ignore) (list "Canada")))

(def-word (polity type (country) name (iran))
	  #'(lambda(&rest ignore) (list "Iran")))

(def-word (polity type (country) name (*var* v (uninstantaited))
		  exports (*var* c))
	  #'(lambda(cd b) (list "a country which exports" (lookup-var 'c b))))

(def-word (commodity type (automobile))
	  #'(lambda(&rest ignore) (list "automobiles")))

(def-word (commodity type (grain))
	  #'(lambda(&rest ignore) (list "grain")))
(def-word (commodity type (uranium))
	  #'(lambda(&rest ignore) (list "uranium")))




(def-word (commodity type (computer))
	  #'(lambda(&rest ignore) (list "computers")))

(def-word (commodity type (money) amount (dollars number (*var* n)))
	  #'(Lambda(cd b) (list (lookup-var 'n b) "dollars")))

(def-word (no)
	  #'(lambda(&rest ignore) (list "not")))

(def-word (yes)
	  #'(lambda(&rest ignore) (list "")))

(def-word (goal-outcome type (failure)
		      outcome-of (*var* demand)
                      outcome-is (state resulted-from (*var* reply))
		      actor (*var* actor))
	#'(lambda(cd b)  (list "the goal of" (lookup-var 'actor b) "that" 
                               (lookup-var 'demand b) "will fail and"
			      (lookup-var 'reply b))))

(def-word 
  (goal-outcome type (success)
                outcome-of (goal intended-by (plan realized-by (act type(mtrans)
                                                                    object (cond if (*var* demand))))
                                 thwarted-by (act initiates-action (*var* reply)))
                actor (*var* actor))
  #'(lambda(cd b) (list "the goal of" (lookup-var 'actor b)
                        "that" (lookup-var 'demand b) "will succeed")))


(def-word (act mode (*var* mode)
	       type (atrans)
	       to (*var* to)
	       object (*var* object)
	       from (*var* actor)
	       actor (*var* actor))
	  #'(lambda(cd b) (list (lookup-var 'actor b) (lookup-var 'mode b)
                                                        
                                "give" (lookup-var 'object b) "to" (lookup-var 'to b))))

(def-word (act mode (*var* mode)
	       type (sell)
	       to (*var* to)
	       object (*var* object)
	       from (*var* actor)
	       actor (*var* actor))
	  #'(lambda(cd b)
			    (list (lookup-var 'actor b)
                                 (lookup-var 'mode b)
                                 
                                  "sell" 
                                  (lookup-var 'object b)
                                  "to" (lookup-var 'to b))))



(def-word (NEG) #'(lambda(x y) (list "not")))
(def-word (act type (agreement)
	       actor (*var* actor)
	       object (*var* object)
	       agreement (act type (atrans)
			      initiates-action (act type (atrans)
						    from  (*var* actor)
						    object (commodity type (money))
						    actor (*var* the-target)) 
			      
			      object (*var* c)
		 	      to (*var* actor)
			      mode (yes)))		
	  #'(lambda(cd b)(list  (lookup-var 'actor b) "will agree to purchase" (lookup-var 'c b)
				  "from" (lookup-var 'object b))))
