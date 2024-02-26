;;; -*- Mode: LISP; Syntax: common-lisp; Base: 10 -*-


(def-rule sell-->possess
	  (ACT type (SELL)
	       TO (*var* x)
	       OBJECT (*var* y)
	       mode (yes))
  result
  (STATE TYPE (POSSESS)
	 OBJECT (*var* y )
	 VALUE (YES)
	 ACTOR (*var* x))
  )


(def-rule build-->possess
	  (ACT type (produce)
	       actor (*var* x)
	       OBJECT (*var* y)
	       mode (yes))
  result
  (STATE TYPE (POSSESS)
	 OBJECT (*var* y )
	 VALUE (YES)
	 ACTOR (*var* x))
  )

(def-rule refuse-to-sell-->demand-increase
	  (ACT type (SELL)
	       actor (polity exports (*var* y)
			     )
	       TO (*var* x (polity imports (*var* y)))
	       OBJECT (*var* y (commodity))
	       mode (neg))
  result
  (STATE TYPE (demand-increase)
	 actor (*var* x)
	 VALUE (YES)
	 OBJECT (*var* y)))


(def-rule refuse-to-buy-->support-from-adversery
  (act type (sell)
       to (*var* actor (polity imports (*var* object)))
       object (*var* object)
       from (*var* target  (polity exports (*var* object)))
       mode (neg))
  result
  (STATE TYPE (need-for-support)
	 actor (*var* target)
	 VALUE (YES)
	 from (*var* actor)))

(def-rule refuse-aid-->support-from-adversery
  (act type ($aid)
       actor (*var* actor)
       object (*var* object)
       to (*var* target  (polity strategic-importance (high)
                                 economic-health (weak)))
       mode (neg))
  result
  (STATE TYPE (need-for-support)
	 actor (*var* target)
	 VALUE (YES)
	 from (*var* actor)))

(def-rule need-for-support-->adversery-move-in
  (STATE TYPE (need-for-support)
	 actor (*var* target)
	 VALUE (YES)
	 from (*var* actor))
  enables
  (act type ($aid)
       actor (*var* target)
       from (polity economic-health (strong)
                    political-rels (adverserial 
                                    with (*var* actor)))))
		

(def-rule aid->reduced-influence
  (act type ($aid)
       actor (*var* target)
       from (polity political-rels (adverserial 
                                    with (*var* actor))))
  result
  (state type (reduced-influence)
         actor (*var* actor)
         with (*var* target)))

(def-rule demand-increase-->price-increase
	  (STATE TYPE (demand-increase)
		 VALUE (YES)
		 actor (*var* x (polity economic-health (strong)))
		 OBJECT (*var* y (commodity availability (common))))
  enables
  (ACT type (SELL)
       actor (polity exports (*var* y)
		     business-relationship (*var* x))
       TO (*var* x)
       OBJECT (*var* y)
       price (money value (>market))
       mode (yes)))

(def-rule demand-increase-->internal-maunfacture
	  (STATE TYPE (demand-increase)
		 VALUE (YES)
		 actor (*var* x (polity economy (*var* e)))
		 OBJECT (*var* y (commodity commodity-type (*var* e))))
  enables
  (ACT type (produce)
       actor (*var* x)
       object (*var* y)))

(def-rule do-it-->conceed-to-do-it
  (*var* act (act actor (*var* actor)))
  result 
  (state type (concession)
         actor (*var* actor)
         with (*var* act)))


(def-rule refuse-to-give-->helplessness
  (*var* act (ACT type (atrans)
                  actor (polity)
                  TO (*var* x (polity imports (*var* y)
                                      economic-health (weak)
                                      strategic-importance (low)                                                      
                                      ))
                  OBJECT (*var* y (commodity type (food)))
                  mode (neg)))
  result
  (STATE TYPE (helplessness)
         actor (*var* x)
         VALUE (YES)
         result-of (*var* act)))


(def-rule a-friend-in-need
  (*var* act (ACT type (invade)
                  actor (*var* actor)
                  object (*var* x (polity positive-political-rel (*var* z))                                                   
                                      )
                 
                  mode (pos)))
  motivates
  (goal TYPE (p-friend)
        actor (*var* z)
        VALUE (YES)
        motivated-by (*var* act )))

(def-rule realizes-a-friend-in-need
   (goal TYPE (p-friend)
        actor (*var* z (polity exports (*var* object)))
        motivated-by (*var* act (*var* act (ACT type (invade)
                                                actor (*var* t))))
        VALUE (YES))                                 

  achieved-by
  (act type (sell)
       actor (*var* z)
       object (*var* object)
       to (*var* t)
       mode (neg)))
  

(def-rule a-friend-indeed
  (goal TYPE (p-friend)
        actor (*var* z)
        VALUE (YES)
        motivated-by (*var* act (*var* act (ACT type (invade)
                                                actor (*var* actor)
                                                object (*var* x (polity positive-political-rel (*var* z))                                                   
                                                              )))))
  implemented-by
  (ACT type (invade)
                  actor (*var* actor)
                  object (*var* x (polity positive-political-rel (*var* z))                                                   
                                      )
                 
                  mode (neg)))
  
(def-rule refuse-to-sell-->helplessness
  (*var* act (ACT type (SELL)
                  actor (polity exports (*var* y))
                  TO (*var* x (polity imports (*var* y)
                                      economic-health (weak)
                                      strategic-importance (low)                                                      
                                      economy (undeveloped)))
                  OBJECT (*var* y (commodity))
                  mode (neg)))
  result
  (STATE TYPE (helplessness)
         actor (*var* x)
         VALUE (YES)
         result-of (*var* act)))

(def-rule refuse-to-aid-until-reimburse-->lesser-evil
  (*var* act (ACT type (atrans)
                  actor (*var* actor)
                  TO (*var* x)
                  OBJECT (*var* y (commodity type (money)))
                  realizes (*var* plan (act type ($reimburse)
                                            actor (*var* x)
                                            object (company)
                                            to (*var* actor)
                                            mode (yes)))
                  mode (neg)))
  result
  (STATE TYPE (choose-lesser-evil)
         actor (*var* x)
         VALUE (YES)
         choice (*var* plan)
         result-of (*var* act)))

(def-rule helpless->concede
  (STATE TYPE (helplessness)
         actor (*var* x)
         VALUE (YES)
         result-of (*var* act (act realizes (*var* plan))))
  forces
  (*var* plan))
   

(def-rule lesser-evil-->choice
  (STATE TYPE (choose-lesser-evil)
         VALUE (YES)
         choice (*var* plan))
  forces
  (*var* plan))
         
