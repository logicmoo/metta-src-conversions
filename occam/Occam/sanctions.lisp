;;; -*- Mode: LISP; Syntax: common-lisp; Base: 10 -*-

(def-cd sanction-21-1
  (coerce actor (polity type(league) name (league-of-nations)
			language (english)
			location (northern-hemisphere)
			continent (europe)
                        strategic-importance (low)
                        government (democracy)
			economic-health (strong)
			business-rel  (*role* target)
                        economy (industrial)
			exports   (*role*  object)
			imports (commodity type (minerals)))
	  target  (polity type (country) name (yugoslavia)
			  location (northern-hemisphere)
			  language (slavic)
			  continent (europe)
			  economic-health (weak)
                          government (communist)
                          economy (undeveloped)
			  business-rel   (polity type (country) name (us))
			  imports  (*role* object)
			  )
	  object (commodity availability (common) 
                            type (food)
                            commodity-type (agricultural))
	  threat (act type (sell)
		      actor (*role* actor)
		      object (*role* object)
		      to (*role* target)
		      mode (neg))
	  demand-obj (polity type (country) name (albania)
			     location (northern-hemisphere)
			     language (albanian)
                             government (communist)
			     continent (europe)
			     economic-health (weak)
			     business-rel  (*role* actor)
			     imports  (commodity type (minerals))
			     )
	  demand (act type (invade)
		      actor (*role* target)
		      object (*role* demand-obj)
		      mode (neg))
;          response (act type (invade)
;			actor (*role* target)
;			object (*role* demand-obj)
;			mode (neg))
          response (*role* demand)
          result (state type (concession)
                        actor (*role* target)
                        with (*role* demand))
         outcome (goal-outcome type (success)
				actor (*role* actor))
	  ))

(def-cd sanction-25-1
  (coerce actor (polity type(league) name (league-of-nations)
			language  (english)
			location (northern-hemisphere)
			continent (europe)
			economic-health (strong)
                        government (democracy)
			business-rel  (*role* target)
                        economy (industrial)
                        strategic-importance (low)                                                  
			exports   (*role* object)
			imports  (commodity type (minerals)))
	  target  (polity type (country) name (greece)
			  location (northern-hemisphere)
			  language (greek)
			  continent (europe)
                          economy (undeveloped)
                          government (democracy)
                          economic-health (weak)
                          imports   (*role* object)
			  )
	  object (commodity availability (common) 
                            type (food)
                            commodity-type (agricultural))
	  threat (act type (sell)
		      actor (*role* actor)
		      object (*role* object)
                       to (*role* target)
		      mode (neg))
	  demand-obj (polity type (country) name (bulgaria)
			     location (northern-hemisphere)
			     language (bulgarian)
			     continent (europe)
			     economic-health (weak)
			     business-rel  (*role* actor)
			     imports  (commodity type (minerals))
			     )
	  demand (act type (invade)
		      actor (*role* target)
		      object (*role* demand-obj)
		      mode (neg))
	  response (*role* demand)
          result (state type (concession)
                        actor (*role* target)
                        with (*role* demand))
	  outcome (goal-outcome type (success)
				actor (*role* actor))

	  ))

(def-cd sanction-33-1
  (coerce actor (polity type(league) name (league-of-nations)
			language  (english)
			location (northern-hemisphere)
			continent (europe)
			economic-health (strong)
			business-rel  (*role* target)
                        government (democraty)
                        exports  (*role* object)
			imports  (commodity type (minerals)))
	  target  (polity type (country) name (italy)
			  location (northern-hemisphere)
			  language (italian)
			  continent (europe)
			  economic-health (strong)
			  economy  (industrial)
			  imports   (*role* object)
			  exports  (commodity type (textiles)))
	  object (commodity availability (common) 
                            type (weapons)
                            commodity-type (industrial))
	  threat (act type (sell)
		      actor (*role* actor)
		      object (*role* object)
		      to (*role* target)
		      mode (neg))
	  demand-obj (polity type (country) name (absyssinia)
			     location (northern-hemisphere)
			     language (amharic)
			     continent (africa)
			     economic-health (weak)
			     business-rel  (*role* actor)
			     imports  (commodity type (minerals))
			     )
	  demand (act type (invade)
		      actor (*role* target)
		      object (*role* demand-obj)
		      mode (neg))
	  response (act type (produce)
                        actor (*role* target)
                        object (*role* object))
          result (state type (possess)
				actor (*role* target)
				value (yes)
				object (*role* object))
          
	  outcome (goal-outcome type (failure)
				actor (*role* actor))
	  ))

(def-cd sanction-48-4
  (coerce actor (polity type(country) name (ussr)
			 government (communist)
			language (russian)
			location (northern-hemisphere)
			continent (asia)
			economic-health (strong)
			business-rel  (polity type (pact) 
					      name (warsaw-pact-counties))
			exports  (commodity type (metals))
			imports  (commodity type (food)))
	  target  (polity type (country) name (yugoslavia)
			  location (northern-hemisphere)
			  language (slavic)
			  continent (europe)
			  economic-health (weak)
			  strategic-importance (high)
                           government (communist)
			  political-rels (dominated with (*role* actor))
			  imports  (commodity type (minerals)))
	  object (commodity availability (common)  type (money) 
                            commodity-type (monetary)
                            amount (dollars number (35000000)))
	  threat (act type ($aid)
		      actor (*role* actor)
		      object (*role* object)
		      to (*role* target)
		      mode (neg))
	  demand (act type (change-rel)
		      actor (*role* target)
		      to (dominated with (*role* actor)))
	  response (act type ($aid)
			actor (*role* target)
			from (polity type (country) name (us)
				       economic-health (strong)
				       location (northern-hemisphere)
				       continent (north-america)
				       imports  (oil)
				       exports  (commodity type (food))
				       political-rels (adverserial 
						       with (*role* actor)))
			object (*role* object))
          result (state type (reduced-influence)
                        actor (*Role* actor)
                        with (*role* target))
	  outcome (goal-outcome type (failure)
				actor (*role* actor))
	  ))

(def-cd sanction-60-3
  (coerce actor (polity type (country) name (us)
			economic-health (strong)
			location (northern-hemisphere)
			continent (north-america)
			imports  (*role* object)
                        government (democracy)
                        exports  (commodity type (food))
			political-rels (adverserial
					with (*role* target)))
	  target  (polity type (country) name (cuba)
			  location (northern-hemisphere)
			  language (spanish)
			  continent (north-america)
                          government (communist)                                                       
                          economic-health (weak)
			  strategic-importance (high)
                          economy (agricultural)
			  business-rel   (polity type (country) name (us))
			  imports  (commodity type (oil))
			  exports  (*role* object)
			  )
	  object (commodity availability (common)  type (sugar)
                            commodity-type (agricultural))
	  threat (act type (sell)
                      to (*role* actor)
                      
		      object (*role* object)
		      from (*role* target)
		      mode (neg))
	  demand (act type (change-government)
		      actor (*role* target)
		      from (communist))
	  response (act type ($aid)
			actor (*role* target)
			from (polity type(country) name (ussr)
				     ideology (communist)
				     language (russian)
				     location (northern-hemisphere)
				     continent (asia)
				     economic-health (strong)
				     business-rel  (polity type (pact) 
							   name (warsaw-pact-counties))
				     exports  (commodity type (metals))
				     imports  (commodity type (food))
				     political-rels (adverserial 
						     with (*role* actor)))
		)
          result (state type (reduced-influence)
                        actor (*Role* actor)
                        with (*role* target))
	  outcome (goal-outcome type (failure)
				actor (*role* actor))
	  ))

(def-cd sanction-61-1
  (coerce actor (polity type (country) name (us)
                        economic-health (strong)
                        location (northern-hemisphere)
                        continent (north-america)
                        imports  (commodity type (oil))
                        government (democracy)
                        exports  (commodity type (food))
                        political-rels  (allied with (polity type (country) 
                                                             name (uk))))
          target  (polity type (country) name (ceylon)
                          location (northern-hemisphere)
                          language  (sinhala)
                          continent (asia)
                          economic-health (weak)
                          government (democracy)
                          strategic-importance (low)
                          business-rel   (*role* actor)
                          imports (commodity type (manufactured-good))
                          exports (commodity type (oil)))
          object (commodity availability (common) 
                            type (money) amount (dollars number (17000000)))
          threat (act type (atrans)
                      actor (*role* actor)
                      object (*role* object)
                      to (*role* target)
                      mode (neg))
          demand-obj (company type (oil))
          demand (act type ($reimburse)
                      actor (*role* target)
                      object (*role* demand-obj)
                      to (*role* actor)
                      mode (yes))
          response (*role* demand)
          result (state type (concession)
                        actor (*role* target)
                        with (*role* demand))		
          outcome (goal-outcome type (success)
                                actor (*role* actor))))

(def-cd sanction-61-2
  (coerce actor (polity type(country) name (ussr)
			ideology (communist)
                         government (communist)
			language (russian)
			location (northern-hemisphere)
			continent (asia)
			economic-health (strong)
			business-rel  (polity type (pact) 
					      name (warsaw-pact-counties))
			exports  (commodity type (metals))
			imports  (commodity type (food)))
		
	  target  (polity type (country) name (albania)
			  location (northern-hemisphere)
			  language (albanian)
                          government (communist)
                          strategic-importance (high)
			  continent (europe)
			  economic-health (weak)
			  business-rel (polity type (country) name (us))
			  imports  (commodity type (minerals)))
	  object (commodity availability (common) type (food))
	  threat (act type ($aid)
		      actor (*role* actor)
		      object (*role* object)
		      to (*role* target)
		      mode (neg))
	  demand (act type (change-rel)
		      actor (*role* target)
		      to (indifferent with (polity type (country) name (china)
						   economic-health (strong)
						   location (northern-hemisphere)
						   political-rels (adverserial 
								   with (*role* actor))
						   continent (asia))))
	  response (act type ($aid)
			actor (*role* target)
			from (polity type (country) name (china)
				     economic-health (strong)
				     location (northern-hemisphere)
				     political-rels (adverserial 
						     with (*role* actor))
				     continent (asia))
			object (*role* object))
           result (state type (reduced-influence)
                        actor (*Role* actor)
                        with (*role* target))
	  outcome (goal-outcome type (failure)
				actor (*role* actor))
	  ))

(def-cd sanction-82-3
  (coerce actor (polity type (country) name (south-africa)
                        language (english)
                        location (southern-hemisphere)
                        business-rel  (polity type (country) name (us))
                        government (apartheid)
                        continent (africa)
                        exports   (commodity type (chromium))
                        imports  (oil)
                        economic-health (strong)
                        political-rels  (allied with (polity type (country) name (us)))
                        )
          target  (polity type (country) name (lesotho)
                          location (northern-hemisphere)
                          language  (sesotho)
                          government (democracy)
                          strategic-importance (low)
                          continent (europe)
                          economic-health (weak)
                          business-rel   (*role* actor)
                          imports (*role* object)
                          exports (commodity type(wool)))
          object (commodity availability (common)type (food))
          threat (act type (atrans)
                      actor (*role* actor)
                      object (*role* object)
                      to (*role* target)
                      instrument (act type (ptrans)
                                      actor (*role* actor) 
                                      object (p-obj type (train))
                                      to (*role* target))
                      mode (neg))
          demand-obj (human membership (anc) number (22))
          demand (act type (atrans)
                      actor (*role* target)
                      to (*role* actor)
                      object (*role* demand-obj)
                      mode (yes))		
          response (*role* demand)
          result (state type (concession)
                        actor (*role* target)
                        with (*role* demand))
          outcome (goal-outcome type (success)
                                actor (*role* actor))))

(def-cd sanction-62-1
  (coerce actor (polity type (country) name (us)
			economic-health (strong)
			location (northern-hemisphere)
			continent (north-america)
			imports  (commodity type (oil))
                        government (democracy)
			exports  (commodity type (food))
			political-rels  (allied 
					 with (polity type (country)
						      name (uk))))
	  target  (polity type (country) name (brazil)
			  location (southern-hemisphere)
			  language (portuguese)
			  continent (south-america)
                          government (democracy)
			  economic-health (strong)
			  strategic-importance (med)
			  business-rel   (*role* actor)
			  imports  (commodity type (machinery))
			  exports (commodity type (soybeans))
			  )
	  object (commodity availability (common)  type (money) amount 
			    (dollars number (174000000)))
	  threat (act type (atrans)
		      actor (*role* actor)
		      object (*role* object)
		      to (*role* target)
		      mode (neg))
	  demand-obj (company type (oil))
	  demand (act type ($reimburse)
		      actor (*role* target)
		      object (*role* demand-obj)
		      to (*role* actor)
		      mode (yes))
	  response (*role* demand)
          result (state type (concession)
                        actor (*role* target)
                        with (*role* demand))
	  outcome (goal-outcome type (success)
				actor (*role* actor))
	  ))

(def-cd sanction-65-3
  (coerce actor (polity type (country) name (uk)
			economic-health (strong)
			location (northern-hemisphere)
			continent (europe)
                        government (democracy)
			imports  (commodity type (food))
			exports  (*role* object)
			political-rels  (allied 
					 with (polity type (country)
						      name (us))))
	  target  (polity type (country) name (rhodesia)
			  location (southern-hemisphere)
			  language (english)
			  continent (africa)
			  economic-health (weak)
                          government (democracy)
			  economy (agricultural)
			  business-rel  (*role* actor)
			  imports (*role* object)
			  exports  (commodity type (tobacco))
			  )
	  object (commodity availability (common)  type (food)
                            commodity-type (agricultural) )
	  threat (act type (sell)
		      actor (*role* actor)
		      to (*role* target)
		      object (*role* object)
		      from (*role* actor)
		      mode (neg))
	  demand (act type (change-rel)
		      actor (*role* target)
		      to (allied with (*role* actor))
		      mode (yes))
	  response (act type (produce)
			actor (*role* target)
			object (*role* object))
          result (state type (possess)
				actor (*role* target)
				value (yes)
				object (*role* object))
	  outcome (goal-outcome type (failure)
				actor (*role* actor))
	  ))

(def-cd sanction-80-1
  (coerce actor (polity type (country) name (us)
			economic-health (strong)
			location (northern-hemisphere)
                        government (democracy)
			continent (north-america)
			imports (commodity type (oil))
			exports  (*role* object)
			political-rels (adverserial with (*role* target)))
	  target (polity type(country) name (ussr)
			 ideology (communist)
                         government (communist)
			 language (russian)
			 location (northern-hemisphere)
			 continent (asia)
			 economic-health (strong)
			 political-rels  (dominant 
					  with (polity type (pact) 
						       name (warsaw-pact-counties)))
			 business-rel  (polity type (pact) 
					       name (warsaw-pact-counties))
			 exports  (commodity type (metals))
			 imports  (*role* object))
	  object (commodity availability (common)  type (grain))
	  threat (act type (sell)
		      actor (*role* actor)
		      to (*role* target)
		      object (*role* object)
		      from (*role* actor)
		      mode (neg))
	  demand-obj (polity type (country) name (afganistan))
	  demand (act type (invade)
		      actor (*role* target)
		      object (*role* demand-obj)
		      mode (neg))
	  response (act type (sell)
			actor (polity type (country) name (argentina)
				      location (southern-hemisphere)
				      language (portuguese)
				      continent (south-america)
				      economic-health (med)
				      strategic-importance (med)
				      business-rel (*role* target)
				      exports (*role* object)
				      imports (oil))
			object (*role* object)
			price (money dollars (99000000) value (>market)) 
			to (*role* target))
          result (state type (possess)
				actor (*role* target)
				value (yes)
				object (*role* object))
		
	   outcome (goal-outcome type (failure)
				actor (*role* actor))
	  ))

(def-cd sanction-83-1
	(coerce actor (polity type (country) name (australia)
				language (english)
				location (southern-hemisphere)
                                economic-health (strong)
				continent (australia)
                                government (democracy)
				exports (*role* object)
				imports (oil))
		  object (commodity availability (common)  type (uranium))
		  target (polity type (country) name (FRANCE)
				 language (FRENCH)
				 economic-health (strong)
                                 government (democracy)
				 continent (europe)
				 location (northern-hemisphere)
				 imports (*role* object)
				 exports (commodity type (wine)))
                  demand-obj (weapons type (nuclear))
		  demand (act type (explode)
			      actor (*role* target)
			      object (*role* demand-obj)
			      location (southern-hemisphere)
			      mode (neg))
		  threat (act type (sell)
			      actor (*role* actor)
			      object (*role* object)
			      to (*role* target)
			      mode (neg))
		  response (act type (sell)
				actor (polity type (country) name (south-africa)
					      language (english)
					      location (southern-hemisphere)
					      business-relationship (*role* target)
					      government (apartheid)
					      continent (africa)
					      exports (*role* object)
					      imports (oil))
				object (*role* object)
				price (money dollars (3000000) value (>market)) 
				to (*role* target))

                  outcome (goal-outcome type (failure)
				actor (*role* actor))
                  
		  result (state type (possess)
				actor (*role* target)
				value (yes)
				object (*role* object))))

(def-cd sanction-68-2
  (coerce actor (polity type (country) name (us)
			economic-health (strong)
			location (northern-hemisphere)
			continent (north-america)	
                        government (democracy)				
			imports (commodity type (oil))
			exports (commodity type (food))
			political-rels (allied with (polity type (country) 
							     name (uk))))
				
	  target  (polity type (country) name (peru)
			  location (southern-hemisphere)
			  language (portuguese)
			  continent (south-america)
                          government (dictatorship)
                          economic-health (weak)
			  strategic-importance (med)
			  business-rel  (*role* actor)
			  imports (commodity type (machinery))
			  exports(commodity type (soybeans)))
  
	  object (commodity availability (common)  type (money)
                            amount (dollars number (30000000)))
	  threat (act type (atrans)
		      actor (*role* actor)
		      object (*role* object)
		      to (*role* target)
		      mode (neg))
	  demand-obj (company type (oil))
	  demand (act type ($reimburse)
		      actor (*role* target)
		      object (*role* demand-obj)
		      to (*role* actor)
		      mode (yes))
          response (*role* demand)
          result (state type (concession)
                        actor (*role* target)
                        with (*role* demand))
	  outcome (goal-outcome type (success)
				actor (*role* actor))
	  ))



(def-cd sanction-81-3
  (coerce actor (polity type (country) name (us)
			economic-health (strong)
			location (northern-hemisphere)
                        government (democracy)        
			continent (north-america)
			imports (commodity type (oil))
			exports (*role* object)
			political-rels (adverserial with (*role* target)))
	  target (polity type(country) name (ussr)
			 ideology (communist)
                         government (communist)
			 language (russian)
			 location (northern-hemisphere)
			 continent (asia)
			 economic-health (strong)
			 political-rels (dominant 
					 with (polity type (pact) 
						      name (warsaw-pact-counties)))
			 business-rel (polity type (pact) 
					      name (warsaw-pact-counties))
			 exports (commodity type (metals))
			 imports (*role* object))
	  object (commodity availability (common)  type (pipeline))
	  threat (act type (sell)
		      actor (*role* actor)
		      to (*role* target)
		      object (*role* object)
		      from (*role* actor)
		      mode (neg))
	  demand-obj (polity type (country) name (poland))
	  demand (act type (control)
		      actor (*role* target)
		      object (*role* demand-obj)
		      mode (neg))
        		
	  response (act type (sell)
			actor (polity type (country) name (FRANCE)
				      language (FRENCH)
				      government (democracy)
				      economic-health (strong)
				      continent (europe)
				      location (northern-hemisphere)
				      business-rel (*role* target)
				      exports (*role* object))
			to (*role* target)
			object (*role* object)
			from (*role* actor)
			price (money dollars (200000000) value (>market))
			mode (yes))
		
	  
                  outcome (goal-outcome type (failure)
				actor (*role* actor))
                  
		  result (state type (possess)
				actor (*role* target)
				value (yes)
				object (*role* object))
	  ;;sub-goal
	  ;;sub-plan
				;;;outcome
	  ))

(def-cd sanction-76-3
  (coerce actor (polity type (country) name (us)
			economic-health (strong)
			location (northern-hemisphere)
                        government (democracy)
			continent (north-america)
			imports (commodity type (oil))
			exports (*role* object))
	  target  (polity type (country) name (ethiopia)
			  continent (africa)
                          government (communist)
                          economic-health (weak)
			  imports (commodity type (food))
			  )
	  object (commodity availability (common)  type (money)
                            amount (dollars number (57000000)))
	  threat (act type ($aid)
		      actor (*role* actor)
		      object (*role* object)
		      to (*role* target)
		      mode (neg))
				
	  demand (act type (human-rights-violations)
		      actor (*role* target)
		      mode (neg))			
	  response (act type ($aid)
			actor (*role* target)
			from (polity type(country) name (ussr)
				     ideology (communist)
				     language (russian)
                                     government (communist)
                                     location (northern-hemisphere)
				     continent (asia)
				     economic-health (strong)
				     business-rel  (polity type (pact) 
							   name (warsaw-pact-counties))
				     exports  (commodity type (metals))
				     imports  (commodity type (food))
				     political-rels (adverserial 
						     with (*role* actor)))
			object (*role* object))

           result (state type (reduced-influence)
                        actor (*Role* actor)
                        with (*role* target))
		
	  outcome (goal-outcome type (failure)
				actor (*role* actor))

	  ))




(def-cd qa-1
  (coerce   demand (act mode (neg)
	       type (sell)
	       to (polity type (country) name (canada)
			  economic-health (strong)
			  location (northern-hemisphere)
			  continent (north-america))
	       object (*role* demand-obj)
	       from (*role* target)
	       actor (*role* target))
   demand-obj (commodity type (automobile))
   threat (act mode (neg)
	       from (*role* actor)
	       object (*role* object)
	       to (*role* target)
	       actor (*role* actor)
	       type (sell))
   object (commodity availability (common)  type (computer)
                     commodity-type (electronic))
   target (polity name (south-korea)
		  economic-health (strong)
		  location (northern-hemisphere)
		  continent (asia)
                  economy (industrial)
                   government (democracy)
		  imports  (*role* object)
		  exports (commodity type (electronics)))
   actor (polity type (country) name (us)
		 economic-health (strong)
		 location (northern-hemisphere)
		 continent (north-america)
                 government (democracy)
		 imports (commodity type (oil))
		 exports (*role* object)
		 political-rels (allied with (polity type (country) 
						     name (uk)))))
  ;"What would happen if the us refused to sell computers to southkorea unless they stopped exporting car to canada"
  )


(def-cd qa-2
  (coerce demand-obj (company type (oil))
	  demand (act type ($reimburse)
		      actor (*role* target)
		      object (*role* demand-obj)
		      to (*role* actor)
		      mode (yes))

	  threat (act mode (neg)
		      from (*role* actor)
		      object (*role* object)
		      to (*role* target)
		      actor (*role* actor)
		      type (atrans))
	  object (commodity availability (common)  type (money) 
			    amount (dollars number (100000000)))
	  target (polity name (iran)
			 economic-health (weak)
			 location (northern-hemisphere)
                               government (dictatorship)
			 continent (asia)
			 exports  (commodity type (oil)))
	  actor (polity type (country) name (us)
			economic-health (strong)
			location (northern-hemisphere)
			continent (north-america)
			imports (commodity type (oil))
                         government (democracy)
			exports (commodity type (weapons))
			political-rels (allied with (polity type (country) 
							    name (uk)))))
  ;"What would happen if the us offered to release  100-million dollars of iranian assests if iran aRRReed to reimburse the us 25-million
 ; for nationalizing oil companies"
  )

(def-cd qa-3
  (coerce actor (polity political-rels (allied with (polity name (france) 
							    type (country)))
			exports (*role* object)
			imports (commodity type (oil))
                              government (democracy)
			continent (north-america) 
			location (northern-hemisphere)
			economic-health (strong) name (us) type (country))
	  target (polity imports (commodity type (food))
			 economic-health (weak) continent (africa)
                               government (communist)
			 name (ethiopia) type (country))
	  object (commodity availability (common)  type (money))
	  threat (act type ($aid) actor (*role* actor)
		      to (*role* target) object (*role* object)
		      from (*role* actor) mode (neg))
	  demand-obj (commodity type (food))
	  demand (act to (northern-ethiopia) object
                      (*role* demand-obj) type (ptrans)))
	  
  ;"What would happen if the US threatened to cut off aid to Ethiopia unless Ethiopia permitted food supplies
;to be distibuted in northen ethiopia?"
)

(def-cd qa-4
  (coerce demand (act mode (neg)
		      type (invade)
		      object (*role* demand-obj)
		      actor (*role* target))
	  demand-obj (polity name (lebanon))
	  threat (act mode (neg)
		      from (*role* actor)
		      object (*role* object)
		      to (*role* target)
		      actor (*role* actor)
		      type (sell))
	  object (commodity availability (unique)  type (weapons) subtype (missle-guidance)
                            commodity-type (industrial))
	  target (polity name (israel)
			 economic-health (strong)
			 economy (industrial)
			 location (northern-hemisphere)
                         government (democracy)
			 continent (middle-east)
			 imports (*role* object))
	  actor (polity type (country) name (us)
			economic-health (strong)
			location (northern-hemisphere)
			continent (north-america)
                         government (democracy)
			imports (commodity type (oil))
			exports (*role* object)
			political-rels (allied with (polity type (country) 
							    name (uk)))))

  ;"What would happen if the US refused to sell missle guidance technology to israel unless israel withdrew its forces
;from lebanon?"
)

(def-cd qa-5
  (coerce actor (polity political-rels (allied 
                                        with (polity name (uk) 
                                                     type (country)))
                        exports (*role* object)
                        imports (commodity type (oil))
                        government (democracy)
                        continent (north-america) location (northern-hemisphere)
                        economic-health (strong) name (us) type (country))
          target (polity imports (commodity type (food))
                         economic-health (weak) continent (africa)
                         government (democracy)
                         name (greece) type (country))
          object (commodity availability (common)  type (money))
          threat (act type ($aid) actor (*role* actor)
                      to (*role* target) object (*role* object)
                      from (*role* actor) mode (neg))
          demand-obj (runway)
          demand (act to (northern-greece) 
                      actor (*role* target)
                      object (*role* demand-obj) 
                      type (build)))
  ;  "What would happen if the US threatened to cut off aid to Greece unless Greece permitted the US to
  ;enlarge enlarge US bases to accomodiate longer runways?"
  )


(defparameter *all-sanctions* (list SANCTION-21-1 SANCTION-25-1 SANCTION-33-1 SANCTION-48-4 sanction-76-3
			      SANCTION-60-3 SANCTION-61-1 SANCTION-61-2 SANCTION-68-2 SANCTION-82-3 SANCTION-62-1
			      SANCTION-65-3 SANCTION-83-1 SANCTION-80-1 SANCTION-81-3))

(defparameter *all-fail-sanctions* (list SANCTION-33-1 SANCTION-48-4 sanction-76-3
			      SANCTION-60-3 SANCTION-61-2
			      SANCTION-65-3 SANCTION-83-1 SANCTION-80-1 SANCTION-81-3))
						
(defparameter *all-suc-sanctions* (list SANCTION-62-1 SANCTION-82-3 SANCTION-68-2 
                                        SANCTION-61-1 SANCTION-25-1 SANCTION-21-1))
(defparameter *all-q* (list (list qa-1 qa-1 'failure)
		      (list qa-2 qa-2 'success)
		      (list qa-3 qa-3 'failure)	
		      (list qa-4 qa-4 'failure)
		      (list qa-5 qa-5 'failure)))
		      
