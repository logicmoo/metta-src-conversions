
(def-lex the-us (add-cd (polity type (country) name (us) unique-id us
			    economic-health (strong)
			    location (northern-hemisphere)
			    continent (north-america)
			    exports (commodity type (computers) unique-id c.1)
			    )))

(def-lex the-united-states (add-cd (polity type (country) name (us) unique-id us
			    economic-health (strong)
			    location (northern-hemisphere)
			    continent (north-america)
			    )))

(def-lex what-would (add-cd (act question-focus(*?*))))
(def-lex do-if (progn (add-cd (goal 
                               actor (nil)
                               motivated-by (Nil)
                               plan (Nil)))
                      (progn (add-request *cd* (cond ((find-cd polity)
                                                      (srf *cd* 'actor *to*))))
                             (add-request *cd* (cond ((find-cd act)
                                                      (srf *cd* 'plan *to*))))
                             (add-request *cd* (cond ((find-cd act :after)
                                                      (srf *cd* 'motivated-by *to*)))))))

 (def-lex turkey (add-cd (polity type (country) name (turkey)
			  location (northern-hemisphere)
			  language (turkish)
			  continent (europe)
                          economy (undeveloped)
                          government (democracy)
                          economic-health (mid)
                         
                          exports (commodity type (cotton))
                          unique-id turkey 
			  )))
(def-lex cyprus (add-cd (polity type (country) name (cyprus)
			  location (northern-hemisphere)
			  language (greek)
			  continent (europe)
                          economy (undeveloped)
                          government (democracy)
                          economic-health (mid)
                          imports  (commodity type (oil))
                          exports (commodity type (cement))
                          unique-id cyprus
			  )))
(def-lex invaded 
  (progn (add-cd (act type (invade) unique-id import-43
                      
                      ))
         (add-request *cd* (cond ((find-cd polity)
                                  (srf *cd* 'actor *to*)
                                  (add-request *cd*
                                               (cond((find-cd polity)
                                                     (srf *cd* 'object *to*)
                                                     ))))))))


(def-lex south-korea (add-cd (polity name (south-korea) unique-id south-korea
				     economic-health (strong)
				     location (northern-hemisphere)
				     continent (asia)
                                     economy (industrial)
                                     government (democracy)
				     imports(commodity type (computers) unique-id c.1)
				     exports(commodity type (automobile) unique-id a.1))))

(def-lex  canada (add-cd (polity type (country) name (canada) unique-id canada
								   economic-health (strong)
								   location (northern-hemisphere)
								   continent (north-america))))

(def-lex iran (add-cd (polity name (iran)  unique-id iran
			      economic-health (weak)
			      location (northern-hemisphere)
			      continent (asia)
			      exports  (commodity type (oil)))))

(def-lex iranian (add-cd (polity name (iran)  unique-id iran
			      economic-health (weak)
			      location (northern-hemisphere)
			      continent (asia)
			      exports  (commodity type (oil)))))

(def-lex assets :ignore)

(def-lex offered :ignore)

(def-lex automobiles (add-cd (commodity type (automobile) unique-id a.1)))
(def-lex computers (add-cd (commodity availability (common)  type (computer)
                     commodity-type (electronic) unique-id c.1)))

(def-lex to :ignore)
(def-lex of :ignore)
(def-lex agreed :ignore)
(def-lex for :ignore)
(def-lex nationalizing :ignore)
(def-lex oil :ignore)
(def-lex companies :ignore)

(def-lex exporting 
  (progn (add-cd (act type (sell) unique-id import-43
                      
                      ))
         (add-request *cd* (cond ((find-cd polity)
                                  (srf *cd* 'actor *to*)
                                  (srf *cd* 'from *to*)
                                  (add-request *cd*
                                               (cond((find-cd commodity)
                                                     (srf *cd* 'object *to*)
                                                     (add-request *cd*
                                                                  (cond((find-cd polity)
                                                                        (srf *cd* 'to *to*)
                                                                        )))))))))))

(def-lex stopped (progn (add-cd (neg))
		     (add-request *cd* (cond ((find-cd  act :after)
					      (srf *to* 'mode *cd*))))))


(def-lex refused (progn (add-cd (neg))
		     (add-request *cd* (cond ((find-cd  act :after)
					      (srf *to* 'mode *cd*))))))


(def-lex sell 
  (progn (add-cd (act type (sell) unique-id sell-11
                      ))
         (add-request *cd* (cond ((find-cd polity)
                                  (srf *cd* 'actor *to*)
                                  (srf *cd* 'from *to*)
                                  (add-request *cd*
                                               (cond((find-cd commodity)
                                                     (srf *cd* 'object *to*)
                                                     (add-request *cd*
                                                                  (cond((find-cd polity)
                                                                        (srf *cd* 'to *to*)
                                                                        )))))))))))




(def-lex release (progn (add-cd (act type (atrans) unique-id realese-11
				  ))
			     (add-request *cd* (cond ((find-cd polity)
							 (srf *cd* 'actor *to*)
							 (srf *cd* 'from *to*)
							 (add-request *cd*
								      (cond((find-cd commodity)
									    (srf *cd* 'object *to*)
									    (add-request *cd*
											 (cond((find-cd polity)
											       (srf *cd* 'to *to*)
											       )))))))))))



(def-lex reimburse (progn (add-cd (act type (atrans) unique-id reimburse-11
				     ))
			(add-request *cd* (cond ((find-cd polity)
						 (srf *cd* 'actor *to*)
						 (srf *cd* 'from *to*)
						 (add-request *cd*
							      (cond((find-cd commodity)
								    (srf *cd* 'object *to*))))
						 (add-request *cd*
							      (cond((find-cd polity)
								    (srf *cd* 'to *to*)
								    ))))))))

(def-lex 100-million (add-cd (commodity type (money) unique-id m2 amount (dollars number (100000000)))))
(def-lex 25-million (add-cd (commodity type (money) unique-id m1 amount (dollars number (25000000)))))

(def-lex unless (progn (add-cd (act type (mtrans) unique-id mtrans-11
				    object (cond)))
		       (add-request *cd* (cond((find-cd act)
					       (srf (role-filler *cd* 'object) 'else *to*)
					       (srf *cd* 'actor (role-filler *to* 'actor))
					       (srf *cd* 'to (role-filler *to* 'to))
					       (add-request *cd* (cond((find-cd act)
								       (srf (role-filler *cd* 'object) 'if *to*)))))))))



(def-lex if (progn (add-cd (act type (mtrans) unique-id mtrans-11
				    object (cond)))
		       (add-request *cd* (cond((find-cd act)
					       (srf (role-filler *cd* 'object) 'else *to*)
					       (srf *cd* 'actor (role-filler *to* 'actor))
					       (srf *cd* 'to (role-filler *to* 'to))
					       (add-request *cd* (cond((find-cd act)
								       (srf (role-filler *cd* 'object) 'if *to*)))))))))

(def-lex what-would-happen-if
	 (progn (add-cd (goal unique-id goal-1
			      outcome (goal-outcome question-focus (*?*))
                              plan ))
		(add-request *cd* (cond((and *end* (find-cd act)) 
		      (let ((goal *cd*)
			    (plan (list->cd '(plan unique-id plan-xxx)))
			    (act *to*))	
			(def-ilink goal intended-by plan)
			(def-ilink plan intends goal)
			(def-ilink plan realized-by act)
			(def-ilink act realizes plan)
			(srf goal 'actor (role-filler act 'actor))
			(srf plan 'actor (role-filler act 'actor))
			(srf goal 'plan *to*)))))))



