

;;;lynn wants  play-doh
(def-cd play-doh-1
	(goal actor (human name (lynn) age (kid) hair (blond)
			   eyes (blue) unique-id lynn)
		goal (state type (poss-by) unique-id poss-by.5
				  actor (human name (lynn) age (kid)
					       hair (blond) eyes (blue)
					       unique-id lynn)
				  value (yes)
				  object (p-obj type (toy)
						stype (play-doh)
						unique-id play-doh.1))
		unique-id play-doh.1))

;;;lynn plans to asks mike
(def-cd play-doh-p-1
	(plan actor (human name (lynn) age (kid) hair (blond)
			   eyes (blue) unique-id lynn)
	      plan (act type (mtrans) unique-id mtrans.5
			actor (human name (lynn) age (kid)
				     hair (blond) eyes (blue)
				     unique-id lynn)
			to (human name (mike)
				  relation (ipt type(family-rel)
						stype (father)
						of (human unique-id lynn))
				  age (grown-up)
				  hair (brown)
				  eyes (green)
				  unique-id mike)
			object (act type (atrans)
				    actor (human name (mike)
						 relation (ipt type(family-rel)
							       stype (father)
							       of (human unique-id lynn))
						 age (grown-up)
						 hair (brown)
						 eyes (green)
						 unique-id mike)
				    object (p-obj type (toy)
						  stype (play-doh)
						  unique-id play-doh.1)
				    to (human name (lynn)
					      age (kid)
					      hair (blond)
					      eyes (blue)
					      unique-id lynn)))
	      unique-id play-doh-p.1))


;;;lynn asks mike
(def-cd play-doh-a-1
	(act type (mtrans)
	     unique-id mtrans.5
	     actor (human name (lynn) age (kid) hair (blond)
			  eyes (blue) unique-id lynn)
	     to (human name (mike)
		       relation (ipt type(family-rel)
				     stype (father)
				     of (human unique-id lynn))
		       age (grown-up)
		       hair (brown) 
		       eyes (green)
		       unique-id mike)
	     object (act type (atrans)
			 actor (human name (mike)
				      relation (ipt type(family-rel)
						    stype (father)
						    of (human unique-id lynn))
				      age (grown-up)
				      hair (brown)
				      eyes (green)
				      unique-id mike)
			 object (p-obj type (toy)
				       stype (play-doh) u
				       nique-id play-doh.1)
			 to (human name (lynn) age (kid)
				   hair (blond) eyes (blue)
				   unique-id lynn))))


;;;mike wants to give lynn  play-doh
(def-cd play-doh-ga-1
	(goal actor (human name (mike)
			   relation (ipt type(family-rel)
					 stype (father)
					 of (human unique-id lynn))
			   age (grown-up)
			   hair (brown)
			   eyes (green)
			   unique-id mike)
	      goal (act type (atrans)  unique-id atrans.5
			actor (human name (mike)
				     relation (ipt type(family-rel)
						   stype (father)
						   of (human unique-id lynn))
				     age (grown-up)
				     hair (brown)
				     eyes (green)
				     unique-id mike)
			object (p-obj type (toy)
				      stype (play-doh)
				      unique-id play-doh.1)
			to (human name (lynn) age (kid) hair (blond)
				  eyes (blue) unique-id lynn))
	      unique-id play-doh-ga.1))


;;;;mike gives lynn  play-doh
(def-cd play-doh-a-2
	(act type (atrans)  unique-id atrans.5
	     actor (human name (mike)
			  relation (ipt type(family-rel)
					stype (father)
					of (human unique-id lynn))
			  age (grown-up)
			  hair (brown)
			  eyes (green)
			  unique-id mike)
	     object (p-obj type (toy)
			   stype (play-doh)
			   unique-id play-doh.1)
	     to (human name (lynn)
		       age (kid)
		       hair (blond)
		       eyes (blue)
		       unique-id lynn)))

;;;lynn gets the play-doh
(def-cd play-doh-r-1
	(goal-outcome type (success)
		      actor (human name (lynn) age (kid)
				   hair (blond) eyes (blue)
				   unique-id lynn)
		      goal (state type (poss-by) unique-id poss-by.5
				  actor (human name (lynn) age (kid)
					       hair (blond) eyes (blue)
					       unique-id lynn)
				  value (yes)
				  object (p-obj type (toy) stype (play-doh)
						unique-id play-doh.1))
		      unique-id play-doh-r.1))


(def-role play-doh-1 plan play-doh-p-1)
(def-ilink play-doh-1 intended-by play-doh-p-1)
(def-ilink play-doh-p-1 intends play-doh-1)

(def-ilink play-doh-p-1 realized-by play-doh-a-1)
(def-ilink play-doh-a-1 realizes play-doh-p-1)

(def-ilink play-doh-a-1 motivates play-doh-ga-1)
(def-ilink play-doh-ga-1 motivated-by play-doh-a-1)

(def-ilink play-doh-a-2 achieves play-doh-1)
(def-ilink play-doh-1 achieved-by play-doh-a-2)

(def-ilink play-doh-a-2 achieves play-doh-ga-1)
(def-ilink play-doh-ga-1 achieved-by play-doh-a-2)

(def-ilink play-doh-1 outcome play-doh-r-1)
(def-ilink play-doh-r-1 outcome-of play-doh-1)
(def-role play-doh-1 outcome play-doh-r-1)


;karen wants to go to the zoo
(def-cd zoo-1
	(goal actor (human name (karen) age (kid) hair (blond)
			   eyes (blue) unique-id karen)
	      goal (state type (location)
			  actor (human name (karen) age (kid) hair (blond)
				       eyes (blue) unique-id karen)
			  value (yes)
			  object (p-obj type (location)
					stype (zoo) unique-id zoo.1))
	      unique-id zoo.1))

;;;karen plans to asks mike
(def-cd zoo-p-1
	(plan actor (human name (karen) age (kid) hair (blond)
			   eyes (blue) unique-id karen)
	      plan (act type (mtrans) unique-id mtrans.7
			actor (human name (karen) age (kid) hair (blond)
				     eyes (blue) unique-id karen)
			to (human name (mike)
				  relation (ipt type(family-rel)
						stype (father)
						of (human unique-id karen))
				  age (grown-up) hair (brown)
				  eyes (green) unique-id mike)
			object (act type (ptrans)
				    actor (human name (mike)
						 relation (ipt type(family-rel)
							       stype (father)
							       of (human unique-id karen))
						 age (grown-up) hair (brown)
						 eyes (green) unique-id mike)
				    object (human name (karen)
						  age (kid)
						  hair (blond)
						  eyes (blue)
						  unique-id karen)
				    to (p-obj type (location)
					      stype (zoo)
					      unique-id zoo.1)))
	      unique-id zoo-p.1))


;;;karen asks mike
(def-cd zoo-a-1
	(act type (mtrans) unique-id mtrans.7
	     actor (human name (karen) age (kid) hair (blond)
			  eyes (blue) unique-id karen)
	     to (human name (mike)
		       relation (ipt type(family-rel)
				     stype (father)
				     of (human unique-id karen))
		       age (grown-up) hair (brown)
		       eyes (green) unique-id mike)
	     object (act type (ptrans)
			 actor (human name (mike)
				      relation (ipt type(family-rel)
						    stype (father)
						    of (human unique-id karen))
				      age (grown-up) hair (brown)
				      eyes (green) unique-id mike)
			 object (human name (karen) age (kid) hair (blond)
				       eyes (blue) unique-id karen)
			 to (p-obj type (location) stype (zoo)
				   unique-id zoo.1))))


;;;mike wants to take karen to the zoo
(def-cd zoo-ga-1
	(goal actor (human name (mike)
			   relation (ipt type(family-rel)
					 stype (father)
					 of (human unique-id karen))
			   age (grown-up) hair (brown)
			   eyes (green) unique-id mike)
	      goal (act type (ptrans)
			actor (human name (mike)
				     relation (ipt type(family-rel)
						   stype (father)
						   of (human unique-id karen))
				     age (grown-up) hair (brown)
				     eyes (green) unique-id mike)
			object (human name (karen) age (kid) hair (blond)
				      eyes (blue) unique-id karen)
			to (p-obj type (location) stype (zoo) unique-id zoo.1))
	      unique-id zoo-ga.1))


;;;;mike karen takes to the zoo
(def-cd zoo-a-2
	(act type (ptrans)
	     actor (human name (mike)
			  relation (ipt type(family-rel)
					stype (father)
					of (human unique-id karen))
			  age (grown-up) hair (brown)
			  eyes (green) unique-id mike)
	     object (human name (karen) age (kid) hair (blond)
			   eyes (blue) unique-id karen)
	     to (p-obj type (location) stype (zoo) unique-id zoo.1)
	     unique-id zoo-a.2))


;;;karen goes to the zoo
(def-cd zoo-r-1
	(goal-outcome type (success)
		      actor (human name (karen) age (kid) hair (blond)
				   eyes (blue) unique-id karen)
		      goal (state type (location)
				  actor (human name (karen) age (kid) hair (blond)
					       eyes (blue) unique-id karen)
				  value (yes)
				  object (p-obj type (location) stype (zoo)
						unique-id zoo.1))
		      unique-id zoo-r.1))


(def-role zoo-1 plan zoo-p-1)
(def-ilink zoo-1 intended-by zoo-p-1)
(def-ilink zoo-p-1 intends zoo-1)

(def-ilink zoo-p-1 realized-by zoo-a-1)
(def-ilink zoo-a-1 realizes zoo-p-1)

(def-ilink zoo-a-1 motivates zoo-ga-1)
(def-ilink zoo-ga-1 motivated-by zoo-a-1)

(def-ilink zoo-a-2 achieves zoo-1)
(def-ilink zoo-1 achieved-by zoo-a-2)

(def-ilink zoo-a-2 achieves zoo-ga-1)
(def-ilink zoo-ga-1 achieved-by zoo-a-2)

(def-ilink zoo-1 outcome zoo-r-1)
(def-ilink zoo-r-1 outcome-of zoo-1)
(def-role zoo-1 outcome zoo-r-1)

(def-cd refrigerator-1
	(goal actor (human name (karen) age (kid) hair (blond)

			   eyes (blue) unique-id karen)
	      goal (state object (p-obj type (refrigerator)
					color (white)
					unique-id ref.001)
			  type (open)
			  value (yes))
	      plan (act type (propel) 
			actor (human  unique-id karen)
			object  (component type (door)
					   of (p-obj unique-id ref.001))
			
			)
	      outcome  (goal-outcome type (failure)
				     actor (human  unique-id karen)
				     goal (state object (p-obj unique-id ref.001)
						 type (open)
						 value (yes)))
	      

	      unique-id refrigerator.1))







;;;karen wants pizza
(def-cd pizza-1
	(goal actor (human name (karen) age (kid) hair (blond)
			   eyes (blue) unique-id karen)
	      goal (state type (poss-by) unique-id poss-by.002
			  actor (human name (karen) age (kid) hair (blond)
				       eyes (blue) unique-id karen)
			  value (yes)
			  object (p-obj type (food) stype (pizza)
					unique-id pizza.001))
	      unique-id pizza.1))

;;;karen plans to asks mike
(def-cd pizza-p-1
	(plan actor (human name (karen) age (kid) hair (blond)
			   eyes (blue) unique-id karen)
	      plan (act type (mtrans) unique-id mtrans.002
			actor (human name (karen) age (kid) hair (blond)
				     eyes (blue) unique-id karen)
			to (human name (mike)
				  relation (ipt type(family-rel)
						stype (father)
						of (human unique-id karen))
				  age (grown-up) hair (brown)
				  eyes (green) unique-id mike)
			object (act type (atrans)
				    actor (human name (mike)
						 relation (ipt type(family-rel)
							       stype (father)
							       of (human unique-id karen))
						 age (grown-up) hair (brown)
						 eyes (green) unique-id mike)
				    object (p-obj type (food) stype (pizza)
						  unique-id pizza.001)
				    to (human name (karen) age (kid) hair (blond)
					      eyes (blue) unique-id karen)))
	      unique-id pizza-p.1))


;;;karen asks mike
(def-cd pizza-a-1
	(act type (mtrans) unique-id mtrans.002
	     actor (human name (karen) age (kid) hair (blond)
			  eyes (blue) unique-id karen)
	     to (human name (mike)
		       relation (ipt type(family-rel)
				     stype (father)
				     of (human unique-id karen))
		       age (grown-up) hair (brown)
		       eyes (green) unique-id mike)
	     object (act type (atrans)
			 actor (human name (mike)
				      relation (ipt type(family-rel)
						    stype (father)
						    of (human unique-id karen))
				      age (grown-up)
				      hair (brown)
				      eyes (green) unique-id mike)
			 object (p-obj type (food) stype (pizza)
				       unique-id pizza.001)
			 to (human name (karen) age (kid) hair (blond)
				   eyes (blue) unique-id karen))))


;;;mike want's to give karen pizza
(def-cd pizza-ga-1
	(goal actor (human name (mike)
			   
			   relation (ipt type(family-rel)
					 stype (father)
					 of (human unique-id karen))
			   age (grown-up)
			   hair (brown)
			   eyes (green) unique-id mike)
	      goal (act type (atrans) unique-id atrans.002
			actor (human name (mike)
				     
				     relation (ipt type(family-rel)
						   stype (father)
						   of (human unique-id karen))
				     age (grown-up)
				     hair (brown)
				     eyes (green) unique-id mike)
			object (p-obj type (food) stype (pizza)
				      unique-id pizza.001)
			to (human name (karen) age (kid) hair (blond)
				  eyes (blue) unique-id karen))
	      unique-id pizza-ga.1))


;;;;mike gives karen pizza
(def-cd pizza-a-2
	(act type (atrans)
	     unique-id atrans.002
	     actor (human name (mike)
			  relation (ipt type(family-rel)
					stype (father)
					of (human unique-id karen))
			  age (grown-up)
			  hair (brown)
			  eyes (green) unique-id mike)
	     object (p-obj type (food) stype (pizza)
			   unique-id pizza.001)
	     to (human name (karen) age (kid) hair (blond)
		       eyes (blue) unique-id karen)
	     ))


;;;karen get's the pizza
(def-cd pizza-r-1
	(goal-outcome type (success)
		      actor (human name (karen) age (kid) hair (blond)
				   eyes (blue) unique-id karen)
		      goal (state type (poss-by)  unique-id poss-by.002
				  actor (human name (karen) age (kid) hair (blond)
					       eyes (blue) unique-id karen)
				  value (yes)
				  object (p-obj type (food) stype (pizza)
						unique-id pizza.001))
		      unique-id pizza-r.1))


(def-role pizza-1 plan pizza-p-1)
(def-ilink pizza-1 intended-by pizza-p-1)
(def-ilink pizza-p-1 intends pizza-1)

(def-ilink pizza-p-1 realized-by pizza-a-1)
(def-ilink pizza-a-1 realizes pizza-p-1)

(def-ilink pizza-a-1 motivates pizza-ga-1)
(def-ilink pizza-ga-1 motivated-by pizza-a-1)

(def-ilink pizza-a-2 achieves pizza-1)
(def-ilink pizza-1 achieved-by pizza-a-2)

(def-ilink pizza-a-2 achieves pizza-ga-1)
(def-ilink pizza-ga-1 achieved-by pizza-a-2)

(def-ilink pizza-1 outcome pizza-r-1)
(def-ilink pizza-r-1 outcome-of pizza-1)
(def-role pizza-1 outcome pizza-r-1)


;;;karen wants a cookie
(def-cd cookie-1
	(goal actor (human name (karen) age (kid) hair (blond)
			   eyes (blue) unique-id karen)
	      goal (state type (poss-by) unique-id poss-by.102
			  actor (human name (karen) age (kid) hair (blond)
				       eyes (blue) unique-id karen)
			  value (yes)
			  object (p-obj type (food) stype (cookie)
					unique-id cookie.001))
	      unique-id cookie.1))

;;;karen plans to asks lynn
(def-cd cookie-p-1
	(plan actor (human name (karen) age (kid) hair (blond)
			   eyes (blue) unique-id karen)
	      plan (act type (mtrans) unique-id mtrans.102
			actor (human name (karen) age (kid) hair (blond)
				     eyes (blue) unique-id karen)
			to (human name (lynn)
				  relation (ipt type(family-rel)
						stype (sister)
						of (human unique-id karen))
				  age (kid) hair (blond)
				  eyes (blue) unique-id lynn)
			object (act type (atrans)
				    actor (human name (lynn)
						 relation (ipt type(family-rel)
							       stype (sister)
							       of (human unique-id karen))
						 age (kid) hair (blond)
						 eyes (blue) unique-id lynn)
				    object (p-obj type (food) stype (cookie)
						  unique-id cookie.001)
				    to (human name (karen) age (kid) hair (blond)
					      eyes (blue) unique-id karen)))
	      unique-id cookie-p.1))


;;;karen asks lynn
(def-cd cookie-a-1
	(act type (mtrans) unique-id mtrans.102
	     actor (human name (karen) age (kid) hair (blond)
			  eyes (blue) unique-id karen)
	     to (human name (lynn)
		       relation (ipt type(family-rel)
				     stype (sister)
				     of (human unique-id karen))
		       age (kid) hair (blond)
		       eyes (blue) unique-id lynn)
	     object (act type (atrans)
			 actor (human name (lynn)
				      relation (ipt type(family-rel)
						    stype (sister)
						    of (human unique-id karen))
				      age (kid)
				      hair (blond)
				      eyes (blue) unique-id lynn)
			 object (p-obj type (food) stype (cookie)
				       unique-id cookie.001)
			 to (human name (karen) age (kid) hair (blond)
				   eyes (blue) unique-id karen))))


;;;lynn want's to give karen a cookie
(def-cd cookie-ga-1
	(goal actor (human name (lynn)
			   
			   relation (ipt type(family-rel)
					 stype (sister)
					 of (human unique-id karen))
			   age (kid)
			   hair (blond)
			   eyes (blue) unique-id lynn)
	      goal (act type (atrans) unique-id atrans.102
			actor (human name (lynn)
				     
				     relation (ipt type(family-rel)
						   stype (sister)
						   of (human unique-id karen))
				     age (kid)
				     hair (blond)
				     eyes (blue) unique-id lynn)
			object (p-obj type (food) stype (cookie)
				      unique-id cookie.001)
			to (human name (karen) age (kid) hair (blond)
				  eyes (blue) unique-id karen))
	      unique-id cookie-ga.1))


;;;;lynn gives karen a cookie
(def-cd cookie-a-2
	(act type (atrans)
	     unique-id atrans.102
	     actor (human name (lynn)
			  relation (ipt type(family-rel)
					stype (sister)
					of (human unique-id karen))
			  age (kid)
			  hair (blond)
			  eyes (blue) unique-id lynn)
	     object (p-obj type (food) stype (cookie)
			   unique-id cookie.001)
	     to (human name (karen) age (kid) hair (blond)
		       eyes (blue) unique-id karen)
	     ))


;;;karen get's the cookie
(def-cd cookie-r-1
	(goal-outcome type (success)
		      actor (human name (karen) age (kid) hair (blond)
				   eyes (blue) unique-id karen)
		      goal (state type (poss-by)  unique-id poss-by.102
				  actor (human name (karen) age (kid) hair (blond)
					       eyes (blue) unique-id karen)
				  value (yes)
				  object (p-obj type (food) stype (cookie)
						unique-id cookie.001))
		      unique-id cookie-r.1))


(def-role cookie-1 plan cookie-p-1)
(def-ilink cookie-1 intended-by cookie-p-1)
(def-ilink cookie-p-1 intends cookie-1)

(def-ilink cookie-p-1 realized-by cookie-a-1)
(def-ilink cookie-a-1 realizes cookie-p-1)

(def-ilink cookie-a-1 motivates cookie-ga-1)
(def-ilink cookie-ga-1 motivated-by cookie-a-1)

(def-ilink cookie-a-2 achieves cookie-1)
(def-ilink cookie-1 achieved-by cookie-a-2)

(def-ilink cookie-a-2 achieves cookie-ga-1)
(def-ilink cookie-ga-1 achieved-by cookie-a-2)

(def-ilink cookie-1 outcome cookie-r-1)
(def-ilink cookie-r-1 outcome-of cookie-1)
(def-role cookie-1 outcome cookie-r-1)


(defvar *all-sbls* (list zoo-1 refrigerator-1 play-doh-1 pizza-1 cookie-1))
