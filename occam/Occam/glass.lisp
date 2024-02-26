;;; -*- Mode: LISP; Base: 10 -*-
;;lynn dropped a clear glass cup,
(def-cd glass-1-a
	(act type (propel)
	     actor (human name (lynn) age (kid)
			  hair (blond) eyes (blue) unique-id lynn)
	     object (p-obj type (cup) color (clear)
			   composition (glass)
			   UNIQUE-ID glass.1)
	     to (p-obj type (floor) location (kitchen))))

;;the cup breaks
(def-cd glass-1-r
	(state type (broken)
	       object (p-obj type (cup) color (clear)
			   composition (glass)
			   UNIQUE-ID glass.1)
	       value (yes)))

(def-ilink glass-1-a after glass-1-r)
(def-ilink glass-1-r before glass-1-a)

;lynn drops a red plastic glass nothing happend
(def-cd glass-2-a
	(act type (propel)
	     actor (human name (lynn) age (kid)
			  hair (blond) eyes (blue) unique-id lynn)
	     object (p-obj type (cup) color (red)
			   composition (plastic)
			   UNIQUE-ID glass.2)
	     to (p-obj type (floor) location (kitchen))))

;lynn drops a red glass cup
(def-cd glass-3-a
	(act type (propel)
	     actor (human name (lynn) age (kid)
			  hair (blond) eyes (blue) unique-id lynn)
	     object (p-obj type (cup) color (red)
			   composition (glass)
			   UNIQUE-ID glass.3)
	     to (p-obj type (floor) location (kitchen))))

;;the cup breaks
(def-cd glass-3-r
	(state type (broken)
	       object (p-obj type (cup) color (red)
			   composition (glass)
			   UNIQUE-ID glass.3)
	       value (yes)))

(def-ilink glass-3-a after glass-3-r)
(def-ilink glass-3-r before glass-3-a)


(defvar *all-tdls* (list glass-1-a glass-2-a glass-3-a))
