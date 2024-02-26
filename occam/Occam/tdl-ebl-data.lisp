
;;israel buys F-15s
(def-cd  buy.1
	  (ACT type (SELL)
	       TO (polity name (israel)
			 economic-health (strong) unique.id israel)
	       OBJECT  (commodity availability (unique)  type (weapons)
                                  sub-type (F-15s) unique.id F-15)
	       mode (yes)))
;;israel has F-15s
(def-cd possess.1
  (STATE TYPE (POSSESS)
	 OBJECT (commodity availability (unique)  type (weapons)
                                  sub-type (F-15s) unique.id F-15)
	 VALUE (YES)
	 ACTOR  (polity name (israel)
			 economic-health (strong) unique.id israel)))
 
(def-ilink buy.1 after possess.1)
(def-ilink  possess.1 before buy.1)


;;us buys memory
(def-cd  buy.2
  (ACT type (SELL)
       TO (polity name (us)
                  economic-health (strong) unique.id us)
       OBJECT  (commodity availability (common)  type (computer)
                          sub-type (memory) unique.id memory)
       mode (yes)))
;;us has memory
(def-cd possess.3
  (STATE TYPE (POSSESS)
         OBJECT  (commodity availability (common)  type (computer)
                            sub-type (memory) unique.id memory)
         VALUE (YES)
         ACTOR  (polity name (us)
                        economic-health (strong) unique.id us)))
 
(def-ilink buy.2 after possess.3)
(def-ilink  possess.3 before buy.2)

;israel buit nuclear weapons


(def-cd build.1
	  (ACT type (produce)
	       actor (polity name (israel)
			 economic-health (strong) unique.id israel)
	       OBJECT  (commodity  type (weapons)
                                  sub-type (nuke) unique.id nuke.1)
               mode (yes)))

;israel has nukes
(def-cd possess.2
  (STATE TYPE (POSSESS)
	 OBJECT (commodity  type (weapons)
                                  sub-type (nuke) unique.id nuke.1)
	 VALUE (YES)
	 ACTOR  (polity name (israel)
			 economic-health (strong) unique.id israel)))
	       

(def-ilink build.1 after possess.2)
(def-ilink  possess.2 before build.1)

;us built memory
(def-cd build.2
	  (ACT type (produce)
	       actor (polity name (us)
			 economic-health (strong) unique.id us)
	       OBJECT   (commodity availability (common)  type (computer)
                            sub-type (memory) unique.id memory)
        mode (yes)))

;israel has nukes
(def-cd possess.4
  (STATE TYPE (POSSESS)
	 OBJECT (commodity availability (common)  type (computer)
                            sub-type (memory) unique.id memory)
         VALUE (YES)
	 ACTOR  (polity name (us)
			 economic-health (strong) unique.id us)))
	       

(def-ilink build.2 after possess.4)
(def-ilink  possess.4 before build.2)
(defparameter  *tdl-ebl-data* (list  build.1 build.2 buy.2 buy.1))

(defun tdl-for-ebl(&aux saved)
  (setq saved *all-gen-rules*)
  (setq *all-gen-rules* nil)
  ;;;this two gen rules are for learning binary states
  ;;;they are given highest priority
  (def-gen-rule gen-result-2-vars-a
  (act type (*var* act-type)
       to (*var* to)
       mode (*var* v)
       object (*var* object))
  after
  (state type (*var* state-type)
	 value (*var* value)
         actor (*var* to)
	 object (*var* object))
  (((*var* *from*) result (*var* *to*))
   ((*var* *from*) after (*var* *to*))))

  (def-gen-rule gen-result-2-vars-b
    (act type (*var* act-type)
         mode (*var* v)
         actor (*var* to)
         object (*var* object))
    after
    (state type (*var* state-type)
           value (*var* value)
           actor (*var* to)
           object (*var* object))
    (((*var* *from*) result (*var* *to*))
     ((*var* *from*) after (*var* *to*))))
  ;;ire-insert old gen-rules
  (setq *all-gen-rules* (append *all-gen-rules* saved))

  ;;destroy rules defined in rules.lisp
  (def-rule sell-->possess
	  (ACT type (SELL)
	       TO (nothing-can-match-me)
	       OBJECT (me-neither)
	       actor (no-match))
          result
          (STATE))
  (def-rule build-->possess
    (ACT type (produce)
         TO (nothing-can-match-me)
         OBJECT (me-neither)
         actor (no-match))
     result
    (STATE))
  (mapc #'occam-lite *tdl-ebl-data*)
  ;;restore *all-gen-rules*
  ;;;note occam-lite still runs TDL on glass examples with these gen-rules
  ;;;but the traces are a little ugly, and don't match the book.
  (setq *all-gen-rules* saved))
