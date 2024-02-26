;;; -*- Mode: LISP; Syntax: common-lisp; Base: 10 -*-



(def-gen-rule gen-result
  (act type (*var* act-type)
       object (*var* object))
  after
  (state type (*var* state-type)
	 value (*var* value)
	 object (*var* object))
  (((*var* *from*) result (*var* *to*))
   ((*var* *from*) after (*var* *to*))))


(def-gen-rule gen-result-object-difference
  (act type (*var* act-type)
       object (*var* object))
  after
  (state type (*var* state-type)
	 value (*var* value)
	 object (*var* object))
  (((*var* *from*) result (*var* *to*))
   ((*var* *from*) after (*var* *to*)))
  object)

(def-gen-rule gen-result-2-vars
  (act type (*var* act-type)
       to (*var* to)
       object (*var* object))
  after
  (state type (*var* state-type)
	 value (*var* value)
         actor (*var* to)
	 object (*var* object))
  (((*var* *from*) result (*var* *to*))
   ((*var* *from*) after (*var* *to*))))

(def-gen-rule prev-action
  (act type (*var* atype-2)
       object  (*var* obj)
       before (*var* act-1 (act type (*var* atype-1)
			       object (*var* obj))))
  after
  (state type (*var* ptype)
	 value (*var* value)
	 object (*var* obj))
  (
   ((*var* *from*) result (*var* *to*))
   ((*var* act-1) result (*var* state-1 (state object (*var* obj))))
   ((*var* state-1) enables (*var* *from*))
   ((*var* *from*) after (*var* *to*))
   ((*var* act-1) after (*var* *from*))
   
   )
  )
(def-gen-rule prev-action-2
  (act type (*var* atype-2)
       object  (*var* x)
       to (*var* obj)
       before (*var* act-1 (act type (*var* atype-1)
                                object (*var* y)
			        to (*var* obj))))
  after
  (state type (*var* ptype)
	 value (*var* value)
	 object (*var* obj))
  (
   ((*var* *from*) result (*var* *to*))
   ((*var* act-1) result (*var* state-1 (state object (*var* obj))))
   ((*var* state-1) enables (*var* *from*))
   ((*var* *from*) after (*var* *to*))
   ((*var* act-1) after (*var* *from*))
   
   )
  act-1
  )

(def-gen-rule prev-plan
  (act type (*var* atype-2) ;the response
       actor (*var* actor)
        before (*var* act-1 (act type (*var* atype-1)  ;the threat
                                 realizes (*var* plan))))  ;the plan
  after
  (state type (*var* ptype)  ;the result
	 actor (*var* actor))
	
  (
   ((*var* *from*) result (*var* *to*))
   ((*var* act-1) result (*var* state-1 (state object (*var* obj)
                                               result-of (*var* act-1))))  ;helpless
   ((*var* state-1) forces (*var* *from*))
   ((*var* *from*) after (*var* *to*))
   ((*var* act-1) after (*var* *from*))
   
   )
  )
