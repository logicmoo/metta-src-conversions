;;; -*- Mode: LISP-*-

(def-cd the-threat ((*var* threat)))

(def-cd the-demand ((*var* demand)))

(def-cd the-result ((*var* result)))

(def-cd the-response ((*var* response)))
(def-cd the-outcome ((*var* outcome)))

(def-schema coerce
  (coerce)
  (coerce
	actor (*var* actor)
        object (*var* object)
        target (*var* target)
        demand (*var* demand)
        demand-obj (*var* demand-obj)
        threat (*var* threat)
        outcome (*var* outcome)
	response (*var* response)
	result (*var* result))

        (list the-threat the-response
              the-result the-demand the-outcome)
	 )



(setf (schema-outcome-slot (get 'coerce 'schema)) 'result)

(def-ilink the-threat  realizes the-demand)
(def-ilink the-demand realized-by the-threat)

(def-ilink the-threat after the-response)
(def-ilink the-response before the-threat)

(def-ilink the-response after the-result)
(def-ilink the-result before the-response)

(def-ilink the-outcome outcome-of the-demand)
(def-ilink the-outcome outcome-is the-result)
(def-ilink the-demand outcome the-outcome)
(def-ilink the-result results-in-outcome the-outcome)
