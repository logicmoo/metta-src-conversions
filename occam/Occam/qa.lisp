;;; -*- Mode: LISP; Syntax: common-lisp; Base: 10 -*-

(defun test-qa( &optional (s '(what-would-happen-if the-us refused to sell 
                               computers to south-korea unless
                               south-korea stopped exporting  automobiles to canada)))
  (format t "~%~a" s)
  (answer-question (simpparse s)))

(defun learn-sanctions()
  (progn (reset-memory)
         (mapc #'occam-lite *all-sanctions*)
         ))



(defun answer-question (q &optional (slot (or (find-question-slot q) 'outcome)))
   (let* ((q2 (transfer-rep q coerce-goal coerce-rep))
          (*uniqueness-threshold* 10)
          (mop (find-most-specific-schemata q2)))
     (cond((null mop) (format t "~%I don't know"))
	 ((cddr mop) (format t "~%That's a tough one"))
	 (t (setq mop (car mop))
            (let* ((copy (merge-cd (copy-cd-structure q2) (copy-cd-structure(schema-cd mop))))
		   (expanded-q (fill-in-roles copy copy))
		   (mop-bindings (cd-match (schema-pattern mop) expanded-q))
		   (events (instantiate-components mop mop-bindings))
		   (focused-slot (role-filler expanded-q slot)))
              (simpgen  focused-slot))))))

(defun find-question-slot(q)
  (first-image #'(lambda(fpair)
		   (cond ((and (cd-p (feature-value fpair))
                               (role-filler (feature-value fpair) 'question-focus))
			      (feature-name fpair))))
	       (cd-features q)))

(def-cd coerce-goal
  (GOAL OUTCOME (*var* outcome)
        ACTOR (*var* actor)
        PLAN (ACT TYPE (MTRANS)
                  OBJECT (COND ELSE (*var* threat
                                           (act object (*var* object)))
                               IF (*var* demand
                                         (act object (*var* demand-obj))))
                  ACTOR (*var* actor)
                  TO (*var* target))))

(def-cd coerce-rep 
  (coerce actor (*var* actor)
          target (*var* target)
          demand (*var* demand)
          object (*var* object)
          demand-obj (*var* demand-obj)
          threat (*var* threat)
          outcome (*var* outcome)))

(setf (schema-transfer (get 'goal 'schema))
      (list coerce-goal coerce-rep (get 'coerce 'schema)))
                           
(defun answer-it (q &aux out cd)
  (let* ( (*uniqueness-threshold* 10)
         (mop (find-most-specific-schemata (second q))))
    (cond((null mop) 0)
	  (t (setq cd (make-general-cd* (mapcar #'schema-cd mop)))
	    (setf out (role-filler*  cd '(outcome type)))
	    (cond((null out) 0)
		 ((eq (cd-head out)
                      (third q))
		  1)
		 (t 0))))))

(defun test-all()
  (mapcar #'answer-it *all-q*))


(defun big-test()
  (tdl-for-ebl)
  (mapc #'occam-lite (append *all-sanctions* *all-sbls* *all-tdls*))
  (test-qa)
  (test-all))
