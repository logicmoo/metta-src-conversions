

(defvar *all-gen-rules* nil "alist of all generalization rules (name . rule)")

;;;def-gen-rule -macro to define generalization rule
;;;stores rule in *all-gen-rules*

(defmacro def-gen-rule (name from link to ilinks &optional  exceptions)
  `(progn
     (setq ,name
	   (make-gen-rule :name ',name
			  :from (list->cd ',from)
			  :to (list->cd ',to)
			  :link ',link
			  :ilinks (mapcar #'(lambda(pre)
					      (make-ilink :from (list->cd (first pre))
							  :ilink (second pre)
							  :to (list->cd (third pre))
							  ))
					  
					  ',ilinks)
			  :exceptions ',exceptions) 
			  )
     (let ((key (assoc ',name *all-gen-rules*)))
       (if key (setf (cdr key) ,name)
	   (setf *all-gen-rules*
		 (append *all-gen-rules* (list (cons ',name ,name))))))
     
     ',name))

;;;(tdl  event schema)- theory driven learning component of occam
(defun tdl (event schema)
  (format-if *trace* "~%Attempting theory driven learning for new event.")
  (let* ((events (aggregate event schema 0)))
    (first-image
      #'(lambda(name.rule)				
	  (let*((rule (cdr name.rule))
		;;make sure rule applies and would specialize schema
		(bindings (match-gen-rule rule event (schema-cd schema))))
	    (when bindings
	      (let* ((similar (similar-outcomes rule event events))
		     (different-events (subset #'(lambda(e)
						   (not(member e similar)))
					       events)))
		(cond((null different-events)	;no exceptions
		      (when (null (gen-rule-exceptions rule))
			(format-if *trace* "~%Situation matches exceptionless gen rule.")
			(xpn (gen-rule-from rule) *trace*)
			(xpn (gen-rule-to rule) *trace*)
			(let((g (make-general-cd* similar)))
			      (make-sub-schema-from-tdl similar g rule schema))))
		     (t (when (gen-rule-exceptions rule)	;dispositional
			  (let*((g (make-general-cd* similar))			      
				(difference (blame-difference g
							      (gen-rule-exceptions rule)
							      different-events
							      (gen-rule-from rule))))
			    
			    (when difference
			      (format-if *trace* "~%Situation matches dispositional gen rule.")
			      (xpn (gen-rule-from rule) *trace*)
			      (xpn (gen-rule-to rule) *trace*)
			      (format-if *trace* "~%Attributing difference to ~a."
				      (gen-rule-exceptions rule))
			      (xpn difference *trace*)						  
			      (make-sub-schema-from-tdl similar g rule schema difference)
			)))))
	  
	  ))))
      *all-gen-rules*)))

;;;(match-gen-rule rule event parent)- determines if event matches a genrule
;;;if bindings of from and to are consistent and link is present and all-vars are bound
;;;parent is used to prevent tdl from recreate the same schema as a specialazation
(defun match-gen-rule(rule event &optional (parent nil))
  (let ((b (cd-match (gen-rule-from rule) event)))
    (when (and b
	       (every #'(lambda(v)
			  (lookup-var v b))
		      	      (collect-vars (gen-rule-from rule)))
	       (if parent
		   (let((parent-b (cd-match (gen-rule-from rule) parent)))
		     (some #'(lambda(v)
			       (not (lookup-var v parent-b)))
			   (collect-vars (gen-rule-from rule))))
		   t))	       
      (let ((e (follow-ilink event (gen-rule-link rule))))
	(when e
	  (cd-match (gen-rule-to rule) e b))))))
  
;;;(similar-outcomes rule event events)- determines which
;;;events have the same outcome (type and value) as event
(defun similar-outcomes (rule event events)
  (let* ((ilink (gen-rule-link rule))
	 (to (follow-ilink event ilink))
	 (to-type (role-filler to 'type))
	 (to-value (role-filler to 'value)))
    (subset #'(lambda(e)
		(let((cd (follow-ilink e (gen-rule-link rule))))
		  (and cd
		       (compatible to-type (role-filler cd 'type))
		       (compatible to-value (role-filler cd 'value)))))
	    events)))

(defvar *type-roles* '(type sub-type) "used to represent type hierachy")
(defvar *match-vars* '(*pattern* *cd*) "fake variables used by matcher")

;;;(generalize-bindings bindings rule)- retain only type information in bindings
;;;this creates the most specific class consistent with examples
;;;add special *from* and *to* variables to bindings to represent
;;;the from and to of generalization rule
(defun generalize-bindings (bindings rule)
  (let((gbindings (mapcar #'(lambda(v.cd)
			      (let((v (car v.cd))
				   (cd (cdr v.cd)))
				(if (member v *match-vars*)
				    v.cd
				    (cons v (generalize-to-class cd)))))
			  bindings)))
    (bind-var-1 '*from* (instantiate (gen-rule-from rule) gbindings)
		(bind-var-1 '*to* (instantiate (gen-rule-to rule) gbindings)
			    gbindings))))

;;;(generalize-to-class  cd)- remove all features except type features
(defun generalize-to-class(cd)
  (make.cd (cd-head cd)
	   (subset #'(lambda(fpair)
		       (member (feature-name fpair)
			       *type-roles*))
		   (cd-features cd))))



;;;(instantiate-mechanism ilinks bindings) create a generalized cd
;;;structure by instanatiating the variables in the structure
(defun instantiate-mechanism (ilinks bindings)
  (cond((null ilinks)
	(lookup-var '*from* bindings))
       (t (let*((ilink (car ilinks))
		(from (ilink-from ilink))
		(to (ilink-to ilink))
		(name (ilink-ilink ilink))
		(from-cd (instantiate from bindings))
		(to-cd (instantiate to bindings))
		)
	    (format-if *trace* "~%Instantiating mechanism.")
	    (xpn from-cd *trace*)
	    (format-if *trace* "~%~a" name)
	    (xpn to-cd *trace*)
	    (push (make-ilink :from from-cd
			      :to to-cd
			      :ilink name)
		  (cd-links from-cd))
	    (push (make-ilink :from to-cd
			      :to from-cd
			      :ilink (inverse-ilink name))
		  (cd-links to-cd))
	    (when (and (var? from)
		       (null (lookup-var (var-name from) bindings)))
	      (setf bindings (bind-var-1 (var-name from) from-cd bindings)))
	    (when (and (var? to)
		       (null (lookup-var (var-name to) bindings)))
	      (setf bindings (bind-var-1 (var-name to) to-cd bindings)))
	    (instantiate-mechanism (cdr ilinks) bindings)))))


;;;(make-rule-from-cd rule gbindings) create a rule for a schema.
;;;only works if mechanism is one ilink because it ignores
;;;remainder.  The remainder should be used as preconditions
(defun make-rule-from-cd(rule bindings)
  (let*((ilink (first (gen-rule-ilinks rule)))
       (from (subsitute-pattern (ilink-from ilink) rule))
       (to (subsitute-pattern (ilink-to ilink) rule))
       (from-vars  (collect-vars from))
       ;;to-pat will only contain variables if in from-pat
       (to-pat (instantiate-pat to bindings from-vars nil))
       (from-pat (instantiate-pat from bindings (collect-vars to-pat) t))
       
      )
    (format-if *trace* "~%Creating new ~a rule" (ilink-ilink ilink))
    (xpn from-pat *trace*)
    (xpn to-pat *trace*)
    (make-ilink :from from-pat :to to-pat :ilink (ilink-ilink ilink))))

;;;replace special from and to variables by corresponding object from rule
(defun subsitute-pattern (var rule)
  (cond((var? var)
	(cond ((eq (var-name var) '*from*)
	       (gen-rule-from rule))
	      ((eq (var-name var) '*to*)
	       (gen-rule-to rule))
	      (t var)))
       (t var)))

;;;(instantiate-pat pat bindings vars lhs)- instantiate pattern using bindings 
;;;for constraint if var is member of vars and lhs is true.  If member
;;;of vars and lhs is false, retain var.  Otherwise, replace var by
;;substituting bindings.
(defun instantiate-pat(pat bindings vars &optional (lhs nil))
  (cond ((var? pat) 
	 (if (member (var-name pat) vars)
	     (if lhs
		 (list '*var* (var-name pat) (lookup-var (var-name pat) bindings))
		 (list '*var* (var-name pat)))
	     (lookup-var (var-name pat) bindings)))
	((role? pat) pat)
	((cd-p pat)
	 (if (var? (cd-head pat))
	     ;;special syntax only if the a scene is a variable
	     (instantiate-pat (cd-head pat) bindings vars lhs)
	     (make.cd (cd-head pat)
		      (mapcar #'(lambda (fpair)
				  (make.feature (feature-name fpair)
						(instantiate-pat (feature-value fpair)
								 bindings vars lhs)))
			      (cd-features pat)))))
	(t pat)))


;;;(blame-difference cd exception events pat)-
;;;finds a feature of cd which is not present in events
;;;if no one feature will do, then the conjunction of features
;;;is tried
(defun blame-difference (cd exception events pat)
  (let ((filler (lookup-var exception (cd-match pat cd)))
	(event-fillers (mapcar #'(lambda(e)
				   (lookup-var exception
					       (cd-match pat e)))
			       events)))
    (when filler
      (or (first-image #'(lambda(fpair)
			   (let((name (feature-name fpair))
				(value (feature-value fpair)))
			     (unless (eq name 'unique-id)
			       (if (some #'(lambda(ef)
					     (compatible value
							 (role-filler ef name)))
					 event-fillers)
				   nil
				   (make.cd (cd-head filler)
					    (list fpair))))))
		       (cd-features filler))
	  (if (some #'(lambda(ef)
			(every #'(lambda(fpair)
				   (let((name (feature-name fpair))
					(value (feature-value fpair)))
				     (compatible value
						 (role-filler ef name))))
			       
			       (cd-features filler)))
		    event-fillers)
	      nil
	      filler)))))



;;;(merge-roles cd1 cd2)-  add roles of cd2 to cd1 unless role
;;;is present in cd1
(defun merge-roles (cd1 cd2)
  (make.cd (cd-head cd1)
	   (append (cd-features cd1)
		   (subset #'(lambda(fpair)
			       (not (find-feature (feature-name fpair)
						  (cd-features cd1))))
			   (cd-features cd2)))))


;;;(make-sub-schema-from-tdl similar gen rule parent difference)-
;;;1. add ilinks from rule to gen- done so match-gen-rule will work
;;;2. retain only type information from bindings
;;;3. create cd for new schema by instantiating ilinks of gen-rule
;;;4. make-sub-schema indexes similar
;;;5. add rule format to schema
;;;6. if difference change schema-cd to account for difference
(defun make-sub-schema-from-tdl(similar gen rule parent &optional(difference nil))
  (process-event-ilinks (car similar) gen similar)
  (let ((bindings (match-gen-rule rule gen)))
    (when bindings 
      (let*((gbindings (generalize-bindings bindings rule))
	    (gdbindings (insert-difference-into-bindings difference rule gbindings))
	    (schema-cd (instantiate-mechanism (gen-rule-ilinks rule) gdbindings))
	    (sub-schema (make-sub-schema schema-cd similar parent :tdl))
	    (irule (make-rule-from-cd rule gdbindings))
	    )
	(push (cons (generate-symbol "RULE-") irule)
	      (schema-rule sub-schema))
	(setf (schema-support sub-schema) rule)
	))))


;;;(insert-difference-into-bindings difference rule bindings)
;;;merge features of difference with the correesponding variable in bindings
(defun insert-difference-into-bindings(difference rule bindings)
  (let ((var-name (gen-rule-exceptions rule)))
    (cond (difference (mapcar #'(lambda(var.val)
				  (let((var (car var.val))
				       (val (cdr var.val)))
				    (if (eq var var-name)
					(cons var (merge-roles val difference))
					var.val)))
			      bindings))
	  (t bindings))))
