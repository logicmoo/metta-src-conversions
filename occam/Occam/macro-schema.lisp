
;;;(process-event-ilinks  event general-cd all-events visited)-
;;;recursively find and generalize connepts connected to event
;;;attach generalizations to general-cd
(defun process-event-ilinks (event general-cd all-events &optional (visited nil))
  (process-ilinks event general-cd all-events
			    (cons (cons event general-cd) visited)
			    (cd-links event)))

;;;(process-ilinks  event general-cd all-events visited ilinks)-
;;;process each ilink in ilinks
;;;if the ilink-to has been visited skip
;;;if some events don't have ilink skip
;;;otherwise- generalize ilink-tos and attach
;;;and recurse on ilink-to
;;;visited is ((event . generalization)*)
(defun process-ilinks (event general-cd all-events visited ilinks)
  (cond ((null ilinks) visited)
	(t (let* ((link (car ilinks))
		  (link-name (ilink-ilink link))
		  (new-event (ilink-to link))
		  (new-events (mapcar #'(lambda(x)(follow-ilink x link-name)) all-events))
		  (visited-event (cdr(assoc new-event visited))))
	     (cond ((and visited-event 
			 (some #'(lambda(link) 
				   (and (eq (ilink-ilink link) link-name)
					(eq (ilink-to link)
					    visited-event)))
			       (cd-links general-cd)))		    
		    (process-ilinks event general-cd all-events visited (cdr ilinks)))
		   ((member nil new-events) 
		    (process-ilinks event general-cd all-events
				    (cons (list new-event) visited)	;not generalized
				    (cdr ilinks)))
		   (t (let ((new-general-cd (or visited-event (make-general-cd* new-events))))
			(push (make-ilink :from general-cd
					  :to new-general-cd
					  :ilink link-name)
			      (cd-links general-cd))
			(push (make-ilink :from new-general-cd
					  :to general-cd
					  :ilink (inverse-ilink link-name))
			      (cd-links new-general-cd))
			(process-ilinks event general-cd all-events
					(if visited-event
					    visited
					    (process-event-ilinks new-event new-general-cd
								  new-events visited))
					(cdr ilinks)))))))))


;;;(create-macro-schema  sub-schema general-cd ilinked-events)-
;;; 1. get a name for the schema
;;; 2. collect and name events
;;; 3. collect and name roles
;;; 4. create a generalized event for the macro schema
;;; 5. create a pattern to instantiate sequence of events
;;; 6. create representational transfer
(defun create-macro-schema (sub-schema general-cd events cluster)
    (format-if *trace* "~%Creating a macro schema") 
  (let* ((macro-name (ask-user-for-name general-cd "schema"))
	 (macro-events (name-events events))
	 (macro-roles  (name-roles events macro-events))
	 (macro-cd (create-macro-cd macro-name macro-roles))
	 (macro-pattern (make.cd (cd-head macro-cd)
				 (mapcar
				   #'(lambda(fpair)
				       (make.feature (feature-name fpair)
						     (list '*var* (feature-name fpair))))
				   (cd-features macro-cd))))
	 (macro-seq (create-sequence-of-events macro-events))
	 (transfer-from (make.cd (replace-ids-with-vars general-cd macro-roles)))
	 (transfer-to (make-transfer-to macro-cd macro-roles transfer-from))
	 (transfered-children (mapcar #'(lambda(e)
					  (transfer-rep e
							transfer-from
							transfer-to))
				      cluster))
	 (transfer-schema (make-sub-schema macro-cd transfered-children nil :sbl))
	 )
    (setf (schema-transfer sub-schema)
	  (list transfer-from transfer-to transfer-schema))
    (format-if *trace* "~%Creating representional transfer.")
    (xpn  transfer-from *trace*)
    (xpn transfer-to *trace*)
    (format-if *trace* "~%Creating macro schema.")
    (xpn macro-cd *trace*)
    (setf (schema-events transfer-schema) macro-seq)
    (setf (schema-pattern transfer-schema) macro-pattern)    
    transfer-schema
    ))

;;;(name-events events)- returns an alist of name and event
(defun name-events(events)
  (mapcar #'(lambda(e)
	      (cons e (ask-user-for-name e "role")))
	  events))

;;;(name-roles  events roles)-
;;;create a name for each unique component of the events
(defun name-roles (events roles)
  (labels ((search-event-for-roles (cd)
	     (cond ((cd-p cd)
		    (let ((id (role-filler cd 'unique-id)))
		      (when
			(and id
			     (not (assoc id roles
					 :test #'(lambda(id role)
						 (eql (role-filler role 'unique-id)
						      id)))))
			(push (cons cd (ask-user-for-name cd "role"))
			      roles))
		      
		      (mapc #'(lambda (fpair)
				(search-event-for-roles (feature-value fpair)))
			    (cd-features cd)))
		    ))))
    (mapc #'search-event-for-roles events)
    roles))

;;;(create-macro-cd  name role-alist)-
;;;make the macro structure by replacing features by role tokens
(defun create-macro-cd (name role-alist)
  (make.cd name
	   (mapcar #'(lambda(cd.role-name)
			    (let ((cd (car cd.role-name))
				  (role-name (cdr cd.role-name)))
			      (make.feature role-name
					    (insert-role-name cd role-alist))))
		   role-alist)))


;;;(insert-role-name cd roles)-
;;;copy cd inserting role-tokens in features
(defun insert-role-name(cd roles &optional (top nil))
  (if (not (cd-p cd))
      cd
      (make.cd (cd-head cd)
	       (mapcar
		 #'(lambda(fpair)
		     (make.feature
		       (feature-name fpair)
		       (let*((value (feature-value fpair))
			     (id (role-filler value 'unique-id))
			     role)
			 (if (and id (null top)
				  (setq role
					(cdr (assoc id roles
						    :test
						    #'(lambda(id role)
							(eql (role-filler role
									  'unique-id)
							     id))))))
			     (list '*role* role)
			     (insert-role-name value roles)))))
		 (cd-features cd)))))


;;;(create-sequence-of-events events-alist)-
;;;each event in network of generalized events is replaced by a variable
;;;the variables correspond to the name of the roles in the macro-schema
(defun create-sequence-of-events(events-alist)
  (let((var.events (mapcar #'(lambda(cd.role)
			       (cons (car cd.role)
				     (make.cd (list '*var* (cdr cd.role)))))
			   events-alist)))
    ;;var.events is ( (old-event . cd-variable)*)
    (mapc #'(lambda(cd.role)
	      (let* ((event (car cd.role))
		     (new-from (cdr(assoc event var.events))))
		(mapcar #'(lambda(ilink)
			    (let((new-to (cdr(assoc (ilink-to ilink) var.events))))
			      (push (make-ilink :from new-from
						:to new-to
						:ilink (ilink-ilink ilink))
				    (cd-links new-from))))
			(cd-links event))))
	  events-alist)
    (cdr(first var.events))))

;;;(replace-ids-with-vars  cd macro-roles)-
;;;copy cd but replace cds with tokens by variables
(defun replace-ids-with-vars (cd roles) 
  (if (not (cd-p cd))
      nil
      (let ((id (role-filler cd 'unique-id))
	    (role (cdr (assoc cd roles)))
	    (rest (make.cd (cd-head cd)
			   (all-images
			     #'(lambda(fpair)
				 (make-feature-unless-null
				   (feature-name fpair)
				   (replace-ids-with-vars
				     (feature-value fpair) roles)))
			     (cd-features cd)))))
	(cond(role (if (contains-no-vars rest)
		       (list '*var* role)
		       (list '*var* role rest)))
	     ((and id 
		   (setq role
			 (cdr (assoc id roles
				     :test
				     #'(lambda(id role) 
					 (eql (role-filler role
							   'unique-id)
					      id))))))
	      (if (contains-no-vars rest)
		  (list '*var* role)
		  (list '*var* role rest)))
	     ((contains-no-vars rest) nil)
	     (t rest)))))



;;;(roles->var cd)- replace roles with variables
(defun roles->vars(cd)
  (cond ((var? cd)(cond ((var-constraint cd)
			      (list '*var* (var-name cd)
				    (roles->vars (var-constraint cd))))
			     (t cd)))
	((role? cd) (list '*var* (role-name cd)))
	((cd-p cd)
	 (make.cd (cd-head cd)
		  (mapcar #'(lambda (fpair)
			    (make.feature (feature-name fpair)
					  (roles->vars (feature-value fpair))))
			  (cd-features cd))))
	(t cd)))

;;;(make-transfer-to cd roles pattern)- create transfer-to
;;;by substiting vars for used roles
;;;roles is ( (cd . role)*)
(defun make-transfer-to (cd roles pattern)
  (let* ((used-vars (unique (collect-vars (cd-head pattern))))
	 (unused-var-alist (subset #'(lambda(cd.role)
				       (member (cdr cd.role) used-vars))
				   roles)))
    (roles->vars (insert-role-name cd unused-var-alist))))


;;;(transfer-rep cd from to)- use representational transfer
;;;to change rep of cd to high-level rep
;;;instantiate to with bindings from from
;;;roles is ( (cd . role)*)
(defun transfer-rep (cd from to &optional (arg t))
  (if (and (cd-p from)
	   (var? (cd-head from)))
      (setf from (cd-head from))) ;scene syntax
  (let*((transfer-cd (instantiate to (cd-match from cd)))
	(roles (mapcar #'(lambda(fpair)
			   (cons (feature-value fpair)
				 (feature-name fpair)))
		       (cd-features transfer-cd))))
    (insert-role-name transfer-cd roles arg)))
