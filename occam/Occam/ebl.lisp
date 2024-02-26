;;; -*- Mode: LISP; -*-

;;;(ebl  event schema)-  top level explanation-based leraning function
;;;1. decompose event by instantiating and expanding components of the schema
;;;2. if outcome needs to be explained
;;;     3. construct abstract explanation from generalization-rule
;;;     4. refine explanation by verifying links with inference rules
;;;     5. mark features of event which were matched to create explanation
;;;     6. remove features of event which are not marked
;;;     7. replace role-tokens in event
;;;     8. create sequence of events from explanation chain
;;;     9. create sub-schema
(defun ebl (event schema)
  (format-if *trace* "~%Attempting explanation-based learning for new event.")
  (when (schema-events schema)			;ie decomposible schema
    (let* ((copy (copy-cd-structure event))
	   (expanded-event (fill-in-roles copy copy))
	   (schema-b (cd-match (schema-pattern schema) expanded-event))
	   (events (instantiate-components schema schema-b))
	   (to-slot (role-filler expanded-event (schema-outcome-slot schema)))
	   (to (first-image #'(lambda(e)
				(when (compatible e to-slot) e))
			    events)))
      ;;to is "outcome"- event which needs to be explained
      (when (unexplained to)
	(first-image #'(lambda (rule)
			 (setf rule (cdr rule)) ;;rule was (name. rule)
			 ;;match aginst gen-rule
			 (let ((b (cd-match (gen-rule-to rule) to))
			       (from (follow-ilink to (inverse-ilink (gen-rule-link rule)))))
			   (when (and b from)
			     (let ((all-b (cd-match (gen-rule-from rule) from b)))
			       ;;;this gen-rule applies so verify explanation
			       (when all-b
				 (setf all-b (bind-var-1 '*from* from all-b))
				 (setf all-b (bind-var-1 '*to* to all-b))
				 (let((bind.trail (infer-links (gen-rule-ilinks rule)
							       all-b)))
				   (when bind.trail
					 (make-ebl-sub-schema (car bind.trail)
							   (cdr bind.trail)
							   schema
							   expanded-event
							   rule
							   event
							   events))
				 
				 ))))))
		     (most-specific-first *all-gen-rules*)
		     )))))

(defun make-ebl-sub-schema (bind trail schema expanded-event rule event events)
  ;;explanation is verified
  ;;trail is alist of cds and patterns
  ;;bind is alist of vars from explanation
  (let ((mark (generate-symbol "EBL-"))
	new-events sub-schema)
  ;;mark the components used in matching
  ;;note that expanded-event and the events
  ;; in trail can share components because
  ;; events were formed by instantation with
  ;; components of expanded-event
  (mapc #'(lambda(cd.pat)
	    (mark-match (cdr cd.pat) (car cd.pat) mark))
	trail)
  (mark-match (schema-cd schema) expanded-event mark)
  ;;remove unreferenced features from expanded-event
  (mapc #'(lambda(x)(reap-marked-cd x mark))
	events)
  (reap-marked-cd expanded-event mark)
  ;;insert *roles* into expanded-event
  (reinsert-roles expanded-event)
  ;;create permant explanation by asserting
  ;; ilinks from gen-rule
  (insert-ilinks (gen-rule-ilinks rule) bind)
  ;;create new schema-events
  (setf new-events (collect-events events))
  (mapc #'replace-features-with-roles new-events)
  ;;the copy of expanded-events ensures that
  ;; the schema-events and schema-cd no longer share
  ;; features since the schema-events will have
  ;; variables inserted
  (setf expanded-event (copy-cd-structure expanded-event))
  (set-role-filler! expanded-event 'outcome (role-filler event 'outcome))  ; for debugging
  ;;insert the sub-schema in memory
  (setf sub-schema (make-sub-schema expanded-event
			      (list event)
			      schema
			      :ebl))
  ;;change the explanation into the general explantion
  ;; by replacing cd's with vars from schema-pattern
  ;; or by replacing roles with vars
  ;; :var marker is from instantiate-components
  (mapc #'(lambda(e)
	    (let((var (or (cdr(assoc :var (cd-alist e)))
			  (roles->vars e))))
						 
	      (setf (cd-head e) (cd-head var))
	      (setf (cd-features e) (cd-features var))))
	new-events)
				     
  (setf (schema-events sub-schema)
	new-events)
  (format-if *trace* "~%Creating generalization with EBL")
  (xpn expanded-event *trace*)
  (mapc #'(lambda(x)
	    (format-if *trace* "~%Formed event ~a with links" x)
	    (mapc #'(lambda(x)(format-if *trace* "~%~a" x))
		  (cd-links x))
	    (xpn x *trace*))
	new-events)
  sub-schema))

;;;(most-specific-first  x)-  reverses list of generalization rules
;;;so that longest explanation chain is used
(defun most-specific-first (x)
  (reverse x))

;;;(unexplained cd)- determines if cd is supported by
;;; an ilink which exolains why cd occured
(defun unexplained(cd)
  (not (or (follow-ilink cd 'resulted-from)
	   (follow-ilink cd 'result-enabled-by)
	   (follow-ilink cd 'enabled-by)
	    (follow-ilink cd 'forced-by)
	   (follow-ilink cd 'realizes)
           (follow-ilink cd 'implements)
	   (follow-ilink cd 'achieves))))
;;;(fill-in-roles  cd base)- replaces role-tokens of form (*role* <slot>)
;;;with the filler of <slot> in base.  Leaves a  :role marker on alist
;;;so that operation  can be undone after generalization
(defun fill-in-roles (cd base)
  (cond ((role? cd) (let((role (role-filler base (role-name cd))))
		      (unless (assoc :role (cd-alist role))
			(push (cons :role cd)
			      (cd-alist role)))
		      role))
	((cd-p cd)
	 (mapc #'(lambda (fpair)
		   (setf (feature-value fpair)
			 (fill-in-roles (feature-value fpair) base)))
	       (cd-features cd))
	 cd)
	(t cd)))

;;;(instantiate-components  schema b)-  instantiate the events of a schema
;;;with values from b and install ilinks.  Leaves a :var marker
;;;on alist.
(defun instantiate-components (schema b)
  (let* ((old (schema-events schema))
	 (new (mapcar #'(lambda (event)
			(instantiate event b)
			;;instantiate could return nil ifevent is unbound var
			)
		      old))
	 (alist (mapcar #'cons old new)))
    (mapc #'(lambda(o n)
	      (when n			     
		(setf (cd-links n)
		      (append (all-images #'(lambda(l)
					      (copy-and-substitute-link l n alist))
					  (cd-links o))
			      (cd-links n)))
		(push (cons :var o)
		      (cd-alist n))))
	  old new)
    (remove nil new)))

;;;(copy-and-substitute-link  l n alist)- create a new ilink like l
;;;from n to a copy of destination of l in alist

(defun copy-and-substitute-link (l n alist)
  (let ((to (cdr (assoc (ilink-to l) alist))))
    (when to
      (make-ilink :from n :ilink (ilink-ilink l) :to to))))

;;;(infer-links  links bindings matches)- tries to verify that
;;;an explanation proposed by links can be established using bindings.
;;;if the ilink is present, then it is simply followed
;;;otherwise infer-link is called to use inference rules.
;;;if destination of link is unbound variable, it is bound
;;;returns (bindings . matches) where bindings is an alist
;;;of variables used in infering the ilinks and matches is
;;;an alist of (cd . pat) of patterns used in the inference
(defun infer-links (links bindings &optional (matches nil))
  (if (null links)
      (cons bindings matches)
      (let*((link (car links))
	    (from-var (ilink-from link))
	    (ilink (ilink-ilink link))
	    (to-var (ilink-to link))
	    (from (lookup-var (var-name from-var) bindings))
	    (to (lookup-var (var-name to-var) bindings))
	    (to? (follow-ilink from ilink)))
	(format-if *trace* "~%Attempting to infer ~a." ilink)
	(xpn (or from from-var) *trace*)
	(xpn (or to? to to-var)*trace*)
	(when from
	  (cond (to?  ;;already infered from instantiation
		 (if to ;;i.e. if variable is bound
		     (when (eql to (follow-ilink from ilink))
		       (format-if *trace* "~%Established by following link.")
		       (infer-links (cdr links) bindings matches))
		     (infer-links (cdr links)
				  (bind-var-1 (var-name to-var)
					      to?
					      bindings))))
		
		(t (first-image #'(lambda (rule)
				    (let((m (infer-link rule from to)))
				      (when m
					(infer-links (cdr links)
						     (if (null to)
							 (bind-var-1 (var-name to-var)
								     (car m)
								     bindings)
							 bindings)
						     (append (cdr m) matches)))))
				(retrieve-rules-from-schemata from ilink))))))))

;;;(infer-link  rule from to)-  determines if "to" can be infered with "rule" from "from"
;;;returns (to (cd-to. pat-to )(cd-from . pat-from)) if inferred. Nil otherwise
(defvar *infer-link-break* nil)
(defun infer-link (rule from to)
  (when *infer-link-break* (break "~a ~a ~a" (setq *ito* to)
                                  (setq *ifrom* from)
                                  (setq *irule* rule)))
  (let((b (cd-match (ilink-from rule) from)))
    (when b
      
      (if (null to)				;;uninstantaited-var
        (let((new-to (instantiate (ilink-to rule) b)))
          (format-if *trace* "~%Established by inference rule.")
          (xpn (ilink-from rule)*trace*)
          (xpn (ilink-to rule)*trace*)
          (cons new-to (list (cons new-to (ilink-to rule))
                             (cons from (ilink-from rule)))))
        (let((new-b (cd-match (ilink-to rule) to b)))
          (when new-b
            (format-if *trace* "~%Established by inference rule.")
            (xpn (ilink-from rule)*trace*)
            (xpn (ilink-to rule)*trace*)
            (cons to (list (cons to (ilink-to rule))
                           (cons from (ilink-from rule))))))))))

;;;(mark-match  pat cd mark)-  put a mark on those components of cd which
;;;are present in pat.  Assumes pat and cd are compatible since they were
;;;returned by infer-link.
(defun mark-match (pat cd mark)
    (cond ((null cd)) 
	  ((var? pat) 
	    (mark-var pat cd mark))
	  ((cd-p cd)
	   (mark-cd cd mark)
           (mapc #'(lambda (fpair)
		 (let ((name (feature-name fpair))
		       (value (feature-value fpair)))
		   (mark-match value (filler-or-ilink cd name) mark)))
	     (cd-features pat)))
          ))

;;;(mark-var  var cd mark)-  recursively mark constraint of var
(defun mark-var (var cd mark)
  (when (var-constraint var)
    (mark-match (var-constraint var) cd mark)))

;;;(mark-cd  cd mark)- instal mark in alist of cd
(defun mark-cd (cd mark)
  (push (cons ':mark mark)
	(cd-alist cd)))

;;;(reap-marked-cd cd mark)-  destructively delete those features of CD
;;;which are not marked.
(defun reap-marked-cd(cd mark)
  (cond ((cd-p cd)
	 (if (has-mark? cd mark)
	     (let((fpairs (all-images #'(lambda(fpair)
					  (let((fv (reap-marked-cd (feature-value fpair)
								   mark)))
					    (make-feature-unless-null (feature-name fpair)
								      fv)))
				      (cd-features cd))))
	       (setf (cd-features cd) fpairs)
	       cd)))
	     
	(t nil)))

;;;(has-mark? cd mark)-  determines if cd is marked
(defun has-mark?(cd mark)
  (first-image #'(lambda(key.value)
		   (and (eq (car key.value) :mark)
			(eq (cdr key.value) mark)))
	       (cd-alist cd)))

;;;(reinsert-roles  cd)-  for each feature of cd
;;;replace the features of that features value with
;;;role tokens
(defun reinsert-roles (cd)
  (mapc #'(lambda (fpair)
	    (let*((value (feature-value fpair)))
	      (if (cd-p value)
		  (replace-features-with-roles value))))
	(cd-features cd)))

;;;(replace-features-with-roles  cd)-  if a feature value has a
;;;:role marker (inserted by fill-in-roles) then replace with
;;;role-token
(defun replace-features-with-roles (cd)
  (mapc #'(lambda (fpair)
	    (let*((value (feature-value fpair)))
	      (when (cd-p value)
		  (let((role (cdr (assoc ':role (cd-alist value)))))
		    (if role
			(setf (feature-value fpair) role)
			(replace-features-with-roles value))))))
	(cd-features cd)))

;;;(collect-events  event-list)- returns a list of events which are
;;;connected by ilinks
(defun collect-events (event-list &optional (visited nil))
  (cond((null event-list) visited)
       ((member (car event-list) visited)
	(collect-events (cdr event-list) visited))
       (t (collect-events (append (all-images #'ilink-to
					      (cd-links (car event-list)))
				  (cdr event-list))
			  (cons (car event-list) visited)))))

;;;(insert-ilinks  links bindings)-  follows through the links
;;;and ensures that the appropriate links and inverse are
;;;present.
(defun insert-ilinks (links bindings)
  (mapc #'(lambda(link)
	    (let*((from-var (ilink-from link))
		  (name (ilink-ilink link))
		  (to-var (ilink-to link))
		  (from (lookup-var (var-name from-var) bindings))
		  (to (lookup-var (var-name to-var) bindings)))
	      ;;vars should always be bound since they were returned
	      ;; by infer-link
	      (unless
		(eql to (follow-ilink from name))
		(push (make-ilink :from from
				  :to to
				  :ilink name)
		      (cd-links from))
		(push (make-ilink :from to
				  :to from
				  :ilink (inverse-ilink name))
		      (cd-links to)))))
	links))
