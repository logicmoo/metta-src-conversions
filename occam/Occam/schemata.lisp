
(defvar *top* nil "start of schema tree")
(defvar *all-sub-schemata* nil "All user defined specializiations- used by reset-memory")

;;;(def-schema name cd pattern events)- macro to define a new schema
(defmacro def-schema(name cd pattern events)
  `(let((schema (make-schema :name  ',name
		   :cd (list->cd ',cd)
		   :pattern (list->cd ',pattern)
		   :events ,events)))
     (setf (get ',name 'schema) schema)  
     (setq *top* (cons schema (subset #'(lambda(m)
				       (not(compatible (schema-cd schema) (schema-cd m))))
				    *top*)))  ;remove duplicates
     
  ))


;;;(def-sub-schema name parent cd pattern events)
;;;like def-schema but creates specialization of parent schema.
(defmacro def-sub-schema (name parent cd pattern events)
  `(let((schema (make-schema :name  ',name
		       :cd (list->cd ',cd)
		       :pattern (list->cd ',pattern)
		       :events ,events))
	(parent-schema (get ',parent 'schema)))
     (setf (get ',name 'schema) schema)
     (mapc #'(lambda (link)
	       (add-link parent-schema link))
	   (make-links (delete-difference (schema-cd schema)
					  (schema-cd parent-schema))
		       parent-schema
		       schema
		       ':sub-schema
		       ))
     ;;insert uniquely into *all-sub-schemata*
     (let ((key (assoc ',name *all-sub-schemata*)))
       (if key (setf (cdr key) (list schema ',parent))
	   (push (list ',name schema ',parent) *all-sub-schemata*)))
     ',name))

;;;(reset-memory)- restores memory to initial state
(defun reset-memory()
  ;;reset top level schemata
  (mapc #'(lambda(x)(setf(schema-links x) nil)) *top*)
  ;;re-index user defined sub-schemata
  (mapc #'(lambda(key)(let((schema (second key))
			   (parent-schema (get (third key) 'schema)))
			(mapc #'(lambda (link)
				  (add-link parent-schema link))
			      (make-links (delete-difference (schema-cd schema)
							     (schema-cd parent-schema))
					  parent-schema
					  schema
					  ':sub-schema
					  ))
			;;reset user defined sub-schemata
			(setf(schema-links schema) nil)))
	*all-sub-schemata*))


;;;(make.schema  cd)- return a new schema structure
(defun make.schema (cd)
       (let* ((name (generate-symbol "SCHEMA-")))
	     (setf (get name 'schema)
		       (make-schema :cd cd :name name))))

;;;MAKE-SUB-SCHEMA creates a new sub schema. It gets the similar features and
;;;stores them as features of the new schema. It stores the events as the
;;;instances of the new sub schema, and adds the sub schema to the sub schema list
;;;of the "parent" schema. Finally, it moves the old events from the
;;;"parent" schemata instance list.
(defun make-sub-schema (cd events parent type)
  (let* ((sub-schema (make.schema cd)))
    ;;;remove-events from parent schema
    (when parent (mapc #'(lambda (link)
			   (if (member (link-child link) events)
			       (remove-link parent link)))
		       (schema-links parent))
	  ;;;index new sub-schema under parent 
	  (mapc #'(lambda (link)
		    (add-link parent link))
		(make-links (delete-difference cd (schema-cd parent))
			    parent
			    sub-schema
			    ':sub-schema))
	  (setf (schema-pattern sub-schema) (schema-pattern parent))
	  (setf (schema-outcome-slot sub-schema) (schema-outcome-slot parent))
	  (setf (schema-events sub-schema) (schema-events parent))
	  (setf (schema-parent sub-schema) parent)
	  )
    ;;;index events under new-schema
    (mapc #'(lambda (event)
	    (index-event event sub-schema))
	  events)
    
    (setf (schema-type sub-schema) type)
    sub-schema
    ))



;;;(index-event  event schema)- create links from schema to event
(defun index-event (event schema &aux diff)
  (format-if *trace* "~%Indexing event ~a under schema ~a." event schema)
  (setq diff (delete-difference event (schema-cd schema)))
  (let ((links (make-links  diff schema event ':event)))
        (mapc #'(lambda (link)
		(add-link schema link))
	    links)))

;;;(make-links  difference from to type)- for each difference between
;;;from and to create a new link to index to.
(defun make-links (difference from to type)
  (mapcar #'(lambda (fpair)
	      (make.link from to (feature-name fpair) (feature-value fpair) type))
	  (cd-features difference)))

;;;(delete-difference  cd1 cd2)- retain only those features of cd1 which are not in cd2
(defun delete-difference (cd1 cd2)
    (cond ((null cd2) cd1)
	  ((role? cd1)   (cond((and (role? cd2)
			       (eq (role-name cd1) (role-name cd2)))
			  nil)
			 (t cd1)))				    
          ((not (cd-p cd1))
           (cond ((eq cd1 cd2) nil)
                 (t cd1)))
          ((not (cd-p cd2)) nil)
          ((eq (cd-head cd1) (cd-head cd2))
           (let ((diff-feat (delete-difference-features (cd-features cd1) (cd-features cd2))))
             (if diff-feat (make.cd (cd-head cd1) diff-feat) nil)))
          (t nil)))

;;;(delete-difference-features fl1 fl2) - retain a feature in fl1 if fl2
;;; doesn't have that feature
(defun delete-difference-features (fl1 fl2 &aux fpair2)
  (all-images #'(lambda (fpair)
		  (let ((name (feature-name fpair))
			(value (feature-value fpair)))
		    (unless (eq name 'unique-id) ;;ignore meaningless difference
		      (cond ((setq fpair2 (find-feature name fl2))
			     (let ((new (delete-difference value
							   (feature-value fpair2))))
			       (if (not (null new))
				   (make.feature (feature-name fpair2) new)
				   nil)))
			    (t fpair)))))
	      fl1))



;;;(make.link  from to slot cd type)- create a link structure
(defun make.link (from to slot cd type)
       (make-link :schema from :child to :slot slot :cd cd :type type))

;;;(add-link  schema link)- add a link to a schema- incrementing counters
(defun add-link (schema link)
  (let* ((new-cd (link-cd link))
	 (slot (link-slot link))
	 (matched-links (subset #'(lambda (lnk)
				    (and (eq (link-slot lnk) slot)
					 (compatible new-cd (link-cd lnk))))
				(schema-links schema)))
	 )
    ;;increment counter of similar events
    (mapc #'(lambda (l)
	      (incf (link-count l)))
	  matched-links)
    (setf (link-count link) (1+ (length matched-links)))
    (push link (schema-links schema))))


;;;(remove-link  schema link)- remove a link from a schema- decrementing counters
(defun remove-link (schema link)
  (let ((new-cd (link-cd link))
	(slot (link-slot link)))
    ;;remove link
    (setf (schema-links schema) (remove link (schema-links schema)))
    ;;decrement counter of similar events
  (mapc #'(lambda (lnk)
	    (if (and (eq (link-slot lnk) slot)
		     (compatible (link-cd link)
				 new-cd))
		(decf (link-count lnk))))
	(schema-links schema))))

;;;(compatible cd1 cd2)- returns T if there are no conflicts between cd1 and cd2
;;;i.e. CD2 has a role that cd1 has but with a different filler
(defun compatible(cd1 cd2)
  (cond((role? cd1) (and (role? cd2)
			 (eq (role-name cd1) (role-name cd2))))
       ((role? cd2) nil)
       ((not (cd-p cd1))(equal cd1 cd2))
       ((neq (cd-head cd1) (cd-head cd2))
	nil)
       (t (every #'(lambda(feature)
		     (let*((name (feature-name feature))
			   (filler1 (feature-value feature))
			   (filler2 (role-filler cd2 name)))
		       (if (and filler2 (neq name 'unique-id))
			   (compatible filler1 filler2)
			   t)))
		 (cd-features cd1)))))
;;;(find-schema-subtree cd)- finds the right branch of *top* to start search
(defun find-schema-subtree(cd)
  (first-image #'(lambda(m)(when (compatible (schema-cd m) cd)
			     m))
	       *top*))

(defvar  *uniqueness-threshold* 2
  "number of times a feature can appear in other schemata and still be predictive")
;;;(find-most-specific-schemata cd schema)- returns set most specific of
;;;most specific schema in memory that account for cd.
(defun find-most-specific-schemata(cd &optional (schema (find-schema-subtree cd)))
    (cond ((compatible-with-schema cd schema)
	   (let* ((sub-schemata
		   (follow-links (schema-links schema) cd ':sub-schema
				 *uniqueness-threshold*))
		  (good-subschemata
		    (mapcan #'(lambda (sub-schema)
				(find-most-specific-schemata cd sub-schema))
			    sub-schemata)))
	     (if good-subschemata
		 good-subschemata
		 (list schema))))
	  (t (delete-schema cd schema)
	     nil)))

;;;(compatible-with-schema cd schema)- makes sure cd matches schema-cd and
;;;if schema formed by :tdl makes sure ilink matches also
(defun compatible-with-schema(cd schema)
  (and (compatible cd (schema-cd schema))
       (if (eq (schema-type schema) :tdl)
	   (and
	     (follow-ilink cd (gen-rule-link (schema-support schema)))
	     (compatible (follow-ilink cd (gen-rule-link (schema-support schema)))
			 (follow-ilink (schema-cd schema) (gen-rule-link (schema-support schema)))))
	   t)))

;;;(follow-links  links instance type max)- for each feature of instance, 
;;;follow links of type
(defun follow-links (links instance type max)
       (unique (mapcan #'(lambda (fpair)
                              (find-link (feature-value fpair)
                                         (feature-name fpair)
                                         links
                                         type
                                         max))
                          (cd-features instance))))



;;;(find-link  cd slot links type max)-
;for each link, return the child if the the slot and value are compatable
;and the link count is less than the threshold
(defun find-link (cd slot links type max)
       (unique (all-images #'(lambda(link)
				    (when (and (eq (link-slot link) slot)
					       (>= max (link-count link))
					       (eq (link-type link) type)
					       (compatible (link-cd link) cd))
				      (link-child link)))
			     links))) 


;;;(delete-schema  cd schema)- remove schema from memory
;;;and reindex children
(defun delete-schema (cd schema)
  (cond ((or (eq (schema-type schema) :sbl)
	     (eq (schema-type schema) :tdl))
	 (format-if *trace* "~%Deleting schema, incorrect prediction.")
	 (if (eq (schema-type schema) :tdl)
	     (xpn (follow-ilink (schema-cd schema)
				(gen-rule-link (schema-support schema))) *trace*)
	     (xpn (delete-difference (schema-cd schema)
				     (make-general-cd cd (schema-cd schema))) *trace*))
	 (let((children (retrieve-children schema))
	      (parent (schema-parent schema)))
	   (remove-schema-indices schema parent)
	   (mapc #'(lambda(event)
		     (index-event event parent))
		 children))))
  (if (schema-transfer schema)
      (setf *top* (remove (third (schema-transfer schema))	;high-level schema
			  *top*))))


;;;(retrieve-children schema)- returns all retreiveable children indexed
;;;under a schema
(defun retrieve-children(schema)
  (unique (all-images #'(lambda(link)
			  (if (and (eq (link-type link) :event)
				   (>= *retrievability-threshold*
				       (link-count link)))
			      (link-child link)))
		      (schema-links schema))))


;;;(remove-schema-indices schema parent)- removes indices from a
;;;parent-schema to a specialization, effectively deleting the schema
(defun remove-schema-indices (schema parent)
  (mapc #'(lambda(link)
	    (if (eq (link-child link) schema)
		(remove-link parent link)))
	(schema-links parent)))

;;;(occam-lite event)- top level call to occam-lite
;;;finds the most specific schema for event and
;;;then attempts ebl, tdl, and sbl.
(defun occam-lite(event)
  (format-if *trace* "~%~%----------------------------------------------------------------")
  (format-if *trace* "~%Looking for most specific schema for ~a" event)
  (xpn event *trace*)
  (let ((schemata (find-most-specific-schemata event)))
    (mapc #'(lambda(schema)
	      (format-if *trace* "~%Found most specific schema for event: ~a" event)
	      (format-if *trace* "~%Event is accounted for by:")
	      (xpn (schema-cd schema) *trace*)	      
	      (cond ((ebl event schema))
		    ((tdl event schema))
		    ((sbl event schema))
		    (t (index-event event schema))))
	  schemata)))

(defun describe-hierarchy( &optional (mop (car *top*)))
  (let((kids (unique (mapcar #'link-child (schema-links mop)))))
    (cons mop (mapcar #'(lambda(x)(if (schema-p x)
                                    (describe-hierarchy x)
                                    (list x)))
                      
                      kids))))
