

(defvar *levels-of-detail* 2
	"Indicates the number of levels of detail to keep in skeleton")



;;;(create-skeleton cd base n)- retain n levels of detail more than base
(defun create-skeleton(cd base n)
	(let ((skeleton (create-skeleton-1 cd base)))	
	  (if (= n 1)
	      skeleton
	      (create-skeleton cd skeleton (- n 1)))))

;;;(create-skeleton cd base n)- retain one level of detail more than base
(defun create-skeleton-1(cd base)
  (cond ((null base) (cd-head-only cd))
	((not (cd-p cd)) cd)
	(t (make.cd (cd-head cd)
		    (mapcar #'(lambda(fpair)
				    (let*((name (feature-name fpair))
					 (filler1 (feature-value fpair))
					 (filler2 (role-filler base name)))
				      (make.feature name
						    (create-skeleton-1 filler1 filler2))))
			    (cd-features cd))))))

(defvar *min-cluster-size* 3 "minimum size of a cluster")

;;;(aggregate event schema size)- Find a set of events similar to event
(defun aggregate(event schema &optional (size  *min-cluster-size*))
  (let((skeleton (create-skeleton event (schema-cd schema) *levels-of-detail*))
       (cluster nil))
    (mapc #'(lambda(feature)
	      (let* ((events (retrieve-events schema feature))
		     (new-cluster(largest-subset feature skeleton events (schema-cd schema))))
		(if (< (length cluster)(length new-cluster))
		    (setq cluster new-cluster))))
	  (cd-features skeleton))
    (push event cluster)
    (if (>= (length cluster) size)
	cluster
	nil)))

(defvar *retrievability-threshold* 4
	"The number of times a features can appear in other
             events and still be retrieved")

;;;(retrieve-events  schema feature)- retrieves events similar to an event
;;;;indexed by feature.
(defun retrieve-events (schema feature)
  (find-link (feature-value feature)
	     (feature-name feature)
	     (schema-links schema)
	     :event
	     *retrievability-threshold*))

;;;(largest-subset  feature skeleton events)- finds the largest subset of events
;;;which have at least two features in common with
(defun largest-subset (feature skeleton events schema-cd)
  (let ((cluster nil))
    (mapc #'(lambda(new-feature)
	      (when (neq new-feature feature)
		(let((cd (feature-value new-feature))
		     (name (feature-name new-feature))
		     (tempcluster nil)
		     )
		  (mapc #'(lambda(newevent)
			    (let((filler (role-filler newevent name)))
			      (if (and  filler (compatible  filler cd))
				  (push newevent tempcluster))))
			events)
		  (if (and (elaborates-on (make-general-cd* tempcluster) schema-cd)                           
                           (< (length cluster)(length tempcluster)))
		      (setq cluster tempcluster)))))
	  (cd-features skeleton))
    cluster))

(defun elaborates-on (cd1 cd2)
  (delete-difference cd1 cd2))

(defvar *enough-for-a-macro-schema* 4
  "Number of events needed to create macro schema")
;;;(sbl event schema)- Top level similarity-based learning function
(defun sbl(event schema)
    (format-if *trace* "~%Attempting similarity-based learning for new event.")
  (let ((cluster (aggregate event schema)))
    (when cluster
          (format-if *trace* "~%Retrieved cluster of similar events.~%~a" cluster)
      (let* ((general-cd (make-general-cd* cluster))
	    (ilinked-events (process-event-ilinks (car cluster)
					  general-cd cluster))
	    (sub-schema (make-sub-schema general-cd cluster schema :sbl)))
	(format-if *trace* "~%Creating new specialization:")
	(xpn general-cd *trace*)	
	(if (> (length ilinked-events)
	       *enough-for-a-macro-schema*)
	    (create-macro-schema sub-schema general-cd
				 (mapcar #'cdr ilinked-events)
				 cluster)
            sub-schema))
      )))
	

;;;(make-general-cd*  cds)- Find all features a number of cds have in common
(defun make-general-cd* (cds)
  (cond((null(cdr cds))
	(car cds))
       ((make-general-cd (car cds)
			 (make-general-cd* (cdr cds))))))

;;;(make-general-cd cd1 cd2)- finds all features that two cds have in common
;;;and creates new unique-ids
(defun make-general-cd(cd1 cd2)
  (cond ((not (cd-p cd1))
	 (cond ((equal cd1 cd2) cd1)
	       (t nil)))
	((not (cd-p cd2)) nil)
	((eq (cd-head cd1) (cd-head cd2))
	 (let ((same (make-general-cd-features (cd-features cd1)
					       (cd-features cd2))))
	   (make.cd (cd-head cd1) same)))
	(t nil)))


;;;(make-general-cd-features  fl1 fl2)- finds all commonalities
;;;or two sets of feature lists
(defun make-general-cd-features (fl1 fl2 &aux fpair2)
  (make-id fl1 fl2
	   (all-images #'(lambda (fpair)
			   (let ((name (feature-name fpair))
				 (value (feature-value fpair)))
			     (cond ((setq fpair2 (find-feature name fl2))
				    (let ((new (make-general-cd value
								(feature-value fpair2))))
				      (if new
					  (make.feature (feature-name fpair2) new)
					  nil)))
				   (t nil))))
		       fl1)))



;;;(make-id fl1 fl2 fl)- add a general id to feature list
;;;a general id is a concatenation of unique ids in a canonical
;;;(alphabetical) order
(defun make-id(fl1 fl2 fl)
  (let((fl1-id  (find-feature 'unique-id fl1))
       (fl2-id  (find-feature 'unique-id fl2)))
    (if (or (null fl1-id)(null fl2-id))
	fl
	(cons (make.feature 'unique-id
			    (make-id-name (feature-value fl1-id)
					  (feature-value fl2-id)))
	      (subset #'(lambda(fpair)(neq (feature-name fpair)
					     'unique-id))
		      fl)))))

;;;(make-id-name id1 id2)- construct if name by combining components
;;;of id1 and id2
(defun make-id-name(id1 id2)
  (let*((idlist1 (split-id id1))
	(idlist2 (split-id id2))
	(unique-ids (unique (append idlist1 idlist2)))
	(slist (sort (mapcar #'(lambda(x)(format nil "~a" x)) unique-ids)
		     #'string<)))
    (intern (append-strings slist "!"))))

;;;(append-strings slist char)- appends stings in slist together, 
;;;seperated by char
(defun append-strings(slist char)
  (cond ((null slist) nil)
	(t(let((x (append-strings(cdr slist) char)))
	    (if x (format nil "~a~a~a" (car slist) char x)
		(car slist))))))

;;;(split-id atom)- returns a list of atoms by seperating components
;;;of atoms.  The component of atom are seperated by !
(defun split-id(atom)
  (let*((s (format nil "~a" atom))
	(i(position #\! s :test #'char=)))
    (if (not i)(list atom)
	(cons (intern (subseq s 0 i))
	      (split-id (intern (subseq s  (+ i 1))))))))
