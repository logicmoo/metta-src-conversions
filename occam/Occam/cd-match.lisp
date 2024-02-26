;;; -*- Mode: LISP;-*-
;;;(cd-match pat cd  bindings)- return set of variable bindings if
;;;pat unifies with cd
(defun cd-match (pat cd  &optional (bindings nil))
  (setq bindings
	(bind-var '*pattern* pat
		  (bind-var '*cd* cd bindings)))
  (cd-match-1 pat cd bindings))

;;;(cd-match-1 pat cd  bindings)- perform the actual matching
(defun cd-match-1 (pat cd  bindings)
    (cond ((null cd) bindings) ;no conflict
	  ((role? pat)(if (and (role? cd)
			       (eql (role-name pat)(role-name cd))
			       )
			  bindings))
	  ((var? pat) 
	    (bind-var pat cd bindings))
	  ((and (cd-p cd) (eql (cd-head pat) (cd-head cd)))
           (match-features  (cd-features pat) cd bindings))
          (t nil)))

;;;(match-features pat-fl cd bindings)- for each feature in pat-fl
;;;insure that if cd contains feature, then feature matches
;;;returns bindings list.  Cd can contain feature as role name
;;;or as ilink
(defun match-features (pat-fl cd bindings &aux filler)
  (if (every #'(lambda (fpair)
		 (let ((name (feature-name fpair))
		       (value (feature-value fpair)))
		   (cond ((setq filler (filler-or-ilink cd name))
			  (setq bindings
				(cd-match-1 value filler bindings))) 
			 (t t))))
	     pat-fl)
      bindings
      nil))

;;;(lookup-var var var-alist)- find variable value in alist
(defun lookup-var (var var-alist)
  (cdr (assoc var var-alist)))

;;;(bind-var var cd bindings)- add var to bindings alist
;;; fails if constraint fails or var incompatible with
;;;present binding
(defun bind-var (var cd bindings)
  (cond((not(var? var)) ;*cd* or *pat*
	(cons (cons var cd) bindings))
       ((var-constraint var)
	(let ((b (cd-match-1 (var-constraint var) cd bindings)))	    
	  (if b (bind-var-1 (var-name var) cd b))))
       (t (bind-var-1 (var-name var) cd bindings))))

;;;(bind-var-1 var cd bindings)- add var to bindings
;;;unless incompatible with existing binding
(defun bind-var-1 (var cd bindings)
  (let ((v (lookup-var var bindings)))
    (if v
	(if (compatible v cd)
	    bindings
	    nil)
	(cons (cons var cd) bindings))))


;;;(instantiate pat bindings)- create a new cd by substituting
;;;values from bindings for variables
(defun instantiate (pat bindings)
  (cond ((var? pat)
	 (lookup-var (var-name pat) bindings))
	((role? pat) pat)
	((cd-p pat)
	 (if (var? (cd-head pat))
	     ;;special syntax only if the a scene is a variable
	     (lookup-var (var-name (cd-head pat)) bindings)
	     (make.cd (cd-head pat)
		      (all-images #'(lambda (fpair)
				  (make-feature-unless-null (feature-name fpair)
				      (instantiate (feature-value fpair)
						   bindings)))
				  (cd-features pat)))))
	(t pat)))
