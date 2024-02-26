;;; -*- Mode: LISP -*-

;;;(def-rule name lhs link rhs) - define a rule to infer rhs given lhs.
;;;index by lhs
(defmacro def-rule (name lhs link rhs)
  `(progn
     (setq ,name (make-ilink :from (list->cd ',lhs)
			     :to (list->cd ',rhs)
			     :ilink ',link))
     (find-schemata-to-index-rule (if (var? (ilink-from ,name))
				  (var-constraint (ilink-from ,name)) 
				  (ilink-from ,name))
			      ,name 
			      ',name)
     ))


;;;(find-schemata-to-index-rule rule-cd rule name)- find schema (or schemata) to
;;;index rule and add to list of rules
(defun find-schemata-to-index-rule(rule-cd rule name)
  (let((schemata (find-most-specific-schemata rule-cd)))
    (mapc #'(lambda(schema)
	      (let ((key (assoc name (schema-rule schema))))
		(if key (setf (cdr key) rule)
		       (setf (schema-rule schema)
			      (append (schema-rule schema) (list (cons name rule)))))))
	  schemata)))

;;;(retrieve-rules-from-schemata  cd link)- returns rules which can be used
;;;to infer link from cd. 
(defun retrieve-rules-from-schemata (cd link) 
  (let ((schemata (find-all-schemata cd (find-schema-subtree  cd))))
    (mapappend  #'(lambda(schema)
		(all-images #'(lambda(r)
				(if (eq (ilink-ilink (cdr r)) ;rule
					link)
				    (cdr r)))
			    (schema-rule schema)))
		schemata)))

;;;(find-all-schemata  cd schema)- returns the set of schemata which are more general
;;; than cd.  Schemata are ordered (most specific first) so that the most specific
;;;rules are tried first.
(defun find-all-schemata (cd schema) 
  (cond ((not(compatible-with-schema cd schema))  
	 nil)	;conflict, so no match
	((let ((sub-schemata (follow-links (schema-links schema) cd ':sub-schema
				 *uniqueness-threshold*)))
	   (append (mapcan #'(lambda (sub-schema)
			       (find-all-schemata cd sub-schema))
			   sub-schemata) (list schema))))	;recurse on sub-schemata
	(t (list schema))))


