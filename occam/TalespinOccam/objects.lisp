
;;;definition of schema structure
(defstruct (schema (:print-function
		(lambda(o stream ignore)
		  (format stream "<~a ~a>"
			  (schema-name o)
			  (schema-cd o)
			  ))))
	   cd 	   	  ;set of features
	   name 
	   (links nil) 	  ;links to other schemata or instances
	   pattern 	  ;cd pattern
	   events	  ;seq. of events, filled
	   type           ;ebl, sbl, tdl
	   (parent nil)   ;parent of this schema
	   transfer       ;representational-transfer
	   rule           ;rule associated with cd
	   support        ;justification information
	   outcome-slot   ;slot which indicates result of event
	  )

;;;definition of cd structure
(defstruct (cd (:print-function
		(lambda(o stream ignore)
		  (format stream "<~a ~a>"
			  (set-plist (cd-name o) o)
			  (cd-head o)
			  ))))
		  head 
		  features
		  name
		  (links nil)
		  (alist nil))

(defun set-plist(x v)
  (setf (get x 'cd) v) x)

;;definition of intentional links which relate 2 cds
(defstruct (ilink (:print-function
		(lambda(o stream ignore)
		  (format stream "<~a ~a ~a>"
			  (ilink-from o)
			  (ilink-ilink o)
			  (ilink-to o)))))
	from ilink to)

;;;definition of link which connects schema and event or schema and sub-schema
(defstruct (link (:print-function
		(lambda(o stream ignore)
		  (format stream "<~a ~a ~a>"
			  (link-slot o)
			  (link-cd o)
			  (link-child o)
			  ))))
	   schema		  ;from
	   child	  ;to
	   slot		  ;slot name
	   cd  		  ;slot value
	   (count 0)	  ;number of "similar" links
	   type		  ; :sub-schema or :event			
	   )

;;;definition of generalization rule
(defstruct (gen-rule (:print-function
		(lambda(o stream ignore)
		  (format stream "<gen-rule ~a ~a ~a ~a>"
			  (gen-rule-name o)
			  (gen-rule-from o)
			  (gen-rule-link o)
			  (gen-rule-to o)
			  ))))
		  name from link to ilinks exceptions)
;;;;;kludges for bugs

(defstruct cp from to uses dispositions)
(defstruct disposition role uses)
