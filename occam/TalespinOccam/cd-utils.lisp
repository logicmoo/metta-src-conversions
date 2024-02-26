;;; -*- Mode: LISP -*- 



;;;(make.feature name value)- create a feature
(proclaim '(inline  make.feature))
(defun make.feature (n v) (list n v))

;;;(feature-name feature)- returns name of a feature
(proclaim '(inline  feature-name))
(defun feature-name (f) (car f))

;;;(feature-value feature)- returns the filler of a feature
(proclaim '(inline  feature-value))
(defun feature-value (f) (cadr f))
(proclaim '(inline  rplacadr))
(defun rplacadr(x y)(rplaca (cdr x) y) y)
(defsetf feature-value rplacadr)

;;;(find-feature feature features)- lookup a feature in a feature list
(proclaim '(inline find-feature))
(defun find-feature(feature features)
  (assoc feature features))

;;;(role-filler cd role) - returns filler of named role of cd
(defun role-filler (cd role)
  (when (cd-p cd)
    (feature-value (find-feature role (cd-features cd)))))

;;;(role-filler* cd roles)- like role-filler but takes a list of roles
;;;(role-filler* (make.cd '(act actor (person name (john))) '(actor name))
;;; = <CD98 JOHN>
(defun role-filler*(cd roles)
  (cond((null roles) cd)
       ((null cd)nil)
       (t (let((new (role-filler cd (car roles))))
	    (if new (role-filler* new (cdr roles))
		nil)))))

;;;(delete-role  cd role)- delete a role from a CD
(defun delete-role (cd role)
    (make.cd (cd-head cd)
             (remove-if #'(lambda (fpair)
			        (eq (feature-name fpair) role))
		    (cd-features cd))))


;;;(delete-role!  cd role)- destructively delete a role from a CD
(defun delete-role! (cd role)
    (setf (cd-features cd)
             (remove-if #'(lambda (fpair)
			        (eq (feature-name fpair) role))
		    (cd-features cd))))

;;;(set-role-filler  cd role filler)- change value of a role
(defun set-role-filler (cd role filler)
    (let ((new (delete-role cd role)))
      (push (make.feature role filler) (cd-features new))
      new
      ))

;;;(set-role-filler!  cd role filler)- destructively change value of a role
(defun set-role-filler! (cd role filler)
  (delete-role! cd role)
  (push (make.feature role filler) (cd-features cd))
  cd)

;;;(set-role-filler!* cd roles filler)- like set-role-filler! but takes list of roles
(defun set-role-filler!*(cd roles filler)
  (cond((null(cdr roles))
	(set-role-filler! cd (car roles) filler) cd )
       (t (set-role-filler!* (role-filler cd (car roles)) (cdr roles) filler) 
	  cd)))


;;;(def-role cd role filler)- like set-role-filler!, doesn't eval role
(defmacro def-role (cd role filler)
  `(set-role-filler! ,cd ',role ,filler))



;;;(make.cd  head features name)-creates a new CD structure with
;;;the specified head and features.
(defvar *debug* nil)
(defun make.cd (head &optional (features nil) (name (generate-symbol "CD-")))
  (if *debug*
    (setf (get name 'cd)   ;plist is used for debugging only
          (make-cd :head head :features features :name name))
    (make-cd :head head :features features :name name)))

;;;(var?  x)- determines if a object is a cd variable
(defun var? (x)
    (and (consp x)
         (eq (car x) '*var*)))

;;;(var-name x)- return the name of a variable
(proclaim '(inline  var-name))
(defun var-name(x)(cadr x))

;;;(var-constraint x)- returns the contraint of a variable
(proclaim '(inline  var-constraint))
(defun var-constraint(x)(caddr x))



;;;(role? x)- determines if a object is a role token
(defun role?(x)
  (and (consp x)
       (eq (car x) '*role*)))


;;;(role-name x)- returns the  name of a role
(proclaim '(inline  role-name))
(defun role-name(x)(cadr x))

;;;(list->cd lis name)- internals the representation of a cd
;;;E.G. (list->cd '(act actor (human name (john))
;;;                     to (*role* actor)
;;;                     from (*var* from)))
;;;    = <CD101 ACT>
(defun list->cd (lis &optional (name (generate-symbol "CD-")))
       (cond ((atom lis) lis)
	      ((var? lis)(cond ((var-constraint lis)
				 (list '*var* (var-name lis)
				       (list->cd (var-constraint lis) nil)))
				(t lis)))
	      ((role? lis) lis)	     
	     (t (make.cd (car lis)
		     (do* ((flist (cdr lis)  (cddr flist))
			   (result nil))
			  ((null flist) result)
			  (push (make.feature (car flist)
				     (list->cd (cadr flist) nil)) 
				result))
		     name))))


;;;(cd->list  cd)-  creates the external rep of a cd.
;;;inverse of list->cd
(defun cd->list (cd)
  (cond ((cd-p cd)
	 (cons (cd-head cd)
	       (mapcan
		 #'(lambda (fp)
		     (cons (feature-name fp)
			   (list (cd->list (feature-value fp)))))
		 (cd-features cd))))
	((var? cd)
	 (if (var-constraint cd)
	     (list '*var* (var-name cd)
		   (cd->list 
		     (var-constraint cd)))
	     cd))
	((role? cd) cd)
	(t cd)))

;;; (def-cd name cd)- macro used to define CDS
;;; sets name to cdstructure
(defmacro def-cd (name cd)
  `(progn
     (setq ,name (list->cd ',cd ',name))
     ,name))

;;;(def-ilink from-cd link to-cd)- macro used to define link between two cds
(defmacro def-ilink (from link to)
	       `(push (make-ilink :from ,from :ilink ',link :to ,to)
		      (cd-links ,from)))

;;;(copy-cd-structure  cd)- creates a copy of a cd structure
(defun copy-cd-structure (cd)
  (cond ((var? cd)(cond ((var-constraint cd)
			      (list '*var* (var-name cd)
				    (copy-cd-structure (var-constraint cd))))
			     (t cd)))
	((role? cd) cd)
	((cd-p cd)
	 (make.cd (cd-head cd)
		  (mapcar #'(lambda (fpair)
			    (make.feature (feature-name fpair)
					  (copy-cd-structure (feature-value fpair))))
			  (cd-features cd))))
	(t cd)))

(defun drop-vars (cd vars)
  (cond ((var? cd) (if (member (var-name cd) vars) 
                     (if (var-constraint cd) 
                       (let  ((c (drop-vars (var-constraint cd) vars)))
                         (list '*var* (var-name cd)
                               c))
                       cd)
                     (drop-vars (var-constraint cd) vars)
                       ))
	((cd-p cd)
	 (make.cd (cd-head cd)
		  (all-images #'(lambda (fpair)
			    (make.feature.unless (feature-name fpair)
					  (drop-vars (feature-value fpair) vars)))
			  (cd-features cd))))
	(t cd)))

(defun make.feature.unless(x y)
  (if (and x y)(make.feature x y)))

;;;(contains-no-vars cd)- returns t if cd doesn't contain any vars
(defun contains-no-vars (cd)
  (cond ((var? cd) nil)
	((role? cd) t)
	((cd-p cd)
	 (every #'(lambda (fpair)
			    (contains-no-vars (feature-value fpair)))
			  (cd-features cd)))
	(t t)))


;;;(collect-vars cd)- returns all vars of cd
(defun collect-vars (cd)
  (cond ((var? cd) (cons (var-name cd)
			 (collect-vars (var-constraint cd))))
	((role? cd) nil)
	((cd-p cd)
	 (mapcan #'(lambda (fpair)
			    (collect-vars (feature-value fpair)))
			  (cd-features cd)))
	(t nil)))

;;;(cd-head-only cd)- removes all features from a cd
(defun cd-head-only(cd)
  (cond ((cd-p cd) (make.cd (cd-head cd)))
	((or (role? cd) (var? cd)) cd)))

;;; (follow-ilink  cd ilink)- returns a cd pointed to by an Ilink
(defun follow-ilink (cd ilink)
	(first-image #'(lambda(l)
			(if (eq ilink (ilink-ilink l))
			    (ilink-to l)))
		       (cd-links cd)))

;;;(filler-or-ilink cd name)- returns a cd which is
;;;either a role filler or pointed to by an Ilink
(defun filler-or-ilink(cd name)
  (or (role-filler cd name) 
      (follow-ilink cd name)))

;;;(make-feature-unless-null name value)
(defun make-feature-unless-null(name value)
  (if value (make.feature name value)))

;;; (inverse-ilink link)- returns the inverse of an ilink
(defun inverse-ilink(link)
  (get link 'inverse-ilink))



(defprop result inverse-ilink resulted-from)
(defprop resulted-from inverse-ilink result)

(defprop forces inverse-ilink forced-by)
(defprop forced-by inverse-ilink forces)

(defprop before inverse-ilink after)
(defprop after inverse-ilink before)

(defprop enables inverse-ilink enabled-by)
(defprop enabled-by inverse-ilink enables)


(defprop blocks inverse-ilink blocked-by)
(defprop blocked-by inverse-ilink blocks)

(defprop leads-to inverse-ilink led-to-by)
(defprop led-to-by inverse-ilink leads-to)

(defprop result-enables inverse-ilink result-enabled-by)
(defprop result-enabled-by inverse-ilink result-enables)

(defprop intends inverse-ilink intended-by)
(defprop intended-by inverse-ilink intends)

(defprop realizes inverse-ilink realized-by)
(defprop realized-by inverse-ilink realizes)


(defprop achieves inverse-ilink achieved-by)
(defprop achieved-by inverse-ilink achieves)

(defprop motivates inverse-ilink motivated-by)
(defprop motivated-by inverse-ilink motivates)


(defprop thwarts inverse-ilink thwarted-by)
(defprop thwarted-by inverse-ilink thwarts)

(defprop outcome inverse-ilink outcome-of)
(defprop outcome-of inverse-ilink outcome)

(defprop initiates-action  inverse-ilink action-initiated-by)
(defprop action-initiated-by inverse-ilink initiates-action)


(defprop implements inverse-ilink implemented-by)
(defprop implemented-by inverse-ilink implements)


(defvar *max-xpn-depth* 4)
(defvar *max-xpn-length* 10)
(defvar *trace* t)
;;;(xpn cd)-  Prettyprint a cd
(defun xpn(cd &optional (trace t))
  (when trace
    (format t "~%")
    (cond((not(or(cd-p cd) (var? cd)))
          (setq cd (get cd 'cd))))
    (if (and (cd-p cd)
             (var? (cd-head cd)))
      (setq cd (cd-head cd)))
    (pp-xpn cd 0 *max-xpn-depth*)))

;;;(pp-xpn  cd column) perform the actual printing of a cd-head
(defun pp-xpn (cd &optional (column 0) (depth 10))
  (cond ((cd-p cd)
	 (let*((head (format nil "(~a" (cd-head cd)))
	       (length (length head)))
	   (format t "~a" head)
	   (if (<= depth 0) (format t ")")
             (pp-features (reverse (remove-ids (cd-features cd)))
			(+ column length) t (- depth 1)))))
	((and (var? cd) (var-constraint cd))
	 (let* ((head (format nil "(*VAR* ~a " (var-name cd)))
		(length (length head)))
	   (format t "~a" head)
	   (pp-xpn (var-constraint cd) (+ column length) (- depth 1))
	   (format t ")")))
	(t (format t "~a" cd))))

(defparameter *remove-ids* nil)
(defparameter *remove-family* '(mother father husband wife))

(defmacro c(x)
  `(get ',x 'cd))

(defun remove-ids(flist)
 (subset #'(lambda(x)(and (if *remove-ids* (neq (feature-name x) 'unique-id)
                              t)
                          (not (member (feature-name x) *remove-family*))))
         flist))

(defun remove-ids-and (flist p)
  (subset #'(lambda(x)(and (neq (feature-name x) 'unique-id)
                               (neq (feature-name x) 'time)
                               (or p (not(member (feature-name x) *property-states*)))))
	      flist))

(defun remove-cd-ids(cd p)
  (cond ((var? cd)(cond ((var-constraint cd)
			      (list '*var* (var-name cd)
				    (remove-cd-ids (var-constraint cd) p)))
			     (t cd)))
	((role? cd) cd)
	((cd-p cd)
	 (make.cd (cd-head cd)
		  (mapcar #'(lambda (fpair)
			    (make.feature (feature-name fpair)
					  (remove-cd-ids (feature-value fpair) p)))
			  (remove-ids-and (cd-features cd) p))))
	(t cd)))
  

;;;(pp-features  features column first depth)- print each feature
;;;followed by its value.  Each feature starts in the same column
(defun pp-features (features column &optional (first t) depth (long 0))
  (cond((or (null features) (>= long *max-xpn-length*))
	(format t ")"))
       (t (let*((name (feature-name (car features)))
		(name-string (format nil " ~a " name))
		(length (length name-string))
		(value (feature-value (car features))))
	    (unless first 
		    (format t "~%")
		    (blank-string column))
	    (format t "~a" name-string)
	    (pp-xpn value (+ column length) depth)
	    (pp-features (cdr features) column nil depth (+ long 1))))))
	  
;;;(blank-string  n)- prints a string of blanks of length n
(defun blank-string (n)
  (unless (= n 0) 
      (format t " ")
      (blank-string (- n 1))))

(defun merge-cd(cd1 cd2 &aux f)
  (cond((cd-p cd1) 
        (make.cd (cd-head cd1)
                 (append
                  (mapcar #'(lambda(fpair)
                              (make.feature (feature-name fpair)
                                            (if (and (cd-p cd2)
                                                     (setq f (role-filler cd2 (feature-name fpair))))
                                              (merge-cd (feature-value fpair) f)
                                              (feature-value fpair))))
                          (cd-features cd1))
                  (all-images #'(lambda(x)(if (null (role-filler cd1 (feature-name x)))
                                            x))
                              (cd-features cd2)))))
       (t cd1)))

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