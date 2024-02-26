(defstruct (trule (:print-function
		(lambda(o stream ignore)
		  (format stream "<trule ~a ~a ~a/~a >"
			  (trule-from o)
			  (trule-to o)
			  (trule-c o)
                          (trule-n o))
			  )))
		from to (c 0) (n 0) cluster neg source a s cp disp)

(defun collect-ids (cd &optional (once nil))
  (cond ((var? cd) once)
        ((role? cd) once)
        ((cd-p cd)
         (let ((r (role-filler cd 'unique-id)))
           (if r (collect-ids-list (cd-features cd) (cons r once))
               (collect-ids-list (cd-features cd)  once))))
        (t once)))

(defun collect-ids-list(list once)
  (if (null list) once
      (collect-ids-list (cdr list) 
                        (collect-ids (feature-value (car list)) once))))

(defun duplicates(list &optional (c nil))
  (cond((null list) c)
       ((member (car list)(cdr list))
        (if (member (car list) c)
          (duplicates (cdr list) c)
          (duplicates (cdr list) (cons (car list) c))))
       (t (duplicates (cdr list) c))))


(defun insert-vars(cd roles &optional (constraint nil) (used nil))
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
                     (if (and id 
                              (setq role
                                    (cdr (assoc id roles))))
                       (if (and constraint 
                                ;(not (member id used))
) 
                         (progn (push id used)
                                (list '*var* role
                                      (remove-cd-ids (insert-vars value roles 
                                                                  constraint used)
                                                     t)))
                         (list '*var* role))
                       (insert-vars value roles constraint used)))))
              (remove-ids-and (cd-features cd) t)))))


(defun create-rule(cds &optional a old-a s old-s)
  (let* ((act (if old-s (make-general-cd old-a a)
                  (make-general-cd* (mapcar #'car cds))))
         (state (if old-s (make-general-cd old-s s)
                    (make-general-cd* (mapcar #'cadr cds))))
         (n (length cds))
         (id-vars  (mapcar #'(lambda(x)
                               (cons x (generate-symbol "V")))
                           (duplicates (collect-ids state (collect-ids act)))))
         (conse (insert-vars act id-vars t))
         (ante (insert-vars state id-vars nil))
         (vars (collect-vars ante))
         (conse (drop-vars conse vars)))
    (when (and conse ante)
      (make-trule :from conse :to ante :c n :n n :source :sbl :a act :s state))))
	

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
