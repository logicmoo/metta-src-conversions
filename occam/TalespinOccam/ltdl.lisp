(defvar *all-cps* nil)

(defmacro def-cp(from to &optional &rest dispositions)
  `(insert-cp (make-cp :from (list->cd ',from)
                       :to (list->cd ',to)
                       :dispositions (mapcar #'(lambda(x)
                                                 (make-disposition :role x))
                                             ',dispositions))))

(defun rule->cp(rule &optional old)
  (let((a (strip-constants (trule-from rule))))
    (when a
      (push-cp (make-cp :from a
                        :to (strip-constants (trule-to rule))
                        :uses (list (or old rule))
                        :dispositions (append (all-images #'(lambda(fpair)
                                                              (unless (eq (feature-name fpair) 'type)
                                                                (make-disposition :role 
                                                                                  (list (feature-name fpair))
                                                                                  :uses (list (or old rule)))))
                                                          (cd-features (trule-from rule)))
                                              (all-images #'(lambda(r)
                                                              (when (cdr r)
                                                                (make-disposition :role r
                                                                                  :uses (list (or old rule)))))
                                                          (paths-to-vars (trule-from rule)))))))))

(defun install-disposition(role cp)
  (push (make-disposition :role (if (cdr role) (butlast role)role))(cp-dispositions cp)))
(defun paths-to-vars(cd &aux x)
  (when (cd-p cd)
    (mapcan #'(lambda(fpair)
                    (if (var? (feature-value fpair))
                      (if (var-constraint (feature-value fpair))
                        (if (setq x (paths-to-vars (var-constraint (feature-value fpair))))
                        (mapcar #'(lambda(y)
                                      (cons (feature-name fpair) y)) 
                                x)
                          (list (list (feature-name fpair))))
                        (list (list (feature-name fpair))))
                      (if (setq x (paths-to-vars  (feature-value fpair)))
                          (mapcar #'(lambda(y)
                                      (cons (feature-name fpair) y)) 
                                x)
                          nil)))
                (cd-features cd))))

(defun push-cp(cp &aux m b r)
  (if (setq m (car (member cp *all-cps* 
                           :test #'(lambda(x y)
                                     (multiple-value-setq (r b)
                                                          (equal-under-substitution (cp-from x)
                                                                                    (cp-from y)))
                                     (if r (equal-under-substitution (cp-to x)
                                                                     (cp-to y))
                                                  
                                                  )))))
    (progn (push (car (cp-uses cp)) (cp-uses m))
           (setf (cp-dispositions  m) (merge-dispositions (cp-dispositions  cp)
                                              (cp-dispositions  m)))
           m)
    (insert-cp cp)))

(defun merge-dispositions (d1 d2)
  (append (mapcar  #'(lambda(d)
                      (setf (disposition-uses d)
                            (append (first-image #'(lambda(dd)
                                               (if (equal (disposition-role d)
                                                          (disposition-role dd))
                                                 (disposition-uses dd)))
                                           d2)
                                    (disposition-uses d)))
                      d)
                  d1)
                              
          (remove-if #'(lambda(d)
                         (member  (disposition-role d) d1 
                                  :test #'(lambda(x y)(equal x (disposition-role y)))))
                     d2)))

(defun insert-cp(cp)
  (setq *all-cps* (merge 'list (list cp) *all-cps* 
                         #'(lambda(x y)
                             (> (+ (length (cd-features (cp-from x)))
                                   (length (cd-features (cp-to x))))
                                (+ (length (cd-features (cp-from y)))
                                   (length (cd-features (cp-to y))))))))
  cp)

(defun strip-constants(cd)
  (cond ((var? cd) (if (var-constraint cd) 
                     (let  ((c (strip-constants (var-constraint cd))))
                       (if c 
                         (list '*var* (var-name cd)
                               c)
                         (list '*var* (var-name cd))))
                     cd))
        ((cd-p cd)
         (make.cd.unless (cd-head cd)
                         (all-images #'(lambda (fpair)
                                         (make.feature.unless (feature-name fpair)
                                                              (strip-constants(feature-value fpair))))
                                     (cd-features cd))))
        (t nil)))

(defparameter *tdl-always* t)
(defparameter *learn-cps* t)
(defun create-cp (new &optional old)
  (when *learn-cps*
    (when (and old (trule-cp old)) (deletef old (cp-uses (trule-cp old)))
          (mapc #'(lambda(d)(deletef old (disposition-uses d)))
                (cp-dispositions (trule-cp old))))
    (let ((cp (rule->cp new old)))
      (when cp  (cond (old (setf (trule-cp old) cp)
                           (if *tdl-always* (setf (trule-source old) :tdl)))
                      (t (if *tdl-always* (setf (trule-source new) :tdl))
                         (setf (trule-cp new) cp)))
            ))
    
    (when (and old (trule-cp old)) (if (null (cp-uses (trule-cp old)))
                (deletef (trule-cp old) *all-cps*)
                (deletef nil (cp-dispositions (trule-cp old))
                         :test #'(lambda(x d) x (null (disposition-uses d))))))))
      

(defun make.cd.unless(x y)
  (if (and x y)
    (make.cd x y)))

(defun print-cps()
  (mapc #'(lambda(cp)
            (format t "~%=================CP=~a========================"
                    (mapcar #'disposition-role (cp-dispositions cp)))
            (xpn (cp-from cp))(xpn (cp-to cp))
            (mapc #'print-rule (cp-uses cp)))
        *all-cps*))
  
(defun equal-under-substitution  (cd1 cd2 &optional (bindings nil) &aux r)
  (cond ((var? cd1) (if (var? cd2)
                      (equal-bind-var (var-name cd1) (var-name cd2) bindings)
                      nil))
        ((var? cd2) nil)
       ((not (cd-p cd1))(values (equal cd1 cd2) bindings))
       ((neq (cd-head cd1) (cd-head cd2))
	nil)
       ((= (length (cd-features cd1))(length (cd-features cd2)))
        (when (every #'(lambda(feature)
		     (let*((name (feature-name feature))
			   (filler1 (feature-value feature))
			   (filler2 (role-filler cd2 name)))
		       (when filler2 
                         (multiple-value-setq (r bindings)
			   (equal-under-substitution filler1 filler2 bindings))
                         r)))
		 (cd-features cd1))
          (values t bindings)))
       (t nil)))

(defun equal-bind-var (cd1 cd2 bindings)
  (let ((a (cdr (assoc cd1 bindings))))
    (if a 
      (values (eq a cd2) bindings)
      (values t (cons (cons cd1 cd2) bindings)))))


(defparameter *generalize-bindings*  t)
(defparameter *tdl*  t)

(defun fix-prediction-failure(prediction)
  (let*((a (prediction-a prediction))
        (s (prediction-cd prediction))
        (r (prediction-rule prediction))
        (new-rule nil)
        (cp (trule-cp r))
        (disp (trule-disp r))
        (cds (gethash (compute-hash a s) *clusters*)))
    
    (when cp
      (cond((and (null disp)
                 (setq new-rule (do ((rule (tdl cds  nil (trule-a r) nil (trule-s r)  nil cp)
                                           (progn (delete-rule rule)
                                           (tdl cds  nil (trule-a r) nil (trule-s r)  nil (trule-cp rule)))))
                                    ((or (null rule)
                                         (null (cd-match (trule-from rule) a)))
                                     rule)
                                  (push rule (cp-uses (trule-cp rule)))
                                  (setf (trule-cluster rule) (compute-hash a s)))))
            (delete-rule r)
            new-rule)))
    (new-rule a s r 
              (cond (new-rule new-rule)
                    (t (tdl-disp (trule-a r) (trule-s r) 
                                 (cons a (trule-neg r))
                                 (trule-n r)
                                 (/ (trule-c r) 
                                    (+ (trule-n r) 1))))))))

(defun new-rule (a s r new)
       (when new
         (push new *all-rules*)
         (setf (trule-cluster new) (compute-hash a s))
         (update-certainty a r new)
         (delete-rule r)
         new))
         
(defun update-certainty (a r new)
  (let* ((old-n (cons a (trule-neg r)))
         (new-n (subset #'(lambda(n) (cd-match (trule-from new) n nil nil)) old-n)))
    (setf (trule-neg new) (set-difference (cons a (trule-neg r)) old-n)
          (trule-n new) (+ 1 (trule-n r))
          (trule-c new) (- (trule-n new) (length new-n)))))

(defun tdl-disp (act state negs n beat-this)
  (when *tdl*
    (first-image
     #'(lambda(rule)
         (let* ((bindings (cd-match (cp-from rule) act nil nil))
                (bindings (when bindings (cd-match (cp-to rule) state bindings nil))))
           (when bindings
             (let* ((difference (blame-difference act (cp-dispositions rule)
                                                 negs
                                                 (+ n 1)
                                                 beat-this))
                   (new (when (and *learn-cps* (not (equal (car difference) 0)))
                          (new-difference act (cp-dispositions rule)
                                                 negs
                                                 (+ n 1)
                                                 beat-this))))
               (cond((or (and (null difference) new)
                         (and difference new (< (car new)
                                                (car difference))))
                     (setq difference new)
                     (install-disposition (cdr new) rule)))
               (when difference
                 (setq difference (cdr difference)) ;;remove number
                 (let* ((gbindings (generalize-bindings bindings))
                        (gact (set-role-filler! (instantiate (cp-from rule) gbindings)
                                                'type
                                                (role-filler act 'type)))
                        (g-side-effect-only (insert-difference difference gact act))
                        (gstate (set-role-filler 
                                 (set-role-filler! (instantiate (cp-to rule) gbindings)
                                                   'type
                                                   (role-filler state 'type))
                                 'mode
                                 (role-filler state 'mode)))
                        (id-vars  (mapcar #'(lambda(x)
                                              (cons x (generate-symbol "V")))
                                          (duplicates (collect-ids state (collect-ids gact)))))                    
                        (conse (insert-vars gact id-vars t))
                        (ante (insert-vars gstate id-vars nil))
                        (r (make-trule :from conse :to ante 
                                       :source :tdl :a act :s state :cp rule :disp difference)))
                   (push r (cp-uses rule))
                   (first-image #'(lambda(d)
                                    (if (equal (butlast difference)
                                               (disposition-role d))
                                      (push r (disposition-uses d))))
                                (cp-dispositions rule))
                   r))))))
     *all-cps*)))

;;;(blame-difference cd exception events pat)-
;;;finds a feature of cd which is not present in events
;;;if no one feature will do, then the conjunction of features
;;;is tried
(defun blame-difference (act roles negs n beat-this &aux winner all)
  (setq winner (car (setq all (sort (mapcan #'(lambda(role)(setq role (disposition-role role))
                                      (cons (if (role-filler* act role)
                                              (cons (count role negs :test #'(lambda(r x)
                                                                               (eq-head (role-filler* x r)
                                                                                      (role-filler* act r))))
                                                  role)
                                              (list 1000 'mark-spitz))
                                            (all-images #'(lambda(f)
                                                            (unless (eq (feature-name f) 'unique-id)
                                                            (cons (count f negs 
                                                                         :test #'(lambda(f x)
                                                                                   (eq-head (role-filler (role-filler* x role)
                                                                                                         (feature-name f))
                                                                                            (feature-value f))))
                                                                  (append role (list (feature-name f))))))
                                                        (cd-features-if (role-filler* act role))))) 
                                  roles)
                          #'(lambda(x y)(if (= (car x)(car y))
                                          (if (= (length x)(length y))
                                            (if (eq (car(last x)) 'type)
                                              t
                                              nil)
                                            (< (length x)(length y)))                                                 
                                          (< (car x)(car y))))))))
 (when (and winner (< (/ (car winner) n) beat-this))
   winner))


(defun new-difference (act roles negs n beat-this &aux winner all)
  (setq winner (car (setq all (sort (mapcan #'(lambda(role)
                                                (cons (if (role-filler act role)
                                                        (cons (count role negs :test #'(lambda(r x)
                                                                                         (eq-head (role-filler x r)
                                                                                                  (role-filler act r))))
                                                              (list role))
                                                        (list 1000 'mark-spitz))
                                                      (all-images #'(lambda(f)
                                                                      (unless (eq (feature-name f) 'unique-id)
                                                                        (cons (count f negs 
                                                                                     :test #'(lambda(f x)
                                                                                               (eq-head (role-filler (role-filler x role)
                                                                                                                     (feature-name f))
                                                                                                        (feature-value f))))
                                                                              (cons role (list (feature-name f))))))
                                                                  (cd-features-if (role-filler act role))))) 
                                           (all-images #'(lambda(fpair)
                                                                       (unless (or (eq (feature-name fpair) 'type)
                                                                                   (eq (feature-name fpair) 'time)
                                                                                   (member (feature-name fpair) roles 
                                                                                           :test #'(lambda(x y)
                                                                                                     (eq x (car (disposition-role y))))))
                                                                         (feature-name fpair)))
                                                                   (cd-features act)))
                                    #'(lambda(x y)(if (= (car x)(car y))
                                                    (if (= (length x)(length y))
                                                      (if (eq (car(last x)) 'type)
                                                        t
                                                        nil)
                                                      (< (length x)(length y)))                                                 
                                                    (< (car x)(car y))))))))
  (when (and winner (< (/ (car winner) n) beat-this))
    winner))



(defun insert-difference (difference gact act &aux r)
  (if (null difference) nil
        (if (setq r (role-filler gact (car difference)))
          (insert-difference (cdr difference) r (role-filler act (car difference)))
          (progn (set-role-filler! gact (car difference)
                                   (setq r (generalize-to-class (role-filler act (car difference)))))
                           (insert-difference (cdr difference) r (role-filler act (car difference)))))))
          
          
(defun eq-head(x y)
  (when (and (cd-p x)(cd-p y))
    (eq (cd-head x)(cd-head y))))

(defun cd-features-if(x)
  (if x (cd-features x)))

(defun delete-rule(r)
  (deletef r *all-rules*)
  (deletef r (cp-uses (trule-cp r)))
  (when *learn-cps*
    (if (null (cp-uses (trule-cp r)))
      (deletef (trule-cp r) *all-cps*)
      (deletef nil (cp-dispositions (trule-cp r))
                     :test #'(lambda(x d) x (null (disposition-uses d)))))))

(defun tdl (cds &optional a old-a s old-s cp cp-not)
  (when *tdl*
    (let* ((act (if old-s 
                  (if cp-not old-a
                      (make-general-cd old-a a))
                  (make-general-cd* (mapcar #'car cds))))
           (state (if old-s (if cp-not old-s
                                (make-general-cd old-s s))
                      (make-general-cd* (mapcar #'cadr cds)))))
      (first-image
       #'(lambda(rule)
           (let* ((bindings (cd-match (cp-from rule) act nil nil))
                  (bindings (when bindings (cd-match (cp-to rule) state bindings nil))))
             (when bindings
               (let* ((gbindings (if *generalize-bindings* 
                                   (generalize-bindings bindings)
                                   bindings))
                      (gact (set-role-filler! (instantiate (cp-from rule) gbindings)
                                              'type
                                              (role-filler act 'type)))
                      (gstate (set-role-filler 
                               (set-role-filler! (instantiate (cp-to rule) gbindings)
                                                 'type
                                                 (role-filler state 'type))
                               'mode
                               (role-filler state 'mode)))
                      (id-vars  (mapcar #'(lambda(x)
                                            (cons x (generate-symbol "V")))
                                        (duplicates (collect-ids state (collect-ids gact)))))
                      
                      (conse (insert-vars gact id-vars t))
                      (ante (insert-vars gstate id-vars nil))
                      (n (length cds))
                      (r (make-trule :from conse :to ante :c n :n n 
                                     :source :tdl :a act :s state :cp rule)))
                 r))))
       (if cp 
         (list cp)
         (if cp-not 
           (cdr(member cp-not *all-cps*))
           *all-cps*))))))



(defparameter *type-roles* '(type sub-type unique-id) "used to represent type hierachy")

;;;(generalize-bindings bindings)- retain only type information in bindings
;;;this creates the most specific class consistent with examples
(defun generalize-bindings (bindings)
  (mapcar #'(lambda(v.cd)
              (let((v (car v.cd))
                   (cd (cdr v.cd)))
                (cons v (generalize-to-class cd))))
          bindings))

;;;(generalize-to-class  cd)- remove all features except type features
(defun generalize-to-class(cd)
  (make.cd (cd-head cd)
	   (subset #'(lambda(fpair)
		       (member (feature-name fpair)
			       *type-roles*))
		   (cd-features cd))))