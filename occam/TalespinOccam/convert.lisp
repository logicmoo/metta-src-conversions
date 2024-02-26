(defparameter *k-states* '(afraid alive broken broken dead peeled happy hungry dirty flowing
                           inflated tied flying  bored
                           shattered sharp
                           bursted
                     mloc on open out ringing ring sd smart stupid thirsty))
(defparameter *k-relations* '(filled loc cont attached))
(defparameter *k-actions* '(atrans grasp ingest mbuild tilt plan propel ungrasp
                            tie expel
                            grasp ptrans want mtrans))

;;; traversed by get-feat-list which also uses push. in the end
;;; new cd roles come out in proper order, hence reversing of roles
;;; xpn prints roles in order reverse to the feat list.
;;; so,roles get reversed by convert-cd and then again by xpn
(defconstant  *role-accessors* '(#'old-cd-actor #'old-cd-object #'old-cd-to
			   #'old-cd-from #'old-cd-ante #'old-cd-conseq
			   #'old-cd-part #'old-cd-val #'old-cd-con
			   #'old-cd-mode #'old-cd-time))

(defparameter *all-properties* '(age mother father husband wife open broken ring on
			   gender hair complexion composition hardness component-of
                           temperture shape
			   color size component kamal))
(defparameter *clusters* (make-hash-table :test #'equal :size 100))

(defun talespin->occam(cd)
  (insert-mode (make.cd (head-type cd)
           (nreverse(cons (make.feature 'type (make.cd (type-type cd)))
                 (all-images #'(lambda (role)
                               (let ((x (funcall (get role 'path-fn) cd)))
                                 (if (listp x)(setq x (car x))) ;modes are lists
                                 (if x (make.feature role (role-filler->cd x)))))
                             *roles*))))))

(defun role-filler->cd (slot-val &optional (done nil))
  (if (symbolp slot-val)
    (let ((new-slot-list  (if (slot-type slot-val)
                            (list 
                             (make.feature 'unique-id slot-val)
                             (make.feature 'type (make.cd (slot-type slot-val))))
                            nil)))
      (unless (member slot-val done)
        (do* ((plist-remaining *all-properties* (cdr plist-remaining))
              (property-name (first plist-remaining) (first plist-remaining))
              (property-val (get slot-val property-name) (get slot-val property-name)))
             
             ;; exit only using return below
             (nil)
          
          ;; body of do*
          ;; create new slot name
          ;; if property is FACTS, value is not atomic
          (cond ((not (null property-val))
                 
                 (push (make.feature property-name (role-filler->cd property-val 
                                                                    (cons slot-val done)))
                       new-slot-list)
                 ))
          (when (null (cddr plist-remaining)) 
            (return))))
      (make.cd (or (slot-head slot-val) slot-val) new-slot-list))
    (make.cd slot-val)))

(defun insert-mode(x)
  (if (and (or (eq (cd-head x) 'act)
                (eq (cd-head x) 'relation)
               (eq (cd-head x) 'state))
           (null (role-filler x 'mode)))
    (set-role-filler! x 'mode (make.cd 'pos))
    x))

(defun slot-type(x)(get x 'is-a))
(defun slot-head(x)(if (get x 'is-a) 'PP))

(defun head-type(x)
  (cond((member (old-cd-head x) *k-actions*) 'act)
       ((member (old-cd-head x) *k-relations*)  'relation)
       ((member (old-cd-head x) *k-states*)  'state)
       
       (t (format t "~%No class ~a" (old-cd-head x)))))

(defun type-type(x)(old-cd-head x))
(defun cd-head-if(x)(if x (cd-head x)))
                
(defparameter *memory* nil)                                     
(defun cluster (act state)
  (if *memory* 
    (push (list act state)
          (gethash (compute-hash act state)
                   *clusters*))
    (setf (gethash (compute-hash act state)
                   *clusters*)
          (list (list act state)))))

(defun compute-hash (act state)
  (list (cd-head-if (role-filler act 'type))
        (cd-head-if (role-filler act 'from))
        (cd-head-if (role-filler state 'type))
        (cd-head-if (role-filler state 'mode))))


(defvar *predictions* nil)
(defstruct prediction cd rule c a s)
(defvar *all-rules* nil)
(defun make-predictions (cd &aux b)
  (mapc #'(lambda(r)
            (when (and (setq b (cd-match (trule-from r) cd nil nil))
                       (every #'(lambda(v)
			  (lookup-var v b))
		      	      (collect-vars (trule-from r))))
              (push (make-prediction :cd (remove-cd-ids (instantiate (trule-to r) b) nil)
                                     :rule  r 
                                     :c (/ (trule-c r) (trule-n r))
                                     :a cd)
                    *predictions*)))
        *all-rules*)
  *predictions*)
                          
(defun predicted-by (rule state act)
  (let((b (cd-match (trule-from rule) act nil nil)))
    (when b (cd-match (trule-to rule) state b nil))))
                      
(defvar *learn*  t)
(defvar *stats* t)
(defvar     *total-states*)   
(defvar     *total-acts*)   
(defvar     *unexpected-states*)   
(defvar     *total-predictions* )
(defvar     *unfufilled-predictions*)

(defun reset-stats()
  (setq     *total-states* 0)   
  (setq     *total-acts* 0)   
  (setq     *unexpected-states* 0)   
  (setq     *total-predictions* 0)
  (setq     *unfufilled-predictions* 0)
  )

(defun print-stats()
  (format t "~%Percent unexpected states  ~a" 
          (if (= 0 *total-states*) 0
              (/ (* *unexpected-states* 100.0) *total-states*)))
  (format t "~%Inaccurate predictions:    ~a" 
          (if (= 0 *total-predictions*) 0
              (/ (* *unfufilled-predictions* 100.0) *total-predictions*))))

(defparameter *sbl* t)
(defvar *testmax*)  
(defun process-time(x &aux cds new)
  (setq x (mapcar #'cdr x))  ;;occam-reps
  ;;car is talespin rep, cdr occam reps
  ;;this is the function to change for cobweb etc.
  (let* ((acts (subset #'(lambda(x)(eq (cd-head x) 'act)) x))
         (states (set-difference x acts))
         expected        ;prediction
         unexpected      ;state
         unfufiled       ;prediction
         new-rule
        )     
    
    (multiple-value-setq (expected unexpected unfufiled)
                         (sort-predictions *predictions* states))
    (cond (*stats*
           (incf *total-states* (length states))
           (incf *total-acts* (length acts))
           (mapc #'(lambda(p)
                     (incf *total-predictions*))
                 *predictions*)
           (incf *unexpected-states* (length unexpected))
           (mapc #'(lambda(p) 
                     (incf *unfufilled-predictions*))
                 unfufiled)
           (when (and *next-test* (not (> *next-test* *testmax*))
		      (>  *total-states* *next-test*))
		 (compute-next-stats))))

    (when (and *yell* unfufiled)
      (format t "~%---U-N-F-U-F-I-L-L-E-D-----")
      (mapc #'(lambda(x)(xpn (prediction-cd x))
               (xpn (trule-from (prediction-rule x)))
               (xpn (trule-to (prediction-rule x)))
               )
            unfufiled))
    
    (cond (*learn* 
           (mapc #'(lambda(x)
                     (cond ((and *tdl* *generalize-bindings* 
                                 (setq new-rule (fix-prediction-failure x)))
                            (setq unexpected (delete-if #'(lambda(z)
                                                             (predicted-by new-rule z 
                                                                           (prediction-a x)))
                                                         unexpected)))
                           (t (when *yell*
                            (xpn (trule-from (prediction-rule x)))
                            (xpn (trule-to (prediction-rule x)))
                            (xpn (prediction-cd x))
                            (xpn (prediction-a x))
                            (format t "~%======unexpected===========~%")
                            (mapc #'xpn unexpected))
                            (push (trule-neg (prediction-rule x)) (prediction-a x))
                            (incf (trule-n (prediction-rule x))))))
                 unfufiled)
           (mapc #'(lambda(x)
                     (incf (trule-n (prediction-rule x)))
                     (incf (trule-c (prediction-rule x)))
                     (cluster (prediction-a x)(prediction-s x)))
                 expected)
           (setq unexpected (delete-if  #'(lambda(s)
                                           (first-image #'(lambda(r)
                                                            (and (cd-match (trule-to r) s)
                                                                 (near-miss r s acts)))
                                                        *all-rules*))
                                       unexpected))
           (mapc #'(lambda(s)
                     (some #'(lambda(a)
                               (setq cds (cons (list a s) (gethash (compute-hash a s) *clusters*)))
                               (setq new (tdl cds))
                               (when new
                                 (push new *all-rules*)
                                 (cluster a s)
                                 (setq unexpected (delete s unexpected))
                                 (push new (cp-uses (trule-cp new)))
                                 (setf (trule-cluster new) (compute-hash a s))
                                 t)
                               )
                           acts))
                 unexpected)
           (when *sbl* (mapc #'(lambda(s)
                     (mapc #'(lambda(a)
                               (cluster a s)
                               (setq cds (gethash (compute-hash a s) *clusters*))
                               (setq new (create-rule cds))
                               (create-cp new)
                               (when new
                                 (push new *all-rules*)
                                 (setf (trule-cluster new) (compute-hash a s)))
                               )
                           acts))
                             unexpected))))
    (when (and *yell* unexpected)
      (format t "~%---U-N-E-X-P-E-C-T-E-D-----")
      (print-cds unexpected)))
  
  (setq *predictions* nil)
  )

(defun near-miss(r s acts)
  (first-image #'(lambda(act)
                   (when (equal (compute-hash act s)
                                (trule-cluster r))
                     (revise-rule r s act)
                     t))
               acts))

(defun revise-rule(r s a &aux cds new)
  (cluster a s)
  (setq cds (gethash (compute-hash a s) *clusters*))
  (cond((and (eq (trule-source r) :sbl)
             (setq new (create-rule cds a (trule-a r) s (trule-s r))))
        (create-cp new r)
        ;(unless (= (trule-n new) (+ 1 (trule-n r)))
        ;(break))
        (setf (trule-to r)(trule-to new)
              (trule-from r)(trule-from new)
              (trule-a r)  (trule-a new)
              (trule-s r)  (trule-s new)
              ; (trule-n r)(trule-n new)
              ; (trule-c r)(trule-c new)
              )
        (incf (trule-n r))
        (incf (trule-c r)))
       ((and (eq (trule-source r) :tdl)
             (setq new (tdl cds a (trule-a r) s (trule-s r) (trule-cp r))))
        ;(unless (= (trule-n new) (+ 1 (trule-n r)))
        ;(break))
        (setf (trule-to r)(trule-to new)
              (trule-from r)(trule-from new)
              (trule-a r)(trule-a new)
              (trule-s r)(trule-s new)
              (trule-n r)(trule-n new)
              (trule-c r)(trule-c new)
              )
        (incf (trule-n r))
        (incf (trule-c r))
        )))

      ; ((eq (trule-source r) :tdl)
      ;  (break))
              

              
(defun sort-predictions(predictions states &aux b)
  (let ((expected nil)
        (unexpected states)
        (unfufiled nil))
    (mapc #'(lambda(p)
              (cond ((setq b (car (member p states 
                                          :test #'(lambda(p s)
                                                    (compatible (prediction-cd p)
                                                                s)))))
                     (setq unexpected (delete b unexpected))
                     (push p expected)
                     (setf (prediction-s p) b))
                    (t (push p unfufiled))))
          predictions)
    (values expected unexpected unfufiled)))

(defun print-predictions()
  (mapc #'(lambda(x)(format T "~%______________________")
           (xpn (prediction-cd x))
           (xpn (trule-from(prediction-rule x))))
        *predictions*))

(defun print-cds(cds)
  (mapc #'xpn cds))
  

(defun print-rules()
  (mapc #'print-rule *all-rules*))

(defun print-rule(x)
 (format t "~%~%~%--------------- ~a ~a/~a ~a ~a--------------------" 
         (trule-cluster x)  (trule-c x)(trule-n x)(trule-source x)(trule-disp x))
 (xpn (trule-from x))
 (xpn (trule-to x)))


(defun process-times()
  (mapc #'process-time  *analyzed-cds*))
                                     
(defun print-hash(&optional all &aux x)
  (maphash #'(lambda(key val)
               (format t "~%~%~%--------------- ~a ~a --------------------" 
                       key (length val))
               (setq x (create-rule val))
               (xpn (trule-from x))
               (xpn (trule-to x))
               (if all (mapc #'(lambda(x)
                                 (xpn (car x))
                                 (xpn (cadr x)))
                             val)))
           *clusters*))

(defvar *yell* nil)
(defun do-stories(n &optional (reset t))
  (when reset (setq *clusters* (make-hash-table :test #'equal :size 100))
        (reset-stats) (setq *learn* t)(setq *stats* t)(setq *predictions* nil)
        (setq *yell* nil)
        (setq *all-rules* nil))
  (unless (= n 0)
    (aspin)
    (do-stories (- n 1) nil)))
               
(defun test-rules(n &optional (reset t))
  (when reset
    (setq *learn* nil)
    (setq *stats* t)
    (setq *yell* t)
    (format t "~%=============Cumulative Stats===============")
    (print-stats)
     (reset-stats))
  (cond((= n 0)
        (format t "~%=============Final Stats===============")
        (print-stats))
       (t (aspin)(test-rules (- n 1) nil))))
        

(defun test-alot(n &optional (reset t))
  (when reset (setq *clusters* (make-hash-table :test #'equal :size 100))
        (setq *learn* t)(setq *stats* t)(setq *predictions* nil)
        (setq *yell* nil)
        (setq *all-rules* nil))
  (unless (= n 0)
    (reset-stats)
    (aspin)
    (format t "~%======================Stats ~a ====================" n)
    (print-stats)
    (test-alot (- n 1) nil)))


(defun full-trace()
  (setq *trace-print-level* (setq *trace-print-length* nil)))


(defvar *next-test* nil)
(defvar *increment* 10)
(defvar *stat-list* nil)

(defun compute-next-stats(&aux cds 		
			       (total-states 0)
			       (total-acts 0)
			       (total-predictions 0)
			       (wtotal-predictions 0)
			       (unfufilled-predictions 0)
			       (wunfufilled-predictions 0)
			       (unexpected-states 0)
			       (expected-states 0)
			       (wexpected-states 0))
  (mapcar #'(lambda(x)
	      (let* ((acts (subset #'(lambda(x)(eq (cd-head x) 'act)) x))
		     (states (set-difference x acts))
		     expected		;prediction
		     unexpected		;state
		     unfufiled		;prediction

		     )
		(setq *predictions* nil)
		(mapc #'make-predictions acts)
		(multiple-value-setq (expected unexpected unfufiled)
				     (sort-predictions *predictions* states))
		(incf total-states (length states))
		(incf total-acts (length acts))
		(mapc #'(lambda(p)
			  (incf total-predictions)
			  (incf wtotal-predictions (prediction-c p))
			  )
		      *predictions*)
		(incf unexpected-states (length unexpected))
		(mapc #'(lambda(p) 
			  (incf unfufilled-predictions)
			  (incf wunfufilled-predictions (prediction-c p)))
		      unfufiled)
		(mapc #'(lambda(p) 
			  (incf expected-states)
			  (incf wexpected-states (prediction-c p)))
		      expected)
		(setq *predictions* nil)))
	  *all*)
  (if (= total-predictions 0)(setq total-predictions 1))
  (if (= wtotal-predictions 0)(setq wtotal-predictions 1))
  (insert-next *next-test*
	       (list (/ (* unexpected-states 100.0) total-states)
		     (* 100.0 (- 1 (/ wexpected-states total-states)))
		     (* 100.0 (- 1 (/ expected-states total-states))))
	       (list (/ (* unfufilled-predictions 100.0) total-predictions)
		     (/ (* wunfufilled-predictions 100.0) wtotal-predictions)))
  (incf *next-test* *increment*))

(defun insert-next (n a b &aux x)
  (if (setq x (assoc n *stat-list*))
    (push (cons a b) (cdr x))
    (push (cons n (list (cons a b))) *stat-list*)))


(defun run-and-test (n max &optional (reset t) (reset-cp nil))
  (unless (= n 0)
  (setq *testmax* max)
  (when reset (setq *clusters* (make-hash-table :test #'equal :size 100))
        (setq *learn* t)(setq *stats* t)(setq *predictions* nil)
        (setq *yell* nil)
	(reset-stats)
	#+SEQUENT (excl:gc t)
	(when reset-cp (setq *all-cps* nil))
	(setq *next-test* *increment*)
        (setq *all-rules* nil))
    (aspin)
    (cond((> *next-test* max)
	  (pprint *stat-list*)
	  (run-and-test (- n 1) max t reset-cp))
	 (t (run-and-test n max nil reset-cp)))))
