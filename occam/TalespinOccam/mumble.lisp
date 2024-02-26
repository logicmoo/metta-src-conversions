;;; -*- Package: USER -*-

;;; ===========================================================================
;;; TaleSpin
;;; Copyright (C) 1987 James R. Meehan
;;; ===========================================================================

(in-package "USER")


(defun init-gen ()
  (setq *me* nil *you* nil *him* nil *her* nil))

(defvar *random-odds* 20)
(defun say (cd &optional (add-to-story-cds? t) )
  (let ((x (next-subject cd)))
    (cond ((eq (get x 'gender) 'male)
           (unless (eq x *him*) (setq *him* nil)))
          ((eq (get x 'gender) 'female)
           (unless (eq x *her*) (setq *her* nil)))))
  (let ((final-punctuation? nil)
	(top-cd cd)
	(say-stream (if (and *random-answers?*
			     (eq (old-cd-head cd) 'query))
		       *dump-stream*
		       say-stream)))
    (declare (special final-punctuation?))
    (when (act? cd) 
       (push (time-marker) *story-cds*)
       (if (< (random 100) *random-odds*)
         (add-random-event)
         ))
    (if (and (not (eq (old-cd-head cd) 'query))
	     add-to-story-cds?)
       (push-cd  cd))
    (unless (eq (old-cd-head cd) 'loc)
      (when say-stream 
	    (say0 cd (or (cdpath '(time) cd) *default-tense*) nil t))
      (format? say-stream "  ")))
  cd)

(defun push-cd(cd)
  (when *story-cds*
    (if (not(unobservable cd))
      (updated-story cd))))

(defvar *noise* nil)
(defun updated-story(new &aux cd)
  (when (and (eq (car *story-cds*) (time-marker))
             (cdr *story-cds*))
    (pop *story-cds*) ;;marker
    (new-time)
    )
  (when  (or (null *learn*)
	     (act? new)
	     (null *noise*)
	     (>= (random 100) *noise*))
	 (push (cons new (setq cd (talespin->occam new)))  *story-cds*))
  (if (act? new)
    (make-predictions cd)));;; delete if you don't make predictions

(defun new-time( &optional (events nil))
  ;(if (null *story-cds*)
   ; (break))
  (Cond ((eq (car *story-cds*) (time-marker))
         (process-time events))
        
        ;((eq (car *story-cds*) 'ignore-time-marker)
         ;(setq *story-cds* (ignore-time-marker (cdr *story-cds*))))
        (t (new-time (cons (pop *story-cds*) events)))))

(defun ignore-time-marker(lis )
  (if (eq (car lis) (time-marker))
    (cdr lis)
    (if (eq (car lis) 'ignore-time-marker)
      lis
      (cons (car lis) (ignore-time-marker (cdr lis))))))
         
      
      
(defun say0 (cd tense inf subj)
  (declare (special final-punctuation?))
  (let ((*first-word-in-sentence?* t))
    (say1 cd tense inf subj))
  (unless final-punctuation?
    (if *question?* (format? say-stream "?") (format? say-stream "."))
    (setq final-punctuation? t)))

(defun say1 (*cd* *tense* *inf* *subj*)
  (LET ((SAY-PROG (get (old-cd-head *cd*) 'SAY-PROG)))
    (COND (SAY-PROG (funcall SAY-PROG))
          (T (warn "Can't say ~S in English." *cd*)))))

(defun say-word (x)
  (if (eq x '?unspec) (setq x 'something)) ;Just in case ...
  (cond (*first-word-in-sentence?*
         (format? say-stream "~<~% ~4,72:;~@(~A~)~>" x))
        ((or (get x 'proper-name)
             (member x *first-person-singular-words*))
         (format? say-stream "~<~%~4,72:; ~@(~A~)~>" x))
        (t (format? say-stream "~<~%~4,72:; ~(~A~)~>" x)))
  (setq *first-word-in-sentence?* nil))

(DEFun SUBCLAUSE (WORD cd TENSE)
  (if WORD (say-word (OR (RELATIVE-PRONOUN cd) WORD)))
  (let ((*question?* nil))
    (SAY1 CD 
          (COND ((STATE? CD) *DEFAULT-TENSE*)
                ((AND (EQ TENSE 'PAST) (EQ (CDPATH '(TIME) CD) 'FUTURE)) 'COND)
                (T TENSE))
          nil
          t)))

(DEFun RELATIVE-PRONOUN (cd)
  (COND ((AND (EQ (old-cd-head CD) 'LOC) (eq (CDPATH '(VAL) CD) '?unspec))
         'WHERE)
        ((eq (NEXT-SUBJECT cd) '?UNSPEC) 'WHO)
        (T NIL)))

(DEFun NEXT-SUBJECT (cd)
  (CASE (old-cd-head CD)
    (CONT (cdpath '(VAL) cd))
    (mloc (if (eq (cdpath '(val) cd) 'world)
              (next-subject (cdpath '(con) cd))
              (cdpath '(val) cd)))
    (query (next-subject (cdpath '(con) cd)))
    (T (cdpath '(ACTOR) cd))))

(DEFun INFCLAUSE (cd SUBJ-FLAG TENSE)
  (SAY1 cd TENSE T SUBJ-FLAG))

(defun SAY-FILLER (filler case) (say-pp filler case))

(defun SAY-PP (x &optional (case 'objective))
  (cond ((eq x *me*)
         (say-word
             (if (get x 'plural)
                 (case case (nominative 'we) (objective 'us) (possessive 'ours))
                 (case case 
                   (nominative 'i) (objective 'me) (possessive 'mine)))))
        ((eq x *you*)
         (say-word
             (if (eq case 'possessive) 'yours 'you)))
        ((eq x *him*)
         (say-word
             (case case (nominative 'he) (objective 'him) (possessive 'his))))
        ((eq x *her*)
         (say-word
             (case case (nominative 'she) ((objective possessive) 'her))))
        (t (if (MEMber x *ALL-OBJECTS* :test #'eq) (say-word 'the))
           (say-word x))))

(defun SAY-PREP (PREP cd)
  (COND (CD (say-word PREP) (SAY-PP CD 'objective))))

(defun IN-MODE? (X) (MEMber X $(MODE) :test #'eq))

(defun SAY-NEG () (if (IN-MODE? 'NEG) (say-word 'not)))

(defun SAY-SUBJ-VERB (subject INFINITIVE)
  (COND (*INF* (if *SUBJ* (SAY-PP subject 'objective))
               (SAY-NEG)
               (say-word 'to)
               (say-word infinitive))
        (t
            (let ((tense 'present)
                  (modal nil)
                  (word-for-subject
                      (cond ((eq subject *me*) 'i)
                            ((eq subject *you*) 'you)
                            ((eq subject *her*) 'she)
                            ((eq subject *him*) 'he)
                            (t subject))))
              (cond ((in-mode? 'maybe) (setq modal 'might))
                    ((eq *tense* 'cond) (setq modal 'would))
                    ($(time) (setq tense $(time)))
                    (*tense* (setq tense *tense*))
                    (t (setq tense *default-tense*)))
              (gen-verb infinitive :subject word-for-subject :tense tense
                        :modal modal
                        :able? (in-mode? 'possible)
                        :number (if (get subject 'plural) 'plural 'singular)
                        :person (case word-for-subject (i 1) (you 2) (t 3))
                        :question? *question?* :neg? (in-mode? 'neg)))))
  (case (get subject 'gender)
    (male (if (null *him*) (setq *him* subject)))
    (female (if (null *her*) (setq *her* subject)))))

(defun SAY-TENSE (INFINITIVE PLURAL)
  (let ((pair (get infinitive *tense*)))
    (cond (pair (say-word (if plural (cdr pair) (car pair))))
          ((member *tense* '(present past future) :test #'eq)
           (warn "Conjugating ~S" infinitive)
           (def-verb infinitive) ;regular verb
           (say-tense infinitive plural))
          (t (error "Need the ~S tense of ~S" *tense* infinitive)))))

(eval-when (eval lisp:load compile)
  (DEFMACRO DSP (NAME &body L)
    (let ((fname (intern (format nil "SAY-~A" name))))
      `(progn (setf (get ',name 'say-prog) ',fname)
              (defun ,fname () ,@l)))))

(DSP ATRANS 
     (COND ((eq $(ACTOR) $(TO))
            (say-subj-verb $(ACTOR) 'TAKE)
            (SAY-FILLER $(OBJECT) 'objective)
            (SAY-PREP 'FROM $(FROM)))
           (T (say-subj-verb $(ACTOR) 'GIVE)
              (say-filler $(TO) 'objective)
              (say-filler $(OBJECT) 'objective))))

(dsp kiss
  (say-subj-verb $(actor) 'kiss)
  (say-filler $(object) 'objective))

(defun concrete? (cd)
  (dolist (role *role-functions*)
    (if (eq (funcall role cd) '?unspec) (return-from concrete? nil)))
  (not (member '?unspec (mode cd) :test #'eq)))

(DSP MTRANS 
     (cond ((eq (old-cd-head $(object)) 'query)
            (say-subj-verb $(ACTOR) 'ASK)
            (say-filler $(TO) 'objective)
            (if (and (eq *cd* top-cd) (concrete? $(object con)))
                (let ((*me* $(actor))
                      (*you* $(to))
                      (*him* nil)
                      (*her* nil)
                      (*default-tense* 'present))
                  (format? say-stream ", ~<~% ~10:;\"~>")
                  (say0 $(object) 'cond nil t)
                  (format? say-stream "\""))
                (SUBCLAUSE 'WHETHER $(OBJECT con) 'COND)))
           (T (say-subj-verb $(ACTOR) 'TELL)
              (say-filler $(TO) 'objective)
              (if (and (eq *cd* top-cd) (concrete? $(object)))
                  (let ((*me* $(actor))
                        (*you* $(to))
                        (*him* nil)
                        (*her* nil)
                        (*default-tense* 'present))
                    (format? say-stream ", ~<~% ~10:;\"~>")
                    (say0 $(object) $(object time) nil t)
                    (format? say-stream "\""))
                  (SUBCLAUSE 'THAT $(OBJECT) $(TIME))))))

(dsp query
  (let ((*question?* t))
    (case $(object)
          (mode (say0 $(con) *tense* *inf* *subj*))
          (actor (say0 (set-role 'actor 'who *cd*) *tense* *inf* *subj*))
          (t (warn "Can't say ~S" *cd*)))))

(DSP PTRANS 
  (COND ((eq $(ACTOR) $(OBJECT))
         (say-subj-verb $(ACTOR) 'GO))
        (T (say-subj-verb $(ACTOR) 'MOVE)
           (say-filler $(OBJECT) 'objective)))
  (SAY-PREP 'TO $(TO)))

(DSP tilt 
 (say-subj-verb $(ACTOR) 'POUR)
 (say-filler $(OBJECT) 'objective)
 (SAY-PREP 'InTO $(TO)))

(DSP MBUILD 
     (say-subj-verb $(ACTOR) 'DECIDE)
     (COND ((eq $(ACTOR) $(OBJECT ACTOR))
            (infclause $(OBJECT) NIL 'FUTURE))
           (T (SUBCLAUSE 'THAT $(OBJECT) 'FUTURE))))

(DSP PROPEL 
     (cond ((eq $(to) 'floor)
            (say-subj-verb $(ACTOR) 'drop)
            (say-filler $(object) 'objective)
            (say-prep `to $(to)))
           (t
            (say-subj-verb $(ACTOR) 'push) 
            (say-filler $(object) 'objective)
            (cond ($(to)(say-prep `to $(to))))
            (cond ($(from) (say-prep `|away from| $(from)))))))
           

(DSP GRASP 
     (COND ((IN-MODE? 'TF)
            (say-subj-verb $(ACTOR) 'LET)
            (say-word 'go) (say-word 'of))
           (T (say-subj-verb $(ACTOR) 'pick)
              (say-word 'up)))
     (say-filler $(OBJECT) 'objective))

(DSP unGRASP 
  (say-subj-verb $(ACTOR) 'LET)
  (say-word 'go) (say-word 'of)
  (say-filler $(OBJECT) 'objective))

(DSP expel 
  (say-subj-verb $(ACTOR) 'exhale) 
  (say-word 'into)
  (say-filler $(to) 'objective))

(DSP tie 
  (say-subj-verb $(ACTOR) 'tie)
  (say-filler $(OBJECT) 'objective))


(DSP INGEST 
     (SAY-SUBJ-VERB
      $(ACTOR)
      (COND ((is-a $(object) 'drink)
             'DRINK) 
            (T 'EAT)))
     (say-filler $(OBJECT) 'objective))

(DSP PLAN 
     (say-subj-verb $(ACTOR) 'PLAN)
     (infclause $(OBJECT) NIL 'FUTURE))

(DSP WANT 
  (cond ((member 'neg $(object mode) :test #'eq)
         (say1 (negate (wants $(actor) (affirm $(object))))
               *tense* *inf* *subj*))
        (t
            (say-subj-verb $(ACTOR) 'WANT)
            (infclause $(OBJECT)
                       (NOT (eq $(ACTOR) (NEXT-SUBJECT $(object))))
                       'FUTURE))))

(DSP LOC 
  (say-subj-verb $(ACTOR) (if (in-mode? 'toward) 'get 'BE))
  (unless (eq $(VAL) '?UNSPEC) (SAY-PREP 'in $(VAL))))

(DSP CONT
  (say-subj-verb $(VAL) (if (in-mode? 'toward) 'get 'HAVE))
  (say-filler $(ACTOR) 'objective))

(DSP open
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'open))

(DSP dirty
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'dirty))

(DSP peeled
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'peeled))

(DSP flowing
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'flowing))

(DSP on
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'on))

(DSP ring
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'ringing))

(DSP broken
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'broken))

(DSP bursted
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'bursted))

(DSP inflated
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'inflated))

(DSP tied
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'sealed))
(DSP sharp
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'sharp))
(DSP shattered
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'shattered))
(DSP attached
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'attached)
  (say-prep 'to $(val)))
(DSP flying
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'flying))

;;;;#changed
(DSP filled
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'filled)
  (say-prep 'with $(val))) 
  

(DSP MLOC
  (cond ((or 
             (and (member (old-cd-head $(con))
                          '(like fear trust hungry thirsty sad)
                          :test #'eq)
                  (eq $(val) $(con actor))))
         (say1 $(con) *tense* *inf* *subj*))
        (t
            (say-subj-verb $(VAL)
                           (if (OR (RELATIVE-PRONOUN $(CON)) (IS-TRUE? $(CON)))
                               'KNOW 'think))
            (SUBCLAUSE 'THAT $(CON) *DEFAULT-TENSE*))))

(dsp achieve
  (flet ((alpha ()
                (case (old-cd-head $(object))
                  (mloc
                      (say-subj-verb $(object val) 'find)
                      (say-word 'out)
                      (SUBCLAUSE 'THAT $(object CON) *DEFAULT-TENSE*))
                  (loc
                      (say-subj-verb $(object ACTOR) 'get)
                      (unless (eq $(object VAL) '?UNSPEC)
                        (SAY-PREP 'NEAR $(object VAL))))
                  (cont
                      (say-subj-verb $(object VAL) 'get)
                      (say-filler $(object ACTOR) 'objective))
                  ((hungry thirsty)
                   (cond ((and (in-mode? 'neg)
                               (member 'neg $(object mode)))
                          (let ((*cd* (set-role 'mode '(pos) *cd*)))
                            (say-subj-verb $(object actor) 'be)
                            (say-word 'still)
                            (say-word (old-cd-head $(object)))))
                         (t
                             (say-subj-verb $(object actor) 'satisfy)
                             (say-word (case (get $(object actor) 'gender)
                                         (male 'his) (female 'her)))
                             (say-word
                                 (if (eq (old-cd-head $(object)) 'hungry)
                                     'hunger 'thirst)))))
                  (sad
                      (cond ((in-mode? 'neg)
                             (let ((*cd* (set-role 'mode '(pos) *cd*)))
                               (say-subj-verb $(object actor) 'be)
                               (say-word 'still)
                               (say-word
                                   (if (member 'neg $(object mode))
                                       'sad 'happy))))
                            (t
                                (say-subj-verb $(object actor) 'become)
                                (say-word 'happy))))
                  (t (say1 $(object) *tense* *inf* t)))))  ;;;bad changed to t
    (cond ((or (eq $(actor) (next-subject $(object)))
               (not (is-a (next-subject $(object)) 'person)))
           (alpha))
          (t (say-subj-verb $(actor) 'persuade)
             (let ((*inf* t) (*subj* t)) (alpha))))))
        
(DSP HEALTHy
  (cond ((in-mode? 'neg)
         (let ((*cd* (affirm *cd*)))
           (say-subj-verb $(ACTOR) 'BE)
           (say-word 'dead)))
        (t (say-subj-verb $(actor) 'be)
           (say-word 'ALIVE))))

(DSP SMART
  (cond ((in-mode? 'neg)
         (let ((*cd* (affirm *cd*)))
           (say-subj-verb $(actor) 'be)
           (say-word 'stupid)))
        (t
         (say-subj-verb $(ACTOR) 'BE)
         (say-word 'smart))))

(DSP HUNGRY (say-subj-verb $(ACTOR) 'BE) (say-word 'HUNGRY))

(DSP THIRSTY (say-subj-verb $(ACTOR) 'BE) (say-word 'THIRSTY))

(DSP bored (say-subj-verb $(ACTOR) 'BE) (say-word 'bored))


(dsp sad
  (cond ((in-mode? 'neg)
         (let ((*cd* (affirm *cd*)))
           (say-subj-verb $(actor) 'be)
           (say-word 'happy)))
        (t
            (say-subj-verb $(actor) 'be)
            (say-word 'sad))))

(DSP CAUSE 
      (COND (*question?* ;(IN-MODE? 'QUES)
             (say1 $(conseq) 'future nil t)   
             ;(SUBCLAUSE NIL $(CONSEQ) 'FUTURE)
             (say-word 'if)
             (SUBCLAUSE NIL 
                        $(ANTE)
                        (CASE *TENSE* 
                               ((FUTURE) 'PRESENT)
                               ((COND) *DEFAULT-TENSE*)
                               (T *TENSE*))))
            (T (say-word 'if)
               (SUBCLAUSE NIL $(ANTE) 'present)
               (say-word 'then)
               (SUBCLAUSE NIL $(CONSEQ) 'future))))

(DSP LIKE
  (say-subj-verb $(ACTOR) 'LIKE)
  (say-filler $(TO) 'objective))

(DSP call-on-phone
  )

(DSP fear
  (say-subj-verb $(actor) 'be)
  (say-word 'afraid)
  (say-word 'of)
  (say-filler $(to) 'objective))

(DSP trust
  (say-subj-verb $(actor) 'trust)
  (say-filler $(to) 'objective))
         
(setf (get 'BERRIES 'PLURAL) T)

