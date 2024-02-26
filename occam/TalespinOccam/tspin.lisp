
;;; -*- Package: USER -*-

;;; ===========================================================================
;;; TaleSpin
;;; Copyright (C) 1987 James R. Meehan
;;; ===========================================================================

(in-package "USER")

(defvar *random-answers?* t)     ;; ==> random answers to all questions
(defvar *dump-stream* nil)
(defvar *english?* t)            ;; ==> t: print story; nil: don't print story

(defvar *auto?* t)               ;; ==> t: auto mode; nil: manual mode

(defvar *story-cds* '())         ;; list of cds used in story
(defvar *all-story-cds* '())         ;; list of cds used in story

(defvar *cd*)
(defvar *init-facts*)
(defvar *all-objects*)
(defvar *all-locations*)
(defvar *closed-locations*)
(defvar *actions*)
(defvar *plans*)
(defvar *goals*)
(defvar *main-char*)
(defvar *conseqs*)
(defvar *personae*)
(defparameter *DEFAULT-TENSE* 'PAST)
(defvar *TENSE*)
(defvar *INF*)
(defvar *SUBJ*)
(defvar *roles*)
(defvar *role-functions*)
(defvar say-stream)
(defvar *first-word-in-sentence?*)
(defvar *person* 3)
(defparameter *me* (gensym))
(defparameter *you* (gensym))
(defparameter *him* (gensym))
(defparameter *her* (gensym))
(defvar *question?* nil)
(defparameter *first-person-singular-words*
  '(i i\'m i\'d i\'ve i\'ll)) ;Always capitalize these.
(defvar top-cd)


(eval-when (eval lisp:load compile)
  (setq *roles* '(actor object to from ante conseq part val con mode time))
  (setq *role-functions* nil)
  (dolist (role *roles*)
    (push
        (setf (get role 'path-fn) (intern (format nil "OLD-CD-~A" role)))
        *role-functions*))
  (setq *role-functions* (nreverse *role-functions*)))

(eval-when (lisp:load eval compile)
  (defmacro format? (&rest forms)
    `(if *english?* (format ,@forms)))
  (defmacro terpri? ()
    `(if *english?* (terpri))))

(defun time-marker ()
    'TIME-MARKER)
(defun time-marker? (x)
    (eq x 'TIME-MARKER))



(defun aspin ()
  (setf *auto?* t)
  (setf *random-answers?* t)
  (setf *dump-stream* nil)
  (spin))

(defun mspin ()
  (setf *auto?* nil)
  (setf *random-answers?* nil)
  (spin))

(defun spin ()
  (setq *story-cds* nil)
  (setq say-stream nil)
  (init-world)
  (setq say-stream nil)
  (setq *story-cds* (list (time-marker)))
  (if (not *auto?*)
     (format t "~2%This is a story about ...~%"))
  (let* ((main-character-index
             (pick-one 
                 (mapcar #'(lambda (p) 
                             (list (string-capitalize (string p))
                                   (string-capitalize (string (get p 'is-a)))))
                         *personae*)))
         (main-character (nth main-character-index *personae*))
         (g (get main-character 'gender)))
    (setq *main-char* main-character)
    (when (member *main-char* '(karen lynn))
      (push 'bored *goals*)
      (push 'thirsty *goals*)
      (push 'bored *goals*))
    (when (eq *main-char* 'lynn)
      (push 'bored *goals*))
    (if (not *auto?*)
       (format? t "~%What is ~@(~A~)'s problem?~%" main-character))
    (let* ((*him* (if (eq g 'male) main-character nil))
           (*her* (if (eq g 'female) main-character nil))
           (problem-index
               (pick-one 
                   (mapcar #'(lambda (g)
                               (list 
                                   (remove #\Newline
                                           (with-output-to-string
                                               (say-stream)
                                             (say (state main-character
                                                         g 'pos) nil)))))
                           *goals*)))
           (problem (nth problem-index *goals*)))
      (format? t "~%One day ...~%")
      (init-gen)
      (assert-fact (mloc 'world (state main-character problem 'pos)))
      (format? t "~2%~30t--- The End ---~%")
      (new-time)
      (values))))

(defun spin-demo (story)
  (let ((main-character (car story))
        (problem (cadr story))
        (*init-facts* (append *init-facts* (cddr story))))
    (setq say-stream *terminal-io*)
    (format? t "~3%Once upon a time, ")
    (init-world)
    (format? t "~%One day, ")
    (assert-fact (mloc 'world (state main-character problem 'pos)))
    (format? t "~2%--- The End ---~%")))

(defvar *analyzed-cds*)

(defun final-analysis ()
  (setf *analyzed-cds* (time-group *story-cds*)))

(defun time-group (cds)
  (let ((res '() )
	(curr-group '()))
    (do ((cds cds (cdr cds)))
	((null cds) res)
      (let ((curr-cd (car cds)))
	(cond ((eq curr-cd 'ignore-time-marker)
               (setf (car res) (append (car res) curr-group))
               (setf curr-group '()))
              ((time-marker? curr-cd)
	       (when curr-group (push curr-group res))
	       (setf curr-group '()))
	      (t (push curr-cd curr-group)))))))


(defparameter *unobservables* '(mloc want achieve bored call-on-phone))
(defparameter *say-unobservables* nil)
(defun observables(x)
  (mapcar #'(lambda(x)
              (remove-if #'(lambda(x)(unobservable (car x)))
                         x))
          x))

(defun unobservable (x)
  (and (old-cd-p x)
       (member (old-cd-head x) *unobservables*)))

(defun pick-one (l)
  (let ((len (length l)))
    (if *auto?*
       (random len)
       (loop
	 (do ((l l (cdr l))
	      (i 1 (1+ i)))
	     ((null l))
	    (format? t "   ~D. ~{~A ~}~%" i (car l)))
	 (format? t "Type 1~@[,~]" (> len 2))
	 (do ((n 2 (1+ n)))
	     ((>= n len)
	      (format? t " or ~D, and then type the RETURN key. -> " len))
	    (format? t " ~D," n))
	 (clear-input)
	 (let ((x (read t)))
	   (if (and (integerp x) (<= 1 x len)) (return (1- x))))
	 (beep)
	 (format? t "~%Oops!  Try again.~%")))))

(defstruct (old-cd (:print-function print-cd))
  head (actor nil) (object nil) (to nil) (from nil) (ante nil) (conseq nil)
  (part nil) (val nil) (con nil) (mode nil) (time nil))

(defvar *cd-level* 0)

(defun print-cd (cd stream depth)
  (declare (ignore depth))
  (let ((*cd-level* (1+ *cd-level*)))
    (cond ((> *cd-level* 4)
           (format stream "..."))
          (t
            (format stream "#{~S" (old-cd-head cd))
            (dolist (role *roles*)
              (let ((x (funcall (get role 'path-fn) cd)))
                (if x (format stream "~* ~s=~s" ;"~<~%~VT  ~1:; ~S=~S~>"
                              (* 3 *cd-level*) role x))))
            (format stream "}")))))

(eval-when (lisp:load eval compile)
  (set-macro-character #\$
                       #'(lambda (stream char)
                           (declare (ignore char))
                           `(cdpath ',(read stream t nil t) *cd*))
                       t))

(defmacro cdpath (roles cd)
  (if (and (consp roles) (eq (car roles) 'QUOTE))
      (let ((v (gensym "CD")))
        (labels ((f (roles cd)
                    (cond ((null roles) cd)
                          ((null (cdr roles)) `(,(g (car roles)) ,cd))
                          (t `(let ((,v (,(g (car roles)) ,cd)))
                                (and ,v ,(f (cdr roles) v))))))
                 (g (role)
                    (intern (format nil "OLD-CD-~A" role))))
          (if (atom cd)
              `(and ,cd ,(f (cadr roles) cd))
              `(let ((,v ,cd)) (and ,v ,(f (cadr roles) v))))))
      `(*cdpath ,roles ,cd)))

(defun *cdpath (roles cd)
  (dolist (role roles cd)
    (setq cd (funcall (get role 'path-fn) cd))
    (if (null cd) (return nil))))

(defmacro set-role (role value cd)
  (unless (and (consp role) (eq (car role) 'quote))
    (error "set-role can't expand ~S" role))
  (let ((g (gensym "CD")))
    `(let ((,g (copy-old-cd ,cd)))
       (setf (,(get (cadr role) 'path-fn) ,g) ,value)
       ,g)))

;

(defun goal-eval (actor goal plans)
  (cond ((or (is-true? goal) 
             (knows? actor goal))
         t)
        ((has-goal? actor goal)
         nil)
        (t (gets-new-goal actor goal)
           (dolist (plan plans)
             (funcall plan)
             (when (is-true? goal)
               (forgets-goal actor goal)
               (return-from goal-eval t)))
           (say (negate (possible (achieve actor goal))))
           nil)))



(defmacro a (type var constraint &rest plan)
          `(let ((things (mapcan #'(lambda(f)
                                    (and (eq (first f) 'is-a)
                                         (eq (third f) ',type)
                                         (list (second f))))
                                *facts*)))
            (mapcan #'(lambda(,var) . ,plan)
                     (append (random-order
                              (remove-if-not #'(lambda (,var)
                                             ,constraint) things))
                             (random-order
                              (remove-if #'(lambda (,var)
                                             ,constraint) things))))))


                             
                                    
          
(defmacro make-plan (comment &rest body)
  (declare (ignore comment))
  `#'(lambda () 
       ;; (print ,comment)
       . ,body))

(defun s-thirst (actor)
  (goal-eval actor (state actor 'thirsty 'neg)
             (a drink drink (knows-loc actor drink)
                (a cup cup (knows-loc actor cup)
                   (list (make-plan `(s-thirst ,actor)
                                    (and
                                     (dfill actor cup drink)
                                     (doit  (random-choice (/ 2.1 (get actor 'age))
                                                           (ingest actor drink cup)
                                                           (propel actor cup 'floor1)))
                                     (and (is-true? (state cup 'dirty 'pos))
                                      (daccess actor 'hot-faucet t)
                                      (doit (ptrans actor cup 'hot-faucet nil))
                                      (daccess actor 'hot-faucet nil)))
                                    ))))))

(defun s-entertain (actor)
  (goal-eval actor (state actor 'bored 'neg)
             (if (>= (random 100) 50)
               (a ball ball (knows-loc actor ball)
                  (let((friend (if (eq actor 'karen)
                                 'lynn
                                 'karen)))
                    (list (make-plan `(s-entertain ,actor)
                                     (and 
                                      (doit (mtrans  actor (question (propel friend ball actor friend)) friend actor))
                                      (doit (ptrans friend friend 'garage 'kitchen))
                                      (doit (grasp friend ball))
                                      (doit (ptrans friend friend 'outside 'garage))
                                      (doit (ptrans actor actor 'outside 'kitchen))
                                      (play-catch friend actor ball))))))
             (a balloon balloon (knows-loc actor balloon)
                (let((friend (if (eq actor 'karen)
                               'lynn
                               'karen))
                     (adult (random-element '(mom dad))))
                   (list (make-plan `(s-entertain ,actor)
                                    (and
                                     (doit (mtrans  actor (question (propel friend balloon actor friend)) 
                                                    friend actor))
                                     (doit (mtrans  actor (question (atrans adult balloon actor adult)) 
                                                    adult actor))
                                      (dcont adult balloon)
                                     (inflate adult balloon)
                                     (doit (atrans adult balloon actor adult))
                                     (doit (ptrans actor actor 'outside 'kitchen))
                                     (doit (ptrans friend friend 'outside 'kitchen))
                                     (play-catch actor friend balloon)))))))))

(defun happy(p1)
  (state p1 'bored 'neg))

(defun inflate (a b)
  (doit (expel a 'air b))
  (cond ((> 4 (random 11))
         (doit (tie a b)))
        (t (doit (ungrasp a b))
           (doit (grasp a b))
           (inflate a b))))
    
(defun play-catch(p1 p2 obj &optional (n 1))
  (cond((and (> n 2)
             (< (random 10) n))
        (doit (happy p1))
        (doit (happy p2)))
       ((>= (random 100) 58)
        (doit (propel p1 obj (random-element '(calla-lilly1 calla-lilly2 window1
                                               rose-bush1 rose-bush2 window1
                                               window1))))
        (cond  ((is-true? (state obj 'bursted 'pos))
                (doit (ptrans p2 p2 'kitchen 'outside))
                (doit (ptrans p1 p1 'kitchen 'outside))
                nil)
               (t (doit (grasp p2 obj))
                  (play-catch p2 p1 obj (+ n 1)))))
       (t (doit (propel p1 obj p2 p1))
          (cond ((and (> n 2)
                      (>= (random 100) 50))
                 (doit (propel p2 obj 'grass p2))
                 (cond  ((is-true? (state obj 'bursted 'pos))
                         (doit (happy p1))
                         (doit (happy p2)))
                        (t (doit (grasp p2 obj))
                           (play-catch p2 p1 obj (+ n 1)))))
                (t (play-catch p2 p1 obj (+ n 1)))))))
      
    

(defun s-answer(actor rec)
  (goal-eval actor (state (get rec 'component-of) 'ring 'neg)
             (list (make-plan `(answer ,actor ,rec)
                        (doit (grasp actor rec))
                        (doit (ungrasp actor rec))
             ))))

(defun s-hunger (actor)
  (goal-eval actor (state actor 'hungry 'neg)
             (a food food (knows-loc actor food)
                   (list (make-plan `(s-hunger ,actor)
                                    (and
                                     (dcont actor food)
                                     (iprep actor food)
                                     (doit  (random-choice (/ 2.1 (get actor 'age))
                                                           (ingest actor food nil)
                                                           (propel actor food 'floor1)))
                                     (if (get food 'component) ;;peel
                                       (doit (propel  actor (get food 'component) 'basket1 actor))
                                       t)
                                       ))))))
                    

(defun s-happiness (actor)
  (let ((goal (state actor 'sad 'neg)))
    (goal-eval actor goal
      (mapcar
          #'(lambda (person)
              (make-plan `(s-happiness ,actor via ,person)
                (and (not (eq person actor))
                     (not (eq (get person 'gender) (get actor 'gender)))
                     (relate actor person actor 'like)
                     (persuade actor person (kiss person actor) goal))))
          *personae*))))


(defun iprep (actor food)
  (if (get food 'component) ;;peel
    (doit (propel actor (get food 'component) nil food))
    t))


(defun dcont (actor object)
  (let ((goal (has actor object)))
    (if (and (is-a object 'food)
             (< (get actor 'age) 10))
      (let ((o (random-element '(mom dad))))
        (doit (mtrans  actor (question (atrans o object actor o)) o actor))
        (doit (grasp o object))
        (doit (atrans o object actor o)))
    (goal-eval actor goal
               (list
                  (make-plan `(dcont ,actor ,object no-owner)
                             
                             (AND (DKNOW ACTOR (WHERE-IS OBJECT))
                                  (DPROX ACTOR ACTOR OBJECT)
                                  (let ((loc (knows-loc actor object)))
                                    (and 
                                     (DACCESS actor loc t)
                                     (DOIT (ATRANS ACTOR OBJECT ACTOR loc))
                                     (daccess actor loc nil))))))))))

(defun dfill (actor cup drink)
  (when (is-true? (state cup 'broken 'neg))
    (let ((goal (bi-state cup drink 'filled 'pos)))
      (goal-eval actor goal
                 (list (make-plan `(dfill ,actor ,cup ,drink)
                                  (AND (Dcont actor cup)
                                       (let ((loc (knows-loc actor drink)))
                                         (cond  ((and loc (is-a loc 'faucet))
                                                 (and (daccess actor loc t)
                                                      (doit (ptrans actor cup loc nil))
                                                      (daccess actor loc nil)))
                                                (t (and (dcont actor drink)
                                                        (doit (tilt ACTOR drink cup NIL)))))))
                                  ))))))

(defun daccess (actor object-loc open?)
  (let ((goal (state object-loc (if (is-a object-loc 'faucet)
                                  'flowing
                                  'open)
                     (if open? 'pos 'neg))))
    (cond((or (not (is-a object-loc 'container))
              (is-true? goal))
           t)
         (t (goal-eval actor goal
               (list (make-plan `(daccess ,object-loc)
                                (let ((comp (component-of object-loc)))
                                  (if open? 
                                    (doit (propel actor comp nil object-loc))
                                    (doit (propel actor comp object-loc nil)))))))))))


(defun lure-away (actor owner object)
  (let* ((new-loc (new-location owner))
         (goal (is-at owner new-loc)))
    (goal-eval actor goal
      (list (make-plan `(lure-away ,actor ,owner ,object)
              (and
                  (persuade actor owner (ptrans owner owner new-loc nil) goal)
                  (doit (atrans actor object actor nil))))))))

(defun new-location (actor)
  (dolist (loc *all-locations*)
    (if (not (eq loc (loc actor))) (return loc))))

(defun dknow (actor info)
  #+dec (declare (notinline mapcar) (optimize safety (speed 0)))
  (let ((goal (mloc actor info)))
    (goal-eval actor goal
      (mapcar #'(lambda (agent)
                  (make-plan `(dknow ,actor ,agent to tell ,info)
                    (AND (KNOWS-LOC ACTOR AGENT)
                         (OR (IS-FRIEND-OF? AGENT ACTOR)
                             (NOT (RELATE ACTOR actor AGENT 'fear)))
                         (not (knows? actor (negate (mloc agent info))))
                         (PERSUADE ACTOR 
                                   AGENT 
                                   (MTRANS AGENT INFO ACTOR AGENT)
                                   GOAL))))
              (remove actor *personae*)))))
                
(defun dprox (actor object to-loc)
  (let ((goal (is-at object to-loc)))
    (GOAL-EVAL ACTOR 
               goal
               (LIST
                   (make-plan `(dprox ,actor ,object to ,to-loc 1)
                     (AND (OR (eq ACTOR OBJECT) (DPROX ACTOR ACTOR OBJECT))
                          (DKNOW ACTOR (WHERE-IS to-loc))
                          (OR (eq ACTOR OBJECT) (DOIT (GRASP ACTOR OBJECT)))
                          (OR (and (not (eq to-loc '?unspec))
                                   (IS-PROX? ACTOR (LOC-NAME-OF to-loc)))
                              (DOIT (PTRANS ACTOR 
                                            OBJECT 
                                            (KNOWS-LOC ACTOR to-loc)
                                            (KNOWS-LOC ACTOR ACTOR))))
                          (OR (eq ACTOR OBJECT)
                              (DOIT (UNGRASP ACTOR OBJECT)))))
                   (make-plan `(dprox ,actor ,object to ,to-loc 2)
                              (AND (NOT (eq ACTOR OBJECT))
                                   (MEMber OBJECT *PERSONAE*)
                                   (PERSUADE ACTOR 
                                             OBJECT 
                                             (PTRANS OBJECT 
                                                     OBJECT 
                                                     to-loc 
                                                     (LOC-NAME-OF OBJECT))
                                             GOAL)))))))

(defun persuade (actor agent action result)
  (GOAL-EVAL ACTOR ACTION 
             (APPEND (LIST (move-plan actor agent action result)
                           (ASK-PLAN ACTOR AGENT ACTION RESULT))
                     (mapcar #'(lambda (food) 
                                 (bargain-plan actor agent action food))
                             (GET-ISA 'FOOD AGENT))
                     (LIST (THREAT-PLAN ACTOR AGENT ACTION)))))

(defun move-plan (actor agent action result)
  (make-plan `(move-plan ,actor ,agent action = ,action result = ,result)
             (do-move-plan actor agent action result)))

(defun do-move-plan (actor agent action result)
  (and (match action (ptrans nil agent nil nil))
       (not (relate actor actor agent 'like))
       (let ((food (new-food actor agent)))
         (and food
              (tell actor agent (is-at food (cdpath '(to) action)))
              (is-true? result)))))

(defun new-food (actor agent)
  (dolist (food (get-isa 'food agent))
    (if (not (knows? actor (has agent food)))
        (return food))))

(defun ASK-PLAN (ACTOR AGENT ACTION RESULT)
  (make-plan `(ask-plan ,actor ,agent ,action ,result)
      (AND (RELATE ACTOR actor AGENT 'trust)
           (RELATE ACTOR ACTOR AGENT 'LIKE)
           (TELL ACTOR AGENT (QUESTION ACTION))
           (IS-TRUE? RESULT))))

(defun BARGAIN-PLAN (ACTOR AGENT ACTION FOOD)
  (LET ((ATRANS-FOOD (ATRANS ACTOR FOOD AGENT ACTOR)))
    (make-plan `(bargain ,actor ,atrans-food)
               (and (not (knows? actor (negate (possible action))))
                    (RELATE ACTOR actor AGENT 'trust)
                    (NOT (KNOWS? ACTOR (HAS AGENT FOOD)))
                    (NOT (HAS-GOAL? ACTOR (HAS ACTOR FOOD)))
                    (DOIT (MBUILD ACTOR (CAUSE ATRANS-FOOD (MAYBE ACTION))))
                    (TELL ACTOR AGENT (QUESTION (CAUSE ATRANS-FOOD (FUTURE ACTION))))
                    (DCONT ACTOR FOOD)
                    (DPROX ACTOR ACTOR AGENT)
                    (DOIT ATRANS-FOOD)
                    (cond ((IS-TRUE? ACTION))
                          (t (say (negate action))
                             (doit (mloc actor 
                                         (relation actor agent 'trust 'neg)))
                             nil))))))

(defun THREAT-PLAN (ACTOR AGENT ACTION)
  (make-plan `(threat ,actor ,agent ,action)
      (and (not (knows? actor (negate (possible action))))
           (NOT (RELATE ACTOR actor AGENT 'fear))
           (TELL ACTOR AGENT 
                 (CAUSE (negate ACTION) (FUTURE (PROPEL ACTOR 'HAND AGENT))))
           (OR (IS-TRUE? ACTION)
               (AND (DOIT (PROPEL ACTOR 'HAND AGENT)) (IS-TRUE? ACTION))))))

(defun TELL (ACTOR AGENT INFO)
  (let ((action (mtrans actor info agent actor)))
    (GOAL-EVAL ACTOR 
               action ;(MLOC AGENT INFO)
               (LIST
                 (make-plan `(tell ,actor ,agent ,info)
                   (AND (DPROX ACTOR ACTOR AGENT)
                        (DOIT action)))))))

(defun DOIT (*CD*) 
  (if (EQ (old-cd-HEAD *CD*) 'MTRANS)
      (let ((x (KNOWS? $(ACTOR) $(OBJECT))))
        (cond (x (SETf (old-cd-object *CD*) x))
              (nil 
                  (member (old-cd-head $(object)) '(loc cont mloc))
               (dknow $(actor) $(object))
               (let ((x (knows? $(actor) $(object))))
                 (if x (setf (old-cd-object *cd*) x)))))))
  (ASSERT-FACT *CD*)
  *CD*)

(defun assert-fact (x)
  (let ((*actions* nil) (*plans* nil) (*conseqs* nil) (l (list x)))
    (loop
      (dolist (i l)
        (progn (now-knows 'world i nil)
        ;;;this will work for now
        (dolist (p *personae*)
          (now-knows p i nil)))
        (conseqs i))
      
      (if *conseqs*
          (setq l (nreverse *conseqs*) *conseqs* nil)
          ;; (shiftf l *conseqs* nil)
          (return)))
    (dolist (cd (nreverse *actions*))
      (doit (set-role 'time *default-tense* cd)))
    (mapc 'funcall (nreverse *plans*))))

(eval-when (lisp:load eval compile)
  (defmacro def-conseqs (name &body body)
    (let ((fname (intern (format nil "~A-CONSEQS" name))))
      `(progn (setf (get ',name 'conseq-fn) ',fname)
              (defun ,fname () ,@body)))))

(defun conseqs (*cd*)
  (let ((proc (get (old-cd-head *cd*) 'conseq-fn)))
    (if proc (funcall proc)))
  ;; (setq *conseqs* (nreverse *conseqs*))
  )

(defun add-conseq (x)
  (push x *conseqs*)
  x)

(defun add-action (x) (push x *actions*) x)


(def-conseqs call-on-phone()
             (ADD-CONSEQ (state $(object) 'ring 'pos))
             (let((actor (random-element (remove *main-char* '(mom dad))))
                  (rec (get $(object) 'component)))
               (push
                (make-plan `(answer-phone ,actor ,red)
                           (s-answer actor rec))
                *plans*)))
(def-conseqs atrans
 (ADD-CONSEQ (HAS $(TO) $(OBJECT)))
 (ADD-CONSEQ (IS-AT $(OBJECT) $(TO)))
 (when (and $(from) $(to))
   (ADD-CONSEQ (negate (HAS $(from) $(OBJECT))))
   (ADD-CONSEQ (negate (is-at $(OBJECT) $(from)))))
 )

(def-conseqs kiss
  (add-conseq (state $(object) 'sad 'neg)))

(def-conseqs grasp
  (if (and (is-a $(object) 'receiver)
           (not (IN-MODE? 'TF)))
    
    (add-conseq (state (get $(object) 'component-of) 'ring 'neg)))
  (ADD-CONSEQ (COND ((IN-MODE? 'TF)
                     
                     (negate (HAS $(ACTOR) $(OBJECT))))
                    (T
                     (HAS $(ACTOR) $(OBJECT)))))
  (ADD-CONSEQ (is-at  $(OBJECT) $(ACTOR))))


(def-conseqs ungrasp
         (ADD-CONSEQ (negate (HAS $(ACTOR) $(OBJECT))))
         (ADD-CONSEQ (negate (is-at  $(OBJECT) $(ACTOR))))
         (when (is-a $(object) 'balloon)
           (ADD-CONSEQ (state $(object) 'flying 'pos))
           (ADD-CONSEQ (state $(object) 'inflated 'neg))))

(def-conseqs tie
  (ADD-CONSEQ (state $(object) 'tied 'pos)))
(def-conseqs expel
  (ADD-CONSEQ (state $(to) 'inflated 'pos)))



(def-conseqs ingest
  (when $(from)
      (add-conseq (bi-state $(from) $(object) 'filled 'neg))
      (add-conseq (state $(from) 'dirty 'pos)))
  (ADD-CONSEQ (STATE $(ACTOR)
                     (if (is-a $(object) 'drink)
                             'THIRSTY 'hungry)
                     'NEG)))

(def-conseqs loc 
  ;(NOTICE $(ACTOR) *CD*)
  nil)

(def-conseqs mbuild
  (if (eq $(ACTOR) $(OBJECT CONSEQ ACTOR))
      (push (CONS $(OBJECT ANTE) $(OBJECT CONSEQ))
            (GET $(ACTOR) 'DEMONS))))

(def-conseqs mloc
  (DEMON-CHECK $(VAL) $(CON))
  ;(unless (MEMber 'NEG $(CON MODE))
    (let ((p (get (old-cd-head $(con)) 'react-fn)))
      (if p (funcall p))))

(defmacro def-reaction (head &body body)
  (let ((name (intern (format nil "~A-REACT" head))))
    `(progn (setf (get ',head 'react-fn) ',name)
            (defun ,name () ,@body))))

(defun DEMON-CHECK (WHO EVENT)
  (let ((l nil))
    (dolist (demon (get who 'demons))
      (if (match (car demon) event)
          (push (cdr demon) *actions*)
          (push demon l)))
    (setf (get who 'demons) (nreverse l))))

(def-conseqs mtrans())
 ; (LET ((ACTOR $(ACTOR))
 ;       (OBJECT $(OBJECT))
  ;      (HEARER $(TO)))
  ;  (COND ((eq (old-cd-head object) 'query) ;(MEMber 'QUES $(OBJECT MODE))
  ;         (COND ((AND (EQ (old-cd-head (cdpath '(con) OBJECT)) 'CAUSE)
  ;                     (eq ACTOR $(OBJECT con ANTE ACTOR))
  ;                     (eq HEARER $(OBJECT con CONSEQ ACTOR)))
  ;                (PROMISE-CONSEQS
   ;                   HEARER 
   ;                   $(OBJECT con CONSEQ)
   ;                   ACTOR 
    ;                  $(OBJECT con ANTE)))
    ;             ((eq $(OBJECT con ACTOR) HEARER)
  ;                (REQUEST-CONSEQS ACTOR HEARER
   ;                                (cdpath '(con) object)))))
   ;       ((NOT (eq ACTOR HEARER))
     ;      (ADD-CONSEQ (MLOC HEARER *CD*))
    ;       (cond ((match object (state hearer 'smart 'neg))
    ;              (add-conseq
    ;                  (mloc hearer (relation hearer actor 'like 'neg))))
   ;              ((RELATE HEARER hearer ACTOR 'trust)
   ;               (ADD-CONSEQ (MLOC HEARER (MLOC ACTOR OBJECT)))))))))

(defun PROMISE-CONSEQS (X XDO Y YDO)
  (LET ((A (CAUSE YDO (AFFIRM XDO))))
    (COND ((not (RELATE X y X 'trust))
           (ADD-action
               (MBUILD X 
                       (CAUSE YDO 
                              (FUTURE (MTRANS X (STATE Y 'SMART 'NEG) Y X)
                                      ))))
           (ADD-action (MTRANS X A Y X)))
          ((RELATE X X Y 'LIKE)
           (ADD-action (MBUILD X A))
           (ADD-action (MTRANS X A Y X)))
          (T (ADD-action (MTRANS X (negate A) Y X))))))

(defun REQUEST-CONSEQS (X Y Z)
  (COND ((OR (NOT (RELATE Y Y X 'LIKE)) (RELATE Y x Y 'fear))
         (add-action (MTRANS Y (negate (future Z)) X Y)))
        ;;(PLAN Y (FUTURE (MTRANS Y (negate Z) X Y))))
        ;;(T (PLAN Y Z)))))
        ((not (eq y (cdpath '(actor) z))) nil)
        (t (case (old-cd-head z)
             (mtrans
                 (let ((a (knows? y (cdpath '(object) z))))
                   (cond (a (add-action (mtrans y a x y)))
                         (t
                          (add-action
                              (mtrans y (negate (mloc y (cdpath '(object) z)))
                                      x y))))))
             (t (add-action z))))))

(def-conseqs plan
  (if (eq $(ACTOR) $(OBJECT ACTOR)) (PUSH $(OBJECT) *actions*)))

(defparameter *always* t)

(def-conseqs propel
  (let ((c (contents $(object))))
    (when c (add-conseq (bi-state $(object) c 'filled 'neg))))
  (when (and (is-a $(object) 'peel)
           (is-a $(from) 'fruit))
    (add-conseq (state $(from) 'peeled 'pos))
    (add-conseq (bi-state $(from) $(object) 'attached 'neg)))
  (let ((h (get (get $(to) 'composition) 'hardness)))
    (when (and  h 
                (eq (get $(object) 'composition) 'glass)
                (or *always* (> (random (* h 3)) (+ h 2))))
      (add-conseq (state $(object) 'broken 'pos))))
  (cond ((eq $(object) 'door-bell-switch1)
         (add-conseq (state 'door-bell1 'ring (or (car $(mode)) 'pos)))
         (if (member 'pos $(mode))
           (add-conseq (propel $(actor) $(object) $(to)$(from) '(neg))))))
  (cond ((eq $(object) 'light-switch1)
         (let ((mode (knows-if 'world (state 'light1 'on nil))))
           (add-conseq (state 'light1 'on (if (member 'pos mode)
                                            'neg
                                            'pos))))))
  (when (and $(to)
             (eq (get $(to) 'composition) 'glass)
             (not (is-a $(object) 'balloon))
             (or *always* (> (random 11) 3)))
    (add-conseq (state $(to) 'sharp 'pos))
    (add-conseq (state $(to) 'shattered 'pos)))
  (when (and (is-a $(object) 'balloon)
             (eq (get $(to) 'shape) 'pointed)
             (or *always* (> (random 11) 3)))
    (add-conseq (state $(object) 'bursted 'pos)))
  (COND ;((MEMber $(TO) *PERSONAE*)
         ;(ADD-CONSEQ (negate (HAS $(ACTOR) $(OBJECT))))
         ;(ADD-CONSEQ (HAS $(to) $(OBJECT))))
        ((and (is-a  $(to)  'container)
              (eq $(object) (component-of $(to))))
         (add-conseq (state $(to) (if (is-a $(to) 'faucet)
                                    'flowing
                                    'open) 'neg)))
        ((and (is-a  $(from)  'container)
              (eq $(object) (component-of $(from))))
         (add-conseq (state $(from) (if (is-a $(from) 'faucet)
                                      'flowing
                                      'open) 'pos))))
  (when (and $(to) (is-a $(to) 'person))
    (ADD-CONSEQ (HAS $(to) $(OBJECT)))
    (add-conseq (is-at $(object) $(to))))
  (when (and $(from)  (is-a $(from) 'person))
        (ADD-CONSEQ (negate (HAS $(from) $(OBJECT))))  ;one more problem and you are gone
        (ADD-CONSEQ (negate (is-at $(OBJECT) $(from))))))

(def-conseqs ptrans
  (cond ((and (is-a $(object) 'drink)
              (is-a $(to) 'cup))
         (add-conseq (bi-state $(to) $(object) 'filled 'pos)))
        ((let ((X (knows-whats-at-loc 'world $(to))))
           (when (and (is-a $(to) 'faucet)
                      (knows? 'world (state $(to) 'flowing 'pos))
                      (is-a x 'liquid))
             (if (eq (get $(to) 'temperture) 'cold)
               (add-conseq (bi-state $(object) x 'filled 'pos))
              (add-conseq (state  $(object) 'dirty 'neg))
               )
             
             t)))
        (t (ADD-CONSEQ (IS-AT $(OBJECT) $(TO)))
           (when (and $(to) $(from))
             (ADD-CONSEQ (negate (is-at $(OBJECT) $(from)))))
           (COND ((NOT (eq $(ACTOR) $(OBJECT)))
                  (when (and $(to) $(from))
                    (ADD-CONSEQ (negate (is-at $(actor) $(from)))))
                  (ADD-CONSEQ (IS-AT $(ACTOR) $(TO))))
                 (T NIL)))))

(def-conseqs tilt
  (cond ((and (is-a $(object) 'drink)
              (is-a $(to) 'cup))
         (add-conseq (bi-state $(to) $(object) 'filled 'pos)))))

(def-conseqs TURN
  (notice $(actor) (add-conseq (state $(object) 'on (if (eq $(to) 'on)
						       'pos 'neg)))))



(def-reaction loc
  (AND (OR (MEMber $(CON ACTOR) (GET-ISA 'FOOD $(VAL)))
           (EQ $(CON ACTOR) 'WATER))
       (SGOAL-CHECK $(VAL)
                    (COND ((EQ $(CON ACTOR) 'WATER) 'THIRSTY)
                          (T 'HUNGRY)))))

(defun SGOAL-CHECK (ACTOR SCALE)
  (if (IN-STATE? ACTOR SCALE)
      (push
          (make-plan `(discovered-goal ,actor ,scale)
                     (if (eq scale 'thirsty)
                         (s-thirst actor)
                         (s-hunger actor)))
          *plans*)))

(def-reaction mloc
  (if (and (not (eq $(val) $(con val)))
           (positive? $(con))
           (or (eq $(con con actor) $(con val))
               (relate $(val) $(val) $(con val) 'trust)))
      (add-conseq (mloc $(val) $(con con))))
  (if (negative? $(con))
      (add-conseq (mloc $(val)
                        (negate (possible
                                    (mtrans $(con val)
                                            $(con con)
                                            nil;'?unspec
                                            $(con val))))))))
                                                     
(def-reaction hungry
  (let ((actor $(con actor)))
    (push (make-plan 'reaction (s-hunger actor)) *plans*)))

(def-reaction thirsty
  (let ((actor $(con actor)))
    (push (make-plan 'reaction (s-thirst actor)) *plans*)))

(def-reaction bored
  (let ((actor $(con actor)))
    (push (make-plan 'reaction (s-entertain actor))
          *plans*)))

(defun NOTICE (WHO CD)
  (LET ((WHERE (LOC-NAME-OF WHO)))
    (dolist (i *personae*)
      (WHEN (eq (LOC I) WHERE) (ADD-CONSEQ (MLOC I CD))))))

(eval-when (lisp:load eval compile)
  (defmacro deletef (item loc &rest l 
                          &key from-end test test-not start end count key)
    (declare (ignore from-end test test-not start end count key))
    (multiple-value-bind (vars vals stores store-form access-form)
                         (get-setf-method loc)
      `(let* (,@(mapcar #'list vars vals)
                (,(car stores) (delete ,item ,access-form ,@l)))
         ,store-form))))
       
(defun ADDFACT (KNOWER CD)
  (let ((h (old-cd-head cd)))
    (deletef (negate cd) (getf (get knower 'facts) h) :test #'equalp)
    (cond ((eq h 'loc)
           (do ((l (getf (get knower 'facts) h) (cdr l))
                (z nil
                   (if (match (is-at (cdpath '(actor) cd) '?unspec) (car l))
                       z 
                       (cons (car l) z))))
               ((null l) (setf (getf (get knower 'facts) h) z))))
          ((AND (EQ KNOWER 'WORLD)
                (EQ h 'HEALTHy)
                (negative? cd))
           (SETq *PERSONAE* (remove (CDPATH '(ACTOR) CD) *PERSONAE*))))
    (pushnew cD (getf (get knower 'facts) h) :test #'equalp))
                                                ;(GET KNOWER 'FACTS))
  (values))

(defun STATE? (CD)
  (MEMber (old-cd-head CD)
          '(LOC MLOC CONT LIKE trust fear HUNGRY THIRSTY HEALTHy SMART sad)
          :test #'eq))

(defun mental-state? (cd)
  (member (old-cd-head cd)
          '(mloc like trust fear hungry thirsty smart sad)
          :test #'eq))

(defun NOW-KNOWS (WHO WHAT SAY?)
  (when (and (eq who 'world)
           (eq (old-cd-head what) 'call-on-phone)
           (not *random-at-same-time*))
    (push (time-marker) *story-cds*))
  (when (and (eq who 'world)
             (member (old-cd-head what) *property-states*))
    (setf (get (old-cd-actor what) (old-cd-head what))(car (old-cd-mode what))))
  (COND ((AND (EQ (old-cd-head WHAT) 'MLOC)
              (or (eq who 'world) (eq who (cdpath '(val) what))))
         (now-knows (cdpath '(val) what) (cdpath '(con) what) say?))
        ((and (eq who 'world) (mental-state? what))
         (now-knows (cdpath '(actor) what) what say?))
        (t (ADDFACT WHO WHAT)
            (COND ((and  (OR SAY? (and (or (EQ WHO 'WORLD) (mental-state? what)) 
                                      (not (eq (old-cd-head what) 'mloc))))
                        (if *say-unobservables*
                          t
                          (not(unobservable what))))
                   (SAY (if (eq who 'world) what (MLOC WHO WHAT))))))))
        
(defun KNOWS? (KNOWER FACT)
  (cond ((and (eq (old-cd-head fact) 'mloc)
              (or (eq knower 'world) (eq knower (cdpath '(val) fact))))
         (knows? (cdpath '(val) fact) (cdpath '(con) fact)))
        ((and (eq knower 'world) (mental-state? fact))
         (knows? (cdpath '(actor) fact) fact))
        (t (memquery knower fact))))

(defun KNOWS-LOC (KNOWER OBJECT)
  (CDPATH '(VAL) (KNOWS? KNOWER (WHERE-IS OBJECT))))

(defun KNOWS-whats-at-LOC (KNOWER loc)
  (CDPATH '(actor) (KNOWS? KNOWER (WHats-at loc))))

(defun KNOWS-OWNER (KNOWER OBJECT)
  (CDPATH '(VAL) (KNOWS? KNOWER (WHO-HAS OBJECT))))

(defun KNOWS-IF (KNOWER CD)
  (CDPATH '(MODE) (KNOWS? KNOWER (SET-ROLE 'MODE '?UNSPEC CD))))

(defun contents(cd)
  (cdpath '(val) (knows? 'world (bi-state cd '?unspec 'filled 'pos))))


(defun MEMQUERY (KNOWER PAT)
  (PAT-MEMBER PAT (getf (get knower 'facts) (old-cd-head pat))))
                  ;(GET KNOWER 'FACTS)))

(defun PAT-MEMBER (PAT CD-LIST)
  (dolist (cd cd-list nil)
    (if (and (old-cd-p cd)
             (match pat cd))
      (return cd))))

(defun HAS-GOAL? (ACTOR PAT) (PAT-MEMBER PAT (GET ACTOR 'GOALS)))

(defun GETS-NEW-GOAL (ACTOR GOAL)
  (push GOAL (GET ACTOR 'GOALS))
  (when *say-unobservables*
    (SAY (WANTS ACTOR (achieve actor GOAL)))))

(defun FORGETS-GOAL (ACTOR GOAL)
  (setf (get actor 'goals)
        (delete (has-goal? actor goal) (get actor 'goals))))

(defun IN-STATE? (X ST) (FIND-OUT 'WORLD (STATE X ST 'POS)))

(defun RELATE (X Y Z REL) (FIND-OUT X (RELATION Y Z REL 'POS)))

(defun FIND-OUT (WHO CD)
  (LET ((MODE (KNOWS-IF WHO CD)))
    (COND (MODE (MEMber 'POS MODE))
	  (*random-answers?*                     ;; auto random decisions
	   (let ((answer (zerop (random 2))))
	     (init-gen) (say (question (mloc who cd))) (init-gen)
	     (now-knows who (set-role 'mode (if answer '(pos) '(neg)) cd) nil)
	     answer))
          (T (init-gen)
             (format? t "~2%   You decide: ")
             (SAY (question (MLOC WHO CD)))
             (init-gen)
             (beep)
             (clear-input)
             (LET ((ANSWER (y-or-n-p "   Type Y for yes or N for no --> ")))
               (terpri?)
               (now-knows WHO (SET-ROLE 'MODE (if ANSWER '(POS) '(NEG)) CD) nil)
               ANSWER)))))

(defun IS-FRIEND-OF? (X Y)
  (AND (NOT (eq X Y)) (RELATE Y X Y 'LIKE)))

(defun LOC (X) (KNOWS-LOC 'WORLD X))

(defun IS-PROX? (X Y) (eq (LOC-NAME-OF X) (LOC-NAME-OF Y)))

(defun negative? (cd) (member 'neg (cdpath '(mode) cd) :test #'eq))

(defun positive? (cd) (not (negative? cd)))

(defun IS-TRUE? (CD)
  (COND ((negative? cd)
         (not (is-true? (negate cd))))
        ((EQ (old-cd-head CD) 'MLOC)
         (KNOWS? (CDPATH '(VAL) CD) (CDPATH '(CON) CD)))
        ((eq (old-cd-head cd) 'loc)
         (is-prox? (cdpath '(actor) cd) (cdpath '(val) cd)))
        (T (KNOWS? 'WORLD CD))))

(defun LOC-NAME-OF (X)
  (do ((new (loc x) (loc new))
       (old x new)
       (l (list x) (cons new l)))
      ((or (null new) (member new l :test #'eq)) old)))

(defun GET-ISA (X Y) (OR (GET Y X) (GET (GET Y 'IS-A) X)))

;;; -------------- CD constructors -------------------

(defun atrans (actor object to from)
  (make-old-cd :head 'atrans :actor actor :object object :to to :from from))

(defun kiss (actor kissee)
  (make-old-cd :head 'kiss :actor actor :object kissee))

(defun CAUSE (X Y) (make-old-cd :head 'CAUSE :ANTE X :CONSEQ Y))

(defun GRASP (ACTOR OBJECT)
 (make-old-cd :head 'GRASP :ACTOR ACTOR :OBJECT OBJECT))

(defun UNGRASP (ACTOR OBJECT) (make-old-cd :head 'unGRASP :ACTOR ACTOR :OBJECT OBJECT))

(defun INGEST (ACTOR OBJECT &optional from)
  (make-old-cd :head 'INGEST :ACTOR ACTOR :OBJECT OBJECT :from from))

(defun expel (ACTOR OBJECT to &optional from)
  (make-old-cd :head 'expel :ACTOR ACTOR :OBJECT OBJECT :from from :to to))

(defun MBUILD (ACTOR OBJECT)
 (make-old-cd :head 'MBUILD :ACTOR ACTOR :OBJECT OBJECT))

(defun tie (ACTOR OBJECT)
 (make-old-cd :head 'tie :ACTOR ACTOR :OBJECT OBJECT))

(defun MTRANS (ACTOR OBJECT TO FROM)
  (make-old-cd :head 'MTRANS :ACTOR ACTOR :OBJECT OBJECT :TO to :FROM FROM))

(defun PLAN (ACTOR OBJECT)
  (make-old-cd :head 'PLAN :ACTOR ACTOR :OBJECT OBJECT))

(defun PROPEL (ACTOR OBJECT TO &optional from mode )
  (make-old-cd :head 'PROPEL :ACTOR ACTOR :OBJECT OBJECT :TO TO :from from :mode mode))

(defun call-on-phone ()
  (make-old-cd :head 'call-on-phone :OBJECT 'phone1))

(defun PTRANS (ACTOR OBJECT TO FROM)
  (make-old-cd :head 'PTRANS :ACTOR ACTOR :OBJECT OBJECT :TO TO :FROM FROM))

(defun tilt (ACTOR OBJECT TO FROM)
  (make-old-cd :head 'tilt :ACTOR ACTOR :OBJECT OBJECT :TO TO :FROM FROM))

(defun TURN (actor object state)
  (make-old-cd :head 'TURN :actor actor :object object :to state))

(defun DOOR (object)
  (make-old-cd :head 'DOOR :object object))

(defun WANTS (ACTOR GOAL)
  (make-old-cd :head 'WANT :ACTOR ACTOR :OBJECT GOAL))

(defun HAS (ACTOR OBJECT)
  (make-old-cd :head 'CONT :ACTOR OBJECT :VAL ACTOR))


(defun IS-AT (ACTOR LOC)
  (make-old-cd :head 'LOC :ACTOR ACTOR :VAL LOC))



(defun MLOC (ACTOR CON)
  (make-old-cd :head 'MLOC :CON CON :VAL actor))

(defun STATE (ACTOR ST MODE)
  (make-old-cd :head ST :ACTOR ACTOR  :MODE (LIST MODE)))

(defun RELATION (ACTOR OBJECT REL MODE)
  (make-old-cd :head REL :ACTOR ACTOR :TO OBJECT :MODE (LIST MODE)))

(defun bi-state (ACTOR OBJECT REL MODE)
  (make-old-cd :head REL :actor ACTOR :val OBJECT :MODE (LIST MODE)))

(defun relation? (cd)
  (member (old-cd-head cd) '(like fear trust) :test #'eq))

(defun act? (cd)
  (member (old-cd-head cd)
	  '(turn ptrans atrans propel mtrans kiss grasp ingest mbuild ungrasp 
            call-on-phone tilt expel tie)
	  :test #'eq))

(defun WHERE-IS (X)
  (make-old-cd :head 'LOC :ACTOR X :VAL '?UNSPEC))

(defun WHATs-at (X)
  (make-old-cd :head 'LOC :ACTOR '?UNSPEC :VAL x))

(defun WHO-HAS (X)
  (make-old-cd :head 'CONT :ACTOR X :VAL '?UNSPEC))

(defun open-status (X)
  (make-old-cd :head 'open :object X :mode '?UNSPEC))

(defun MODE (CD) (CDPATH '(MODE) CD))

(defun AFFIRM (CD)
  (COND ((MEMber 'POS (MODE CD)) CD)
        (T (SET-ROLE 'MODE (CONS 'POS (remove 'NEG (MODE CD))) CD))))

(defun negate (CD)
  (COND ((negative? cd) (AFFIRM CD))
        (T (set-role 'MODE (CONS 'NEG (remove 'POS (MODE CD))) CD))))

(defun MAYBE (CD)
  (COND ((MEMber 'MAYBE (MODE CD)) CD)
        (T (set-role 'MODE (CONS 'MAYBE (MODE CD)) CD))))

(defun possible (cd)
  (cond ((member 'possible (mode cd)) cd)
        (t (set-role 'mode (cons 'possible (mode cd)) cd))))

(defun achieve (actor goal)
  (make-old-cd :head 'achieve :actor actor :object goal))

;  (cond ((member 'toward (mode cd)) cd)
;        (t (set-role 'mode (cons 'toward (mode cd)) cd))))

(defun QUESTION (CD)
  (make-old-cd :head 'QUERY :con cd :object 'mode))

(defun pick-a-pot ()
  'pan1)

(defun TF (CD)
  (COND ((MEMber 'TF (MODE CD)) CD)
        (T (set-role 'MODE (CONS 'TF (MODE CD)) CD))))

(defun FUTURE (CD) (set-role 'TIME 'FUTURE CD))

(defun MATCH (PAT CONST)
  (declare (inline every))
  (and (eq (old-cd-head pat) (old-cd-head const))
       (every #'(lambda (slot)
                  (match-item? (funcall slot pat) (funcall slot const)))
              '(old-cd-actor old-cd-object old-cd-to old-cd-from old-cd-ante old-cd-conseq
                old-cd-val old-cd-con old-cd-time))
       (match-mode? (old-cd-mode pat) (old-cd-mode const))))

(defun match-item? (pat const)
  (cond ((null pat) t)
        ((null const) t)
        ((eq pat '?unspec) t)
        ((old-cd-p pat) (and (old-cd-p const) (match pat const)))
        ((old-cd-p const) nil)
        ((eq pat const) t)
        (t nil)))

(defun match-mode? (p c)
  (or (null c) (eq p '?unspec) (subsetp p c)))

(defun beep ()
  ;; Do something to get the person's attention.
  #+:ccl (ccl:ed-beep)
  #+dec (write-char #.(int-char 7))
  )

(defun random-order(x) (randomize x nil))
(defun random-element(x)
  (nth (random (length x)) x))
(defun randomize(X y)
  (if (null x) y
      (let ((a(random-element x)))
        (randomize (remove a x) (cons a y)))))
  
  
                   
(defun random-append(x y)(append x y))

(defun is-a(x Y)
  (when x (or (eq x y)
              (is-a (get x 'is-a) y))))

(defun component-of (x)
  (get x 'component))

(defun random-choice (n x y)
  (if (> (random 1.0) n)
    x
    y))
  

(defun display-sequence(&optional (e *analyzed-cds*) (time 0))
  (when e
    (format t "~%~%~%=============== Time ~a===================~%" time)
    (mapc #'(lambda(x)(terpri)(print-cd (car x) *standard-output* 10)) (car e))
    (display-sequence (cdr e)(+ 1 time))))
  
(defparameter *random-at-same-time* nil)
(defun add-random-event()
  (when *random-events*
    (let* ((e (random-element *random-events*))
           (cd (apply (first e) (rest e))))
      (setq *random-events* (remove e *random-events*))
      (if (act? cd)
        (progn (add-action cd) 
               (when *random-at-same-time* (remove-time-marker)))
        (progn (add-conseq cd)
               (when (not *random-at-same-time*)
                 (push (time-marker) *story-cds*)))))))

(defun remove-time-marker () 
  (if (eq (car *story-cds*) (time-marker))
    (pop *story-cds*)
    (progn (break "No time-marker")
           (push 'ignore-time-marker  *story-cds*))))
