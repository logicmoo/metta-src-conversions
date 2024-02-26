;;; -*- Mode: LISP; Syntax: common-lisp


(defmacro def-lex (word def)
  `(setf (get ',word 'def) ',def)
  )

(defvar *clist* nil)
(defvar *requests* nil)
(defvar *end* nil)

(defvar *cd* nil)
(defvar *to* nil)

(defmacro find-cd(type &optional (token nil))
  `(let*((c (list->cd (if (atom ',type)
                         (list ',type)
                         ',type))))
     (first-image #'(lambda(cd)(if (and (not (eq cd *cd*))
					(compatible cd c))
				   (setq *to* cd)))
		  ,(cond ((null token)'*clist*)
			 ((eq token :after) '(reverse (member  *cd* (reverse *clist*))))
			 ((eq token :before) '(member  *cd* *clist*))
			 ))))
     
(defmacro add-cd(cd)
  `(progn (push (setq *cd* (list->cd ',cd)) *clist*)
	  *cd*)
  )

(defmacro add-request(cd x)
  `(push (cons ,cd ',x) *requests*))

(defun remove-cd(cd)
  (setq *clist*(remove cd *clist*)))

(defun srf(cd role filler &optional (remove t))
  (set-role-filler! cd role filler)
  (if remove (remove-cd filler)))

(defun simpparse(lis)
  (setq *clist* nil)
  (setq *end* nil)
  (setq *requests* nil)
  (mapc #'(lambda(x) (unless (get x 'def) (error "no def for ~a" x))
	    (push (cons nil (get x 'def)) *requests*)
	    (consider-requests))
	lis)
  (consider-requests t)
  (car *clist*))
 

(defun consider-requests(&optional last)
  (if last (setq *end* t))
  (let((x (first-image #'(lambda(r)
			   (setq *cd* (car r))
			   (when (eval (cdr r))
			     (setq *requests* (remove r *requests*))))
		       *requests*)))
    (if x (consider-requests))))
