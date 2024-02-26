;;; -*- Mode: LISP; Syntax: common-lisp; Base: 10 -*-


(defun simpgen (cd)
	(let ((*gen-lis* (list cd))(first-word? T) (question? nil))
	  (format t "~%")
	  (next-word *gen-lis* first-word? question?)))

(defun next-word (*gen-lis* first-word? question? &aux new-words-and-cds)
  (cond ((null *gen-lis*)(end-sentence question?))
	((null (car *gen-lis*))
	 (pop *gen-lis*)(next-word *gen-lis* first-word? question?))
	((or(stringp (car *gen-lis*))
	    (symbolp (car *gen-lis*))
	    (numberp (car *gen-lis*)))
	 (print-word (pop *gen-lis*) first-word?)
	 (setq first-word? nil)
	 (next-word *gen-lis* first-word? question?))
	((setq new-words-and-cds (word-match (car *gen-lis*)))
	 (pop *gen-lis*)
	 (setq *gen-lis* (append new-words-and-cds
				*gen-lis*))
	 (next-word *gen-lis* first-word? question?))
	(T  (push  (cd-head (pop *gen-lis*)) *gen-lis*)
	   (next-word *gen-lis* first-word? question?))))
 

(defun end-sentence (question?)
    (if question?
        (format t "?~%")
        (format t ".~%")))

(defun print-word  (w first-word?)
       (if first-word?
            (format t "~a" (string-upcase (format nil "~a" w) :start 0 :end 1))
            (if (and (stringp w)
		     (string-equal w ""))
		nil
		(format t " ~a" w))))

(defstruct lex-def cd gen-fn)

(defun word-match (cd &optional (lis *word-lis*) &aux b)
       (if (null lis) nil
	   (cond ((setq b (cd-match (lex-def-cd (car lis)) cd nil))
		  (cond ((funcall (lex-def-gen-fn (car lis)) cd b))
			(t (word-match cd (cdr lis)))))
		 (t(word-match cd (cdr lis))))))

(defvar *word-lis* nil)
(defmacro def-word (cd gen-fn)
    `(let((def (make-lex-def)))
         (push def *word-lis*)
         (setf (lex-def-cd def) ',(list->cd cd))
         (setf (lex-def-gen-fn def) ,gen-fn)
         def))


