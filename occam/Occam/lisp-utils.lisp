;;; -*- Mode: LISP
(unless (fboundp 'neq)
  (defun neq(x y)(not(eq x y))))

(defmacro format-if(var &rest args)
  `(if ,var (format t . ,args)))

;;;(first-image #'odd-p '(10 9 8 7)) = 9
(defmacro first-image (fn lis)
  `(do* ((flist ,lis (cdr flist))
	 (result nil))
	((or (null flist) (setq result (funcall ,fn (car flist)))) result)))

;;;(all-images #'(lambda(x)(if (odd-p x) (- x 1))) '(10 9 8 7)) = (8 6)
(defmacro all-images (fn lis)
  `(do* ((flist ,lis (cdr flist))
	 (little-result )
	 (result nil))
	((null flist) (nreverse result))
     (if (setq little-result (funcall ,fn (car flist)))
	 (push little-result result))))

;;;(subset #'odd-p '(10 9 8 7)) = (9 7)
(defmacro subset(fn lis)
  `(remove-if-not  ,fn ,lis))

;;;nondestructive mapcan
(defmacro mapappend (fn lis)
  `(do* ((flist ,lis (cdr flist))
	 (result nil))
	((null flist)  (apply #'append (nreverse result)))
     (setf result  (cons (funcall ,fn (car flist)) result))))

;;;(unique '(1 2 3 2 3)) = (1 2 3)
(defun unique (lis)
  (do* ((flist lis (cdr flist))
	 (result nil))
	((null flist) (nreverse result))
     (if (not (member (car flist) result))
	 (push (car flist) result))))

;;;like unique with equal
(defun unique-equal (lis)
  (do* ((flist lis (cdr flist))
	 (result nil))
	((null flist) (nreverse result))
     (if (not (member (car flist) result :test #'equal))
	 (push (car flist) result))))


;;;create new interned symbol
(defun generate-symbol(x)(intern (format nil "~a" (gensym x))))


;;;macro to define properties
(defmacro defprop(sym ind val)
  `(setf (get ',sym ',ind)
	 ',val))




;;;(ask-user-for-name  object type)- prompt user to name object
(defvar *answers* nil "if :ask user is prompted")

(defun ask-user-for-name (object type &aux name)
  (cond ((eq *answers* :ask)
	 (format-if *trace* "What do you want to call this ~a?~%" type)
	 (xpn object *trace*)
	 (read))
	 (t (if (null *answers*)
		(setf *answers* '(delta-agency the-plan ask sub-goal sub-act
					outcome goal the-helper the-actor the-obj)))
	    (setf name (pop *answers*))
	    (format-if *trace* "~%Creating new ~a called ~a." type name)
	    (xpn object *trace*)
	    name)))
