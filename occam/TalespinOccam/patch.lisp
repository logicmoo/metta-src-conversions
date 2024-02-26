(defvar *real-goals* '(thirsty))
(defun spin ()
  (setq *goals* *real-goals*)
  (setq *story-cds* nil)
  (setq say-stream nil)
  (init-world)
  (setq *goals* *real-goals*)
  (setq say-stream *terminal-io*)
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

(defvar *all*)
(defun process-time(x &aux cds new)
  (setq x (mapcar #'cdr x))
  (push x *all*))


(defun print-all( &optional (s terminal-io))
  (format s "(setq *all* (append *all* (list ")
  (mapc #'(lambda(x)
	    (format s "~%(list")
	    (mapc #'(lambda(x)
		      (format s " (list->cd '~a)"
			      (cd->list x)))
		  x)
	    (format s ")~%"))
	*all*)
  (format s ")))~%"))