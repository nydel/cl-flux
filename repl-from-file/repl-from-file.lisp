(ql:quickload '(:bordeaux-threads
		:cl-daemonize
		:cl-fad))

(defparameter *filepath* "/home/cl-user/owncloud/Notes/flux.txt")

(defun read-commands-file ()
  (with-open-file (@file *filepath* :direction :input)
    (loop for line = (read-line @file nil 'eof)
	 until (equal line 'eof)
	 collect line)))

(defun write-commands-file (lines)
  (let* ((lines2 (reverse lines))
	 (lines3 (push "flux" lines2)))
    (with-open-file (@file *filepath* :direction :output
			   :if-exists :overwrite
			   :if-does-not-exist :create)
      (format @file "~{~a~^~&~}" lines3))))

(defun remove-commented-lines (list)
  (remove-if #'(lambda (y)
		 (equal (subseq y 0 1) ";")) list))

(defun commented-p (string)
  (equal (subseq string 0 1) ";"))

(defun comment-string (string)
  (concatenate 'string ";" string))

(defun eval-a-command (string)
  (let ((cmd (read-from-string string)))
    (cons (eval cmd) (concatenate 'string ";" string))))

(defun process-commands-file ()
  (let* ((file-content (cdr (read-commands-file)))
	 (command-contents-next nil))
    (loop for i in file-content
       collect (cond ((commented-p i)
		      (push i command-contents-next))
		     (t
		      (format *standard-output* "~&CL> ~a ~T===>>~T ~a~&"
			      i
			      (eval (read-from-string i)))
		      (push (comment-string i) command-contents-next))))
    (write-commands-file command-contents-next)))

(defun force-quit-thread ()
  (mapcar #'(lambda (y) (when (string-equal (bt:thread-name y) "command-file-thread")
			  (bt:destroy-thread y))) (bt:all-threads)))

(defvar *quit-thread* nil)
(setf *quit-thread* nil)



(defun quit-thread ()
  (setf *quit-thread* t))

(defun thread-command-file (&key every stop-for announce-p)
  (let ((every (if every every 10))
	(stop-for (if stop-for stop-for 'quit-thread))
	(announce-p (if announce-p announce-p nil)))

    (bt:make-thread
     #'(lambda ()
	 (when announce-p (format *standard-output* "~&~&announcement: command-file-thread is activating.~&~&"))
	 (process-commands-file)
	 (sleep every)
	 (if *quit-thread*
	     (format *standard-output* "~&~&quit: exited by way of quit-thread~&~&")
	     (thread-command-file :every every :stop-for stop-for :announce-p announce-p)))
     :name "command-file-thread")))
