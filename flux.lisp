(ql:quickload '(:bordeaux-threads
		:cl-markup
		:cl-notify
		:closer-mop
		:hunchentoot
		:local-time
		:sqlite))

(use-package '(:bordeaux-threads
	       :closer-mop
	       :hunchentoot
	       :sqlite))

(defvar *db-flux* '())

(defclass unit-data ()
  ((unit-name
    :accessor unit-name
    :initarg :unit-name
    :initform 'units)
   (unit-value
    :accessor unit-value
    :initarg :unit-value
    :initform nil)))

(defclass instance (unit-data)
  ((object-name
    :accessor object-name
    :initarg :object-name
    :initform 'unnamed-flux-instance-class)
   (timestamp
    :accessor timestamp
    :initarg :timestamp
    :initform (get-universal-time))))

(defclass binc (instance)
  ((operation
    :accessor operation
    :initarg :operation
    :initform '-)
   (lhs
    :accessor lhs
    :initarg :lhs
    :initform nil)
   (rhs
    :accessor rhs
    :initarg :rhs
    :initform nil)))

(defun defbinc (mg-dosage &key operation lhs rhs unit-name timestamp)
  (make-instance 'binc
		 :timestamp (if timestamp timestamp (get-universal-time))		 
		 :unit-name (if unit-name unit-name 'mg)
		 :unit-value mg-dosage
		 :object-name 'binc
		 :operation (if operation operation '-)
		 :lhs (if lhs lhs nil)
		 :rhs (if rhs rhs nil)))

(defun consassoclist (binc-entry)
  (let ((binc-slots (class-slots (class-of binc-entry))))
    (loop for i in binc-slots
       collect (cons (slot-definition-name i)
		     (slot-value-using-class
		      (class-of binc-entry)
		      binc-entry
		      i)))))

(defun cons-flux-alist (flux-entry)
  (let* ((flux-type (class-of flux-entry))
	 (flux-slots (class-slots flux-type)))
    (loop for slot in flux-slots
       collect (cons (slot-definition-name slot)
		     (slot-value-using-class
		      flux-type
		      flux-entry
		      slot)))))

(defun master-mg-table-creation ()
  (with-open-database (@db "database-flux-test-01.db")
    (execute-non-query @db "CREATE TABLE MASTERMG (\"MG\"); INSERT INTO MASTERMG (\"MG\") VALUES (\"0\);")))

(defun set-master-mg (value)
  (with-open-database (@db "database-flux-test-01.db")
    (execute-non-query @db
		       (format nil "UPDATE MASTERMG SET MG=\"~a\";"
			       value))))

(defun retrieve-master-mg ()
  (read-from-string
   (car
    (car
     (with-open-database
	 (@db "database-flux-test-01.db")
       (execute-to-list @db "SELECT * FROM MASTERMG;"))))))
    
(defun conc-flux-table-creation (flux-entry)
  (let ((flux-alist (cons-flux-alist flux-entry)))
    (concatenate 'string
		 "CREATE TABLE "
		 (write-to-string (class-name (class-of flux-entry)))
		 " ("
		 (with-output-to-string (str)
		   (format str "~{\"~a\"~^,~^ ~}" (loop for cell in flux-alist
						     collect (car cell)))
		   str)
		 ");")))

(defun conc-flux-sql-cmd (flux-entry)
  (let ((flux-alist (cons-flux-alist flux-entry)))
    (concatenate 'string
		 "INSERT INTO "
		 (write-to-string (class-name (class-of flux-entry)))
		 " ("
		 (with-output-to-string (str)
		   (format str "~{\"~a\"~^,~^ ~}" (loop for cell in flux-alist
						 collect (car cell)))
		   str)
		 ") VALUES ("
		 (with-output-to-string (str)
		   (format str "~{\"~a\"~^,~^ ~}" (loop for cell in flux-alist
						 collect (cdr cell)))
		   str)
		 ");")))

;(defun create-sqlite-database ()
;  (connect "database-flux-test-01.db")
;  (disconnect 

(defun create-sqlite-table (flux-entry)
  (let ((cmd (conc-flux-table-creation flux-entry)))
    (with-open-database (@db "database-flux-test-01.db")
      (execute-non-query @db cmd))))

(defun put-to-sqlite-db (flux-entry)
  (let ((cmd (conc-flux-sql-cmd flux-entry)))
    (with-open-database (@db "database-flux-test-01.db")
      (execute-non-query @db cmd))))



			     

(defun &binc- (binc-entry)
  (format t "~%~%the entry ~a is of operation '-  ===>> pushing to *db-flux* now.~%~%"
	  binc-entry)
  (setf (slot-value binc-entry 'operation) '-)
  (push binc-entry *db-flux*)
  (put-to-sqlite-db binc-entry)
  (set-master-mg (- (retrieve-master-mg) (unit-value binc-entry))))

(defun &binc+ (binc-entry)
  (format t "~%~%the entry ~a is of operation '+  ===>> handling appropriately..~%~%"
	  binc-entry)
  ;(create the event)
  (&binc- binc-entry))

(defun flux-do (binc-entry)
  (let ((op (operation binc-entry)))
    (cond ((eq op '-)
	   (&binc- binc-entry))
	  ((eq op '+)
	   (&binc+ binc-entry)))))

(defun pull-entries ()
  (sqlite:with-open-database
      (@db "database-flux-test-01.db")
    (sqlite:execute-to-list @db "select * from binc;")))

(defun format-binc-table ()
  (princ #\Newline)
  (let* ((table1 (pull-entries))
	 (table  (subseq table1 (- (length table1) 14))))
    (loop for entry in table do
	 (format *standard-output*
		 "~&~T~T~a~T~a~T~T~a~T~T~a~a~T~T~a/~a~T~T"
		 (nth 2 entry)
		 (local-time:format-timestring
		  nil
		  (local-time:universal-to-timestamp 
		   (read-from-string (nth 3 entry)))
		  :format '((:year 4 #\0) "."
			    (:month 2 #\0) "."
			    (:day 2 #\0) "  "
			    (:hour 2 #\0) ":"
			    (:min 2 #\0) ":"
			    (:sec 2 #\0)))
		 (nth 4 entry)
		 (nth 1 entry)
		 (nth 0 entry)
		 (if (read-from-string (nth 5 entry)) "L" "_")
		 (if (read-from-string (nth 6 entry)) "R" "_"))))
  (format t "~%~T~T....~%~T~T~a~%~%" (retrieve-master-mg)))

(defun &list ()
  (format-binc-table))
	    

(defun &binc (mg-dosage &key operation lhs rhs unit-name timestamp)
  (let ((binc-entry (defbinc mg-dosage
			:operation operation
			:lhs lhs
			:rhs rhs
			:unit-name unit-name
			:timestamp timestamp)))
    (flux-do binc-entry))
  (format-binc-table))

(defun cooldown-confirm-add (binc-entry-arguments)
  (let ((yesorno (read-char)))
    (cond ((equal yesorno #\y)
	   (eval (append '(&binc) binc-entry-arguments)))
	  ((equal yesorno #\n)
	   (&list))
	  (t
	   nil))))

(defun cooldown (minutes &rest binc-entry-arguments)
  ;create a cooldown object that knows when the cooldown ends
  ;and when it was created
  ;and maybe the theoretical binc whose arguments are passed
  (make-thread
   #'(lambda ()
       (sleep (* 60 minutes))
       (format t "~%~%a cooldown has ended. attached to it is:~%~%~a~%~%shall we add this binc? (y/n) >> going ahead and doing it..." binc-entry-arguments)
       (cl-notify:notify-init)
       (cl-notify:dispatch-notification ":CL-FLUX" :body (format nil "cooldown for ~a ended!" binc-entry-arguments))
       (eval (append '(&binc) binc-entry-arguments)))))
;  (thread-yield))


(defun id-gen ()
  (let* ((username (sb-posix:getenv "USER"))
	 (charlist (concatenate 'list username))
	 (charcodes (mapcar (lambda (y) (char-code y)) charlist))
	 (sumcodes (eval (append '(+) charcodes)))
	 (uname-chunk (if (< sumcodes 1000)
			  (* 100 sumcodes)
			  (* 10 sumcodes)))
	 (userid (sb-posix:getuid))
	 (uid-string (write-to-string userid))
	 (uid-chunk (cond ((zerop userid)
			   "00000")
			  ((< userid 10)
			   (concatenate 'string "0000" uid-string))
			  ((< userid 100)
			   (concatenate 'string "000" uid-string))
			  ((< userid 1000)
			   (concatenate 'string "00" uid-string))
			  ((< userid 10000)
			   (concatenate 'string "0" uid-string))))
	 (hostname (machine-instance))
	 (hn-charlist (concatenate 'list hostname))
	 (hn-charcodes (mapcar (lambda (y) (char-code y)) hn-charlist))
	 (hn-sumcodes (eval (append '(+) hn-charcodes)))
	 (hname-chunk (cond ((> hn-sumcodes 9999)
			     hn-sumcodes)
			    ((> hn-sumcodes 999)
			     (* 10 hn-sumcodes))
			    ((< hn-sumcodes 1000)
			     (* 100 hn-sumcodes))))
	 (timestamp (get-universal-time)))
    (with-output-to-string (str)
      (format str "~a-~a-~a-~a"
	      uid-chunk hname-chunk uname-chunk timestamp)
      str)))
    

;; username section

;; 1 - take ascii code values of letters of username
;; 2 - add the values all together
;; 3 - if the value is a four-digit-number, multiply it by 10 & return it
;; 4 - if the value is a three-digit-number, multiply it by 100 & return it






(defclass cooldown ()
  ((unique-id :accessor unique-id
	      :initarg :unique-id
	      :initform (id-gen))
   (timestamp-created :accessor timestamp-created
		      :initarg :timestamp-created
		      :initform (get-universal-time))
   (timestamp-beginning :accessor timestamp-beginning
			:initarg :timestamp-beginning)
   (length-in-seconds :accessor length-in-seconds
		      :initarg :length-in-seconds)
   (timestamp-ending :accessor timestamp-ending
		     :initarg :timestamp-ending)
   (status :accessor status
	   :initarg :status)
   (pf-each :accessor pf-each
	    :initarg :pf-each)
   (periodic-function :accessor periodic-function
		      :initarg :periodic-function)
   (finale-function :accessor finale-funcation
		    :initarg :finale-function)
   (alias :accessor alias
	  :initarg :alias)
   (repeated-p :accessor repeated-p
	       :initarg :repeated-p)
   (repetitions :accessor repetitions
		:initarg :repetitions)
   (prompt-p :accessor prompt-p
	     :initarg :prompt-p)
   (prompt-timeout :accessor prompt-timeout
		   :initarg :prompt-timeout)
   (prompt-toggle-repeat-p :accessor prompt-toggle-repeat-p
			   :initarg :prompt-toggle-repeat-p)))

#|


(defvar *master-port* 9903)

(defvar *master-acceptor*
  (make-instance 'easy-acceptor :port *master-port*))

(defun init-web () (start *master-acceptor*))

(defun kill-web () (stop *master-acceptor*))

(defun uri-binc-entry-handler ()
  (define-easy-handler (weblog :uri "/binc-entry-handler") (mg lhs rhs)
    (format t "~%~%MG: ~a~%~%" mg)
    (setf (content-type*) "text/html")
    (format nil "~a"
	    (markup:html
	     (:head
	      (:title "cl-flux w3-gui"))
	     (:body
	      (:div
	       (:p "mg: " mg)
	       (:p "lhs: " lhs)
	       (:p "rhs: " rhs)))))))

|#



;(set-macro-character #\& (get-macro-character #\)))

;(set-dispatch-macro-character #\# #\&
;    #'(lambda (stream char1 char2)
;	(let ((accum nil)
;	      (pair (read-delimited-list #\] stream t)))
;	  (do ((i (ceiling (car pair)) (1+ i)))
;	      ((> i (floor (cadr pair)))
;	       (list 'quote (nreverse accum)))
;	    (push i accum)))))
			      

;(let* ((f (


;(defun sample-of-lparallel ()
;(let* ((p (promise))
;       (f (future
;            (sleep 0.05)
;            (fulfill p 'f-was-here)))
;       (g (future
;            (sleep 0.049999)
;            (fulfill p 'g-was-here))))
;  (list (force p) (force f) (force g))))
