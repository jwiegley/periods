(declaim (optimize (debug 3) (safety 3) (speed 1) (space 0)))

(in-package :periods)

(defmacro if-let (((var value)) &body body)
  `(let ((,var ,value))
     (if ,var
	 (progn ,@body))))

;;;_ * Simple numerics

(defparameter *token-stack* nil)
(defparameter *parser-readtable* (copy-readtable nil))

(defun ignore-character (stream char)
  (declare (ignore stream))
  (declare (ignore char))
  (values))

(set-macro-character #\, #'ignore-character nil *parser-readtable*)
(set-macro-character #\/ #'ignore-character nil *parser-readtable*)

(defun read-from-stream (in &optional (eof-error-p t))
  (peek-char t in eof-error-p)		; skip whitespace
  (let ((*readtable* *parser-readtable*)
	(*package* (find-package :periods)))
    (read in eof-error-p)))

(defun peek-token (in &optional (eof-error-p t))
  (or (first *token-stack*)
      (progn
	(peek-char t in eof-error-p)	; skip whitespace
	(let ((token (read-from-stream in eof-error-p)))
	  (if token
	      (progn
		(push token *token-stack*)
		token)
	      (if eof-error-p
		  (error "End of input stream")))))))

(defun read-token (in &optional (eof-error-p t))
  (or (pop *token-stack*)
      (read-from-stream in eof-error-p)))

(defun unread-token (in token)
  (declare (ignore in))
  (push token *token-stack*)
  nil)

;;;_  + Coded

(defun p/number (in &optional (eof-error-p t))
  (and (integerp (peek-token in eof-error-p))
       (read-token in)))

;;;_  + English units

(defun p/cardinal (in &optional (eof-error-p t))
  (or (p/number in eof-error-p)
      (let ((token (peek-token in eof-error-p)))
	(case token
	  (one (read-token in) 1)
	  (two (read-token in) 2)
	  (three (read-token in) 3)
	  (four (read-token in) 4)
	  (five (read-token in) 5)
	  (six (read-token in) 6)
	  (seven (read-token in) 7)
	  (eight (read-token in) 8)
	  (nine (read-token in) 9)
	  (ten (read-token in) 10)))))

(defun p/ordinal (in &optional (eof-error-p t))
  (let ((token (peek-token in eof-error-p)))
    ;; jww (2007-11-28): I no longer parse "the other week", or "every other
    ;; week"
    (case token
      ((1st first) (read-token in) (list :ordinal 1))
      ((2nd second) (read-token in) (list :ordinal 2))
      ((3rd third) (read-token in) (list :ordinal 3))
      ((4th fourth) (read-token in) (list :ordinal 4))
      ((5th fifth) (read-token in) (list :ordinal 5))
      ((6th sixth) (read-token in) (list :ordinal 6))
      ((7th seventh) (read-token in) (list :ordinal 7))
      ((8th eight) (read-token in) (list :ordinal 8))
      ((9th ninth) (read-token in) (list :ordinal 9))
      ((10th tenth) (read-token in) (list :ordinal 10)))))

(defun p/days-of-week (in &optional (eof-error-p t))
  (let ((token (peek-token in eof-error-p)))
    (case token
      (sunday (read-token in) (list :day-of-week 0))
      (monday (read-token in) (list :day-of-week 1))
      (tuesday (read-token in) (list :day-of-week 2))
      (wednesday (read-token in) (list :day-of-week 3))
      (thursday (read-token in) (list :day-of-week 4))
      (friday (read-token in) (list :day-of-week 5))
      (saturday (read-token in) (list :day-of-week 6)))))

(defun p/months-of-year (in &optional (eof-error-p t))
  (let ((token (peek-token in eof-error-p)))
    (case token
      ((January january Jan jan) (read-token in) (list :month 1))
      ((February february Feb feb) (read-token in) (list :month 2))
      ((March march Mar mar) (read-token in) (list :month 3))
      ((April april Apr apr) (read-token in) (list :month 4))
      ((May may) (read-token in) (list :month 5))
      ((June june Jun jun) (read-token in) (list :month 6))
      ((July july Jul jul) (read-token in) (list :month 7))
      ((August august Aug aug) (read-token in) (list :month 8))
      ((September september Sep sep) (read-token in) (list :month 9))
      ((October october Oct oct) (read-token in) (list :month 10))
      ((November november Nov nov) (read-token in) (list :month 11))
      ((December december Dec dec) (read-token in) (list :month 12)))))

(defun p/time-unit (in &optional (eof-error-p t))
  (let ((token (peek-token in eof-error-p)))
    (case token
      ((nanosecond nanoseconds) (read-token in) :nanosecond)
      ((microsecond microseconds) (read-token in) :microsecond)
      ((millisecond milliseconds) (read-token in) :millisecond)
      ((second seconds) (read-token in) :second)
      ((minute minutes) (read-token in) :minute)
      ((hour hours) (read-token in) :hour)
      ((day days) (read-token in) :day)
      ((week weeks) (read-token in) :week)
      ((month months) (read-token in) :month)
      ((year years) (read-token in) :year))))

(defun p/period-unit (in &optional (eof-error-p t))
  (let ((token (peek-token in eof-error-p)))
    (if (eq token 'per)
	(progn
	  (read-token in)
	  (case (peek-token in eof-error-p)
	    (nanosecond
	     (read-token in) (list :every (list :duration :nanosecond 1)))
	    (microsecond (read-token in)
			 (list :every (list :duration :microsecond 1)))
	    (millisecond (read-token in)
			 (list :every (list :duration :millisecond 1)))
	    (second (read-token in) (list :every (list :duration :second 1)))
	    (minute (read-token in) (list :every (list :duration :minute 1)))
	    (hour (read-token in) (list :every (list :duration :hour 1)))
	    (day (read-token in) (list :every (list :duration :day 1)))
	    (week (read-token in) (list :every (list :duration :week 1)))
	    (month (read-token in) (list :every (list :duration :month 1)))
	    ((year annum)
	     (read-token in) (list :every (list :duration :year 1)))
	    (otherwise
	     (unread-token in 'per))))
	(case token
	  (hourly (read-token in) (list :every (list :duration :hour 1)))
	  (daily (read-token in) (list :every (list :duration :day 1)))
	  (weekly (read-token in) (list :every (list :duration :week 1)))
	  (monthly (read-token in) (list :every (list :duration :month 1)))
	  ((yearly annually)
	   (read-token in) (list :every (list :duration :year 1)))))))

;;;_ * A fixed point in time

(defun p/fixed-time (in &optional (eof-error-p t))
  (if-let ((token (peek-token in eof-error-p)))
    (case token
      (now (read-token in) (list :fixed :now))

      (today (read-token in) (list :rel :today))
      (tomorrow (read-token in) (list :rel :tomorrow))
      (yesterday (read-token in) (list :rel :yesterday))

      (otherwise
       (let ((months-of-year (p/months-of-year in eof-error-p)))
	 (if months-of-year
	     (let ((number (or (cadr (p/ordinal in nil))
			       (p/cardinal in nil))))
	       (list :rel :this
		     (append (list :rel) months-of-year
			     (if number
				 (list (if (> number 40) :year :day)
				       number)))))
	     (if-let ((first (p/number in eof-error-p)))
	       (if-let ((ch (peek-char nil in nil)))
		 (if (char= #\/ ch)
		     (progn
		       (read-char in)
		       (let ((second (p/number in eof-error-p)))
			 (if second
			     (let ((ch (peek-char nil in nil)))
			       (if (and ch (char= #\/ ch))
				   (progn
				     (read-char in)
				     (let ((third (p/number in eof-error-p)))
				       (if third
					   (list :fixed
						 :year first
						 :month second
						 :day third)
					   (error "Failed to read literal date string"))))
				   (list :fixed :year first :month second)))
			     (error "Failed to read literal date string"))))
		     (unread-token in first))))))))))

;;;_ * A duration of time

(defun p/time-duration (in &optional (eof-error-p t))
  (if-let ((duration (p/duration-spec in eof-error-p)))
    ;; jww (2007-11-28): I don't support "3 months, 2 weeks" yet
    (loop while (eq 'and (peek-token in nil)) do
	 (read-token in)
	 (nconc duration (p/duration-spec in eof-error-p)))
    (cons :duration duration)))

(defun p/duration-spec (in &optional (eof-error-p t))
  (let ((number (p/cardinal in eof-error-p)))
    (if number
	(let ((unit (p/time-unit in eof-error-p)))
	  (if unit
	      (list unit number)
	      (unread-token in number)))
	(progn
	  (if (eq 'a (peek-token in eof-error-p))
	      (read-token in))
	  (if-let ((unit (p/time-unit in eof-error-p)))
	    (list unit 1))))))

;;;_ * A relative point in time

(defun make-duration-relative (data)
  (let (new-list)
    (loop for arg in (rest data) do
	 (when (keywordp arg)
	   (ecase arg
	     (:year
	      (push :month new-list)
	      (push 1 new-list))
	     (:month
	      (push :day new-list)
	      (push 1 new-list))
	     (:week
	      (push :day-of-week new-list)
	      (push 0 new-list))
	     (:day
	      (push :hour new-list)
	      (push 0 new-list))
	     (:hour
	      (push :minute new-list)
	      (push 0 new-list))
	     (:minute
	      (push :second new-list)
	      (push 0 new-list))
	     (:second
	      (push :millisecond new-list)
	      (push 0 new-list))
	     (:millisecond
	      (push :microsecond new-list)
	      (push 0 new-list))
	     (:microsecond
	      (push :nanosecond new-list)
	      (push 0 new-list)))))
    (cons :rel (nreverse new-list))))

(defun p/relative-time (in &optional (eof-error-p t))
  (let ((token (read-token in eof-error-p)))
    (case token
      (the (list :rel :next (p/time-reference in)))

      (this
       (let ((reference (p/time-reference in)))
	 (if (eq :duration (car reference))
	     (list reference
		   (list :rel :from
			 (list :rel :this
			       (make-duration-relative reference))))
	     (list :rel :this reference))))

      ((next following)
       (let ((reference (p/time-reference in)))
	 (if (eq :duration (car reference))
	     (list reference
		   (list :rel :from
			 (list :rel :next
			       (make-duration-relative reference))))
	     (list :rel :next reference))))

      ((last previous preceeding)
       (let ((reference (p/time-reference in)))
	 (if (eq :duration (car reference))
	     (list reference
		   (list :rel :from
			 (list :rel :last
			       (make-duration-relative reference))))
	     (list :rel :last reference))))

;;;	  (((? the p/ws) p/ordinal p/ws p/time-reference (? #\s))
;;;	   (list p/ordinal (if (eq :duration (car p/time-reference))
;;;			       (cons :rel (rest p/time-reference))
;;;			       p/time-reference)))

      ((before prior)
       (list :rel :before (p/time-reference in)))

      (after
       (list :rel :after (p/time-reference in)))

      ((beginning ;; (the p/ws beginning p/ws of)
	)
       (list :rel :begin (p/time-reference in)))

      (( ;; (the p/ws beginning p/ws of)
	starting
	;; (the p/ws start p/ws of)
	from
	since)
       (list :rel :from (p/time-reference in)))

      ((in during)
       (list :rel :in (p/time-reference in)))
   
      ((/ ;; (the p/ws end p/ws of)
	stopping
	finishing
	to
	until)
       (list :rel :to (p/time-reference in)))

      (of
       (list :rel :of (p/time-reference in)))
   
      ((ending
	;; (the p/ws end p/ws of)
	)
       (list :rel :end (p/time-reference in)))

      (otherwise
       (unread-token in token)))))

(defun p/time-reference (in &optional (eof-error-p t))
  (or (p/fixed-time in eof-error-p)
      (p/days-of-week in eof-error-p)
      (p/period-unit in eof-error-p)
      (p/time-duration in eof-error-p)
      (p/relative-time in eof-error-p)))

(defun p/qualified-time (in &optional (eof-error-p t))
  (let ((everyp (eq (peek-token in eof-error-p) 'every))
	result)
    (if-let ((time (p/time-reference in eof-error-p)))
      (setf result (if everyp (progn
				(read-token in)
				(list :every time))
		       time))
      (loop for time = (p/time-reference in nil) while time do
	   (setf result (list result time))))
    result))

(defun p/time (in &optional (eof-error-p t))
  (let ((*token-stack* nil))
    (if-let ((time (p/qualified-time in eof-error-p)))
      (if (eq (peek-token in nil) 'ago)
	  (progn
	    (read-token in)
	    (list :ago time))
	  time))))

;;;_ * A recurring period of time

(defun compile-duration (data)
  (let (new-list)
    (do ((old data (cdr old)))
	((null old))
      (if (keywordp (first old))
	  (ecase (first old)
	    (:year (push :years new-list))
	    (:month (push :months new-list))
	    (:week
	     (push :days new-list)
	     (push (* 7 (first (rest old))) new-list)
	     (setf old (rest old)))
	    (:day (push :days new-list))
	    (:hour (push :hours new-list))
	    (:minute (push :minutes new-list))
	    (:second (push :seconds new-list))
	    (:millisecond (push :milliseconds new-list))
	    (:microsecond (push :microseconds new-list))
	    (:nanosecond (push :nanoseconds new-list)))
	  (progn
	    (assert (integerp (first old)))
	    (push (first old) new-list))))
    (lambda (anchor)
      (time-range :duration (apply #'duration (nreverse new-list))
		  :anchor anchor))))

(defun compile-relative-time (data)
  (case (first data)
    (:this (compile-time (cadr data)))
    (:last
     (let ((reference (compile-time (cadr data))))
       ;; What about the difference between these:
       ;;   the last two months
       ;;   the last month
       ;;   last month
       ;; jww (2007-11-27): At the moment, all three forms present as having a
       ;; duration next on the list, which is not correct.  The last one
       ;; should be a relative time unit.
       (lambda (anchor)
	 (let ((range (funcall reference anchor)))
	   (if (get-range-begin range)
	       (time-range-previous range)
	       (time-range :begin ))))))

    (:next
     (let ((reference (compile-time (cadr data))))
       (lambda (anchor)
	 (time-range-next (funcall reference anchor)))))

    ;; jww (2007-12-02): is there a distinction here?
    ((:to :before)
     (let ((reference (compile-time (cadr data))))
       (lambda (anchor)
	 (time-range :end (time-range-begin
			   (funcall reference anchor))
		     :anchor anchor))))

    (:from
     (let ((reference (compile-time (cadr data))))
       (lambda (anchor)
	 (time-range :begin (time-range-begin
			     (funcall reference anchor))
		     :anchor anchor))))
    (:after
     (let ((reference (compile-time (cadr data))))
       (lambda (anchor)
	 (time-range :begin (time-range-end
			     (funcall reference anchor))
		     :anchor anchor))))

    (otherwise
     (let* ((reltime (apply #'relative-time data))
	    (smallest-resolution (find-smallest-resolution data))
	    (duration (compile-duration (list smallest-resolution 1))))
       (lambda (anchor)
	 (time-range :begin    reltime
		     :duration (time-range-duration
				(funcall duration anchor))
		     :anchor   anchor))))))

(defun compile-time (data)
  (if (keywordp (first data))
      (case (first data)
	(:fixed
	 (let* ((moment (apply #'fixed-time (rest data)))
		(smallest-resolution (find-smallest-resolution (rest data)))
		(duration (compile-duration (list smallest-resolution 1))))
	   (lambda (anchor)
	     (time-range :begin moment
			 :duration (time-range-duration
				    (funcall duration anchor))))))
	(:duration
	 (let ((duration (compile-duration (rest data))))
	   (lambda (anchor)
	     (time-range :begin anchor
			 :duration (time-range-duration
				    (funcall duration anchor))))))
	(:rel (compile-relative-time (rest data)))
	(:every
	 ;; jww (2007-11-26): Create a time period object here -- or must that
	 ;; be one step removed from this function?
	 (assert "Compiling of period expressions not yet supported")))

      (let (result)
	(dolist (element data)
	  (let ((function (compile-time element)))
	    (setf result
		  (if result
		      (let ((previous result))
			(lambda (anchor)
			  (let ((range (funcall function anchor)))
			    (funcall previous (time-range-begin range)))))
		      function))))
	(or result #'identity))))

(defun parse-time-period (string)
  (funcall (compile-time (p/time (make-string-input-stream string)))
	   (fixed-time :hour 0)))

(defun parse-time-range (string)
  ;; jww (2007-12-01): The call to fixed-time here should be sensitive to the
  ;; input precision
  (funcall (compile-time (p/time (make-string-input-stream string)))
	   (fixed-time :hour 0)))

(defun time-parser-tests ()
  (dolist
      (expr '("this year"
	      "next year"
	      "last year"
	      "the year before last"
	      "jan 8"
	      "jan 2008"
	      "2008/01/01"
	      "2 months"
	      "2 months since jan 8"
	      "january of last year"
	      "three months ago"
	      "1 months, 2 days ago"
	      "every friday starting tomorrow"
	      "every day this week"
	      "every day of this week"
	      "every ten days"
	      "the day after tuesday"
	      "monthly"
	      "monthly from the beginning of this year"
	      "monthly from now until the end of the year"
	      "the last week of last year"
	      ;; "every weekend this year"
	      ))
    (format t "EXPR <  ~A~%     >= ~S~%" expr
	    (p/time (make-string-input-stream expr)))))

;; EOF