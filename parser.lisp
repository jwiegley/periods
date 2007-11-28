(declaim (optimize (debug 3) (safety 3) (speed 1) (space 0)))

(in-package :periods)

;;;_ * Simple numerics

(defvar *next-token* nil)

(defun peek-token (in &optional (eof-error-p t))
  (or *next-token*
      (progn
	(peek-char t in eof-error-p)	; skip whitespace
	(setf *next-token* (read in eof-error-p)))))

(defun read-token (in &optional (eof-error-p t))
  (if *next-token*
      (prog1
	  *next-token*
	(setf *next-token* nil))
      (progn
	(peek-char t in eof-error-p)	; skip whitespace
	(read in eof-error-p))))

(defun unread-token (in token)
  (declare (ignore in))
  (assert (null *next-token*))
  (setf *next-token* token))

;;;_  + Coded

(defun p/number (in)
  (if (digit-char-p (peek-char t in))
      (read in)))

;;;_  + English units

(defun p/cardinal (in)
  (or (p/number in)
      (let ((token (peek-token in)))
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

(defun p/ordinal (in)
  (let ((token (peek-token in)))
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

(defun p/days-of-week (in)
  (let ((token (peek-token in)))
    (case token
      (sunday (read-token in) (list :day-of-week 0))
      (monday (read-token in) (list :day-of-week 1))
      (tuesday (read-token in) (list :day-of-week 2))
      (wednesday (read-token in) (list :day-of-week 3))
      (thursday (read-token in) (list :day-of-week 4))
      (friday (read-token in) (list :day-of-week 5))
      (saturday (read-token in) (list :day-of-week 6)))))

(defun p/months-of-year (in)
  (let ((token (peek-token in)))
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

(defun p/time-unit (in)
  (let ((token (peek-token in)))
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

(defun p/period-unit (in)
  (let ((token (peek-token in)))
    (if (eq token 'per)
	(progn
	  (read-token in)
	  (setf token (read-token in))
	  (case token
	    (nanosecond
	     (read-token in) (list :every (list :duration :nanosecond 1)))
	    (microsecond (read-token in)
			 (list :every (list :duration :microsecond 1)))
	    (millisecond (read-token in)
			 (list :every (list :duration :millisecond 1)))
	    (second (read-token in) (list :every (list :duration :second 1)))
	    (minute (read-token in) (list :every (list :duration :minute 1)))
	    ))
	(case token
	  (hourly (read-token in) (list :every (list :duration :hour 1)))
	  (daily (read-token in) (list :every (list :duration :day 1)))
	  (weekly (read-token in) (list :every (list :duration :week 1)))
	  (monthly (read-token in) (list :every (list :duration :month 1)))
	  ((yearly annually)
	   (read-token in) (list :every (list :duration :year 1)))))))

;;;_ * A fixed point in time

(defun p/fixed-time (in)
  (let ((token (peek-token in)))
    (case token
      (now (read-token in) (list :fixed :now))

      (today (read-token in) (list :rel :today))
      (tomorrow (read-token in) (list :rel :tomorrow))
      (yesterday (read-token in) (list :rel :yesterday))

      (otherwise
       (let ((months-of-year (p/months-of-year in)))
	 (if months-of-year
	     (let ((number (or (p/cardinal in)
			       (p/ordinal in))))
	       (list :rel :this
		     (append (list :rel) months-of-year
			     (if number
				 (list (if (> number 40) :year :day)
				       number)))))
	     (let ((first (p/number in)))
	       (when first
		 (if (char= #\/ (peek-char nil in))
		     (let ((second (p/number in)))
		       (if (char= #\/ (peek-char nil in))
			   (let ((third (p/number in)))
			     ;; jww (2007-11-28): Backtrack all the way if
			     ;; this fails!
			     (when third
			       (list :fixed :year first :month second
				     :day third)))
			   (list :fixed :year first :month second)))
		     (progn
		       (unread-token in first)
		       nil))))))))))

;;;_ * A duration of time

(defun p/time-duration (in)
  (let ((duration (p/duration-spec in)))
    (when duration
      ;; jww (2007-11-28): I don't support "3 months, 2 weeks" yet
      (loop for and-next-p = (eq 'and (peek-token in))
	 while and-next-p do (nconc duration (p/duration-spec in)))
      (cons :duration duration))))

(defun p/duration-spec (in)
  (let ((number (p/cardinal in)))
    (if number
	;; jww (2007-11-28): What if this fails?
	(list (p/time-unit in) number)
	(progn
	  (if (eq 'a (peek-token in))
	      (read-token in))
	  ;; jww (2007-11-28): Check for error
	  (list (p/time-unit in) 1)))))

;;;_ * A relative point in time

(defun p/relative-time (in)
  (let ((number (p/cardinal in)))
    (if number
	(list number (p/time-reference in))
	(case (peek-token in)
	  (the (list :rel :next (p/time-reference in)))

	  (this
	   (let ((reference (p/time-reference in)))
	     (list :rel :this
		   (if (eq :duration (car reference))
		       (cons :rel (rest reference))
		       reference))))

	  ((next following)
	   (let ((reference (p/time-reference in)))
	     (list :rel :next
		   (if (eq :duration (car reference))
		       (cons :rel (rest reference))
		       reference))))

	  ((last previous preceeding)
	   (let ((reference (p/time-reference in)))
	     (list :rel :last
		   (if (eq :duration (car reference))
		       (cons :rel (rest reference))
		       reference))))

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
	   (list :rel :end (p/time-reference in)))))))

(defun p/time-reference (in)
  (or (p/fixed-time in)
      (p/days-of-week in)
      (p/period-unit in)
      (p/time-duration in)
      (p/relative-time in)))

(defun p/qualified-time (in)
  (let ((everyp (eq (peek-token in) 'every))
	result)
    (if everyp (read-token in))
    (let ((time (p/time-reference in)))
      (setf result (if everyp
		       (list :every time)
		       time)))
    (loop for time = (p/time-reference in)
       while time do (setf result (list result time)))
    result))

(defun p/time (in)
  (let ((time (p/qualified-time in)))
    (if (eq (peek-token in nil) 'ago)
	(setf time (list :ago time)))
    time))

;;;_ * A recurring period of time

(defun compile-duration (data)
  (let (new-list)
    (do ((old data (cdr old)))
	((null old))
      (if (keywordp (first old))
	  (ecase (first old)
	    (:year
	     (push :years new-list))
	    (:month
	     (push :months new-list))
	    (:week
	     (push :days new-list)
	     (push (* 7 (first (rest old))) new-list)
	     (setf old (rest old)))
	    (:day
	     (push :days new-list))
	    (:hour
	     (push :hours new-list))
	    (:minute
	     (push :minutes new-list))
	    (:second
	     (push :seconds new-list))
	    (:millisecond
	     (push :milliseconds new-list))
	    (:microsecond
	     (push :microseconds new-list))
	    (:nanosecond
	     (push :nanoseconds new-list)))
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

    (:before
     (let ((reference (compile-time (cadr data))))
       (lambda (anchor)
	 (time-range :end (time-range-begin
			   (funcall reference anchor))
		     :anchor anchor))))

    (:after
     (let ((reference (compile-time (cadr data))))
       (lambda (anchor)
	 (time-range :begin (time-range-end
			     (funcall reference anchor))
		     :anchor anchor))))

    (otherwise
     (let ((reltime (apply #'relative-time data))
	   (duration (compile-duration data)))
       (lambda (anchor)
	 (time-range :begin    reltime
		     :duration (funcall duration anchor)
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
			 :duration (funcall duration anchor)))))
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
		      (lambda (anchor)
			(funcall function (funcall result anchor)))
		      function))))
	(or result #'identity))))

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
	      ))
    (format t "EXPR <  ~A~%     >= ~S~%" expr
	    (p/time (make-string-input-stream expr)))))
