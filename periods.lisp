(declaim (optimize (debug 3) (safety 3) (speed 1) (space 0)))

(defpackage :periods
  (:use :cl :local-time :com.gigamonkeys.parser)
  (:nicknames :time-periods)
  (:export leapp
	   this-year
	   days-in-month
	   increment-time
	   decrement-time
	   floor-time
	   parse-time-period
	   time-period-generator
	   time-periods
	   map-over-time
	   do-over-time
	   sleep-until))

(in-package :periods)

(defparameter *days-in-months*
  #(31 28 31 30 31 30 31 31 30 31 30 31))

;; Snippet courtesy of Xach on #lisp
(declaim (inline leapp))
(defun leapp (year)
  (cond ((zerop (mod year 400)) t)
        ((zerop (mod year 100)) nil)
        ((zerop (mod year 4)) t)
        (t nil)))

(declaim (inline this-year))
(defun this-year ()
 (nth-value 5 (get-decoded-time)))

(defun days-in-month (month &optional year)
  (let ((days-in-month
	 (aref *days-in-months* (1- month)))
	(the-year (or year (this-year))))
    (if (and (= month 2)
	     (leapp the-year))
	(incf days-in-month)
	days-in-month)))

(defun floor-time (time &optional resolution)
  (declare (type local-time time))
  (multiple-value-bind
	(ms ss mm hh day month year)
      (local-time:decode-local-time time)
    (block nil
      (if (eq resolution :millisecond) (return))
      (setf ms 0)
      (if (eq resolution :second) (return))
      (setf ss 0)
      (if (eq resolution :minute) (return))
      (setf mm 0)
      (if (eq resolution :hour) (return))
      (setf hh 0)
      (if (eq resolution :day) (return))
      (setf day 1)
      (if (eq resolution :month) (return))
      (setf month 1))
    (encode-local-time ms ss mm hh day month year)))

(defun find-smallest-resolution (step-by)
  (cond
    ((member :milliseconds step-by)
     :millisecond)
    ((member :seconds step-by)
     :second)
    ((member :minutes step-by)
     :minute)
    ((member :hours step-by)
     :hour)
    ((member :days step-by)
     :day)
    ((member :months step-by)
     :month)))

(defun increment-time* (epoch &key
			(terminus nil)
			(terminus-forward-p nil)
			(years nil)
			(months nil)
			(days nil)
			(hours nil)
			(minutes nil)
			(seconds nil)
			(milliseconds nil)
			(floorp nil))
  (multiple-value-bind
	(ms ss mm hh day month year day-of-week)
      (local-time:decode-local-time epoch)
    (declare (ignorable day-of-week))

    (when floorp
      (block nil
	(if milliseconds (return))
	(setf ms 0)
	(if seconds (return))
	(setf ss 0)
	(if minutes (return))
	(setf mm 0)
	(if hours (return))
	(setf hh 0)
	(if days (return))
	(setf day 1)
	(if months (return))
	(setf month 1)))

    (labels
	((skip-year (skip)
	   (incf year skip))
	 (skip-month (skip)
	   (if (plusp skip)
	       (let ((remainder (- (+ month skip) 12)))
		 (if (plusp remainder)
		     (progn
		       (skip-year 1)
		       (setf month 1)
		       (skip-month (1- remainder)))
		     (incf month skip)))
	       (let ((remainder (+ (1- month) skip)))
		 (if (minusp remainder)
		     (progn
		       (skip-year -1)
		       (setf month 12)
		       (skip-month remainder))
		     (incf month skip)))))
	 (skip-day (skip)
	   (if (plusp skip)
	       (let ((remainder (- (+ day skip)
				   (days-in-month month year))))
		 (if (plusp remainder)
		     (progn
		       (skip-month 1)
		       (setf day 1)
		       (skip-day (1- remainder)))
		     (incf day skip)))
	       (let ((remainder (+ (1- day) skip)))
		 (if (minusp remainder)
		     (progn
		       (skip-month -1)
		       (setf day (days-in-month month year))
		       (skip-day remainder))
		     (incf day skip)))))
	 (skip-hour (skip)
	   (if (plusp skip)
	       (let ((remainder (- (+ hh skip) 23)))
		 (if (plusp remainder)
		     (progn
		       (skip-day 1)
		       (setf hh 0)
		       (skip-hour (1- remainder)))
		     (incf hh skip)))
	       (let ((remainder (+ hh skip)))
		 (if (minusp remainder)
		     (progn
		       (skip-day -1)
		       (setf hh 59)
		       (skip-hour remainder))
		     (incf hh skip)))))
	 (skip-minute (skip)
	   (if (plusp skip)
	       (let ((remainder (- (+ mm skip) 59)))
		 (if (plusp remainder)
		     (progn
		       (skip-hour 1)
		       (setf mm 0)
		       (skip-minute (1- remainder)))
		     (incf mm skip)))
	       (let ((remainder (+ mm skip)))
		 (if (minusp remainder)
		     (progn
		       (skip-hour -1)
		       (setf mm 59)
		       (skip-minute remainder))
		     (incf mm skip)))))
	 (skip-second (skip)
	   (if (plusp skip)
	       (let ((remainder (- (+ ss skip) 59)))
		 (if (plusp remainder)
		     (progn
		       (skip-minute 1)
		       (setf ss 0)
		       (skip-second (1- remainder)))
		     (incf ss skip)))
	       (let ((remainder (+ ss skip)))
		 (if (minusp remainder)
		     (progn
		       (skip-minute -1)
		       (setf ss 59)
		       (skip-second remainder))
		     (incf ss skip)))))
	 (skip-millisecond (skip)
	   (if (plusp skip)
	       (let ((remainder (- (+ ms skip) 999)))
		 (if (plusp remainder)
		     (progn
		       (skip-second 1)
		       (setf ms 0)
		       (skip-millisecond (1- remainder)))
		     (incf ms skip)))
	       (let ((remainder (+ ms skip)))
		 (if (minusp remainder)
		     (progn
		       (skip-second -1)
		       (setf ms 999)
		       (skip-millisecond remainder))
		     (incf ms skip))))))
      (if years (skip-year years))
      (if months (skip-month months))
      (if days (skip-day days))
      (if hours (skip-hour hours))
      (if minutes (skip-minute minutes))
      (if seconds (skip-second seconds))
      (if milliseconds (skip-millisecond milliseconds)))

    (let ((new-epoch (encode-local-time ms ss mm hh day month year)))
      (if (and terminus
	       (if terminus-forward-p
		   (local-time:local-time> new-epoch terminus)
		   (local-time:local-time< new-epoch terminus)))
	  nil
	  new-epoch))))

(defun increment-time (epoch &key
		       (terminus nil)
		       (specifier nil)
		       (years nil)
		       (months nil)
		       (days nil)
		       (hours nil)
		       (minutes nil)
		       (seconds nil)
		       (milliseconds nil)
		       (floorp nil))
  (if specifier
      (let* ((step-by (getf specifier :step-by))
	     (skip (getf specifier :skip))
	     (to (getf specifier :to))
	     (end-of-range
	      (apply #'increment-time*
		     epoch
		     :terminus (or terminus to)
		     :floorp floorp
		     step-by)))
	(if skip
	    (values epoch end-of-range
		    (apply #'increment-time* epoch
			   :terminus (or terminus to)
			   :floorp floorp
			   skip))
	    (values epoch end-of-range end-of-range)))
      (increment-time* epoch
		       :terminus terminus
		       :years years
		       :months months
		       :days days
		       :hours hours
		       :minutes minutes
		       :seconds seconds
		       :milliseconds milliseconds
		       :floorp floorp)))

(defun decrement-time (epoch &key
		       (terminus nil)
		       (years nil)
		       (months nil)
		       (days nil)
		       (hours nil)
		       (minutes nil)
		       (seconds nil)
		       (milliseconds nil)
		       (floorp nil))
  (increment-time* epoch
		   :terminus terminus
		   :terminus-forward-p t
		   :years (- years)
		   :months (- months)
		   :days (- days)
		   :hours (- hours)
		   :minutes (- minutes)
		   :seconds (- seconds)
		   :milliseconds (- milliseconds)
		   :floorp floorp))

(defun sleep-until (epoch)
  (let ((now (local-time:now)))
    (if (local-time:local-time> epoch now)
	(sleep (+ (* 86400 (- (local-time:local-time-day epoch)
			      (local-time:local-time-day now)))
		  (- (local-time:local-time-sec epoch)
		     (local-time:local-time-sec now))
		  (/ (- (local-time:local-time-msec epoch)
			(local-time:local-time-msec now)) 1000))))))

(defun time-period-generator (&key
			      (specifier nil)
			      (from nil)
			      (to nil)
			      (years nil)
			      (months nil)
			      (days nil)
			      (hours nil)
			      (minutes nil)
			      (seconds nil)
			      (milliseconds nil)
			      (floorp nil)
			      (sleep-until-period-p nil))
  "Create a generator to iterate through successive time periods.

  For example: (time-period-generator :days 4) returns a function that can be
  called repeatedly.  Each time it is called, it returns either NIL to
  represent no further periods, or a three values:

    (values START-OF-PERIOD END-OF-PERIOD START-OF-NEXT-PERIOD)

  Because the START-OF-PERIOD is likely to be of most interest, a common idiom
  for using the generator might be:

    (loop
       with generator = (time-period-generator :days 4)
       for moment = (funcall generator)
       for i from 1 to 10
       while (and moment (<= i 10))
       do (format t \"The new date is: ~S~%\" moment))

  This loop prints out what the date is, every four days from now, ten times.

  There are two different ways of specifying the period to be looped.  For
  convenience, there are several keywords for specifying basic spans of time:

    :FROM <LOCAL-TIME>       (where <TIME> can also be :NOW)
    :TO   <LOCAL-TIME>

    :YEARS <INTEGER>
    :MONTHS <INTEGER>
    :DAYS <INTEGER>
    :HOURS <INTEGER>
    :MINUTES <INTEGER>
    :SECONDS <INTEGER>
    :MILLISECONDS <INTEGER>

  The other way of specifying time is intended for machine generation.  In
  this case, you use the :SPECIFIER keyword and a time specifier.  The easiest
  way to create a time specifier is to call `parse-time-period', which takes a
  natural language string, for example:

    (parse-time-period \"every second thursday in this year\")

  Please see the documentation for that function for more details."
  (let ((time-specifier
	 (if specifier
	     (parse-time-period specifier)
	     `(:step-by
	       ,@(let (steps)
		      (if years
			  (push `(:years ,years) steps))
		      (if months
			  (push `(:months ,months) steps))
		      (if days
			  (push `(:days ,days) steps))
		      (if hours
			  (push `(:hours ,hours) steps))
		      (if minutes
			  (push `(:minutes ,minutes) steps))
		      (if seconds
			  (push `(:seconds ,seconds) steps))
		      (if milliseconds
			  (push `(:milliseconds ,milliseconds) steps))
		      steps)
	       :from ,from
	       :to   ,to)))
	epoch terminated-p)
    (format t "time-specifier = ~S~%" time-specifier)
    ;; construct a closure which iterates over the period
    (lambda (&optional new-from)
      (unless terminated-p
	(let ((begin
	       (or new-from
		   epoch
		   (getf time-specifier :from)
		   (if floorp
		       (floor-time (local-time:now)
				   (find-smallest-resolution
				    (getf time-specifier :step-by)))
		       (local-time:now)))))
	  (multiple-value-bind
		(period-start period-end next-period-start)
	      (increment-time begin :specifier time-specifier
			      :floorp floorp)
	    (if period-start
		(progn
		  (setf epoch next-period-start)
		  (if sleep-until-period-p
		      (sleep-until period-start))
		  (values period-start period-end next-period-start))
		(progn
		  (setf terminated-p t)
		  nil))))))))

(defun time-periods (&rest time-specifiers)
  (loop
     with generator = (apply #'time-period-generator time-specifiers)
     for period = (funcall generator)
     while period
     collect period))

(defun map-over-time (closure &rest time-specifiers)
  (loop
     with generator = (apply #'time-period-generator time-specifiers)
     for period = (funcall generator)
     while period
     do (funcall closure period)))

(defmacro do-over-time (&rest time-specifiers)
  (let ((moment (gensym)))
    (multiple-value-bind (generator loop-words)
	(eval `(time-period-generator ,@time-specifiers))
      `(loop
	  :for ,moment = (funcall ,generator)
	  :while ,moment
	  ,@loop-words))))

(provide 'periods)

;; periods.lisp ends here
