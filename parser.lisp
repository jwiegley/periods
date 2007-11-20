(in-package :periods)

;;;_ * Simple numerics

;;;_  + Coded

(defprod p/ws () (+ (/ #\Space #\Tab #\Newline)))

(defchartype p/digit '(satisfies digit-char-p))

(defprod p/digits () (+ p/digit))

(defprod p/number ()
  (^ p/digits (parse-integer p/digits)))

;;;_  + English

(defprod p/cardinal ()
  (/
   (^ p/number (1- p/number))
   (^ "one" 0)
   (^ "two" 1)
   (^ "three" 2)
   (^ "four" 3)
   (^ "five" 4)
   (^ "six" 5)
   (^ "seven" 6)
   (^ "eight" 7)
   (^ "nine" 8)
   (^ "ten" 9)))

(defprod p/ordinal ()
  (/ 
   (^ (/ "1st" "first") 0)
   (^ "other" 1)
   (^ (/ "2nd" "second") 1)
   (^ (/ "3rd" "third") 2)
   (^ (/ "4th" "fourth") 3)
   (^ (/ "5th" "fifth") 4)
   (^ (/ "6th" "sixth") 5)
   (^ (/ "7th" "seventh") 6)
   (^ (/ "8th" "eighth") 7)
   (^ (/ "9th" "ninth") 8)
   (^ (/ "10th" "tenth") 9)))

;;;_ * Fixed time

(defprod p/rel-time-unit ()
  (/
   (^ "millisecond"  (list))
   (^ "second"       (list :millisecond 0))
   (^ "minute"       (list :second 0))
   (^ "hour"         (list :minute 0))
   (^ "day"          (list :hour 0))
   (^ "week"         (list :day-of-week 0))
   (^ "month"        (list :day 1))
   (^ "year"         (list :month 1))))

(defprod p/days-of-week ()
  (/
   (^ "sunday"       (list :day-of-week 0))
   (^ "monday"       (list :day-of-week 1))
   (^ "tuesday"      (list :day-of-week 2))
   (^ "wednesday"    (list :day-of-week 3))
   (^ "thursday"     (list :day-of-week 4))
   (^ "friday"       (list :day-of-week 5))
   (^ "saturday"     (list :day-of-week 6))))

(defprod p/fixed-time (anchor reverse)
  (/
   (^ "now" (now))

   (^ p/time-range
      (progn
	(format t "p/fixed-time = ~S~%" p/time-range)
	(time-range-begin p/time-range)))

   (^ (p/time-duration
       (/ (^ (p/ws (/
		    (^ "from")
		    (^ "after")
		    (^ "since")) p/ws p/fixed-time)
	     (setf anchor p/fixed-time
		   reverse nil))
	  (^ (p/ws (/
		    (^ "before")) p/ws p/fixed-time)
	     (setf anchor p/fixed-time
		   reverse t))
	  (^ (p/ws "ago")
	     (setf anchor (now)
		   reverse t))))
      (progn
	(format t "p/time-duration = ~S~%" p/time-duration)
       (add-time anchor p/time-duration :reverse reverse)))))

(defprod p/dur-time-unit ()
  (/
   (^ "millisecond"  (list :milliseconds 1))
   (^ "second"       (list :seconds 1))
   (^ "minute"       (list :minutes 1))
   (^ "hour"         (list :hours 1))
   (^ "day"          (list :days 1))
   (^ "week"         (list :days 7))
   (^ "month"        (list :months 1))
   (^ "year"         (list :years 1))))

(defprod p/time-duration (reverse moment)
  (^ (p/cardinal p/ws p/dur-time-unit (? #\s))
     (progn
       (setf (nth 1 p/dur-time-unit)
	     (* (nth 1 p/dur-time-unit) (1+ p/cardinal)))
       (apply #'duration p/dur-time-unit))))

;; jww (2007-11-20): Handle "the fourth tuesday"
(defprod p/time-range (reverse moment end duration)
  (/
   (^ ((/ (^ "from")
	  (^ "since")) p/ws (^ p/fixed-time
			       (setf moment p/fixed-time)) p/ws
       (/ (^ "until")
	  (^ "to")) p/ws (^ p/fixed-time
			    (setf end p/fixed-time)))
      (time-range :begin moment :end end))

   (^ ((/ (^ "from")
	  (^ "since")) p/ws (^ p/fixed-time
	  (setf moment p/fixed-time)) p/ws
       (/ (^ "until")
	  (^ "to")) p/ws (^ p/time-duration
	  (setf duration p/time-duration)))
      (time-range :begin moment :duration duration))

   (^ ((/ (^ "from")
	  (^ "since")) p/ws (^ p/time-duration
	  (setf duration p/time-duration)) p/ws
       (/ (^ "until")
	  (^ "to")) p/ws (^ p/fixed-time
	  (setf end p/fixed-time)))
      (time-range :duration duration :end end))
   
   (^ ("this" p/ws p/rel-time-unit)
      (let* ((rtime (apply #'relative-time p/rel-time-unit))
	     (anchor
	      (floor-time (now) (find-smallest-resolution p/rel-time-unit)))
	     (begin (previous-time anchor rtime :accept-anchor t)))
	(time-range :begin begin :end (next-time begin rtime))))

   (^ ((/ (^ "next")
	  (^ "this next")
	  (^ "the following")) p/ws p/rel-time-unit)
      (let* ((rtime (apply #'relative-time p/rel-time-unit))
	     (anchor
	      (floor-time (now) (find-smallest-resolution p/rel-time-unit)))
	     (next (next-time anchor rtime)))
	(time-range :begin next :end (next-time next rtime))))

   (^ ((/ (^ "last")
	  (^ "this last")
	  (^ "this past")
	  (^ "the preceding")) p/ws p/rel-time-unit)
      (let* ((rtime (apply #'relative-time p/rel-time-unit))
	     (anchor
	      (floor-time (now) (find-smallest-resolution p/rel-time-unit)))
	     (previous (previous-time anchor rtime :accept-anchor t)))
	(time-range :begin (previous-time previous rtime)
		    :end previous)))

   ;;   ;; jww (2007-11-20): There is a difference between:
   ;;   ;;   two weeks from today (today + 14 days)
   ;;   ;;   last week (the sunday before last until this past sunday)
   ;;   (^ (p/cardinal p/ws p/rel-time-unit (? #\s)
   ;;		  (? (/ (^ (p/ws (/ (^ "from")
   ;;				    (^ "since")) p/ws p/fixed-time)
   ;;			   (progn
   ;;			     (format t "Hello~%")
   ;;			     (setf reverse nil
   ;;				   moment p/fixed-time)))
   ;;			(^ (p/ws "ago")
   ;;			   (setf reverse t)))))
   ;;      (let* ((rtime (apply #'relative-time p/rel-time-unit))
   ;;	     (anchor
   ;;	      (or moment
   ;;		  (floor-time (now) (find-smallest-resolution
   ;;				     p/rel-time-unit))))
   ;;	     (start anchor))
   ;;	(format t "moment = ~S~%" moment)
   ;;	(dotimes (i (1+ p/cardinal))
   ;;	  (setf start
   ;;		(next-time start rtime
   ;;			   :accept-anchor (and reverse
   ;;					       (local-time= start anchor))
   ;;			   :reverse reverse)))
   ;;	(if reverse
   ;;	    (time-range :begin (previous-time start rtime) :end start)
   ;;	    (time-range :begin start :end (next-time start rtime)))))

   (^ ("this" p/ws p/days-of-week)
      (let* ((rtime (apply #'relative-time p/days-of-week))
	     (anchor
	      (floor-time (now) (find-smallest-resolution p/days-of-week)))
	     ;; jww (2007-11-20): Does "this sunday" means next sunday, even
	     ;; if today is Sunday?
	     (begin (next-time anchor rtime)))
	(time-range :begin begin :duration (duration :days 1))))

   (^ ((/ (^ "next")
	  (^ "this next")
	  (^ "the following")) p/ws p/days-of-week)
      (let* ((rtime (apply #'relative-time p/days-of-week))
	     (anchor
	      (floor-time (now) (find-smallest-resolution p/days-of-week)))
	     (next (next-time anchor rtime)))
	(time-range :begin next :duration (duration :days 1))))

   (^ ((/ (^ "last")
	  (^ "this last")
	  (^ "this past")
	  (^ "the preceding")) p/ws p/days-of-week)
      (let* ((rtime (apply #'relative-time p/days-of-week))
	     (anchor
	      (floor-time (now) (find-smallest-resolution p/days-of-week)))
	     (previous (previous-time anchor rtime :accept-anchor t)))
	(time-range :begin (previous-time previous rtime)
		    :duration (duration :days 1))))

   ;;   (^ (p/cardinal p/ws p/days-of-week
   ;;		  (? #\s) p/ws
   ;;		  (? (/ (^ "from now" (setf reverse nil))
   ;;			(^ "ago" (setf reverse t)))))
   ;;      (let* ((rtime (apply #'relative-time p/days-of-week))
   ;;	     (anchor
   ;;	      (floor-time (now) (find-smallest-resolution p/days-of-week)))
   ;;	     (start anchor))
   ;;	(dotimes (i (1+ p/cardinal))
   ;;	  (setf start
   ;;		(next-time start rtime
   ;;			   :accept-anchor (and reverse
   ;;					       (local-time= start anchor))
   ;;			   :reverse reverse)))
   ;;	(if reverse
   ;;	    (time-range :begin (previous-time start (relative-time :day 1))
   ;;			:end start)
   ;;	    (time-range :begin start
   ;;			:end (next-time start (relative-time :day 1)))))) 

   (^ "today" (this-day-range))
   (^ "tomorrow" (day-range (next-day)))
   (^ "yesterday" (day-range (previous-day)))))

(defprod period ()
  (^ (step-by (? (p/ws skip)) (? (p/ws range)))
     (time-period :range range :step step-by :skip skip)))

(defprod step-by ()
  (/
   (^ ("every" p/ws units)      (apply #'duration units))
   (^ "hourly"		      (duration :hours 1))
   (^ "daily"		      (duration :days 1))
   (^ "weekly"		      (duration :days 7))
   (^ "monthly"		      (duration :months 1))
   (^ (/ "yearly" "annually") (duration :years 1))))

(defprod skip ()
  (^ ("every" p/ws skip-units)
     (apply #'duration skip-units)))

(defprod range (the-start the-end)
  (^ (/
      ("from" p/ws (@ time-spec (setf the-start time-spec))
	      p/ws "to"
	      p/ws (@ time-spec (setf the-end time-spec)))
      (@ ("since" p/ws time-spec) (setf the-start time-spec the-end (now)))
      (@ ("until" p/ws time-spec) (setf the-start (now) the-end time-spec)))
     (time-range :begin the-start :end the-end)))

(defprod units ()
  ((^ unit unit)
   (? (? (+ ("," p/ws (^ unit (append unit units)))) ",")
      (p/ws "and" p/ws (^ unit (append unit units))))))

(defprod skip-units ()
  ((^ skip-unit skip-unit)
   (? (? (+ ("," p/ws (^ skip-unit (append skip-unit skip-units)))) ",")
      (p/ws "and" p/ws (^ skip-unit (append skip-unit skip-units))))))

(defprod unit ()
  (^ ((? p/number p/ws) basic-unit) (list basic-unit (or p/number 1))))

(defprod skip-unit ()
  (^ ((? p/ordinal p/ws) basic-unit) (list basic-unit (or p/ordinal 1))))

(defprod basic-unit ()
  (^ ((^ 
       (/ "millisecond"
	  "second"
	  "minute"
	  "hour"
	  "day"
	  "week"			; jww (2007-11-19): meaning?
	  "month"
	  "year"))
      (? "s"))
     (intern (string-upcase (format nil "~as" basic-unit)) :keyword)))

(defprod time-spec (reverse)
  (/ (^ "now" (fixed-time :hour 0))
     (^ (unit p/ws (/ (@ "ago" (setf reverse t))
                    (@ "from now" (setf reverse nil))))
        (add-time (floor-time (now) :day) (apply #'duration unit)
		  :reverse reverse))))

(defparser time-period-parser (^ period))

(defun parse-time-period (string)
  (multiple-value-bind (ok value) (time-period-parser string)
    (if ok value nil)))

(defmacro tdp (production input)
  `((lambda (x)
      (parselet ((foo (^ ,production)))
        (foo x))) ,input))
