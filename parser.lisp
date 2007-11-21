(in-package :periods)

;;;_ * Simple numerics

;;;_  + Coded

(defprod p/ws () (+ (/ #\Space #\Tab #\Newline)))

(defchartype p/digit '(satisfies digit-char-p))

(defprod p/digits () (+ p/digit))

(defprod p/number ()
  (^ p/digits (parse-integer p/digits)))

;;;_  + English units

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

(defstruct time-quantity
  (resolution :day :type keyword)
  (anchor nil :type (or fixed-time null))
  (terminus nil :type (or fixed-time null))
  (resolvedp nil :type boolean)
  (offset nil :type (or relative-time null))
  (multiple 1 :type integer)
  (duration nil :type duration)
  (reverse nil :type boolean))

(defun calculate-anchor (quantity)
  (let ((begin
	 (time-range-begin
	  (time-range :begin (time-quantity-anchor quantity)
		      :end (time-quantity-terminus quantity)
		      :duration (time-quantity-duration quantity))))
	(offset (time-quantity-offset quantity)))
    (if (and offset (not (time-quantity-resolvedp quantity)))
	(dotimes (i (time-quantity-multiple quantity))
	  (setf begin (if (time-quantity-reverse quantity)
			  (previous-time begin offset)
			  (next-time begin offset)))))
    begin))

(defprod p/days-of-week ()
  (/
   (^ "sunday"
      (make-time-quantity :resolution :day
			  :offset (relative-time :day-of-week 0)
			  :duration (duration :days 1)))
   (^ "monday"
      (make-time-quantity :resolution :day
			  :offset (relative-time :day-of-week 1)
			  :duration (duration :days 1)))
   (^ "tuesday"
      (make-time-quantity :resolution :day
			  :offset (relative-time :day-of-week 2)
			  :duration (duration :days 1)))
   (^ "wednesday"
      (make-time-quantity :resolution :day
			  :offset (relative-time :day-of-week 3)
			  :duration (duration :days 1)))
   (^ "thursday"
      (make-time-quantity :resolution :day
			  :offset (relative-time :day-of-week 4)
			  :duration (duration :days 1)))
   (^ "friday"
      (make-time-quantity :resolution :day
			  :offset (relative-time :day-of-week 5)
			  :duration (duration :days 1)))
   (^ "saturday"
      (make-time-quantity :resolution :day
			  :offset (relative-time :day-of-week 6)
			  :duration (duration :days 1)))))

(defprod p/months-of-year ()
  (/
   (^ (/ "January" "january" "Jan" "jan")
      (make-time-quantity :resolution :month
			  :offset (relative-time :month 1 :day 1)
			  :duration (duration :months 1)))
   (^ (/ "February" "january" "Jan" "jan")
      (make-time-quantity :resolution :month
			  :offset (relative-time :month 2 :day 1)
			  :duration (duration :months 1)))
   (^ (/ "March" "march" "Mar" "mar")
      (make-time-quantity :resolution :month
			  :offset (relative-time :month 3 :day 1)
			  :duration (duration :months 1)))
   (^ (/ "April" "april" "Apr" "apr")
      (make-time-quantity :resolution :month
			  :offset (relative-time :month 4 :day 1)
			  :duration (duration :months 1)))
   (^ (/ "May" "may")
      (make-time-quantity :resolution :month
			  :offset (relative-time :month 5 :day 1)
			  :duration (duration :months 1)))
   (^ (/ "June" "june" "Jun" "jun")
      (make-time-quantity :resolution :month
			  :offset (relative-time :month 6 :day 1)
			  :duration (duration :months 1)))
   (^ (/ "July" "july" "Jul" "jul")
      (make-time-quantity :resolution :month
			  :offset (relative-time :month 7 :day 1)
			  :duration (duration :months 1)))
   (^ (/ "August" "august" "Aug" "aug")
      (make-time-quantity :resolution :month
			  :offset (relative-time :month 8 :day 1)
			  :duration (duration :months 1)))
   (^ (/ "September" "september" "Sep" "sep")
      (make-time-quantity :resolution :month
			  :offset (relative-time :month 9 :day 1)
			  :duration (duration :months 1)))
   (^ (/ "October" "october" "Oct" "oct")
      (make-time-quantity :resolution :month
			  :offset (relative-time :month 10 :day 1)
			  :duration (duration :months 1)))
   (^ (/ "November" "november" "Nov" "nov")
      (make-time-quantity :resolution :month
			  :offset (relative-time :month 11 :day 1)
			  :duration (duration :months 1)))
   (^ (/ "December" "december" "Dec" "dec")
      (make-time-quantity :resolution :month
			  :offset (relative-time :month 12 :day 1)
			  :duration (duration :months 1)))))

(defprod p/time-unit ()
  (/
   (^ "millisecond"
      (make-time-quantity :resolution :millisecond
			  :duration (duration :milliseconds 1)))
   (^ "second"
      (make-time-quantity :resolution :second
			  :offset (relative-time :millisecond 0)
			  :duration (duration :seconds 1)))
   (^ "minute"
      (make-time-quantity :resolution :minute
			  :offset (relative-time :second 0)
			  :duration (duration :minutes 1)))
   (^ "hour"
      (make-time-quantity :resolution :hour
			  :offset (relative-time :minute 0)
			  :duration (duration :hours 1)))
   (^ "day"
      (make-time-quantity :resolution :day
			  :offset (relative-time :hour 0)
			  :duration (duration :days 1)))
   (^ "week"
      (make-time-quantity :resolution :day-of-week
			  :offset (relative-time :day-of-week 0)
			  :duration (duration :days 7)))
   (^ "month"
      (make-time-quantity :resolution :month
			  :offset (relative-time :day 1)
			  :duration (duration :months 1)))
   (^ "year"
      (make-time-quantity :resolution :year
			  :offset (relative-time :month 1)
			  :duration (duration :years 1)))))

(defprod p/period-unit ()
  (/
   (^ "per-millisecond"  (duration :milliseconds 1))
   (^ "per-second"       (duration :seconds 1))
   (^ "per-minute"       (duration :minutes 1))
   (^ "hourly"           (duration :hours 1))
   (^ "daily"            (duration :days 1))
   (^ "weekly"           (duration :days 7))
   (^ "monthly"          (duration :months 1))
   (^ (/ "yearly"
	 "annually")     (duration :years 1))))

;;;_ * A fixed point in time

(defprod p/fixed-time (year month day)
  (/
   (^ "now"
      (make-time-quantity :resolution :millisecond
				      :duration (duration :milliseconds 1)
				      :anchor (now)))

   (^ ((^ p/number (setf year p/number)) "/"
       (^ p/number (setf month p/number))
       (? ("/" (^ p/number (setf day p/number)))))
      (make-time-quantity :resolution (if day :day :month)
			  :duration (duration (if day :days :months) 1)
			  ;; jww (2007-11-20): Umm.. this could be much better
			  :anchor (strptime (format nil "~4,'0D/~2,'0D/~2,'0D"
						    year month day))))

   (^ (p/months-of-year p/ws (/ p/ordinal p/cardinal))
      (let ((number (1+ (or p/ordinal p/cardinal))))
	(format t "initial p/months-of-year ~S~%" p/months-of-year)
	(if (> number 40)
	    (setf (relative-time-year (time-quantity-offset p/months-of-year))
		  number)
	    (progn
	      (setf (relative-time-day (time-quantity-offset p/months-of-year))
		    number
		    (duration-months (time-quantity-duration p/months-of-year)) 0
		    (duration-days (time-quantity-duration p/months-of-year)) 1)))
	(format t "p/months-of-year is now ~S~%" p/months-of-year)
	p/months-of-year))))

;;;_ * A duration of time

(defprod p/time-duration (reverse moment)
  ((^ p/duration-spec p/duration-spec)
   (? (+
       ((/ ("," (? p/ws))
	   (p/ws "and" p/ws)
	   ("," (? p/ws) "and" p/ws))
	(^ p/duration-spec
	   (setf (time-quantity-duration p/time-duration)
		 (add-duration (time-quantity-duration p/time-duration)
			       (time-quantity-duration p/duration-spec)))))))))

(defprod p/duration-spec (reverse moment)
  (^ ((? (/ "a" p/cardinal) p/ws) p/time-unit (? #\s))
     (let ((duration (time-quantity-duration p/time-unit)))
       (setf (time-quantity-duration p/time-unit)
	     (if (and p/cardinal (plusp p/cardinal))
		 (multiply-duration duration (1+ p/cardinal))
		 duration))
       p/time-unit)))

;;;_ * A relative point in time

(defmacro relative-time-helper (set-start begin)
  `(let* ((anchor
	   (setf (time-quantity-anchor p/time-reference)
		 (floor-time (now) (time-quantity-resolution
				    p/time-reference))))
	  (rtime (time-quantity-offset p/time-reference))
	  (dowp (eq (time-quantity-resolution p/time-reference)
		    :day-of-week))
	  (start ,set-start))
     (setf (time-quantity-anchor p/time-reference) ,begin
	   (time-quantity-resolvedp p/time-reference) t) 
     p/time-reference))

(defprod p/relative-time (reverse moment end duration)
  (/
   (^ ("this" p/ws p/time-reference)
      (progn
	(format t "this: ~S~%" p/time-reference)
	(relative-time-helper
	 (if dowp
	     (next-time anchor rtime)
	     (previous-time anchor rtime :accept-anchor t))
	 start)))

   (^ ((/ "next" "this next" "the following")
       p/ws p/time-reference)
      (relative-time-helper
       (if dowp
	   (next-time (next-time anchor rtime) rtime)
	   (next-time anchor rtime))
       start))

   (^ ((/ "last" "this last" "this past" "the preceding")
       p/ws p/time-reference)
      (progn
	(format t "saw time-reference ~S~%" p/time-reference)
	(relative-time-helper
	 (if dowp
	     anchor
	     (previous-time anchor rtime :accept-anchor t))
	 (previous-time start rtime))
	(prog1
	    p/time-reference
	  (format t "which is now ~S~%" p/time-reference))))

   (^ ((/ p/cardinal ((? "the" p/ws) p/ordinal))
       p/ws p/time-reference (? #\s))
      (progn
	(setf (time-quantity-multiple p/time-reference)
	      (1+ (or p/cardinal p/ordinal)))
	p/time-reference))

   (^ ("before" p/ws p/time-reference)
      (progn
	(calculate-anchor p/time-reference)
	(setf (time-quantity-terminus p/time-reference)
	      (time-quantity-anchor p/time-reference)
	      (time-quantity-anchor p/time-reference) nil)))

   (^ ("after" p/ws p/time-reference)
      (progn
	(calculate-anchor p/time-reference)
	(setf (time-quantity-anchor p/time-reference)
	      (time-quantity-terminus p/time-reference)
	      (time-quantity-terminus p/time-reference) nil)))

   (^ ((/ "beginning"
	  ("the" p/ws "beginning" p/ws "of")
	  "from"
	  "since"
	  "in"
	  "of")
       p/ws p/time-reference)
      (progn
	(format t "from: ~S~%" p/time-reference)
	p/time-reference))

   (^ ((/ "ending"
	  ("the" p/ws "end" p/ws "of")
	  "to"
	  "until")
       (p/ws p/time-reference))
      (let ((anchor (calculate-anchor p/time-reference)))
	(setf (time-quantity-anchor p/time-reference) nil
	      (time-quantity-terminus p/time-reference) anchor)))

   (^ ("during" p/ws p/time-reference)
      p/time-reference)))

;;;_ * The high-level entry point

(defprod p/time-reference ()
  (/
   (^ p/fixed-time     p/fixed-time)
   (^ p/days-of-week   p/days-of-week)
   (^ p/time-duration  p/time-duration)
   (^ p/relative-time  p/relative-time)))

(defprod p/qualified-time (quantity)
  (^ ((^ p/time-reference (setf quantity p/time-reference))
      (? (+ p/ws
	    (^ p/qualified-time
	       ;; Try to merge the details of this later time specification into
	       ;; the first one (the anchor and duration of which take priority)
	       (setf (time-quantity-offset quantity)
		     (time-quantity-offset p/qualified-time))
	       (unless (time-quantity-anchor quantity)
		 (setf (time-quantity-anchor quantity)
		       (time-quantity-anchor p/qualified-time)))
	       (unless (time-quantity-duration quantity)
		 (setf (time-quantity-duration quantity)
		       (time-quantity-duration p/qualified-time)))
	       (format t "qualified-time: ~S~%" p/qualified-time)))))
     quantity))

(defprod p/time (quantity)
  (^ ((^ p/qualified-time (setf quantity p/qualified-time))
      (? p/ws (^ "ago"
		 (setf (time-quantity-terminus quantity) (now)
		       (time-quantity-reverse quantity) t))))
     quantity))

;;;_ * A recurring period of time

(defprod p/time-period ()
  (/
   ;; monthly [from now until when]
   (^ (p/time-step (? (p/ws p/time-range)))
      (time-period :range p/time-range :step p/time-step :skip nil))

   ;; every 2 weeks for a year ...
   (^ (p/time-step "for" p/time-duration (? (p/ws p/time-range)))
      (time-period :range p/time-range :step p/time-step :skip nil))

   ;; (for) 2 hours weekly ...
   (^ ((? "for") p/time-duration p/time-step (? (p/ws p/time-range)))
      (time-period :range p/time-range :step p/time-step :skip nil))))

(defprod p/time-step ()
  (/
   (^ ("every" p/ws p/time-duration) p/time-duration)
   (^ p/period-unit p/period-unit)))

;;;_ * These are the top level entry points, which return real objects

;;(defparser fixed-time-parser
;;    (^ p/time-range
;;       (calculate-anchor p/time-range)))
;;
;;(defun parse-fixed-time (string)
;;  (multiple-value-bind (ok value) (fixed-time-parser string)
;;    (if ok value nil)))
;;
;;(defparser duration-parser
;;    (^ p/time-duration
;;       (progn
;;	 ;; for side effects...
;;	 (calculate-anchor p/time-duration)
;;	 (time-quantity-duration p/time-duration))))
;;
;;(defun parse-duration (string)
;;  (multiple-value-bind (ok value) (duration-parser string)
;;    (if ok value nil)))
;;
;;(defparser time-range-parser
;;    (^ p/time-range
;;       (progn
;;	 ;; for side effects...
;;	 (calculate-anchor p/time-range)
;;	 (time-range :begin (time-quantity-anchor p/time-range)
;;		     :end (time-quantity-terminus p/time-range)
;;		     :duration (time-quantity-duration p/time-range)))))
;;
;;(defun parse-time-range (string)
;;  (multiple-value-bind (ok value) (time-range-parser string)
;;    (if ok value nil)))
;;
;;(defparser time-period-parser (^ p/time-period))
;;
;;(defun parse-time-period (string)
;;  (multiple-value-bind (ok value) (time-period-parser string)
;;    (if ok value nil)))

(defmacro tdp (production input)
  `((lambda (x)
      (parselet ((foo (^ ,production)))
        (foo x))) ,input))
