(in-package :periods)

(defprod ws () (+ (/ #\Space #\Tab #\Newline)))

(defchartype digit '(satisfies digit-char-p))

(defprod digits () (+ digit))

(defprod number ()
  (^ digits (parse-integer digits)))

(defprod period ()
  (^ (step-by (? (ws skip)) (? (ws range)))
     (time-period :range range :step step-by :skip skip)))

(defprod step-by ()
  (/
   (^ ("every" ws units)      (apply #'duration units))
   (^ "hourly"		      (duration :hours 1))
   (^ "daily"		      (duration :days 1))
   (^ "weekly"		      (duration :days 7))
   (^ "monthly"		      (duration :months 1))
   (^ (/ "yearly" "annually") (duration :years 1))))

(defprod skip ()
  (^ ("every" ws skip-units)
     (apply #'duration skip-units)))

(defprod range (the-start the-end)
  (^ (/
      ("from" ws (@ time-spec (setf the-start time-spec))
	      ws "to"
	      ws (@ time-spec (setf the-end time-spec)))
      (@ ("since" ws time-spec) (setf the-start time-spec the-end (now)))
      (@ ("until" ws time-spec) (setf the-start (now) the-end time-spec)))
     (time-range :begin the-start :end the-end)))

(defprod units ()
  ((^ unit unit)
   (? (? (+ ("," ws (^ unit (append unit units)))) ",")
      (ws "and" ws (^ unit (append unit units))))))

(defprod skip-units ()
  ((^ skip-unit skip-unit)
   (? (? (+ ("," ws (^ skip-unit (append skip-unit skip-units)))) ",")
      (ws "and" ws (^ skip-unit (append skip-unit skip-units))))))

(defprod unit ()
  (^ ((? number ws) basic-unit) (list basic-unit (or number 1))))

(defprod skip-unit ()
  (^ ((? ordinal ws) basic-unit) (list basic-unit (or ordinal 1))))

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

(defprod ordinal ()
  (/ 
   (^ "other" 1)
   (^ "second" 1)
   (^ "third" 2)
   (^ "fourth" 3)
   (^ "fifth" 4)
   (^ "sixth" 5)
   (^ "seventh" 6)
   (^ "eighth" 7)
   (^ "ninth" 8)
   (^ "tenth" 9)))

(defprod time-spec (reverse)
  (/ (^ "now" (now))
     (^ (unit ws (/ (@ "ago" (setf reverse t))
                    (@ "from now" (setf reverse nil))))
        (add-time (now) (apply #'duration unit)
		  :reverse reverse))))

(defparser time-period-parser (^ period))

(defun parse-time-period (string)
  (multiple-value-bind (ok value) (time-period-parser string)
    (if ok value nil)))

(defmacro tdp (production input)
  `((lambda (x)
      (parselet ((foo (^ ,production)))
        (foo x))) ,input))
