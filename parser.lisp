(in-package :periods)

(defprod ws () (+ (/ #\Space #\Tab #\Newline)))

(defchartype digit '(satisfies digit-char-p))

(defprod digits () (+ digit))

(defprod number ()
  (^ digits (parse-integer digits)))

(defprod period ()
  (^ (step-by ws (? (skip ws)) (? time-pair))
     (append step-by skip time-pair)))

(defprod step-by ()
  (/
   (^ ("every" ws units)      `(:step-by ,units))
   (^ "hourly"		      '(:step-by (:hours 1)))
   (^ "daily"		      '(:step-by (:days 1)))
   (^ "weekly"		      '(:step-by (:weeks 1)))
   (^ "monthly"		      '(:step-by (:months 1)))
   (^ (/ "yearly" "annually") '(:step-by (:years 1)))))

(defprod skip ()
  (^ ("every" ws skip-units)
     `(:skip ,skip-units)))

(defprod time-pair (the-start the-end)
  (^ (/
      ("from" ws (@ time-spec (setf the-start time-spec))
	      ws "to"
	      ws (@ time-spec (setf the-end time-spec)))
      (@ ("since" ws time-spec) (setf the-start time-spec the-end :now))
      (@ ("until" ws time-spec) (setf the-start :now the-end time-spec)))
     (list :from the-start :to the-end)))

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
	  "week"
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

(defprod time-spec (moment)
  (/ (^ "now" :now)
     (^ (unit ws (/ (@ "ago" (setf moment :ago))
                    (@ "from now" (setf moment :hence))))
        `(,@unit ,moment))))

(defparser time-period (^ period))

(defun parse-time-period (string)
  (multiple-value-bind (ok value) (time-period string)
    (if ok value nil)))

(defmacro tdp (production input)
  `((lambda (x)
      (parselet ((foo (^ ,production)))
        (foo x))) ,input))
