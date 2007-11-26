(declaim (optimize (debug 3) (safety 3) (speed 1) (space 0)))

(in-package :periods)

;;;_  + FIXED-TIME parsing

(defun read-integer (in &optional length skip-whitespace-p)
  (parse-integer
   (with-output-to-string (out)
     (loop
	for c = (peek-char nil in nil)
	while (and c (or (digit-char-p c)
			 (and skip-whitespace-p
			      (char= c #\Space)))
		   (or (null length)
		       (>= (decf length) 0)))
	do (write-char (read-char in) out)))))

(defun read-fixed-time (str in)
  (let (year (month 1) (day 1) (hour 0) (minute 0) (second 0))
    (loop
       for c = (read-char in nil)
       for next = (peek-char nil str nil)
       while c
       do
       (if (char= c #\%) ; specifier
	   (progn
	     (setf c (read-char in))
	     (cond
	       ((char= c #\%)
		(if (char= c next)
		    (read-char str)
		    (error "Expected '%', got '~C'" next)))

	       ((char= c #\A))		; full weekday name
	       ((char= c #\a))		; abbreviated weekday name

	       ((char= c #\B))		; full month name
	       ((or (char= c #\b)	; abbreviated month name
		    (char= c #\h)))	; same as %b

	       ((char= c #\C) ; century, zero prefix
		(setf year (* 100 (read-integer str 2))))

	       ;;((char= c #\c))	; national representation of date/time

	       ((char= c #\D) ; equiv: %m/%d/%y
		(let ((date (read-fixed-time (make-string-input-stream "%m/%d/%y")
					   str)))
		  (setf year (nth 5 date)
			month (nth 4 date)
			day (nth 3 date))))

	       ((char= c #\d)
		(setf day (read-integer str 2))
		;; jww (2007-11-12): Check valid
		)
	       ((char= c #\e) ; day of month, space prefix
		(setf day (read-integer str 2 t)))

	       ;;((char= c #\E))	; POSIX locale extensions
	       ;;((char= c #\O))

	       ((char= c #\F) ; equiv: %Y-%m-%d
		(let ((date (read-fixed-time (make-string-input-stream "%Y-%m-%d")
					   str)))
		  (setf year (nth 5 date)
			month (nth 4 date)
			day (nth 3 date))))

	       ((char= c #\G)) ; year as a decimal number with century
	       ((char= c #\g)) ; same as %G, without century

	       ((or (char= c #\H)
		    (char= c #\I)) ; hour on the 12-hour clock
		(setf hour (read-integer str 2))
		(if (> hour 59)
		    (error "Hours exceed maximum range: ~D" hour)))

	       ((or (char= c #\k)	; hour, space prefix
		    (char= c #\l))	; 12-hour hour, space prefix
		(setf hour (read-integer str 2 t)))

	       ((char= c #\j)) ; day of the year as a decimal

	       ((char= c #\M)
		(setf minute (read-integer str 2))
		(if (> minute 59)
		    (error "Minutes exceed maximum range: ~D" minute)))

	       ((char= c #\m)
		(setf month (read-integer str 2))
		;; jww (2007-11-12): Check validity
		(if (or (< month 1)
			(> month 12))
		    (error "Month exceeds possible range: ~D" month)))

	       ((char= c #\p)) ; national AM/PM, as appropriate

	       ((char= c #\R) ; equiv: %H:%M
		(let ((date (read-fixed-time (make-string-input-stream "%H:%M")
					   str)))
		  (setf hour (nth 2 date)
			minute (nth 1 date))))

	       ((char= c #\r) ; equiv: %I:%M:%S %p
		(let ((date (read-fixed-time (make-string-input-stream "%I:%M:%S %p")
					   str)))
		  (setf hour (nth 2 date)
			minute (nth 1 date)
			second (nth 0 date))))

	       ((char= c #\S)
		(setf second (read-integer str 2))
		(if (> second 59)
		    (error "Seconds exceed maximum range: ~D" second)))

	       ((char= c #\s)) ; seconds since Epoch, UTC (unix time)

	       ((char= c #\T) ; equiv: %H:%M:%S
		(let ((date (read-fixed-time (make-string-input-stream "%H:%M:%S")
					   str)))
		  (setf hour (nth 2 date)
			minute (nth 1 date)
			second (nth 0 date))))

	       ((char= c #\t) ; tab
		(unless (char= #\Tab (read-char str))
		  (error "Expected a tab character, got '~C'" next)))

	       ((char= c #\U))		; week number of the year (Sun) 00-53
	       ((char= c #\u))		; weekday as a decimal (Mon) 1-7
	       ((char= c #\V))		; week of the year 1-53 (*)

	       ((char= c #\v) ; equiv: %e-%b-%Y
		(let ((date (read-fixed-time (make-string-input-stream "%e-%b-%Y")
					   str)))
		  (setf year (nth 5 date)
			month (nth 4 date)
			day (nth 3 date))))

	       ((char= c #\W))		; week number of the year (Mon) 00-53
	       ((char= c #\w))		; weekday as a decimal (Sun) 0-6
	       ;;((char= c #\X))	; national representation of the time
	       ;;((char= c #\x))	; national representation of the date

	       ((char= c #\Y)
		(setf year (read-integer str 4)))

	       ((char= c #\y)
		(setf year (read-integer str 2))
		(if (< year 70)
		    (incf year 2000)
		    (incf year 1900)))

	       ((char= c #\Z))		; time zone name
	       ((char= c #\z))		; time zone offset from UTC
	       ;;((char= c #\+))	; national representation of date/time

	       ((char= c #\|) ; abort if string is ended
		(if (null next)
		    (return)))))

	   (if (char= c next)
	       (read-char str)
	       (error "Expected '~C', got '~C'" c next))))
    (list 0 second minute hour day month year)))

(defun strptime-decoded (string &key (format *input-time-format*))
  (with-input-from-string (in format)
    (with-input-from-string (str string)
      (read-fixed-time str in))))

(defun strptime (string &key (format *input-time-format*)
		 (default-year nil))
  (let ((decoded (strptime-decoded string :format format)))
    (unless (nth 6 decoded)
      (setf (nth 6 decoded) (or default-year (current-year))))
    (apply #'encode-local-time decoded)))

(defun strftime (fixed-time &key (format *output-time-format*))
  (declare (type fixed-time fixed-time))
  (declare (type string format))
  (multiple-value-bind
	(millisecond second minute hour day month year day-of-week
		     daylight-p time-zone time-zone-abbrev)
      (local-time:decode-local-time fixed-time)
    (declare (ignore millisecond))
    (declare (ignorable day-of-week))
    (declare (ignorable daylight-p))
    (with-output-to-string (out)
      (with-input-from-string (in format)
	(loop
	   for c = (read-char in nil)
	   while c
	   do
	   (if (char= c #\%)		; specifier
	       (progn
		 (setf c (read-char in))
		 (cond
		   ((char= c #\%)
		    (write-char #\% out))

		   ((char= c #\A))	; full weekday name
		   ((char= c #\a))	; abbreviated weekday name

		   ((char= c #\B))	; full month name
		   ((or (char= c #\b)	; abbreviated month name
			(char= c #\h)))	; same as %b

		   ((char= c #\C)	; century, zero prefix
		    (format out "~2,'0D" (floor year 100)))

		   ;;((char= c #\c))	; national representation of date/time

		   ((char= c #\D)	; equiv: %m/%d/%y
		    (princ (strftime fixed-time :format "%m/%d/%y") out))

		   ((char= c #\d)
		    (format out "~2,'0D" day))
		   ((char= c #\e)	; day of month, space prefix
		    (format out "~2,' D" day))

		   ;;((char= c #\E))	; POSIX locale extensions
		   ;;((char= c #\O))

		   ((char= c #\F)	; equiv: %Y-%m-%d
		    (princ (strftime fixed-time :format "%Y-%m-%d") out))
					;
		   ((char= c #\G))     ; year as a decimal number with century
		   ((char= c #\g))	; same as %G, without century

		   ((char= c #\H)	; hour, zero prefix
		    (format out "~2,'0D" hour))
		   ((char= c #\I)	; hour on the 12-hour clock
		    (if (> hour 12)
			(format out "~2,'0D" (- hour 12))
			(if (= hour 0)
			    (format out "~2,'0D" 12)
			    (format out "~2,'0D" hour))))

		   ((char= c #\k)	; hour, space prefix
		    (format out "~2,' D" hour))
		   ((char= c #\l)	; 12-hour hour, space prefix
		    (if (> hour 12)
			(format out "~2,' D" (- hour 12))
			(if (= hour 0)
			    (format out "~2,' D" 12)
			    (format out "~2,' D" hour))))

		   ((char= c #\j))	; day of the year as a decimal

		   ((char= c #\M)
		    (format out "~2,'0D" minute))

		   ((char= c #\m)
		    (format out "~2,'0D" month))

		   ((char= c #\p))	; national AM/PM, as appropriate

		   ((char= c #\R)	; equiv: %H:%M
		    (princ (strftime fixed-time :format "%H:%M") out))

		   ((char= c #\r)	; equiv: %I:%M:%S %p
		    (princ (strftime fixed-time :format "%I:%M:%S %p") out))

		   ((char= c #\S)
		    (format out "~2,'0D" second))

		   ((char= c #\s)	; seconds since Epoch, UTC (unix time)
		    (format out "~D" (local-time:unix-time fixed-time)))

		   ((char= c #\T)	; equiv: %H:%M:%S
		    (princ (strftime fixed-time :format "%H:%M:%S") out))

		   ((char= c #\t)	; tab
		    (write-char #\Tab out))

		   ((char= c #\U))	; week number of the year (Sun) 00-53
		   ((char= c #\u))	; weekday as a decimal (Mon) 1-7
		   ((char= c #\V))	; week of the year 1-53 (*)

		   ((char= c #\v)	; equiv: %e-%b-%Y
		    (princ (strftime fixed-time :format "%e-%b-%Y") out))

		   ((char= c #\W))	; week number of the year (Mon) 00-53
		   ((char= c #\w))	; weekday as a decimal (Sun) 0-6
		   ;;((char= c #\X))	; national representation of the time
		   ;;((char= c #\x))	; national representation of the date

		   ((char= c #\Y)
		    (format out "~4,'0D" year))
		   ((char= c #\y)
		    (format out "~4,'0D" (floor year 100)))

		   ((char= c #\Z)	; time zone name
		    (format out "~A" time-zone-abbrev))
		   ((char= c #\z)	; time zone offset from UTC
		    (format out "~D" time-zone))
		   ;;((char= c #\+))	; national representation of date/time

		   ((char= c #\|)	; abort if string is ended
		    (if (and (zerop (sec-of fixed-time))
			     (zerop (nsec-of fixed-time)))
			(return)))))

	       (write-char c out)))))))

(provide 'strptime)

;; strptime.lisp ends here
