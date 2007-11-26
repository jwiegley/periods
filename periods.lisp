;;; periods --- A library for working with periods of time

;; Copyright (C) 2007 John Wiegley.  All rights reserved.

;; Author: John Wiegley <johnw@newartisans.com>
;; Created: 29 Oct 2007
;; Modified: 17 Nov 2007
;; Version: 0.2
;; Keywords: lisp programming development
;; X-URL: http://www.newartisans.com/

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;; 
;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.
;; 
;; - Neither the name of New Artisans LLC nor the names of its
;;   contributors may be used to endorse or promote products derived from
;;   this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; The PERIODS library is fully described in the PDF documentation which
;; accompanies this source code.  Please refer there for complete details.

(declaim (optimize (debug 3) (safety 3) (speed 1) (space 0)))

(defpackage :periods
  (:use :common-lisp :local-time
	#+:periods-use-series :series
	#+:periods-use-parser :com.gigamonkeys.parser)
  (:nicknames :time-periods)
  (:export leapp
	   days-in-month
	   increment-time
	   decrement-time
	   floor-time
	   parse-time-period
	   time-period-generator
	   time-periods
	   map-over-time
	   do-over-time
	   collate-by-time-period
	   sleep-until

	   *input-time-format*
	   *output-time-format*

	   fixed-time
	   read-fixed-time
	   strptime
	   strptime-decoded
	   strftime)
  (:shadow day-of))

(in-package :periods)

#+:periods-use-series (series::install)

;;;_ * Global variables

(defvar *input-time-format* "%Y/%m/%d%| %H:%M:%S")
(defvar *output-time-format* *input-time-format*)

;;;_ * Basic utility functions

(defparameter *days-in-months* #(31 28 31 30 31 30 31 31 30 31 30 31))

;; Snippet courtesy of Xach on #lisp
(declaim (inline leapp))
(defun leapp (year)
  (cond ((zerop (mod year 400)) t)
        ((zerop (mod year 100)) nil)
        ((zerop (mod year 4)) t)
        (t nil)))

(declaim (inline current-year))
(defun current-year ()
 (nth-value 5 (get-decoded-time)))

(defun days-in-month (month &optional year)
  (let ((days (aref *days-in-months* (1- month)))
	(the-year (or year (current-year))))
    (if (and (= month 2)
	     (leapp the-year))
	(incf days)
	days)))

(declaim (inline nanosecond-part microsecond-part millisecond-part))

(defun nanosecond-part (bignum)
  (nth-value 1 (floor bignum 1000)))
(defun microsecond-part (bignum)
  (nth-value 1 (floor (floor bignum 1000) 1000)))
(defun millisecond-part (bignum)
  (nth-value 0 (floor bignum 1000000)))

(declaim (inline set-nanosecond-part set-microsecond-part
		 set-millisecond-part))

(defun set-nanosecond-part (bignum nsecs)
  (+ (* (floor bignum 1000) 1000) nsecs))
(defun set-microsecond-part (bignum usecs)
  (+ (* (floor bignum 1000000) 1000000)
     (* usecs 1000)
     (nth-value 1 (floor bignum 1000))))
(defun set-millisecond-part (bignum msecs)
  (+ (* msecs 1000000)
     (nth-value 1 (floor bignum 1000000))))

(defun floor-time (fixed-time &optional resolution)
  "Reduce a fixed time to be no finer than RESOLUTION.

  For example, if the date is 2007-04-20, and the resolution is :month, the
  date is floored to 2007-04-01.  Anything smaller than the resolution is
  reduced to zero (or 1, if it is a day or month being reduced)."
  (declare (type local-time fixed-time))
  (multiple-value-bind
	(nsec ss mm hh day month year)
      (decode-local-time fixed-time)
    (block nil
      (if (eq resolution :nanosecond) (return))
      (setf nsec (set-nanosecond-part nsec 0))
      (if (eq resolution :microsecond) (return))
      (setf nsec (set-microsecond-part nsec 0))
      (if (eq resolution :millisecond) (return))
      (setf nsec (set-millisecond-part nsec 0))
      (if (eq resolution :second) (return))
      (setf ss 0)
      (if (eq resolution :minute) (return))
      (setf mm 0)
      (if (eq resolution :hour) (return))
      (setf hh 0)
      (if (or (eq resolution :day)
	      (eq resolution :day-of-week)) (return))
      (setf day 1)
      (if (eq resolution :month) (return))
      (setf month 1))
    (encode-local-time nsec ss mm hh day month year)))

(defun find-smallest-resolution (step-by)
  (cond
    ((member :nanosecond step-by) :nanosecond)
    ((member :microsecond step-by) :microsecond)
    ((member :millisecond step-by) :millisecond)
    ((member :second step-by) :second)
    ((member :minute step-by) :minute)
    ((member :hour step-by) :hour)
    ((member :day step-by) :day)
    ((member :day-of-week step-by) :day-of-week)
    ((member :month step-by) :month)
    (t (error "Could not find a time resolution in ~S" step-by))))

;;;_ * FIXED-TIME

(deftype fixed-time ()
  'local-time)

(defun fixed-time (&rest args)
  "Return a fixed point in time relative to now.

  If the keyword argument :NOW is given, all else is ignored; this is
  equivalent to calling `LOCAL-TIME:NOW'.

  Otherwise, any keyword arguments given override their corresponding elements
  in the current time.  Further, any elements smaller in resolution than the
  finest specified element are reduced to 0 or 1, according to their position.

  For example, assuming the current time is @2007-11-17T23:02:00.000, compare
  these outputs:

    (fixed-time :month 4) => @2007-04-01T00:00:00.000
    (fixed-time :day 10)  => @2007-11-10T00:00:00.000
    (fixed-time :hour 15) => @2007-11-17T15:00:00.000

  This behavior makes it very easy to return a fixed time for \"april of this
  year\", etc.  If you wish to determine the date of the previous April, while
  preserving the current day of the month, hour of the day, etc., then see the
  function `PREVIOUS-TIME'."
  (if (member :now args)
      (local-time:now)
      (multiple-value-bind
	    (nsec ss mm hh day month year)
	  (decode-local-time (local-time:now))
	(block nil
	  (let ((this-nsec (getf args :nanosecond))
		(this-usec (getf args :microsecond))
		(this-ms (getf args :millisecond))
		(this-ss (getf args :second))
		(this-mm (getf args :minute))
		(this-hh (getf args :hour))
		(this-day (getf args :day))
		(this-month (getf args :month))
		(this-year (getf args :year))
		(truncate t))
	    (if this-nsec
		(setf nsec (set-nanosecond-part nsec this-nsec)
		      truncate nil)
		(if truncate (setf nsec (set-nanosecond-part nsec 0))))
	    (if this-usec
		(setf nsec (set-microsecond-part nsec this-usec)
		      truncate nil)
		(if truncate (setf nsec (set-microsecond-part nsec 0))))
	    (if this-ms
		(setf nsec (set-millisecond-part nsec this-ms)
		      truncate nil)
		(if truncate (setf nsec (set-millisecond-part nsec 0))))
	    (if this-ss
		(setf ss this-ss truncate nil)
		(if truncate (setf ss 0)))
	    (if this-mm
		(setf mm this-mm truncate nil)
		(if truncate (setf mm 0)))
	    (if this-hh
		(setf hh this-hh truncate nil)
		(if truncate (setf hh 0)))
	    (if this-day
		(setf day this-day truncate nil)
		(if truncate (setf day 1)))
	    (if this-month
		(setf month this-month truncate nil)
		(if truncate (setf month 1)))
	    (if this-year
		(setf year this-year))))
	(encode-local-time nsec ss mm hh day month year))))

(declaim (inline year-of
		 month-of
		 day-of
		 hour-of
		 minute-of
		 second-of
		 millisecond-of
		 microsecond-of
		 nanosecond-of))

(defun year-of (fixed-time)
  (nth-value 6 (decode-local-time fixed-time)))
(defun month-of (fixed-time)
  (nth-value 5 (decode-local-time fixed-time)))
(defun day-of (fixed-time)
  (nth-value 4 (decode-local-time fixed-time)))
(defun hour-of (fixed-time)
  (nth-value 3 (decode-local-time fixed-time)))
(defun minute-of (fixed-time)
  (nth-value 2 (decode-local-time fixed-time)))
(defun second-of (fixed-time)
  (nth-value 1 (decode-local-time fixed-time)))
(defun millisecond-of (fixed-time)
  (millisecond-part (nth-value 0 (decode-local-time fixed-time))))
(defun microsecond-of (fixed-time)
  (microsecond-part (nth-value 0 (decode-local-time fixed-time))))
(defun nanosecond-of (fixed-time)
  (nanosecond-part (nth-value 0 (decode-local-time fixed-time))))

(declaim (inline day-of-week))
(defun day-of-week (fixed-time)
  (declare (type fixed-time fixed-time))
  (nth-value 7 (decode-local-time fixed-time)))

(declaim (inline falls-on-weekend-p))
(defun falls-on-weekend-p (fixed-time)
  (let ((dow (day-of-week fixed-time)))
    (or (= 0 dow) (= 6 dow))))

;;;_ * RELATIVE-DURATION

(defstruct duration
  (years 0 :type integer)
  (months 0 :type integer)
  (days 0 :type integer)
  (hours 0 :type integer)
  (minutes 0 :type integer)
  (seconds 0 :type integer)
  (milliseconds 0 :type integer)
  (microseconds 0 :type integer)
  (nanoseconds 0 :type integer))

(declaim (inline duration))
(defun duration (&rest args)
  "Create a DURATION object.

  One thing to note about duration: there is no way to determine the total
  length of a duration in terms of any specific time quantity, without first
  binding that duration to a fixed point in time (after all, how many days are
  in a month if you don't know which month it is?)  Therefore, those looking
  for a function like \"duration-seconds\" are really wanting to work with
  ranges, not just durations."
  (apply #'make-duration args))

(defmacro with-skippers (&body body)
  `(labels
       ((skip-year (skip)
	  (incf year skip))

	(skip-month (skip)
	  (if (minusp skip)
	      (let ((remainder (+ (1- month) skip)))
		(if (minusp remainder)
		    (progn
		      (skip-year -1)
		      (setf month 12)
		      (skip-month (1+ remainder)))
		    (incf month skip)))
	      (if (plusp skip)
		  (let ((remainder (- (+ month skip) 12)))
		    (if (plusp remainder)
			(progn
			  (skip-year 1)
			  (setf month 1)
			  (skip-month (1- remainder)))
			(incf month skip))))))
	
	(skip-day (skip)
	  (if (minusp skip)
	      (let ((remainder (+ (1- day) skip)))
		(if (minusp remainder)
		    (progn
		      (skip-month -1)
		      (setf day (days-in-month month year))
		      (skip-day (1+ remainder)))
		    (incf day skip)))
	      (if (plusp skip)
		  (let ((remainder (- (+ day skip)
				      (days-in-month month year))))
		    (if (plusp remainder)
			(progn
			  (skip-month 1)
			  (setf day 1)
			  (skip-day (1- remainder)))
			(incf day skip))))))
	
	(skip-hour (skip)
	  (if (minusp skip)
	      (let ((remainder (+ hh skip)))
		(if (minusp remainder)
		    (progn
		      (skip-day -1)
		      (setf hh 59)
		      (skip-hour (1+ remainder)))
		    (incf hh skip)))
	      (if (plusp skip)
		  (let ((remainder (- (+ hh skip) 23)))
		    (if (plusp remainder)
			(progn
			  (skip-day 1)
			  (setf hh 0)
			  (skip-hour (1- remainder)))
			(incf hh skip))))))
	
	(skip-minute (skip)
	  (if (minusp skip)
	      (let ((remainder (+ mm skip)))
		(if (minusp remainder)
		    (progn
		      (skip-hour -1)
		      (setf mm 59)
		      (skip-minute (1+ remainder)))
		    (incf mm skip)))
	      (if (plusp skip)
		  (let ((remainder (- (+ mm skip) 59)))
		    (if (plusp remainder)
			(progn
			  (skip-hour 1)
			  (setf mm 0)
			  (skip-minute (1- remainder)))
			(incf mm skip))))))
	
	(skip-second (skip)
	  (if (minusp skip)
	      (let ((remainder (+ ss skip)))
		(if (minusp remainder)
		    (progn
		      (skip-minute -1)
		      (setf ss 59)
		      (skip-second (1+ remainder)))
		    (incf ss skip)))
	      (if (plusp skip)
		  (let ((remainder (- (+ ss skip) 59)))
		    (if (plusp remainder)
			(progn
			  (skip-minute 1)
			  (setf ss 0)
			  (skip-second (1- remainder)))
			(incf ss skip))))))
	
	(skip-millisecond (skip)
	  (if (minusp skip)
	      (let ((remainder (+ (millisecond-part nsec) skip)))
		(if (minusp remainder)
		    (progn
		      (skip-second -1)
		      (setf nsec (set-millisecond-part nsec 999))
		      (skip-millisecond (1+ remainder)))
		    (setf nsec (set-millisecond-part
				nsec (+ (millisecond-part nsec) skip)))))
	      (if (plusp skip)
		  (let ((remainder (- (+ (millisecond-part nsec) skip) 999)))
		    (if (plusp remainder)
			(progn
			  (skip-second 1)
			  (setf nsec (set-millisecond-part nsec 0))
			  (skip-millisecond (1- remainder)))
			(setf nsec (set-millisecond-part
				    nsec (+ (millisecond-part nsec)
					    skip))))))))

	(skip-microsecond (skip)
	  (if (minusp skip)
	      (let ((remainder (+ (microsecond-part nsec) skip)))
		(if (minusp remainder)
		    (progn
		      (skip-second -1)
		      (setf nsec (set-microsecond-part nsec 999))
		      (skip-microsecond (1+ remainder)))
		    (setf nsec (set-microsecond-part
				nsec (+ (microsecond-part nsec) skip)))))
	      (if (plusp skip)
		  (let ((remainder (- (+ (microsecond-part nsec) skip) 999)))
		    (if (plusp remainder)
			(progn
			  (skip-second 1)
			  (setf nsec (set-microsecond-part nsec 0))
			  (skip-microsecond (1- remainder)))
			(setf nsec (set-microsecond-part
				    nsec (+ (microsecond-part nsec)
					    skip))))))))

	(skip-nanosecond (skip)
	  (if (minusp skip)
	      (let ((remainder (+ (nanosecond-part nsec) skip)))
		(if (minusp remainder)
		    (progn
		      (skip-second -1)
		      (setf nsec (set-nanosecond-part nsec 999))
		      (skip-nanosecond (1+ remainder)))
		    (setf nsec (set-nanosecond-part
				nsec (+ (nanosecond-part nsec) skip)))))
	      (if (plusp skip)
		  (let ((remainder (- (+ (nanosecond-part nsec) skip) 999)))
		    (if (plusp remainder)
			(progn
			  (skip-second 1)
			  (setf nsec (set-nanosecond-part nsec 0))
			  (skip-nanosecond (1- remainder)))
			(setf nsec (set-nanosecond-part
				    nsec (+ (nanosecond-part nsec)
					    skip)))))))))
     ,@body))

(defun add-time (fixed-time duration &key (reverse nil))
  "Given a FIXED-TIME, add the supplied DURATION.

  Example (reader notation requires calling `LOCAL-TIME:ENABLE-READ-MACROS'):

    (add-time @2007-05-20T12:10:10.000 (duration :hours 50))
      => @2007-05-22T14:10:10.000

  NOTE: This function always adds the largest increments first, so if the
  duration is (duration :years 1 :days 20), and the current day is
  @2003-01-09, the result will be @2004-02-29 -- not @2004-03-01, as it would
  be, if days were added before years."
  (declare (type fixed-time fixed-time))
  (declare (type duration duration))
  (declare (type boolean reverse))
  (if (and (zerop (duration-years duration))
	   (zerop (duration-months duration))
	   (zerop (duration-days duration))
	   (zerop (duration-hours duration))
	   (zerop (duration-minutes duration)))
      (multiple-value-bind (quotient remainder)
	  (floor (funcall (if reverse #'- #'+)
			  (+ (* (unix-time fixed-time) 1000000000)
			     (nsec-of fixed-time))
			  (+ (* (duration-seconds duration) 1000000000)
			     (* (duration-milliseconds duration) 1000000)
			     (* (duration-microseconds duration) 1000)
			     (duration-nanoseconds duration)))
		 1000000000)
	(local-time :unix quotient :nsec remainder))
      (multiple-value-bind
	    (nsec ss mm hh day month year)
	  (decode-local-time fixed-time)
	(let ((identity (if reverse -1 1)))
	  (with-skippers
	    (if (duration-years duration)
		(skip-year (* identity (duration-years duration))))
	    (if (duration-months duration)
		(skip-month (* identity (duration-months duration))))
	    (if (duration-days duration)
		(skip-day (* identity (duration-days duration))))
	    (if (duration-hours duration)
		(skip-hour (* identity (duration-hours duration))))
	    (if (duration-minutes duration)
		(skip-minute (* identity (duration-minutes duration))))
	    (if (duration-seconds duration)
		(skip-second (* identity (duration-seconds duration))))
	    (if (duration-milliseconds duration)
		(skip-millisecond (* identity (duration-milliseconds duration))))
	    (if (duration-microseconds duration)
		(skip-microsecond (* identity (duration-microseconds duration))))
	    (if (duration-nanoseconds duration)
		(skip-nanosecond (* identity (duration-nanoseconds duration))))))
	(encode-local-time nsec ss mm hh day month year))))

(declaim (inline subtract-time))
(defun subtract-time (fixed-time duration)
  (add-time fixed-time duration :reverse t))

(defun bounded-add (left right bound)
  "A bounded addition operator.  Returns: VALUE CARRY."
  (assert (< left bound))
  (multiple-value-bind (quotient remainder)
      (floor right bound)
    (let ((sum (+ left remainder)))
      (if (< sum bound)
	  (values sum quotient)
	  (values (- (+ left remainder) bound)
		  (+ 1 quotient))))))

(defun bounded-subtract (left right bound)
  "A bounded subtraction operator.  Returns: VALUE CARRY."
  (assert (< left bound))
  (multiple-value-bind (quotient remainder)
      (floor right bound)
    (if (>= left remainder)
	(values (- left remainder) quotient)
	(values (+ left (- bound remainder))
		(+ 1 quotient)))))

(declaim (inline add-years subtract-years))
(defun add-years (duration years)
  (incf (duration-years duration) years)
  duration)
(defun subtract-years (duration years)
  (decf (duration-years duration) years)
  duration)

(declaim (inline add-months subtract-months))
(defun add-months (duration months)
  (incf (duration-months duration) months)
  duration)
(defun subtract-months (duration months)
  (decf (duration-months duration) months)
  duration)

(declaim (inline add-days subtract-days))
(defun add-days (duration days)
  (incf (duration-days duration) days)
  duration)
(defun subtract-days (duration days)
  (decf (duration-days duration) days)
  duration)

(declaim (inline add-hours subtract-hours))
(defun add-hours (duration hours)
  (incf (duration-hours duration) hours)
  duration)
(defun subtract-hours (duration hours)
  (decf (duration-hours duration) hours)
  duration)

(declaim (inline add-minutes subtract-minutes))
(defun add-minutes (duration minutes)
  (incf (duration-minutes duration) minutes)
  duration)
(defun subtract-minutes (duration minutes)
  (decf (duration-minutes duration) minutes)
  duration)

(declaim (inline add-seconds subtract-seconds))
(defun add-seconds (duration seconds)
  (incf (duration-seconds duration) seconds)
  duration)
(defun subtract-seconds (duration seconds)
  (decf (duration-seconds duration) seconds)
  duration)

(declaim (inline add-milliseconds subtract-milliseconds))
(defun add-milliseconds (duration milliseconds)
  (incf (duration-milliseconds duration) milliseconds)
  duration)
(defun subtract-milliseconds (duration milliseconds)
  (decf (duration-milliseconds duration) milliseconds)
  duration)

(declaim (inline add-microseconds subtract-microseconds))
(defun add-microseconds (duration microseconds)
  (incf (duration-microseconds duration) microseconds)
  duration)
(defun subtract-microseconds (duration microseconds)
  (decf (duration-microseconds duration) microseconds)
  duration)

(declaim (inline add-nanoseconds subtract-nanoseconds))
(defun add-nanoseconds (duration nanoseconds)
  (incf (duration-nanoseconds duration) nanoseconds)
  duration)
(defun subtract-nanoseconds (duration nanoseconds)
  (decf (duration-nanoseconds duration) nanoseconds)
  duration)

(defun time-difference (left right)
  "Compute the duration existing between fixed-times LEFT and RIGHT.

  The order of left or right is ignored; the returned DURATION, if added to
  the earlier value, will result in the later.

  A complexity of this process which might surprise some is that larger
  quantities are added by `ADD-TIME' before smaller quantities.  For example,
  what is the difference between 2003-02-10 and 2004-03-01?  If you add years
  before days, the difference is 1 year and 20 days.  If you were to add days
  before years, however, the difference would be 1 year and 21 days.  The
  question, do you advance to 2004 and then calculate between 2-10 and 3-01,
  or do you move from 2-10 to 3-01, and then increment the year?  The PERIODS
  library chooses to add years before days, since this follows human reckoning
  a bit closer (i.e., a person would likely flip to the 2004 calendar and then
  start counting off days, rather than the other way around).  This difference
  in reckoning can be tricky, however, so bear this in mind.a"
  (if (local-time< left right)
      (rotatef left right))
  (let ((nsec (- (nsec-of left) (nsec-of right)))
	(sec (- (universal-time left) (universal-time right))))
    (if (minusp nsec)
	(decf sec))
    (duration :seconds sec :nanoseconds nsec)))

(declaim (inline add-duration))
(defun add-duration (left right)
  "Add one duration to another."
  (duration :years (+ (duration-years left)
		      (duration-years right))
	    :months (+ (duration-months left)
		       (duration-months right))
	    :days (+ (duration-days left)
		     (duration-days right))
	    :hours (+ (duration-hours left)
		      (duration-hours right))
	    :minutes (+ (duration-minutes left)
			(duration-minutes right))
	    :seconds (+ (duration-seconds left)
			(duration-seconds right))
	    :milliseconds (+ (duration-milliseconds left)
			     (duration-milliseconds right))
	    :microseconds (+ (duration-microseconds left)
			     (duration-microseconds right))
	    :nanoseconds (+ (duration-nanoseconds left)
			    (duration-nanoseconds right))))

(declaim (inline subtract-duration))
(defun subtract-duration (left right)
  "Subtract one duration from another."
  (duration :years (- (duration-years left)
		      (duration-years right))
	    :months (- (duration-months left)
		       (duration-months right))
	    :days (- (duration-days left)
		     (duration-days right))
	    :hours (- (duration-hours left)
		      (duration-hours right))
	    :minutes (- (duration-minutes left)
			(duration-minutes right))
	    :seconds (- (duration-seconds left)
			(duration-seconds right))
	    :milliseconds (- (duration-milliseconds left)
			     (duration-milliseconds right))
	    :microseconds (- (duration-microseconds left)
			     (duration-microseconds right))
	    :nanoseconds (- (duration-nanoseconds left)
			    (duration-nanoseconds right))))

(declaim (inline multiply-duration))
(defun multiply-duration (left multiplier)
  "Add one duration to another."
  (duration :years (* (duration-years left) multiplier)
	    :months (* (duration-months left) multiplier)
	    :days (* (duration-days left) multiplier)
	    :hours (* (duration-hours left) multiplier)
	    :minutes (* (duration-minutes left) multiplier)
	    :seconds (* (duration-seconds left) multiplier)
	    :milliseconds (* (duration-milliseconds left) multiplier)
	    :microseconds (* (duration-microseconds left) multiplier)
	    :nanoseconds (* (duration-nanoseconds left) multiplier)))

(declaim (inline time-stepper))
(defun time-stepper (duration &key (reverse nil))
  (declare (type duration duration))
  (declare (type boolean reverse))
  (lambda (time)
    (add-time time duration :reverse reverse)))

(declaim (inline time-generator))
(defun time-generator (start duration &key (reverse nil))
  (declare (type fixed-time start))
  (declare (type duration duration))
  (declare (type boolean reverse))
  (let (next)
    (lambda ()
      (setf next (add-time (or next start) duration
			   :reverse reverse)))))

#+periods-use-series
(defmacro scan-times (start duration &key (reverse nil))
  "This macro represents continguous time durations as a SERIES.

  Example:

    (subseries (scan-times @2007-11-01 (duration :months 1)) 0 10)

  `UNTIL-IF' can be used to bound the end of the range by a date:

    (collect (until-if #'(lambda (time)
                           (local-time:local-time>= time @2009-01-01))
                       (scan-times @2007-11-01 (duration :months 1))))"
  `(map-fn 'fixed-time (time-generator ,start ,duration :reverse ,reverse)))

(defmacro loop-times (forms start duration end
		      &key (reverse nil) (inclusive-p nil))
  "Map over a set of times separated by DURATION, calling CALLABLE with the
  start of each."
  (let ((generator-sym (gensym))
	(start-sym (gensym))
	(end-sym (gensym)))
    `(let ((,start-sym ,start)
	   (,end-sym ,end))
       (assert (,(if reverse
		     'local-time>
		     'local-time<) ,start-sym ,end-sym))
       (loop
	  with ,generator-sym = (time-generator ,start-sym ,duration)
	  for value = (funcall ,generator-sym)
	  while ,(if reverse
		     (if inclusive-p
			 `(local-time>= value ,end-sym)
			 `(local-time> value ,end-sym))
		     (if inclusive-p
			 `(local-time<= value ,end-sym)
			 `(local-time< value ,end-sym)))
	  ,@forms))))

(defmacro map-times (callable start duration end
		     &key (reverse nil) (inclusive-p nil))
  "Map over a set of times separated by DURATION, calling CALLABLE with the
  start of each."
  `(loop-times (do (funcall ,callable value))
      ,start ,duration ,end :reverse ,reverse
      :inclusive-p ,inclusive-p))

(defmacro list-times (start duration end
		      &key (reverse nil) (inclusive-p nil))
  "Return a list of all times within the given range."
  `(loop-times (collect value)
      ,start ,duration ,end :reverse ,reverse
      :inclusive-p ,inclusive-p))

(defmacro do-times ((var start duration end &optional (result nil))
		    &body body)
  "A 'do' style version of the functional `MAP-TIMES' macro.

  The disadvantage to `DO-TIMES' is that there is no way to ask for a reversed
  time sequence, or specify an inclusive endpoint."
  `(block nil
     (map-times #'(lambda (,var) ,@body) ,start ,duration ,end)
     ,result))

;;;_ * RELATIVE-TIME

(defstruct relative-time
  (year nil :type (or keyword integer null))
  (month nil :type (or keyword integer null))
  (week nil :type (or keyword integer null))
  (day-of-week nil :type (or keyword integer null))
  (day nil :type (or keyword integer null))
  (hour nil :type (or keyword integer null))
  (minute nil :type (or keyword integer null))
  (second nil :type (or keyword integer null))
  (millisecond nil :type (or keyword integer null))
  (microsecond nil :type (or keyword integer null))
  (nanosecond nil :type (or keyword integer null)))

(declaim (inline relative-time))
(defun relative-time (&rest args)
  (apply #'make-relative-time args))

(declaim (inline range-dec))
(defun range-dec (value min max)
  (if (= value min)
      max
      (1- value)))

(declaim (inline range-inc))
(defun range-inc (value min max)
  (if (= value max)
      min
      (1+ value)))

(defun enclosing-duration (relative-time)
  "Return a DURATION which, if applied to a time, causes `NEXT-TIME' to move
  to the next matching occurrence of that time pattern.

  For example, if you ask for ':day 18' on Nov 18, it will return the same
  time back to you.  If you add enclosing duration for that relative time to
  Nov 18 and then ask again, you'll get Dec 18."
  (cond
    ((relative-time-month relative-time)
     (duration :months 1))
    ((relative-time-day relative-time)
     (duration :days 1))
    ((relative-time-hour relative-time)
     (duration :hours 1))
    ((relative-time-minute relative-time)
     (duration :minutes 1))
    ((relative-time-second relative-time)
     (duration :seconds 1))
    ((relative-time-millisecond relative-time)
     (duration :milliseconds 1))
    ((relative-time-microsecond relative-time)
     (duration :microseconds 1))
    ((relative-time-nanosecond relative-time)
     (duration :nanoseconds 1))
    ((relative-time-day-of-week relative-time)
     (duration :days 1))
    (t
     (error "`enclosing-duration' has failed."))))

(defun details-match-relative-time-p (relative-time nsec ss mm hh day month year
				      day-of-week daylight-p
				      timezone tz-abbrev)
  (declare (ignore daylight-p))
  (declare (ignore timezone))
  (declare (ignore tz-abbrev))
  "Return T if the given time elements honor the details in RELATIVE-TIME."
  (and (or (not (relative-time-nanosecond relative-time))
	   (= (nanosecond-part nsec)
	      (relative-time-nanosecond relative-time)))
       (or (not (relative-time-microsecond relative-time))
	   (= (microsecond-part nsec)
	      (relative-time-microsecond relative-time)))
       (or (not (relative-time-millisecond relative-time))
	   (= (millisecond-part nsec)
	      (relative-time-millisecond relative-time)))
       (or (not (relative-time-second relative-time))
	   (= ss (relative-time-second relative-time)))
       (or (not (relative-time-minute relative-time))
	   (= mm (relative-time-minute relative-time)))
       (or (not (relative-time-hour relative-time))
	   (= hh (relative-time-hour relative-time)))
       (or (not (relative-time-day relative-time))
	   (= day (relative-time-day relative-time)))
       (or (not (relative-time-month relative-time))
	   (= month (relative-time-month relative-time)))
       (or (not (relative-time-year relative-time))
	   (= year (relative-time-year relative-time)))
       (or (not (relative-time-day-of-week relative-time))
	   (= day-of-week (relative-time-day-of-week relative-time)))))

(declaim (inline matches-relative-time-p))
(defun matches-relative-time-p (fixed-time relative-time)
  "Return T if the given FIXED-TIME honors the details in RELATIVE-TIME."
  (apply #'details-match-relative-time-p
	 relative-time (multiple-value-list (decode-local-time fixed-time))))

;; jww (2007-11-18): The following bug occurs:
;;   (next-time @2008-04-01 (relative-time :month 2 :day 29))
;;     => @2009-03-29T00:33:08.004

;; jww (2007-11-18): There appears to be a bug in local-time itself:
;;   (local-time:parse-timestring "2008-02-29T00:00:00.000")
;;     => @2008-03-01T00:00:00.000

;; jww (2007-11-22): This function fails to compile under CMUCL, although it
;; does work under SBCL and LispWorks.  I get this:
;;
;;   Error in function LISP::ASSERT-ERROR:
;;      The assertion (MEMBER C::KIND '(:OPTIONAL :CLEANUP :ESCAPE)) failed.
;;      [Condition of type SIMPLE-ERROR]

(defun next-time (anchor relative-time
		  &key (reverse nil) (accept-anchor nil) (recursive-call nil))
  "Compute the first time after FIXED-TIME which matches RELATIVE-TIME.

  This function finds the first moment after FIXED-TIME which honors every
  element in RELATIVE-TIME:

    (next-time @2007-05-20 (relative-time :month 3)) => @2008-03-20

  The relative time constructor arguments may also be symbolic:

    (relative-time :month :this)
    (relative-time :month :next)
    (relative-time :month :prev)

  To find the date two weeks after next February, a combination of `NEXT-TIME'
  and `ADD-TIME' must be used, since \"next February\" is a relative time
  concept, while \"two weeks\" is a duration concept:

    (add-time (next-time @2007-05-20 (relative-time :month 2))
              (duration :days 14))

  NOTE: The keyword arguments to `RELATIVE-TIME' are always singular; those to
  `DURATION' are always plural.

  The following form resolves to the first sunday of the given year:

    (next-time (previous-time @2007-05-20 
                              (relative-time :month 1 :day 1))
               (relative-time :week-day 0))

  This form finds the first Friday the 13th after today:

    (next-time @2007-05-20 (relative-time :day 13 :day-of-week 5))

  NOTE: When adding times, `NEXT-TIME' always seeks the next time that fully
  honors your request.  If asked for Feb 29, the year of the resulting time
  will fall in a leap year.  If asked for Thu, Apr 29, it returns the next
  occurrence of Apr 29 which falls on a Friday.  Example:

    (next-time @2007-11-01
               (relative-time :month 4 :day 29 :day-of-week 4))
      => @2010-04-29T00:00:00.000"
  (declare (type (or fixed-time null) anchor))
  (declare (type relative-time relative-time))
  (declare (type boolean reverse))

  (let ((moment (or anchor (local-time:now))))
    (multiple-value-bind
	  (nsec ss mm hh day month year day-of-week)
	(decode-local-time moment)

      ;; If the moment we just decoded already matches the relative-time,
      ;; either return it immediately (if :ACCEPT-ANCHOR is T), or else
      ;; recurse exactly one level to get the next relative time.
      (if (and (not recursive-call)
	       (details-match-relative-time-p relative-time
					      nsec ss mm hh day month year
					      day-of-week nil nil nil))
	  (return-from next-time
	    (if accept-anchor
		moment
		(next-time (add-time moment
				     (enclosing-duration relative-time)
				     :reverse reverse)
			   relative-time
			   :reverse reverse
			   :recursive-call t))))

      (let ((identity (if reverse -1 1))
	    (test (if reverse #'> #'<))
	    now-nsec now-ss now-mm now-hh
	    now-day now-month now-year now-day-of-week)

	(labels
	    ((decode-now ()
	       (if anchor
		   (multiple-value-setq
		       (now-nsec now-ss now-mm now-hh
				 now-day now-month
				 now-year now-day-of-week)
		     (decode-local-time (local-time:now)))
		   (setf now-nsec nsec
			 now-ss ss
			 now-mm mm
			 now-hh hh
			 now-day day
			 now-month month
			 now-year year)))
	     (now-nsec () (or now-nsec (progn (decode-now) now-nsec)))
	     (now-ss () (or now-ss (progn (decode-now) now-ss)))
	     (now-mm () (or now-mm (progn (decode-now) now-mm)))
	     (now-hh () (or now-hh (progn (decode-now) now-hh)))
	     (now-day () (or now-day (progn (decode-now) now-day)))
	     (now-month () (or now-month (progn (decode-now) now-month)))
	     (now-year () (or now-year (progn (decode-now) now-year))))

	  (with-skippers
	    (macrolet
		((set-time-value (sym now-func accessor
				      &optional min max skip-function)
		   `(let ((value (,accessor relative-time)))
		      (when value
			(if (keywordp value)
			    (let ((now-value
				   ,(if (member now-func '(nanosecond-part
							   microsecond-part
							   millisecond-part))
					`(,now-func (now-nsec))
					`(,now-func))))
			      (case value
				(:this (setf value now-value))
				(:next (setf value
					     ,(if max
						  `(range-inc now-value ,min ,max)
						  `(1+ now-value))))
				(:prev (setf value
					     ,(if min
						  `(range-dec now-value ,min ,max)
						  `(- now-value))))
				(otherwise
				 (error "Unknown relative-time keyword for ~S: ~S"
					(quote ,accessor) value)))))

			,(if skip-function
			     `(if (funcall
				   test value
				   ,(if (member now-func '(nanosecond-part
							   microsecond-part
							   millisecond-part))
					`(,now-func nsec)
					`,sym))
				  (,skip-function (* identity 1))))

			,(if (member sym '(set-nanosecond-part
					   set-microsecond-part
					   set-millisecond-part))
			     `(setf nsec (,sym nsec value))
			     `(setf ,sym value))))))

	      (set-time-value set-nanosecond-part nanosecond-part
			      relative-time-nanosecond 0 999 skip-microsecond)
	      (set-time-value set-microsecond-part microsecond-part
			      relative-time-microsecond 0 999 skip-millisecond)
	      (set-time-value set-millisecond-part millisecond-part
			      relative-time-millisecond 0 999 skip-second)
	      (set-time-value ss now-ss relative-time-second 0 59
			      skip-minute)
	      (set-time-value mm now-mm relative-time-minute 0 59
			      skip-hour)
	      (set-time-value hh now-hh relative-time-hour 0 23
			      skip-day)

	      (when (relative-time-day relative-time)
		(unless
		    (if (relative-time-month relative-time)
			(if (relative-time-year relative-time)
			    (<= (relative-time-day relative-time)
				(days-in-month (relative-time-month relative-time)
					       (relative-time-year relative-time)))
			    (<= (relative-time-day relative-time)
				(max 29 (days-in-month
					 (relative-time-month relative-time)))))
			(<= (relative-time-day relative-time) 31))
		  (error "Invalid day specifier in relative-time: ~S"
			 relative-time))
		(set-time-value day now-day relative-time-day 1
				(days-in-month month year)
				skip-month))

	      (set-time-value month now-month relative-time-month 1 12
			      skip-year)
	      (set-time-value year now-year relative-time-year)

	      ;; if the day was 29, 30 or 31, skip forward until a date is found
	      ;; which makes the expression possible.  That is, specifying :day
	      ;; 31 in April will result in a date of May 31.a
	      (do () ((<= day (days-in-month month year)))
		(skip-month identity))

	      (if (relative-time-day-of-week relative-time)
		  (loop
		     for new-time =
		       (encode-local-time nsec ss mm hh day month year)
		     for new-dow = (nth-value 7 (decode-local-time new-time))
		     while (/= new-dow (relative-time-day-of-week
					relative-time))
		     do (skip-day identity))))))

	(encode-local-time nsec ss mm hh day month year)))))

(declaim (inline previous-time))
(defun previous-time (anchor relative-time &key (accept-anchor nil))
  "This function is the reverse of `NEXT-TIME'.  Please look there for more."
  (next-time anchor relative-time :reverse t :accept-anchor accept-anchor))

(declaim (inline relative-time-stepper))
(defun relative-time-stepper (relative-time &key (reverse nil))
  (declare (type relative-time relative-time))
  (declare (type boolean reverse))
  (lambda (time)
    (next-time time relative-time :reverse reverse)))

(declaim (inline relative-time-generator))
(defun relative-time-generator (anchor relative-time &key (reverse nil))
  (declare (type relative-time relative-time))
  (declare (type (or fixed-time null) anchor))
  (declare (type boolean reverse))
  (let (next)
    (lambda ()
      (setf next (next-time (or next anchor) relative-time
			    :reverse reverse)))))

#+periods-use-series
(defmacro scan-relative-times (anchor relative-time &key (reverse nil))
  `(scan-fn 'fixed-time (relative-time-generator ,anchor ,relative-time
						 :reverse ,reverse)))

(defmacro loop-relative-times (forms anchor relative-time end
			       &key (reverse nil) (inclusive-p))
  (let ((generator-sym (gensym))
	(anchor-sym (gensym))
	(end-sym (gensym)))
    `(let ((,anchor-sym ,anchor)
	   (,end-sym ,end))
       (loop
	  with ,generator-sym =
	    (relative-time-generator ,anchor-sym ,relative-time
				     :reverse ,reverse)
	  for value = (funcall ,generator-sym)
	  while ,(if reverse
		     (if inclusive-p
			 `(local-time>= value ,end-sym)
			 `(local-time> value ,end-sym))
		     (if inclusive-p
			 `(local-time<= value ,end-sym)
			 `(local-time< value ,end-sym)))
	  ,@forms))))

(defmacro map-relative-times (callable anchor relative-time end
			      &key (reverse nil) (inclusive-p nil))
  "Map over a set of times separated by DURATION, calling CALLABLE with the
  start of each."
  `(loop-relative-times (do (funcall ,callable value))
      ,anchor ,relative-time ,end :reverse ,reverse
      :inclusive-p ,inclusive-p))

(defmacro list-relative-times (anchor relative-time end
			       &key (reverse nil) (inclusive-p nil))
  "Return a list of all times within the given range."
  `(loop-relative-times (collect value)
      ,anchor ,relative-time ,end :reverse ,reverse
      :inclusive-p ,inclusive-p))

(defmacro do-relative-times ((var anchor relative-time end
				  &optional (result nil)) &body body)
  "A 'do' style version of the functional `MAP-RELATIVE-TIMES' macro.

  The disadvantage to `DO-RELATIVE-TIMES' is that there is no way to ask for a
  reversed time sequence, or specify an inclusive endpoint."
  `(block nil
     (map-relative-times #'(lambda (,var) ,@body)
			 ,anchor ,relative-time ,end)
     ,result))

;; These routines return the present time if it matches
(declaim (inline this-monday
		 this-tuesday
		 this-wednesday
		 this-thursday
		 this-friday
		 this-saturday
		 this-sunday))

(defun this-monday (anchor &key (reverse nil))
  (next-time anchor (relative-time :day-of-week 1) :reverse reverse
	     :accept-anchor t))
(defun this-tuesday (anchor &key (reverse nil))
  (next-time anchor (relative-time :day-of-week 2) :reverse reverse
	     :accept-anchor t))
(defun this-wednesday (anchor &key (reverse nil))
  (next-time anchor (relative-time :day-of-week 3) :reverse reverse
	     :accept-anchor t))
(defun this-thursday (anchor &key (reverse nil))
  (next-time anchor (relative-time :day-of-week 4) :reverse reverse
	     :accept-anchor t))
(defun this-friday (anchor &key (reverse nil))
  (next-time anchor (relative-time :day-of-week 5) :reverse reverse
	     :accept-anchor t))
(defun this-saturday (anchor &key (reverse nil))
  (next-time anchor (relative-time :day-of-week 6) :reverse reverse
	     :accept-anchor t))
(defun this-sunday (anchor &key (reverse nil))
  (next-time anchor (relative-time :day-of-week 0) :reverse reverse
	     :accept-anchor t))

;; These routines do not return the present time if it matches
(declaim (inline next-monday
		 next-tuesday
		 next-wednesday
		 next-thursday
		 next-friday
		 next-saturday
		 next-sunday))

(defun next-monday (anchor &key (reverse nil))
  (next-time anchor (relative-time :day-of-week 1) :reverse reverse))
(defun next-tuesday (anchor &key (reverse nil))
  (next-time anchor (relative-time :day-of-week 2) :reverse reverse))
(defun next-wednesday (anchor &key (reverse nil))
  (next-time anchor (relative-time :day-of-week 3) :reverse reverse))
(defun next-thursday (anchor &key (reverse nil))
  (next-time anchor (relative-time :day-of-week 4) :reverse reverse))
(defun next-friday (anchor &key (reverse nil))
  (next-time anchor (relative-time :day-of-week 5) :reverse reverse))
(defun next-saturday (anchor &key (reverse nil))
  (next-time anchor (relative-time :day-of-week 6) :reverse reverse))
(defun next-sunday (anchor &key (reverse nil))
  (next-time anchor (relative-time :day-of-week 0) :reverse reverse))

;; These routines do not return the present time if it matches
(declaim (inline previous-monday
		 previous-tuesday
		 previous-wednesday
		 previous-thursday
		 previous-friday
		 previous-saturday
		 previous-sunday))

(defun previous-monday (anchor)
  (previous-time anchor (relative-time :day-of-week 1)))
(defun previous-tuesday (anchor)
  (previous-time anchor (relative-time :day-of-week 2)))
(defun previous-wednesday (anchor)
  (previous-time anchor (relative-time :day-of-week 3)))
(defun previous-thursday (anchor)
  (previous-time anchor (relative-time :day-of-week 4)))
(defun previous-friday (anchor)
  (previous-time anchor (relative-time :day-of-week 5)))
(defun previous-saturday (anchor)
  (previous-time anchor (relative-time :day-of-week 6)))
(defun previous-sunday (anchor)
  (previous-time anchor (relative-time :day-of-week 0)))

(defun year-begin (anchor)
  (previous-time anchor (relative-time :month 1 :day 1 :hour 0
				       :minute 0 :second 0
				       :millisecond 0
				       :microsecond 0
				       :nanosecond 0)
		 :accept-anchor t))

(defun month-begin (anchor)
  (previous-time anchor (relative-time :day 1 :hour 0
				       :minute 0 :second 0
				       :millisecond 0
				       :microsecond 0
				       :nanosecond 0)
		 :accept-anchor t))

(defun sunday-week-begin (anchor)
  (previous-time anchor (relative-time :day-of-week 0 :hour 0
				       :minute 0 :second 0
				       :millisecond 0
				       :microsecond 0
				       :nanosecond 0)
		 :accept-anchor t))

(defun monday-week-begin (anchor)
  (previous-time anchor (relative-time :day-of-week 1 :hour 0
				       :minute 0 :second 0
				       :millisecond 0
				       :microsecond 0
				       :nanosecond 0)
		 :accept-anchor t))

(defun day-begin (anchor)
  (previous-time anchor (relative-time :hour 0 :minute 0 :second 0
				       :millisecond 0
				       :microsecond 0
				       :nanosecond 0)
		 :accept-anchor t))

(defun hour-begin (anchor)
  (previous-time anchor (relative-time :minute 0 :second 0
				       :millisecond 0
				       :microsecond 0
				       :nanosecond 0)
		 :accept-anchor t))

(defun minute-begin (anchor)
  (previous-time anchor (relative-time :second 0 :millisecond 0
				       :microsecond 0 :nanosecond 0)
		 :accept-anchor t))

(defun second-begin (anchor)
  (previous-time anchor (relative-time :millisecond 0 :microsecond 0
				       :nanosecond 0)
		 :accept-anchor t))

(defun millisecond-begin (anchor)
  (previous-time anchor (relative-time :microsecond 0 :nanosecond 0)
		 :accept-anchor t))

(defun microsecond-begin (anchor)
  (previous-time anchor (relative-time :nanosecond 0)
		 :accept-anchor t))

(defun year-end (anchor &key (inclusive-p nil))
  (let ((time (next-time anchor (relative-time :month 1
					       :day 1
					       :hour 0
					       :minute 0
					       :second 0
					       :millisecond 0
					       :microsecond 0
					       :nanosecond 0))))
    (if inclusive-p
	time
	(subtract-time time (duration :nanoseconds 1)))))

(defun month-end (anchor &key (inclusive-p nil))
  (let ((time (next-time anchor (relative-time :day 1
					       :hour 0
					       :minute 0
					       :second 0
					       :millisecond 0
					       :microsecond 0
					       :nanosecond 0))))
    (if inclusive-p
	time
	(subtract-time time (duration :nanoseconds 1)))))

(defun sunday-week-end (anchor &key (inclusive-p nil))
  (let ((time (next-sunday anchor)))
    (if inclusive-p
	time
	(subtract-time time (duration :nanoseconds 1)))))

(defun monday-week-end (anchor &key (inclusive-p nil))
  (let ((time (next-monday anchor)))
    (if inclusive-p
	time
	(subtract-time time (duration :nanoseconds 1)))))

(defun day-end (anchor &key (inclusive-p nil))
  (let ((time (next-time anchor (relative-time :hour 0
					       :minute 0
					       :second 0
					       :millisecond 0
					       :microsecond 0
					       :nanosecond 0))))
    (if inclusive-p
	time
	(subtract-time time (duration :nanoseconds 1)))))

(defun hour-end (anchor &key (inclusive-p nil))
  (let ((time (next-time anchor (relative-time :minute 0
					       :second 0
					       :millisecond 0
					       :microsecond 0
					       :nanosecond 0))))
    (if inclusive-p
	time
	(subtract-time time (duration :nanoseconds 1)))))

(defun minute-end (anchor &key (inclusive-p nil))
  (let ((time (next-time anchor (relative-time :second 0
					       :millisecond 0
					       :microsecond 0
					       :nanosecond 0))))
    (if inclusive-p
	time
	(subtract-time time (duration :nanoseconds 1)))))

(defun second-end (anchor &key (inclusive-p nil))
  (let ((time (next-time anchor (relative-time :millisecond 0
					       :microsecond 0
					       :nanosecond 0))))
    (if inclusive-p
	time
	(subtract-time time (duration :nanoseconds 1)))))

(defun millisecond-end (anchor &key (inclusive-p nil))
  (let ((time (next-time anchor (relative-time :microsecond 0
					       :nanosecond 0))))
    (if inclusive-p
	time
	(subtract-time time (duration :nanoseconds 1)))))

(defun microsecond-end (anchor &key (inclusive-p nil))
  (let ((time (next-time anchor (relative-time :nanosecond 0))))
    (if inclusive-p
	time
	(subtract-time time (duration :nanoseconds 1)))))

(defun this-year (&optional fixed-time)
  (year-begin fixed-time))
(defun this-month (&optional fixed-time)
  (month-begin fixed-time))
(defun this-sunday-week (&optional fixed-time)
  (sunday-week-begin fixed-time))
(defun this-monday-week (&optional fixed-time)
  (monday-week-begin fixed-time))
(defun this-day (&optional fixed-time)
  (day-begin fixed-time))
(defun this-hour (&optional fixed-time)
  (hour-begin fixed-time))
(defun this-minute (&optional fixed-time)
  (minute-begin fixed-time))
(defun this-second (&optional fixed-time)
  (second-begin fixed-time))
(defun this-millisecond (&optional fixed-time)
  (millisecond-begin fixed-time))
(defun this-microsecond (&optional fixed-time)
  (microsecond-begin fixed-time))

(defun next-year (&optional fixed-time)
  (year-end fixed-time :inclusive-p t))
(defun next-month (&optional fixed-time)
  (month-end fixed-time :inclusive-p t))
(defun next-sunday-week (&optional fixed-time)
  (sunday-week-end fixed-time :inclusive-p t))
(defun next-monday-week (&optional fixed-time)
  (monday-week-end fixed-time :inclusive-p t))
(defun next-day (&optional fixed-time)
  (day-end fixed-time :inclusive-p t))
(defun next-hour (&optional fixed-time)
  (hour-end fixed-time :inclusive-p t))
(defun next-minute (&optional fixed-time)
  (minute-end fixed-time :inclusive-p t))
(defun next-second (&optional fixed-time)
  (second-end fixed-time :inclusive-p t))
(defun next-millisecond (&optional fixed-time)
  (millisecond-end fixed-time :inclusive-p t))
(defun next-microsecond (&optional fixed-time)
  (microsecond-end fixed-time :inclusive-p t))

(defun previous-year (&optional fixed-time)
  (previous-time (year-begin fixed-time)
		 (relative-time :month 1 :day 1 :hour 0
				:minute 0 :second 0
				:millisecond 0
				:microsecond 0
				:nanosecond 0)))
(defun previous-month (&optional fixed-time)
  (previous-time (month-begin fixed-time)
		 (relative-time :day 1 :hour 0
				:minute 0 :second 0
				:millisecond 0
				:microsecond 0
				:nanosecond 0)))
(defun previous-sunday-week (&optional fixed-time)
  (previous-sunday (previous-time fixed-time (relative-time :day-of-week 0)
				  :accept-anchor t)))
(defun previous-monday-week (&optional fixed-time)
  (previous-monday (previous-time fixed-time (relative-time :day-of-week 1)
				  :accept-anchor t)))
(defun previous-day (&optional fixed-time)
  (previous-time (day-begin fixed-time)
		 (relative-time :hour 0 :minute 0 :second 0
				:millisecond 0
				:microsecond 0
				:nanosecond 0)))
(defun previous-hour (&optional fixed-time)
  (previous-time (hour-begin fixed-time)
		 (relative-time :minute 0 :second 0
				:millisecond 0
				:microsecond 0
				:nanosecond 0)))
(defun previous-minute (&optional fixed-time)
  (previous-time (minute-begin fixed-time)
		 (relative-time :second 0 :millisecond 0
				:microsecond 0
				:nanosecond 0)))
(defun previous-second (&optional fixed-time)
  (previous-time (second-begin fixed-time)
		 (relative-time :millisecond 0
				:microsecond 0
				:nanosecond 0)))
(defun previous-millisecond (&optional fixed-time)
  (previous-time (second-begin fixed-time)
		 (relative-time :microsecond 0
				:nanosecond 0)))
(defun previous-nanosecond (&optional fixed-time)
  (previous-time (second-begin fixed-time)
		 (relative-time :nanosecond 0)))

;;;_ * RANGE

(defstruct (time-range  (:conc-name get-range-))
  (fixed-begin nil)
  (begin nil)
  (begin-inclusive-p t)
  (fixed-end nil)
  (end nil)
  (end-inclusive-p nil)
  (duration nil)
  (anchor nil))

(declaim (inline time-range))
(defun time-range (&rest args)
  (apply #'make-time-range args))

(defun time-range-begin (range)
  (or (get-range-fixed-begin range)
      (etypecase (get-range-begin range)
	(fixed-time
	 (setf (get-range-fixed-begin range)
	       (get-range-begin range)))

	(relative-time
	 ;; jww (2007-11-26): What if a duration was set?
	 (setf (get-range-fixed-begin range)
	       (previous-time (time-range-anchor range)
			      (get-range-begin range))))

	(null
	 (and (get-range-end range)
	      (get-range-duration range)
	      (setf (get-range-begin range)
		    (subtract-time (time-range-end range)
				   (time-range-duration range))))))))

(defsetf time-range-begin (range) (value)
  `(setf (get-range-begin ,range) ,value))

(defun time-range-begin-inclusive-p (range)
  (get-range-begin-inclusive-p range))

(defun time-range-end (range)
  (or (get-range-fixed-end range)
      (etypecase (get-range-end range)
	(fixed-time
	 (setf (get-range-fixed-end range)
	       (get-range-end range)))

	(relative-time
	 ;; jww (2007-11-26): What if a duration was set?
	 (setf (get-range-fixed-end range)
	       (previous-time (time-range-anchor range)
			      (get-range-end range))))

	(null
	 (and (get-range-begin range)
	      (get-range-duration range)
	      (setf (get-range-end range)
		    (add-time (time-range-begin range)
			      (time-range-duration range))))))))

(defun time-range-end-inclusive-p (range)
  (get-range-end-inclusive-p range))

(defun time-range-duration (range)
  (or (get-range-duration range)
      (and (time-range-begin range)
	   (time-range-end range)
	   (setf (get-range-duration range)
		 (time-difference
		  (if (get-range-begin-inclusive-p range)
		      (get-range-begin range)
		      (add-time (get-range-begin range)
				(duration :nanoseconds 1)))
		  (if (get-range-end-inclusive-p range)
		      (get-range-end range)
		      (subtract-time (get-range-end range)
				     (duration :nanoseconds 1))))))))

(defun time-range-anchor (range)
  (or (get-range-anchor range)
      (and (get-range-begin range)
	   (not (typep (get-range-begin range) 'relative-time))
	   (setf (get-range-anchor range) (get-range-begin range)))
      (and (get-range-end range)
	   (not (typep (get-range-end range) 'relative-time))
	   (setf (get-range-anchor range) (get-range-end range)))
      (setf (get-range-anchor range) (local-time:now))))

(defun time-within-range-p (fixed-time range)
  (let ((begin (time-range-begin range))
	(end (time-range-end range)))
    (and (or (null begin)
	     (if (get-range-begin-inclusive-p range)
		 (local-time>= fixed-time begin)
		 (local-time> fixed-time begin)))
	 (or (null end)
	     (if (get-range-end-inclusive-p range)
		 (local-time<= fixed-time end)
		 (local-time< fixed-time end))))))

(defun time-within-begin-end-p (fixed-time begin end)
  (and (or (null begin)
	   (local-time>= fixed-time begin))
       (or (null end)
	   (local-time< fixed-time end))))

(defun time-range-next (range)
  (let ((begin (get-range-begin range))	; uncooked
	(end (get-range-end range))
	(anchor (time-range-end range)))
    (unless (or (null (time-range-begin range))
		(null (time-range-end range)))
      (cond
	((typep begin 'relative-time)
	 (time-range :begin (next-time anchor begin)
		     :end (and (typep end 'relative-time)
			       (next-time anchor end))
		     :duration (and (not (typep end 'relative-time))
				    (time-range-duration range))))
	((typep begin 'fixed-time)
	 (time-range :begin (add-time anchor (time-range-duration range))
		     :end (and (typep end 'relative-time)
			       (next-time anchor end))
		     :duration (and (not (typep end 'relative-time))
				    (time-range-duration range))))))))

(defun time-range-previous (range)
  (let ((begin (get-range-begin range))	; uncooked
	(end (get-range-end range))
	(anchor (time-range-begin range)))
    (unless (or (null (time-range-begin range))
		(null (time-range-end range)))
      (cond
	((typep begin 'relative-time)
	 (time-range :begin (previous-time anchor begin)
		     :end (and (typep end 'relative-time)
			       (previous-time anchor end))
		     :duration (and (not (typep end 'relative-time))
				    (time-range-duration range))))
	((typep begin 'fixed-time)
	 (time-range :begin (subtract-time anchor (time-range-duration range))
		     :end (and (typep end 'relative-time)
			       (previous-time anchor end))
		     :duration (and (not (typep end 'relative-time))
				    (time-range-duration range))))))))

(defun year-range (fixed-time &key (begin-inclusive-p t)
		   (end-inclusive-p nil))
  (time-range :begin (year-begin fixed-time) :end (next-year fixed-time)
	      :begin-inclusive-p begin-inclusive-p
	      :end-inclusive-p end-inclusive-p))
(defun month-range (fixed-time &key (begin-inclusive-p t)
		   (end-inclusive-p nil))
  (time-range :begin (month-begin fixed-time) :end (next-month fixed-time)
	      :begin-inclusive-p begin-inclusive-p
	      :end-inclusive-p end-inclusive-p))
(defun sunday-week-range (fixed-time &key (begin-inclusive-p t)
		   (end-inclusive-p nil))
  (time-range :begin (sunday-week-begin fixed-time)
	      :end (next-sunday-week fixed-time)
	      :begin-inclusive-p begin-inclusive-p
	      :end-inclusive-p end-inclusive-p))
(defun monday-week-range (fixed-time &key (begin-inclusive-p t)
		   (end-inclusive-p nil))
  (time-range :begin (monday-week-begin fixed-time)
	      :end (next-monday-week fixed-time)
	      :begin-inclusive-p begin-inclusive-p
	      :end-inclusive-p end-inclusive-p))
(defun day-range (fixed-time &key (begin-inclusive-p t)
		   (end-inclusive-p nil))
  (time-range :begin (day-begin fixed-time) :end (next-day fixed-time)
	      :begin-inclusive-p begin-inclusive-p
	      :end-inclusive-p end-inclusive-p))
(defun hour-range (fixed-time &key (begin-inclusive-p t)
		   (end-inclusive-p nil))
  (time-range :begin (hour-begin fixed-time) :end (next-hour fixed-time)
	      :begin-inclusive-p begin-inclusive-p
	      :end-inclusive-p end-inclusive-p))
(defun minute-range (fixed-time &key (begin-inclusive-p t)
		   (end-inclusive-p nil))
  (time-range :begin (minute-begin fixed-time) :end (next-minute fixed-time)
	      :begin-inclusive-p begin-inclusive-p
	      :end-inclusive-p end-inclusive-p))
(defun second-range (fixed-time &key (begin-inclusive-p t)
		   (end-inclusive-p nil))
  (time-range :begin (second-begin fixed-time) :end (next-second fixed-time)
	      :begin-inclusive-p begin-inclusive-p
	      :end-inclusive-p end-inclusive-p))

(defun this-year-range ()
  (year-range (now)))
(defun this-month-range ()
  (month-range (now)))
(defun this-sunday-week-range ()
  (sunday-week-range (now)))
(defun this-monday-week-range ()
  (monday-week-range (now)))
(defun this-day-range ()
  (day-range (now)))
(defun this-hour-range ()
  (hour-range (now)))
(defun this-minute-range ()
  (minute-range (now)))
(defun this-second-range ()
  (second-range (now)))

;;;_ * PERIOD

(defstruct time-period
  (range)
  (step)
  (skip))

(defun time-period (&rest args)
  (apply #'make-time-period args))

(defun time-period-begin (period)
  (time-range-begin (time-period-range period)))

(defun time-period-end (period)
  (time-range-end (time-period-range period)))

(defun time-period-generator (period)
  (declare (type time-period period))
  (let ((step-stepper (time-stepper (time-period-step period)))
	(skip-stepper (and (time-period-skip period)
			   (time-stepper (time-period-skip period))))
	(begin (time-period-begin period))
	(end (time-period-end period))
	(end-inclusive-p
	 (time-range-end-inclusive-p (time-period-range period))))
    (lambda ()
      (if begin
	  (let* ((this-end (funcall step-stepper begin))
		 (next-begin (if skip-stepper
				 (funcall skip-stepper begin)
				 this-end)))
	    (when end
	      (if (if end-inclusive-p
		      (local-time> next-begin end)
		      (local-time>= next-begin end))
		  (setf next-begin nil)))
	    (multiple-value-prog1
		(values begin this-end next-begin)
	      (setf begin next-begin)))
	  (values nil nil nil)))))

(defmacro loop-time-period (forms period)
  (let ((generator-sym (gensym)))
    `(loop
	with ,generator-sym = (time-period-generator ,period)
	for (begin end next-begin) =
	  (multiple-value-list (funcall ,generator-sym))
	while begin
	,@forms)))

(defmacro map-time-period (callable period)
  `(loop-time-period (do (funcall ,callable begin end next-begin))
      ,period))

(defmacro list-time-period (period)
  `(loop-time-period (collect (list begin end next-begin))
      ,period))

(defmacro do-time-period ((begin-var end-var next-begin-var period
				     &optional (result nil))
			  &rest body)
  `(block nil
     (map-time-period
      #'(lambda (,begin-var ,end-var ,next-begin-var)
	  ,@body) ,period)
     ,result))

#+periods-use-series
(defun scan-time-period (period)
  (declare (optimizable-series-function))
  (multiple-value-bind (begins ends next-begins)
      (map-fn '(values
		(or fixed-time null)
		(or fixed-time null)
		(or fixed-time null))
	      (time-period-generator period))
    (until-if #'null begins ends next-begins)))

;;;_ * Library functions

;;;_  + SERIES functions

#+periods-use-series
(defun collate-by-time-period (item-series period &key (key #'identity))
  "Return two series, one is a series of lists grouped by ranges within the
  period, and the other is a series of ranges, each element of which
  corresponds to the group elements in the same position within the first
  series."
  (let (next-series)
    (multiple-value-call #'map-fn
      '(values fixed-time fixed-time series)
      #'(lambda (begin end next-begin)
	  (declare (ignore next-begin))
	  (list begin end
		(let (matching)
		  (multiple-value-setq (matching next-series)
		    (split-if (or next-series item-series)
			      #'(lambda (item)
				  (time-within-begin-end-p
				   (funcall key item) begin end))))
		  matching)))
      (scan-time-period period))))

;;;_  + General purpose

(defun sleep-until (fixed-time)
  (let ((now (local-time:now)))
    (when (local-time:local-time> fixed-time now)
      (let ((duration (time-difference fixed-time now)))
	(sleep (/ (+ (* (duration-seconds duration) 1000000000)
		     (* (duration-milliseconds duration) 1000000)
		     (* (duration-microseconds duration) 1000)
		     (duration-nanoseconds duration)) 1000000000))))))

(provide 'periods)

;; periods.lisp ends here
