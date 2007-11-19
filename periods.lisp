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
;; should have accompanied this source code.  Please refer there for complete
;; details.

(declaim (optimize (debug 3) (safety 3) (speed 1) (space 0)))

(defpackage :periods
  (:use :common-lisp :local-time
	#+periods-use-series :series
	#+periods-use-parser :com.gigamonkeys.parser)
  (:nicknames :time-periods)
  (:export leapp
	   current-year
	   days-in-month
	   increment-time
	   decrement-time
	   floor-time
	   parse-time-period
	   time-period-generator
	   time-periods
	   map-over-time
	   do-over-time
	   collect-by-period
	   sleep-until))

(in-package :periods)

;;;_ * Types

;;;_ * Basic utility functions

(defparameter *days-in-months*
  #(31 28 31 30 31 30 31 31 30 31 30 31))

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
  (let ((days-in-month
	 (aref *days-in-months* (1- month)))
	(the-year (or year (current-year))))
    (if (and (= month 2)
	     (leapp the-year))
	(incf days-in-month)
	days-in-month)))

(defun floor-time (time &optional resolution)
  "Reduce a fixed time to be no finer than RESOLUTION.

  For example, if the date is 2007-04-20, and the resolution is :month, the
  date is floored to 2007-04-01.  Anything smaller than the resolution is
  reduced to zero (or 1, if it is a day or month being reduced)."
  (declare (type local-time time))
  (multiple-value-bind
	(ms ss mm hh day month year)
      (decode-local-time time)
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
    ((member :millisecond step-by) :millisecond)
    ((member :second step-by) :second)
    ((member :minute step-by) :minute)
    ((member :hour step-by) :hour)
    ((member :day step-by) :day)
    ((member :month step-by) :month)))

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
	    (ms ss mm hh day month year)
	  (decode-local-time (local-time:now))
	(block nil
	 (or (and (setf ms (getf args :millisecond))
		  (return))
	     (setf ms 0))
	 (or (and (setf ss (getf args :second))
		  (return))
	     (setf ss 0))
	 (or (and (setf mm (getf args :minute))
		  (return))
	     (setf mm 0))
	 (or (and (setf hh (getf args :hour))
		  (return))
	     (setf hh 0))
	 (or (and (setf day (getf args :day))
		  (return))
	     (setf day 1))
	 (or (and (setf month (getf args :month))
		  (return))
	     (setf month 1)))
	(encode-local-time ms ss mm hh day month year))))

(declaim (inline year-of
		 quarter-of
		 month-of
		 day-of
		 hour-of
		 minute-of
		 second-of
		 millisecond-of))

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
  (nth-value 0 (decode-local-time fixed-time)))

(declaim (inline day-of-week))
(defun day-of-week (fixed-time)
  (declare (type fixed-time fixed-time))
  (nth-value 7 (decode-local-time fixed-time)))

(declaim (inline falls-on-weekend-p))
(defun falls-on-weekend-p (fixed-time)
  (let ((dow (day-of-week fixed-time)))
    (or (= 0 dow) (= 6 dow))))

;;;_ * DURATION

(defstruct duration
  (years nil :type (or integer null))
  (quarters nil :type (or integer null))
  (months nil :type (or integer null))
  (days nil :type (or integer null))
  (hours nil :type (or integer null))
  (minutes nil :type (or integer null))
  (seconds nil :type (or integer null))
  (milliseconds nil :type (or integer null)))

(defmacro duration (&rest args)
  "Create a DURATION object.

  One thing to note about duration: there is no way to determine the total
  length of a duration in terms of any specific time quantity, without first
  binding that duration to a fixed point in time (after all, how many days are
  in a month if you don't know which month it is?)  Therefore, those looking
  for a function like \"duration-seconds\" are really wanting to work with
  ranges, not just durations."
  `(make-duration ,@args))

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
	      (let ((remainder (+ ms skip)))
		(if (minusp remainder)
		    (progn
		      (skip-second -1)
		      (setf ms 999)
		      (skip-millisecond (1+ remainder)))
		    (incf ms skip)))
	      (if (plusp skip)
		  (let ((remainder (- (+ ms skip) 999)))
		    (if (plusp remainder)
			(progn
			  (skip-second 1)
			  (setf ms 0)
			  (skip-millisecond (1- remainder)))
			(incf ms skip)))))))
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
  (multiple-value-bind
	(ms ss mm hh day month year)
      (decode-local-time fixed-time)
    (let ((identity (if reverse -1 1)))
     (with-skippers
       (if (duration-years duration)
	   (skip-year (* identity (duration-years duration))))
       (if (duration-quarters duration)
	   (skip-month (* identity (* 3 (duration-quarters duration)))))
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
	   (skip-millisecond (* identity (duration-milliseconds duration))))))
    (encode-local-time ms ss mm hh day month year)))

(declaim (inline subtract-time))
(defun subtract-time (fixed-time duration)
  (add-time fixed-time duration :reverse t))

(declaim (inline bounded-minus))
(defun bounded-minus (left right bound &optional zero-base-p)
  (if (< left right)
      (+ left (+ (- bound right) (if zero-base-p 1 0)))
      (- left right)))

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
  (multiple-value-bind
	(l-ms l-ss l-mm l-hh l-day l-month l-year)
      (decode-local-time left)
    (multiple-value-bind
	  (r-ms r-ss r-mm r-hh r-day r-month r-year)
	(decode-local-time right)
      (let ((months (bounded-minus l-month r-month 12))
	    (days (bounded-minus l-day r-day
				 (days-in-month (if (= 1 l-month)
						    12
						    (1- l-month))
						l-year)))
	    (hours (bounded-minus l-hh r-hh 23 t))
	    (minutes (bounded-minus l-mm r-mm 59 t))
	    (seconds (bounded-minus l-ss r-ss 59 t))
	    (milliseconds (bounded-minus l-ms r-ms 999 t)))
	(make-duration :years (if (< l-month r-month)
				  (1- (- l-year r-year))
				  (- l-year r-year))
		       :months (if (< l-day r-day) (1- months) months)
		       :days (if (< l-hh r-hh) (1- days) days)
		       :hours (if (< l-mm r-mm) (1- hours) hours)
		       :minutes (if (< l-ss r-ss) (1- minutes) minutes)
		       :seconds (if (< l-ms r-ms) (1- seconds) seconds)
		       :milliseconds milliseconds)))))

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
    (lambda (&rest args)
      (declare (ignore args))
      (setf next (add-time (or next start) duration
			   :reverse reverse)))))

#+periods-use-series
(defmacro scan-times (start duration
		      &key (end nil) (reverse nil) (inclusive-p nil))
  "This macro represents continguous time durations as a SERIES.

  The following returns all months until 2009 (non-inclusive):

    (collect (scan-times @2007-11-01 (duration :months 1)
                         :end @2009-01-01))

  The :END is optional.  It could have been done with the `UNTIL-IF' collector
  just as easily:

    (collect (until-if #'(lambda (time)
                           (local-time:local-time>= time @2009-01-01))
                       (scan-times @2007-11-01 (duration :months 1))))"
  (let ((test-func
	 (if end
	     `#'(lambda (time)
		  ,(if reverse
		       (if inclusive-p
			   `(local-time< time ,end)
			   `(local-time<= time ,end))
		       (if inclusive-p
			   `(local-time> time ,end)
			   `(local-time>= time ,end)))))))
    `(let ((generator-func (time-generator ,start ,duration :reverse ,reverse)))
       ,(if test-func
	    `(series:scan-fn 'fixed-time generator-func generator-func
			     ,test-func)
	    '(series:scan-fn 'fixed-time generator-func generator-func)))))

(defmacro loop-times (forms start duration end
		      &key (reverse nil) (inclusive-p nil))
  "Map over a set of times separated by DURATION, calling CALLABLE with the
  start of each."
  (let ((generator-sym (gensym)))
    `(progn
       (assert (,(if reverse
		     'local-time>
		     'local-time<) ,start ,end))
       (loop
	  with ,generator-sym = (time-generator ,start ,duration)
	  for value = (funcall ,generator-sym)
	  while ,(if reverse
		     (if inclusive-p
			 `(local-time>= value ,end)
			 `(local-time> value ,end))
		     (if inclusive-p
			 `(local-time<= value ,end)
			 `(local-time< value ,end)))
	  ,@forms))))

(defmacro map-times (callable start duration end
		     &key (reverse nil) (inclusive-p nil))
  "Map over a set of times separated by DURATION, calling CALLABLE with the
  start of each."
  `(loop-times (do (funcall ,callable value))
      ,start ,duration ,end :reverse ,reverse
      :inclusive-p ,inclusive-p))

(defmacro list-of-times (start duration end
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
     ,(map-times `#'(lambda (,var) ,@body) start duration end)
     ,result))

;;;_ * RELATIVE-TIME

(defstruct relative-time
  (year nil :type (or keyword integer null))
  (quarter nil :type (or keyword integer null))
  (month nil :type (or keyword integer null))
  (week nil :type (or keyword integer null))
  (day-of-week nil :type (or keyword integer null))
  (day nil :type (or keyword integer null))
  (hour nil :type (or keyword integer null))
  (minute nil :type (or keyword integer null))
  (second nil :type (or keyword integer null))
  (millisecond nil :type (or keyword integer null)))

(defmacro relative-time (&rest args)
  `(make-relative-time ,@args))

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
    ((relative-time-day-of-week relative-time)
     (duration :days 1))
    (t
     (error "`enclosing-duration' has failed."))))

;; jww (2007-11-18): The following bug occurs:
;;   (next-time (relative-time :month 2 :day 29) @2008-04-01)
;;     => @2009-03-29T00:33:08.004

;; jww (2007-11-18): There appears to be a bug in local-time itself:
;;   (local-time:parse-timestring "2008-02-29T00:00:00.000")
;;     => @2008-03-01T00:00:00.000
(defun next-time (anchor relative-time &key (reverse nil) (accept-anchor nil))
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

  (do ((new-time anchor))
      (nil)
    (multiple-value-bind
	  (ms ss mm hh day month year)
	(decode-local-time (or anchor (setf anchor (local-time:now))))

      (let ((identity (if reverse -1 1))
	    (test (if reverse #'> #'<))
	    now-ms now-ss now-mm now-hh
	    now-day now-month now-year now-day-of-week)

	(labels ((decode-now ()
		   (if anchor
		       (multiple-value-setq
			   (now-ms now-ss now-mm now-hh
				   now-day now-month
				   now-year now-day-of-week)
			 (decode-local-time (local-time:now)))
		       (setf now-ms ms
			     now-ss ss
			     now-mm mm
			     now-hh hh
			     now-day day
			     now-month month
			     now-year year)))
		 (now-ms () (or now-ms (progn (decode-now) now-ms)))
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
			    (case value
			      (:this (setf value (,now-func)))
			      (:next (setf value
					   ,(if max
						`(range-inc (,now-func) ,min ,max)
						`(1+ (,now-func)))))
			      (:prev (setf value
					   ,(if min
						`(range-dec (,now-func) ,min ,max)
						`(- (,now-func)))))
			      (otherwise
			       (error "Unknown relative-time keyword for ~S: ~S"
				      (quote ,accessor) value))))
			,(if skip-function
			     `(if (funcall test value ,sym)
				  (,skip-function (* identity 1))))
			(setf ,sym value)))))

	      (set-time-value ms now-ms relative-time-millisecond 0 999
			      skip-second)
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
		       (encode-local-time ms ss mm hh day month year)
		     for new-dow = (nth-value 7 (decode-local-time new-time))
		     while (/= new-dow (relative-time-day-of-week
					relative-time))
		     do (skip-day identity))))))

	(setf new-time (encode-local-time ms ss mm hh day month year))

	(if (and (not accept-anchor)
		 (local-time= new-time anchor))
	    (progn
	      (format t "forced to loop~%")
	      (setf anchor
		    (add-time anchor (enclosing-duration relative-time)
			      :reverse reverse)))
	    (return new-time))))))

(declaim (inline previous-time))
(defun previous-time (anchor relative-time)
  "This function is the reverse of `NEXT-TIME'.  Please look there for more."
  (next-time anchor relative-time :reverse t))

(defun relative-time-stepper (anchor relative-time &key (reverse nil))
  (declare (type relative-time relative-time))
  (declare (type (or fixed-time null) anchor))
  (declare (type boolean reverse))
  (lambda (time)
    (next-time time relative-time :reverse reverse)))

(declaim (inline relative-time-generator))
(defun relative-time-generator (anchor relative-time &key (reverse nil))
  (declare (type relative-time relative-time))
  (declare (type (or fixed-time null) anchor))
  (declare (type boolean reverse))
  (let (next)
    (lambda (&rest args)
      (declare (ignore args))
      (setf next (next-time (or next anchor) relative-time
			    :reverse reverse)))))

(defmacro map-relative-times (callable relative-time end anchor
			      &key (reverse nil) (inclusive-p))
  (let ((generator-sym (gensym))
	(value-sym (gensym)))
    `(progn
       ,(if anchor
	    `(assert (,(if reverse
			   'local-time>
			   'local-time<) ,anchor ,end)))
       (loop
	  with ,generator-sym =
	  (relative-time-generator relative-time ,anchor
				   :reverse ,reverse)
	  for ,value-sym = (funcall ,generator-sym)
	  while ,(if reverse
		     (if inclusive-p
			 `(local-time>= ,value-sym ,end)
			 `(local-time> ,value-sym ,end))
		     (if inclusive-p
			 `(local-time<= ,value-sym ,end)
			 `(local-time< ,value-sym ,end)))
	  do (funcall ,callable ,value-sym)))))

(defmacro do-relative-times ((var relative-time anchor end
				  &optional (result nil)) &body body)
  "A 'do' style version of the functional `MAP-RELATIVE-TIMES' macro.

  The disadvantage to `DO-RELATIVE-TIMES' is that there is no way to ask for a
  reversed time sequence, or specify an inclusive endpoint."
  `(block nil
     ,(map-relative-times `#'(lambda (,var) ,@body) relative-time anchor end)
     ,result))

#+periods-use-series
(defmacro scan-relative-times (relative-time anchor
			       &key (end nil) (reverse nil) (inclusive-p))
  (let ((test-func
	 (if end
	     `#'(lambda (time)
		  ,(if reverse
		       (if inclusive-p
			   `(local-time< time ,end)
			   `(local-time<= time ,end))
		       (if inclusive-p
			   `(local-time> time ,end)
			   `(local-time>= time ,end)))))))
    `(let ((generator-func (relative-time-generator ,relative-time ,anchor
						    :reverse ,reverse)))
       ,(if test-func
	    `(series:scan-fn 'fixed-time generator-func generator-func
			     ,test-func)
	    '(series:scan-fn 'fixed-time generator-func generator-func)))))

;; These routines return the present time if it matches
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

(defun year-begin (anchor)
  (previous-time anchor (relative-time :month 1 :day 1 :hour 0
				       :minute 0 :second 0
				       :millisecond 0)))
(defun quarter-begin (anchor))
(defun month-begin (anchor)
  (previous-time anchor (relative-time :day 1 :hour 0
				       :minute 0 :second 0
				       :millisecond 0)))
(defun sunday-week-begin (anchor)
  (previous-time anchor (relative-time :day-of-week 0 :hour 0
				       :minute 0 :second 0
				       :millisecond 0)))
(defun monday-week-begin (anchor)
  (previous-time anchor (relative-time :day-of-week 1 :hour 0
				       :minute 0 :second 0
				       :millisecond 0)))
(defun day-begin (anchor)
  (previous-time anchor (relative-time :hour 0 :minute 0 :second 0
				       :millisecond 0)))
(defun hour-begin (anchor)
  (previous-time anchor (relative-time :minute 0 :second 0
				       :millisecond 0)))
(defun minute-begin (anchor)
  (previous-time anchor (relative-time :second 0 :millisecond 0)))
(defun second-begin (anchor)
  (previous-time anchor (relative-time :millisecond 0)))

(defun year-end (anchor &key (inclusive-p nil))
  (let ((time (next-time anchor (relative-time :month 1
					       :day 1
					       :hour 0
					       :minute 0
					       :second 0
					       :millisecond 0))))
    (if inclusive-p
	time
	(subtract-time time (duration :milliseconds 1)))))
(defun quarter-end (anchor &key (inclusive-p nil)))
(defun month-end (anchor &key (inclusive-p nil))
  (let ((time (next-time anchor (relative-time :day 1
					       :hour 0
					       :minute 0
					       :second 0
					       :millisecond 0))))
    (if inclusive-p
	time
	(subtract-time time (duration :milliseconds 1)))))
(defun sunday-week-end (anchor &key (inclusive-p nil))
  (let ((time (next-sunday anchor)))
    (if inclusive-p
	time
	(subtract-time time (duration :milliseconds 1)))))
(defun monday-week-end (anchor &key (inclusive-p nil))
  (let ((time (next-monday anchor)))
    (if inclusive-p
	time
	(subtract-time time (duration :milliseconds 1)))))
(defun day-end (anchor &key (inclusive-p nil))
  (let ((time (next-time anchor (relative-time :hour 0
					       :minute 0
					       :second 0
					       :millisecond 0))))
    (if inclusive-p
	time
	(subtract-time time (duration :milliseconds 1)))))
(defun hour-end (anchor &key (inclusive-p nil))
  (let ((time (next-time anchor (relative-time :minute 0
					       :second 0
					       :millisecond 0))))
    (if inclusive-p
	time
	(subtract-time time (duration :milliseconds 1)))))
(defun minute-end (anchor &key (inclusive-p nil))
  (let ((time (next-time anchor (relative-time :second 0
					       :millisecond 0))))
    (if inclusive-p
	time
	(subtract-time time (duration :milliseconds 1)))))
(defun second-end (anchor &key (inclusive-p nil))
  (let ((time (next-time anchor (relative-time :millisecond 0))))
    (if inclusive-p
	time
	(subtract-time time (duration :milliseconds 1)))))

;;;_ * RANGE

(defstruct range
  (begin)
  (begin-inclusive-p)
  (end)
  (end-inclusive-p)
  (duration)
  (anchor))

(defun range-begin (range))
(defun range-end (range))
(defun range-duration (range))
(defun time-in-range-p (range))

(defun year-range (fixed-time))
(defun quarter-range (fixed-time))
(defun month-range (fixed-time))
(defun sunday-week-range (fixed-time))
(defun monday-week-range (fixed-time))
(defun day-range (fixed-time))
(defun hour-range (fixed-time))
(defun minute-range (fixed-time))
(defun second-range (fixed-time))

(defun this-millenium ())
(defun this-century ())
(defun this-decade ())
(defun this-year ())
(defun this-quarter ())
(defun this-month ())
(defun this-sunday-week ())
(defun this-monday-week ())
(defun this-day ())
(defun this-hour ())
(defun this-minute ())
(defun this-second ())

(defun next-millenium (&optional fixed-time))
(defun next-century (&optional fixed-time))
(defun next-decade (&optional fixed-time))
(defun next-year (&optional fixed-time))
(defun next-quarter (&optional fixed-time))
(defun next-month (&optional fixed-time))
(defun next-sunday-week (&optional fixed-time))
(defun next-monday-week (&optional fixed-time))
(defun next-day (&optional fixed-time))
(defun next-hour (&optional fixed-time))
(defun next-minute (&optional fixed-time))
(defun next-second (&optional fixed-time))

(defun last-millenium (&optional fixed-time))
(defun last-century (&optional fixed-time))
(defun last-decade (&optional fixed-time))
(defun last-year (&optional fixed-time))
(defun last-quarter (&optional fixed-time))
(defun last-month (&optional fixed-time))
(defun last-sunday-week (&optional fixed-time))
(defun last-monday-week (&optional fixed-time))
(defun last-day (&optional fixed-time))
(defun last-hour (&optional fixed-time))
(defun last-minute (&optional fixed-time))
(defun last-second (&optional fixed-time))

;;;_ * PERIOD

(defstruct period
  (entire-range)
  (include-stepper)
  (skip-stepper)
  (ranges))

(defun compose-period (range step &key (skip nil) (predicate nil)))
(defun period-begin (range))
(defun period-end (range))
(defun time-in-period-p (range))

(defun range-stepper (period))
(defun map-ranges (period))
(defun do-ranges (period))
(defun scan-ranges (period))

(defun parse-period-description (string))

;;;_ * Library functions

;;;_ * Old Code

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
	     (until (getf specifier :until))
	     (end-of-range
	      (apply #'increment-time*
		     epoch
		     :terminus (or terminus until)
		     :terminus-forward-p t
		     :floorp floorp
		     step-by)))
	(when end-of-range
	  (if skip
	      (values epoch end-of-range
		      (apply #'increment-time* epoch
			     :terminus (or terminus until)
			     :terminus-forward-p t
			     :floorp floorp
			     skip))
	      (values epoch end-of-range end-of-range))))
      (increment-time* epoch
		       :terminus terminus
		       :terminus-forward-p t
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
		   :terminus-forward-p nil
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
			      (until nil)
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
    :UNTIL   <LOCAL-TIME>

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
	     (parse-time-period  specifier)
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
	       :from  ,from
	       :until ,until)))
	epoch terminated-p)
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
     with generator-func = (apply #'time-period-generator time-specifiers)
     for period = (multiple-value-list (funcall generator-func))
     while (car period)
     collect period))

(defun map-over-time (closure &rest time-specifiers)
  (loop
     with generator-func = (apply #'time-period-generator time-specifiers)
     for period = (funcall generator-func)
     while period
     do (funcall closure period)))

(defmacro do-over-time (&rest time-specifiers)
  (let ((moment (gensym)))
    (multiple-value-bind (generator-func loop-words)
	(eval `(time-period-generator ,@time-specifiers))
      `(loop
	  :for ,moment = (funcall ,generator-func)
	  :while ,moment
	  ,@loop-words))))

;; This version of `collect-by-period' evolved thanks to help from pkhuong on
;; #lisp.
(defun collect-by-period (list periods &key (key #'identity))
  (let (result)
    (dolist (item list
	     (mapcar (lambda (entry)
		       (prog1 entry
			 (setf (cdr entry)
			       (nreverse (cdr entry)))))
		     result))
      (let ((date (funcall key item)))
        (dolist (period periods)
          (when (and (local-time>= date (nth 0 period))
                     (local-time<= date (nth 1 period)))
            (push item (cdr (or (assoc period result)
                                (first (push (cons period nil)
                                             result)))))))))))

(provide 'periods)

;; periods.lisp ends here
