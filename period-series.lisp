(in-package :cl-user)

(defpackage :period-series
     (:use :common-lisp :periods :series)
     (:export scan-times
              scan-relative-times
              scan-time-period
              collate-by-time-period))

(in-package :period-series)

(defmacro scan-times (start duration &key (reverse nil))
  "This macro represents continguous time durations as a SERIES.

  Example:

  (subseries (scan-times @2007-11-01 (duration :months 1)) 0 10)

  `UNTIL-IF' can be used to bound the end of the range by a date:

  (collect (until-if #'(lambda (time)
                          (local-time:timestamp>= time @2009-01-01))
                     (scan-times @2007-11-01 (duration :months 1))))"
  `(map-fn 'fixed-time (time-generator ,start ,duration :reverse ,reverse)))

(defmacro scan-relative-times (anchor relative-time &key (reverse nil))
  `(scan-fn 'fixed-time (relative-time-generator ,anchor ,relative-time
						 :reverse ,reverse)))

(defun scan-time-period (period)
  (multiple-value-call #'until-if
    #'null (map-fn '(values
		     (or fixed-time null)
		     (or fixed-time null)
		     (or fixed-time null))
		   (time-period-generator period))))

(defun collate-by-time-period (item-series period &key (key #'identity))
  "Return two series, one is a series of lists grouped by ranges within the
period, and the other is a series of ranges, each element of which corresponds
to the group elements in the same position within the first series."
  (multiple-value-call #'map-fn
    '(values fixed-time fixed-time series)
    (let (next-series)
      #'(lambda (begin end next-begin)
	  (declare (ignore next-begin))
	  (values begin end
		  (let (matching)
		    (multiple-value-setq (matching next-series)
		      (split-if (or next-series item-series)
				#'(lambda (item)
				    (periods::time-within-begin-end-p
				     (funcall key item) begin end))))
		    matching))))
    (scan-time-period period)))
