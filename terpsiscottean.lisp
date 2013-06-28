;;;; terpsiscottean.lisp

(in-package #:terpsiscottean)

(defmacro deffigure (name &body figure)
  "deffigure defines a complete, multi-person, timed figure.
   For example:
       (deffigure 1C-set
         (2
           (1M
             (right :face in :step pas-de-basque)
             (left :face in :step pas-de-basque))
           (1W
             (right :face in :step pas-de-basque)
             (left :face in :step pas-de-basque))))
   Specifically, there's a list of lists that start with a number.
   This number represents the number of bars the dancing will take.
   After the number is a list of the participants, each of which will match
   the regular expresion /[1-5][MW]/. After each participant is a sequence
   of the figures to be done. The primitives up, down, in, out, left, and
   right are provided, and take the options :face, and :step. The time allotted
   will be evenly distributed among all primitives listed. Additionally, more
   complex figures previously defined with deffigure can be used, which will
   take up however many bars were specified in the deffigure form.
   The :face option specifies which direction the dancer should be facing at
   the end of the figure. Supported values are up, down, in, out, left, right,
   first-corner, second-corner.
   The :step option has no real effect, but adds semantic information about which
   step should be employed. Supported values are :skip-change, :pas-de-basque,
   :strathspey, :strathspey-set, and :slipstep."
  (dolist (barcount-with-steps figure)
    (let ((barcount (first barcount-with-steps))
	   (couples-with-steps (rest barcount-with-steps)))
      (unless (and (numberp barcount) (listp couples-with-steps))
	(error "~S is not in a valid format" barcount-with-steps))
      (dolist (couple-with-steps couples-with-steps)
	(let ((couple (symbol-name (first couple-with-steps)))
	      (steps (rest couple-with-steps)))
	  (unless (cl-ppcre:scan "[1-5][MW]" couple)
	    (error "~S is not a valid couple name" couple))
	  (let ((rank (parse-integer couple :junk-allowed t))
		(gender (elt couple 1)))
	    (format t "~&Figure ~a" name)
	    (dolist (step steps)
	      (format t "~&~:r ~a will ~s" rank (if (char-equal gender #\M) "man" "woman") step))))))))