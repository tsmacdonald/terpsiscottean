;;;; terpsiscottean.lisp

(in-package #:terpsiscottean)

(defparameter *figures* (make-figures-table)
  "A mapping of figure names to figure implementations (functions)")

(defmacro deffigure (name human-readable-name &body figure)
  "deffigure defines a complete, multi-person, timed figure.
   For example:

       (deffigure 1C-set \"1C set\"
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
   the end of the figure. Supported values are specified by the direction type.

   The :step option has no real effect at this level, but adds semantic information
   about which step should be employed. Supported values are :skip-change,
   :pas-de-basque, :strathspey, :strathspey-set, and :slipstep.

   As an implementation detail, note that the aforementioned primitives are not
   truly primitive. They are all implemented by the mighty SWAP method, which has
   appropriate arguments added to it intelligently. Anything that needs to *happen*
   with the dancing should be implemented as an after-method of SWAP. Any data that
   needs to be kept track of besides position will be stored in the dancer object
   itself."
  (let ((commands ()))
    (dolist (barcount-with-steps figure)
      (let ((barcount (first barcount-with-steps))
	    (couples-with-steps (rest barcount-with-steps)))
	(unless (and (numberp barcount) (listp couples-with-steps))
	  (error "~S is not in a valid format" barcount-with-steps))
	(dolist (couple-with-steps couples-with-steps)
	  (let ((couple (symbol-name (first couple-with-steps)))
		(steps (rest couple-with-steps)))
	    (unless (cl-ppcre:scan "[1-5][MWmw]" couple)
	      (error "~S is not a valid couple name" couple))
	    (let ((rank (parse-integer couple :junk-allowed t))
		  (gender (char-upcase (elt couple 1))))
	      (format t "~&Figure ~a" name)
	      (dolist (step steps) ;;Insert floor arg to each step
		(push (append (list (first step)
				    'floor)
			      (rest step)) ;;Insert floor argument
		      commands))))))) ;;TODO: store other information here too
    (format t "~S" (reverse commands))
    `(setf (gethash ',name *figures*)
	   (make-instance 'figure
			  :name ,human-readable-name
			  :duration 1 ;;TODO
			  :steps (lambda (floor &key face step args)
				   (declare (ignorable floor face step args))
				   ,@(mapcar
				      (lambda (command)
					`(dance (gethash ',(first command) *figures*) ,@(rest command)))
				      (nreverse commands)))))))

;;add boilerplate to defun that assigns dancer variable and deals with common kwargs
;;dance-space will have to have a mapping from dancer name (1W or what have you) to coords

(defmacro defdance (name &body figures)
  "Defines a dance, which is a named sequence of figures.
   For example:
       (defdance \"Maclisp and Macro\"
         (1C-set) ;;1
         (1C-cast-off :to 'second-place) ;;3
         (1C-set) ;;5
         (1C-cross) ;;7
         (sideline-reel) ;;9
         (rights-and-lefts) ;;17
         (1C-cast-off :to 'third-place) ;;25
         (1C-lead-up :places 2) ;;27
         (1C-cast-off :to 'second-place) ;; 29
         (1C-cross) ;; 31"
  `(todo ,name ,figures))