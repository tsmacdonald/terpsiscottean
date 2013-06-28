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
   the end of the figure. Supported values are specified by the direction type.
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

(defmacro defdance (name &body figures)
  "Defines a dance, which is a named sequence of figures.
   For example:
       (defdance \"Tim's Silly Little Dance\"
         (1C-set)
         (1C-cast-off :to second-place)
         (1C-set)
         (1C-cross)
         (sideline-reel) ;;9
         (rights-and-lefts) ;;17
         (six-hands-round)) ;;25"
  'todo)

(deftype gender ()
  '(member man woman))

(deftype direction ()
  '(member up down in out left right first-corner second-corner))

(defclass dancer ()
  ((gender :initarg :gender :initform 'man :accessor gender :type gender)
   (facing :initarg :facing :initform 'up :accessor facing :type direction))
  (:documentation "Dancers have genders and face a particular direction. They don't know where they are."))

(defclass dance-space ()
  ((height :initarg :height :initform 9 :accessor height)
   (width :initarg :width :initform 6 :accessor width)
   (grid :initform :reader grid))
  (:documentation "A grid in which dancers can move"))

(defmethod initialize-instance :after ((d dance-space) &rest args)
  (declare (ignorable args))
  (setf (slot-value d 'grid) (make-array (list (width d) (height d)) :element-type 'dancer :adjustable nil)))