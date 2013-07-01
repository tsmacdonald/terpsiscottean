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
   (facing :initarg :facing :initform 'in :accessor facing :type direction))
  (:documentation "Dancers have genders and face a particular direction. They don't know where they are."))

(defclass dance-space ()
  ((height :initarg :height :initform 0 :accessor height)
   (width :initarg :width :initform 0 :accessor width)
   (grid :accessor grid))
  (:documentation "A grid in which dancers can move"))

(defmethod initialize-instance :after ((d dance-space) &rest args)
  (declare (ignorable args))
  (setf (slot-value d 'grid) (make-array (list (width d) (height d)) :element-type 'dancer :adjustable nil :initial-element nil)))

(defun make-dance-space (&key (width 6) (height 9))
  "Makes a new dance space with the given dimensions."
  (make-instance 'dance-space :width width :height height))

(defgeneric add-couples (dance-space &key &allow-other-keys))

(defmethod add-couples ((floor dance-space)
			&key (number 4) (starting-position (cons 1 1))
			(horizontal-gap 1) (vertical-gap 1))
  (unless (and
	   (>= (array-dimension (grid floor) 0)
	       (+ (car starting-position)
		  (* horizontal-gap 3) ;;2 for inter-dancer space; 1 for behind women
		  1 ;; man dancers
		  1)) ;; woman dancers
	   (>= (car starting-position) horizontal-gap))
    (error "dance space too narrow for specified parameters"))
  (unless (>= (array-dimension (grid floor) 1)
	      (+ (cdr starting-position)
		 (* (1+ vertical-gap) number))) ;;1+ to account for dancer, not just gap
    (error "Dance space too short for specified parameters"))

  (let ((ys (loop for row below number
		   collecting (+ (* (1+ vertical-gap) row)
				 (cdr starting-position)))))
    (dolist (y ys)
      (setf (aref (grid floor) (car starting-position) y)
	    (make-instance 'dancer :gender 'man))
      (setf (aref (grid floor)
		  (+ (* 2 horizontal-gap) (car starting-position))
		  y)
	    (make-instance 'dancer :gender 'woman)))
    t))


(defmethod print-object ((floor dance-space) stream)
  (let ((grid (grid floor)))
    (loop for y below (array-dimension grid 1) doing
	 (format stream "~&")
	 (loop for x below (array-dimension grid 0) doing
	      (let ((cell (aref grid x y)))
		(format stream "[~A]" (if cell (print-object cell stream) "_")))))))

(defclass print-object-tricker () ())
(defmethod print-object ((p print-object-tricker) stream)
  (format stream ""))
(defmethod print-object ((dancer dancer) stream)
  (format stream "~A"
	  (if (eql (gender dancer) 'man)
	      "M"
	      "W"))
  (make-instance 'print-object-tricker))