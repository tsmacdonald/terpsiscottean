;;;; terpsiscottean.lisp

(in-package #:terpsiscottean)

(defclass figure ()
  ((name :initarg :name :accessor name :documentation "The human-readable name of the figure")
   (duration :initarg :duration :accessor duration)
   (steps :initarg :steps :accessor steps))
  (:documentation
   "A figure; part of a dance. The most important part is the steps slot (see documentation
    elsewhere for details about what that entails)."))

(defmethod print-object ((figure figure) stream)
  (format stream "#<~S, a ~A-bar figure~A.>" (name figure) (duration figure)
	  (if (steps figure) "" " with no steps")))

(defun make-figures-table ()
  (let ((table (make-hash-table)))
    (setf (gethash 'debug table)
	  (make-instance 'figure
			 :steps (lambda (floor stream fmt-string &rest args)
				  (declare (ignorable floor))
				  (apply #'format (append (list stream fmt-string)
							  args)))))
    table))
				  
(defparameter *figures* (make-figures-table)
  "A mapping of figure names to figure implementations (functions)")


(defclass dancer ()
  ((gender :initarg :gender :initform 'man :accessor gender :type gender)
   (facing :initarg :facing :initform 'in :accessor facing :type direction)
   (set-number :initarg :set-number :initform 0 :accessor set-number :type integer))
  (:documentation "Dancers have genders and face a particular direction."))

(defmethod print-object ((dancer dancer) stream)
  (format stream "~A"
	  (if (eql (gender dancer) 'man)
	      "M"
	      "W")))

(defclass dance-space ()
  ((height :initarg :height :initform 0 :accessor height)
   (width :initarg :width :initform 0 :accessor width)
   (grid :accessor grid))
  (:documentation "A grid in which dancers can move"))

(defmethod initialize-instance :after ((d dance-space) &rest args)
  (declare (ignorable args))
  (setf (slot-value d 'grid) (make-array (list (width d) (height d))
					 :element-type 'dancer
					 :adjustable nil
					 :initial-element nil)))
(defun make-dance-space (&key (width 6) (height 9))
  "Makes a new dance space with the given dimensions."
  (make-instance 'dance-space :width width :height height))

(defmethod print-object ((floor dance-space) stream)
  (let ((grid (grid floor)))
    (loop for y below (array-dimension grid 1) doing
	 (format stream "~&")
	 (loop for x below (array-dimension grid 0) doing
	      (let ((cell (aref grid x y)))
		(format stream "[~A]" (or cell "_")))))))

(defgeneric dance (steps floor &key face step ))

(defmethod dance ((figure figure) (floor dance-space) &key (face 'in) (step 'skip-change))
  (funcall (steps figure) floor))
  

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
			  :steps (lambda (floor &key (face 'in) (step 'skip-change))
				   (declare (ignorable floor face step))
				   ,@(mapcar
				      (lambda (command)
					`(dance (gethash ',(first command) *figures*) ,@(rest command)))
				      (nreverse commands)))))))

;;add boilerplate to defun that assigns dancer variable and deals with common kwargs
;;dance-space will have to have a mapping from dancer name (1W or what have you) to coords

(deffigure right "Step right"
    (1
     (1M
      (debug t "~&Going right!"))))
  
(deffigure left "Step left"
    (1
     (1M
      (debug t "~&Going left!"))))

(defun test-figure-def ()
  (deffigure 1C-set "1C set"
    (2
     (1M
      (right :face 'in :step 'pas-de-basque)
      (left :face 'in :step 'pas-de-basque))
     (1W
      (right :face 'in :step 'pas-de-basque)
      (left :face 'in :step 'pas-de-basque)))))

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

(deftype gender ()
  '(member man woman))

(deftype direction ()
  '(member up down in out left right first-corner second-corner))


(defgeneric add-couples (dance-space &key number starting-position
				     horizontal-gap vertical-gap))

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
    floor))
