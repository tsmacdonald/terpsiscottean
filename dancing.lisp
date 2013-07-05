(in-package #:terpsiscottean)

;;
;; Figure
;;

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
			 :steps (lambda (floor stream fmt-string &key (args nil))
				  (declare (ignorable floor))
				  (apply #'format (append (list stream fmt-string)
							  args)))))
    table))

;;
;; Dancer
;;

(deftype gender ()
  '(member man woman))

(deftype direction ()
  '(member up down in out left right first-corner second-corner))

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

;;
;; Dance Space
;;

(defclass dance-space ()
  ((height :initarg :height :initform 1 :accessor height)
   (width :initarg :width :initform 1 :accessor width)
   (grid :accessor grid))
  (:documentation "A grid in which dancers can move"))

(defmethod initialize-instance :after ((d dance-space) &rest args)
  "Initialize the grid to be the right size, based on the height and width of the dance space."
  (declare (ignorable args))
  (setf (slot-value d 'grid) (make-array (list (width d) (height d))
					 :element-type '(or dancer null)
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

;;
;; Methods
;;

(defgeneric dance (steps floor &key face step args)
  (:documentation "Execute the dance defined by STEPS on FLOOR."))

(defmethod dance ((figure figure) (floor dance-space) &key (face 'in) (step 'skip-change) (args nil))
  (funcall (steps figure) floor :face face :step step :args args))

(defmethod dance ((figure figure) (floor null) &key (face 'in) (step 'skip-change) (args nil))
  (funcall (steps figure) floor :face face :step step :args args))

(defmethod dance ((figure-name symbol) (floor dance-space) &rest rest)
  (apply #'dance (append (list (gethash figure-name *figures*)) rest)))
