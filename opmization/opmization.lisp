;; Calculates the Rosenbrock Function for a list of values
;; List -> number
;; (ROSENBROCK '(1 2))
;; -> 201
(defun rosenbrock (list)
  (let ((result 0))
    (loop
       for i from 0 upto (1- (length list))
       if (not (eql i (1- (length list))))
       do (setf result (+ result
	     (single-rosenbrock (nth i list) (nth (1+ i) list))))
       else
       return result)))

;; Calculates the Rosenbrock Function for a two values
;; Number, number -> number
(defun single-rosenbrock (x1 x2)
  (+ (expt (- 1 x1) 2) (* 100 (expt (- x2 (expt x1 2)) 2))))

;; Generates a certain amount of inputs
;; Number, number, number, number -> List of lists
(defun generate-inputs (num-inputs num-vars min max)
  (loop
     for i from 1 upto num-inputs
     collect (loop
		for j from 1 upto num-vars
		collect (- (random max) (random (- min))))))

;; Produces a list of the mutated inputs
;; List of lists, number -> list of lists
(defun mutation (inputs F)
  (let ((size (length inputs)))
    (mapcar #'(lambda (input)
		(mapcar #'+ input
			(mapcar #'(lambda (x) (* F x))
				(mapcar #'-
					(nth (random size) inputs)
					(nth (random size) inputs)))))
	    inputs)))

;; Generates the trial vector, that is a crossover between the inputs and the mutation vector
;; List of lists, number, number -> list of lists
(defun trial-vector (inputs F CR)
  (mapcar #'(lambda (input mutation)
	    (if (<= (random 1.0) CR) mutation input))
	  inputs (mutation inputs F)))

;; Optimizes the values and than finds the values that produce the smallest value of rosenbrock
;; List of lists
(defun optimize-rosenbrock (inputs F CR max-generations)
  (labels ((evolute (generation num-generation)
	     (if (not (eql num-generation max-generations))
		 (evolute
		  (mapcar #'(lambda (gen final)
			      (if (< (rosenbrock gen) (rosenbrock final)) gen final))
			  generation (trial-vector generation F CR)) (1+ num-generation))
		 (find-smallest-value generation))))
    (evolute inputs 0)))

;; Finds the values that produce the smallest value of rosenbrock
;; List of lists -> list
(defun find-smallest-value (list)
  (labels ((rec (lst smallest)
	     (if (not (null lst))
		 (rec (rest lst)
		      (if (< (rosenbrock (first lst)) (rosenbrock smallest)) (first lst) smallest))
		 smallest)))
    (rec list (first list))))
		 
;; Definition of the variables
(defvar inputs (generate-inputs 100 2 -1.0 2.0))
