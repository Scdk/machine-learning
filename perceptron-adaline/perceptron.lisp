;(ql:quickload :vgplot)

;; Definition of the table of the database
(defvar table '((1.0	1.0 1.0)
				(1.1	1.5 1.0)
				(2.5	1.7 1.0)
				(1.0	2.0 1.0)
				(0.3	1.4 1.0)
				(2.8	1.0 1.0)
				(0.8	1.5 1.0)
				(2.5	0.5 1.0)
				(2.3	1.0 1.0)
				(0.5	1.1 1.0)
				(1.9	1.3 1.0)
				(2.0	0.9 1.0)
				(0.5	1.8 1.0)
				(2.1	0.6 1.0)))

(defvar table-output '(1 1 -1 1 1 -1 1 -1 -1 1 -1 -1 1 -1))

;; Functiions that separates w from b
(defun w-separated (ajusted-weights)
  (butlast ajusted-weights))
(defun b-separated (ajusted-weights)
  (last ajusted-weights))

;; Creation of the function that calculates yliquid
(defun yliquid (ajusted-weights inputs)
  (+ (first (b-separated ajusted-weights))
      (apply #'+ (map 'list  #'* (w-separated ajusted-weights) (w-separated inputs)))))

;; Creation of the unit step function
(defun unit-step (inputs ajusted-weights threshold)
  (if (>= (yliquid ajusted-weights inputs) threshold) 1 -1))

;; Creation of the funciton that ajusts the weights values
(defun ajust-weights (input weights output learning-rate)
  (map 'list #'+ weights
       (map 'list #'(lambda (x) (* (* x output) learning-rate)) input)))

;; Apply the ajust-weights fuction in to a list of inputs and outputs
(defun apply-ajust-weights (inputs weights outputs learning-rate threshold)
  (cond
    ((and (null inputs) (null outputs)) weights)
    ((eql
      (first outputs)
      (unit-step (first inputs) weights threshold))
     (apply-ajust-weights (rest inputs) weights (rest outputs) learning-rate threshold))
    (t (apply-ajust-weights
		(rest inputs)
		(ajust-weights (first inputs) weights (first outputs) learning-rate)
		(rest outputs)
		learning-rate
		threshold))))

;; Creation of the perceptron function
(defun perceptron (inputs weights outputs learning-rate threshold )
  (if (equal
	   (w-separated weights)
	   (w-separated (apply-ajust-weights
					 inputs weights outputs learning-rate threshold)))
       weights
      (perceptron
       inputs
       (apply-ajust-weights inputs weights outputs learning-rate threshold)
       outputs
       learning-rate
       threshold)))

;; Creation of the function that compares the expected and the obtained outputs
(defun compare-exits (inputs weights outputs learning-rate threshold)
  (compare-exits-aux
   inputs
   (perceptron inputs weights outputs learning-rate threshold)
   outputs
   threshold
   '()))
(defun compare-exits-aux (inputs weights outputs threshold result)
  (cond
	((null inputs) (format t "Weights: [~{~a~^ ~}] || b: ~a ~%"
						   (w-separated weights)
						   (first (b-separated weights))))
	(t
	 (format t "Expected output: ~a | Obtained output: ~a ~%"
			 (first outputs)
			 (unit-step (first inputs) weights threshold))
	 (compare-exits-aux (rest inputs) weights (rest outputs) threshold
						(append result
								(list (unit-step (first inputs) weights threshold)))))))

(defun plot (x y)
  (progn
	(vgplot:plot x y "k+;Points"
				 '(0.3 2.8)
				 (list (equation
						(perceptron table '(0 0 0) table-output 1 0) 0.3)
					   (equation
						(perceptron table '(0 0 0) table-output 1 0) 2.8))
				 "r-;Boundary")
	(vgplot:title "Perceptron plot")))

(defun apply-plot (inputs)
  (plot (map 'list #'first inputs) (map 'list #'second inputs)))

(defun equation (weights x1)
  (destructuring-bind  (w1 w2 b) weights
	(/ (- (* (* w1 -1) x1) b) w2)))
