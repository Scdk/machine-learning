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

(defun random-weights ()
  (list (make-RANDOM-WEIGHTS) (make-RANDOM-WEIGHTS) 1))

(defun make-random-weights ()
  (- (random 0.5) (random 0.5)))

;; Functiions that separates w from b
(defun w-separated (ajusted-weights)
  (butlast ajusted-weights))
(defun b-separated (ajusted-weights)
  (last ajusted-weights))

;; Creation of the function that calculates yliquid
(defun yliquid (ajusted-weights input)
  (+ (first (b-separated ajusted-weights))
	 (apply #'+ (map 'list  #'* (w-separated ajusted-weights) (w-separated input)))))

(defun unit-step (inputs ajusted-weights threshold)
  (if (>= (yliquid ajusted-weights inputs) threshold) 1 -1))

(defun squared-error (output weights input)
  (expt (- output (yliquid weights input)) 2))

(defun apply-squared-error (outputs weights inputs)
  (apply-squared-error-aux outputs weights inputs 0))

(defun apply-squared-error-aux (outputs weights inputs result)
  (cond
	((and (null outputs) (null inputs)) result)
	(t (apply-squared-error-aux
		(rest outputs)
		weights
		(rest inputs)
		(+ result (squared-error (first outputs) weights (first inputs)))))))

(defun ajust-weights (input weights output learning-rate)
  (map 'list #'+ weights
       (map 'list #'(lambda (x) (* (* x (- output (yliquid weights input))) learning-rate)) input)))

(defun apply-ajust-weights (inputs weights outputs learning-rate)
  (cond
	((and (null inputs) (null outputs)) weights)
	(t (apply-ajust-weights
		(rest inputs)
		(ajust-weights (first inputs) weights (first outputs) learning-rate)
		(rest outputs)
		learning-rate))))

(defun adaline(inputs weights outputs learning-rate max-cicles)
  (cond
	((eql max-cicles 0) weights)
	(t (adaline
		inputs
		(apply-ajust-weights inputs weights outputs learning-rate)
		outputs
		learning-rate
		(- max-cicles 1)))))

(defun adaline-plot (inputs weights outputs learning-rate max-cicles)
 (adaline-aux-plot inputs weights outputs learning-rate max-cicles 0 '()))

(defun adaline-aux-plot (inputs weights outputs learning-rate max-cicles cicles error)
  (cond
	((eql max-cicles 0) (list cicles error))
	(t (adaline-aux-plot
		inputs
		(apply-ajust-weights inputs weights outputs learning-rate)
		outputs
		learning-rate
		(- max-cicles 1)
		(+ cicles 1)
		(append error (list (apply-squared-error outputs weights inputs)))))))

(defun plot (x y)
  (progn
	(vgplot:plot x y "k+;Points"
				 '(0.3 2.8)
				 (list (equation
						(adaline table (RANDOM-WEIGHTS)
								 TABLE-OUTPUT 0.01 1000) 0.3)
					   (equation
						(adaline table (RANDOM-WEIGHTS)
								 TABLE-OUTPUT 0.01 1000) 2.8))
				 "r-;Boundary")
	(vgplot:title "Adaline plot")))

(defun apply-plot (inputs)
  (plot (map 'list #'first inputs) (map 'list #'second inputs)))

(defun plot-error (x y)
  (progn
	(vgplot:plot x y "r+; Squared Error")
	(vgplot:title "Squared error plot")))

(defun apply-plot-error (list)
  (plot-error (loop for x upto (first list) collect x) (second list)))

(defun equation (weights x1)
  (destructuring-bind  (w1 w2 b) weights
	(/ (- (* (* w1 -1) x1) b) w2)))

(defun compare-exits (inputs weights outputs learning-rate max-cicles)
  (compare-exits-aux
   inputs
   (adaline inputs weights outputs learning-rate max-cicles)
   outputs
   max-cicles
   '()))
(defun compare-exits-aux (inputs weights outputs max-cicles result)
  (cond
	((null inputs) (format t "Weights: [~{~a~^ ~}] || b: ~a ~%"
						   (w-separated weights)
						   (first (b-separated weights))))
	(t
	 (format t "Expected output: ~a | Obtained output: ~a ~%"
			 (first outputs)
			 (unit-step (first inputs) weights 0))
	 (compare-exits-aux (rest inputs) weights (rest outputs) max-cicles
						(append result
								(list (unit-step (first inputs) weights 0)))))))
