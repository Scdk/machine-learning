;; Definition of the table of the database
(defvar table '((0.0 1.0)
				(0.5 1.0)
				(1.0 1.0)
				(1.5 1.0)
				(2.0 1.0)
				(2.5 1.0)
				(3.0 1.0)
				(3.5 1.0)
				(4.0 1.0)
				(4.5 1.0)
				(5.0 1.0)))

(defvar table-output '(2.26 3.80 4.43 5.91 6.18 7.26 8.15 9.14 10.87 11.58 12.55))

(defun random-weights ()
  (list (make-RANDOM-WEIGHTS) 1))

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

;; Creation of the linear regression function
(defun linear-regression (x a b)
  (+ (* a (first x)) b))

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
	 (format t "Expected output: ~2$ | Obtained output: ~2$ ~%"
		 (linear-regression (first inputs)
				    (a-calculus table TABLE-OUTPUT)
				    (b-calculus (a-calculus table TABLE-OUTPUT) table TABLE-OUTPUT))
			 (linear-regression (first inputs) (first weights) (second weights)))
	 (compare-exits-aux (rest inputs) weights (rest outputs) max-cicles
						(append result
							(list (linear-regression (first inputs) (first weights) (second weights))))))))

(defun sum (list)
  (apply #'+ list))

(defun avarage (list)
  (/ (sum list) (list-length list)))

(defun a-calculus (inputs outputs)
  (let* ((x (map 'list #'first inputs)) (y outputs))
    (/
      (sum (map 'list #'(lambda (i j) (* (- i (avarage x)) (- j (avarage y)))) x y)) 
      (sum (map 'list #'(lambda (i) (expt (- i (avarage x)) 2)) x)))))

(defun b-calculus (a inputs outputs)
  (let* ((avg-x (avarage (map 'list #'first inputs))) (avg-y (avarage outputs)))
    (- avg-y (* a avg-x))))

(defun pearson (inputs outputs)
  (let* ((x (map 'list #'first inputs)) (y outputs))
     (/
      (sum (map 'list #'(lambda (i j) (* (- i (avarage x)) (- j (avarage y)))) x y))
      (* (sqrt (sum (map 'list #'(lambda (i) (expt (- i (avarage x)) 2)) x)))
	 (sqrt (sum (map 'list #'(lambda (i) (expt (- i (avarage y)) 2)) y)))))))

(defun determination (inputs outputs)
  (expt (pearson inputs outputs) 2))
