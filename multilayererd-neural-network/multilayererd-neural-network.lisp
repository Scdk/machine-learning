 ;(ql:quickload :vgplot)

;; Functions that generates the random weigts between -0.5 and 0.5
(defun random-weights (i j)
  (make-random-weights i j '()))
(defun make-random-weights (i j list)
  (cond
    ((eql i 0) list)
    (t (make-random-weights (- i 1) j  (append list (list (make-random-weights-aux j '())))))))
(defun make-random-weights-aux (j list)
  (if (eql j 0) (append list (list 1)) (make-random-weights-aux (- j 1) (append list (list (- (random 0.5) (random 0.5)))))))

;; Definition of the tables of inputs and outputs
(defvar table-in '((1 1 1) (-1 1 1) (1 -1 1) (-1 -1 1)))
(defvar table-out '(-1 1 1 -1))
(defvar test-ij (random-weights 3 2))
(defvar test-jk (random-weights 1 3))

;; Functions that separates w from b
(defun w-separated (ajusted-weights)
  (butlast ajusted-weights))
(defun b-separated (ajusted-weights)
  (first (last ajusted-weights)))

;; Creation of the function that calculates the squared error
;; Number, number -> number
;; (SQUARED-ERROR -1 -0.99542284) -> 1.0475196e-5
(defun squared-error (target output)
  (* 1/2 (expt (- target output) 2)))

;; Creation of the function that calculates the sigmoid function
(defun sigmoid (output)
  (- (/ 2 (+ 1 (exp (* -1 output)))) 1))

;; Creation of the function that calculates the derivate of the sigmoid function
(defun derivated-sigmoid (output)
  (* 1/2 (+ 1 (sigmoid output)) (- 1 (sigmoid output))))


;; (multiple-value-bind (w-ij w-jk cicles error) (TRAINING table-in table-out 100 0.1 100000 0.0001) (declare (ignore w-ij w-jk)) (plot-error cicles error))
(defun plot-error (cicles error)
  (plot-error-aux (loop for x upto cicles collect x) error))
(defun plot-error-aux (x y)
  (progn
	(vgplot:plot x y "r+; Squared Error")
	(vgplot:title "Squared error plot")))

;; Compares the output presented by the program and the target
;; (multiple-value-bind (w-ij w-jk) (TRAINING table-in table-out 3 0.1 100000 0.0001) (COMPARE-OUTPUTS table-in table-out w-ij w-jk))
(defun compare-outputs (inputs targets weights-ij weights-jk)
  (cond
    ((null inputs)
     (format t "~% Weights and bias of the hiden layer: ~% ~%")
     (map 'nil #'(lambda (weights) (format t "Weights: [~{~a~^ ~}] | b: ~a ~%"
                                         (w-separated weights)
                                         (b-separated weights))) weights-ij)
     (format t "~% Weights and bias of the exit layer: ~% ~%")
     (map 'nil #'(lambda (weights) (format t "Weights: [~{~a~^ ~}] | b: ~a ~%"
                                         (w-separated weights)
                                         (b-separated weights))) weights-jk))
    (t
     (format t "Expected: ~a | Result ~a ~%"
             (first targets)
             (foward (y-in-k (first weights-jk) (zj (y-in-j weights-ij (first inputs))))))
     (compare-outputs (rest inputs) (rest targets) weights-ij weights-jk))))

;; Trains the weights based on a number of cicles or minimum squared error
(defun training (inputs targets num-hiden-layer learning-rate max-cicles min-error)
  (let*
      ((weights-jk (random-weights 1 num-hiden-layer))
       (weights-ij (random-weights num-hiden-layer (list-length (first inputs)))))
  (test-cicles inputs targets weights-ij weights-jk learning-rate max-cicles min-error 0 (list 0))))
;; Tests if the number maximum of cicles was reached
(defun test-cicles (inputs targets weights-ij weights-jk learning-rate max-cicles min-error cicles error)
  (cond
    ((or
      (eql cicles max-cicles)
      (and (<= (first (last error)) min-error) (< 0 cicles)))
     (values weights-ij weights-jk cicles (rest error)))
    (t (multiple-value-bind (w-ij w-jk er)
           (ajust-all-weights inputs targets weights-ij weights-jk learning-rate 0)
         (test-cicles inputs targets w-ij w-jk learning-rate max-cicles min-error (1+ cicles) (append error (list er)))))))
;; Ajust all the weights and bias for all the inputs and targets
(defun ajust-all-weights (inputs targets weights-ij weights-jk learning-rate error)
  (cond
    ((null inputs) (values weights-ij weights-jk error))
    (t (let*
           ((y-j (y-in-j weights-ij (first inputs)))
            (y-k (y-in-k (first weights-jk) (zj y-j)))
            (d-k (delta-k (first targets) y-k))
            (d-ij (delta-w-ij learning-rate (delta-j y-j (little-delta-j d-k (first weights-jk))) (first inputs)))
            (d-jk (delta-w-jk learning-rate d-k (zj y-j))))
           (ajust-all-weights (rest inputs) (rest targets)
                       (ajust-ij weights-ij d-ij)
                       (ajust-jk weights-jk d-jk)
                       learning-rate (+ error (squared-error (first targets) (foward y-k))))))))

;; Ajust all the weights and bias of the neuroniums of the exit layer
;; List of lists, list -> list of lists
;; (ajust-jk test-jk (delta-w-jk 1 (DELTA-K (first table-out) (y-in-k (first test-jk) (zj (y-in-j test-ij (first table-in))))) (zj (y-in-j test-ij (first table-in)))))
;; ((0.9510964 0.3300889 0.7426945 0.4461761))
(defun ajust-jk (weights-jk delta-w-jk)
  (mapcar #'(lambda (w-jk)
              (mapcar #'(lambda (w d-jk) (+ w d-jk)) w-jk delta-w-jk))
          weights-jk))

;; Ajust all the weights and bias of the neuroniums of the hiden layer
;; List of lists, list of lists -> list of lists
;; (AJUST-IJ test-ij (delta-w-ij 0.5 (delta-v (y-in-j test-ij (first table-in)) (LITTLE-DELTA-J (DELTA-K (first table-out) (y-in-k (first test-jk) (zj (y-in-j test-ij (first table-in))))) (first test-jk))) (first table-in)))
;; ((0.29795903 0.18949562 1.1187123) (-0.26113257 0.2730902 1.0807483) (-0.09269339 0.13343424 1.0831144))
(defun ajust-ij (weights-ij delta-w-ij)
  (mapcar #'(lambda (w-ij d-ij)
              (mapcar #'(lambda (w d) (+ w d)) w-ij d-ij))
          weights-ij delta-w-ij))

;; Returns a list of lists of the delta-w for each weight and bias of the hiden layer
;; Number, list, list -> list of lists
;; (delta-w-ij 0.5 (delta-v (y-in-j test-ij (first table-in)) (LITTLE-DELTA-J (DELTA-K (first table-out) (y-in-k (first test-jk) (zj (y-in-j test-ij (first table-in))))) (first test-jk))) (first table-in))
;; -> ((0.1187123 0.1187123 0.1187123) (0.08074836 0.08074836 0.08074836) (0.083114445 0.083114445 0.083114445))
(defun delta-w-ij (learning-rate delta-j input)
  (mapcar #'(lambda (d)
              (mapcar #'(lambda (x) (* learning-rate d x)) input))
          delta-j))

;; Returns a list of the delta-v of each neuronium in the hiden layer
;; List, number -> list
;; (delta-j (y-in-j test-ij (first table-in)) (LITTLE-DELTA-J (DELTA-K (first table-out) (y-in-k (first test-jk) (zj (y-in-j test-ij (first table-in))))) (first test-jk)))
;; -> (0.2374246 0.16149671 0.16622889)
(defun delta-j (y-in-j little-delta-j)
  (mapcar #'(lambda (y) (* (derivated-sigmoid y) little-delta-j))
          y-in-j))

;; Number, list -> number
;; (LITTLE-DELTA-J (DELTA-K (first table-out) (y-in-k (first test-jk) (zj (y-in-j test-ij (first table-in))))) (first test-jk))
;; -> -0.3351531
(defun little-delta-j (delta-k weights-jk)
  (apply #'+
         (mapcar #'(lambda (w) (* delta-k w)) weights-jk)))

;; Returns a list of the delta-w-jk of the jk weights and of the bias
;; Number, number, list -> list
;; (delta-w-jk 1 (DELTA-K (first table-out) (y-in-k (first test-jk) (zj (y-in-j test-ij (first table-in))))) (zj (y-in-j test-ij (first table-in))))
;; -> (0.8609802 0.7760887 0.781649 -0.5538239)
(defun delta-w-jk (learning-rate delta-k zj)
  (mapcar #'(lambda (z) (* learning-rate delta-k z)) (append zj '(1))))

;; (DELTA-K (first table-out) (y-in-k (first test-jk) (zj (y-in-j test-ij (first table-in)))))
;; -> -0.5538239
(defun delta-k (target y-in-k)
  (* (- target (sigmoid y-in-k))
     (derivated-sigmoid y-in-k))) 

;; Returns the yk of the MLN
;; Number -> Number
;; (foward (y-in-k (first test-jk) (zj (y-in-j test-ij (first table-in))))) -> -1.6468933
(defun foward (y-in-k)
  (sigmoid y-in-k))

;; Returns the y-in-k
;; List, list -> Number
;; (y-in-k '(1 2 3 1) '(1 2 3)) -> 15
(defun y-in-k (weights-jk zj)
  (+ (b-separated weights-jk)
     (apply #'+ (map 'list  #'* (w-separated weights-jk) zj))))

;; Returns the list of the zj
;; List-> List
(defun zj (y-in-j)
  (mapcar #'(lambda (y) (sigmoid y)) y-in-j))

;; Returns the list of y-in-j
;; List of lists, list -> List
;; (y-in-j test-ij (first table-in)) -> (1.25003 0.85046095 0.87451196)
(defun y-in-j (weights-ij input)
  (mapcar #'(lambda (w-ij)
              (+ (b-separated w-ij)
                 (apply #'+ (map 'list  #'* (w-separated w-ij) input))))
          weights-ij))
