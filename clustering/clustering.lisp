;(ql:quickload :vgplot)

;; Colects all the elements of the base file and transforms they in a list of lists
;; String -> List of lists
(defun file-to-list (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect (mapcar #'string-first (uiop::split-string line :separator '(#\ ))))))

;; Gets the first word from a string and returns it
;; String -> Value
(defun string-first (string)
  (with-input-from-string (str string) (read str nil nil)))

;; Create a list of k's
;; List of lists, number -> list of lists
(defun make-random-k-old (data number)
  (labels ((mk-rdm (num list)
	     (cond
	       ((eql 0 num) list)
	       (t (mk-rdm (1- num) (append list (list (random-k data))))))))
    (mk-rdm number '())))

;; Create a list of k's
;; List of lists, number -> list of lists
(defun make-random-k (data number)
  (labels ((mk-rdm (num list)
	     (cond
	       ((eql 0 num) list)
	       (t (mk-rdm (1- num) (append list (list (list (random 10) (random 10)))))))))
    (mk-rdm number '())))

;; Generates a random value of  a single k
;; List of lists -> list
(defun random-k (data)
  (let* ((min (min-max data #'min)) (max (min-max data #'max)))
    (list
     (-  (random max) (random (* -1 min)))
     (-  (random max) (random (* -1 min))))))

;; Apply's min or max funcition to all data
;; List of lists, function -> number
(defun min-max (data fn)
  (apply fn (mapcar #'(lambda (x) (apply fn x)) data)))

;; Calculates the distance berween two points
;; List, list -> Number
;; (DISTANCE-POINTS '(3 4) '(5 6))
;; -> 2.828427
(defun distance-points (p1 p2)
  (sqrt (+ (expt (- (first p1) (first p2)) 2) (expt (- (second p1) (second p2)) 2))))

;; Creates the b list
;; List of lists, list of lists -> list
(defun make-b (data list-of-k)
  (labels ((find-fitting-k (d b)
	     (cond
	       ((null d) b)
	       (t (find-fitting-k (rest d) (append b (list
						      (loop
							 for i from 0 upto (1- (list-length list-of-k))
							 when (eql
							     (distance-points (nth i list-of-k) (first d))
							     (apply #'min (mapcar #'(lambda (k) (distance-points (first d) k)) list-of-k)))
							 do (return (1+ i))))))))))
    (find-fitting-k data '())))

;; Choses a number of entrances
;; List of lists, number -> List of lists
(defun make-random-k++ (data num)
  (loop
     for i upto (1- num)
     collect (nth (random (1- (list-length data))) data)))

;; Returns a list of lists of the distances of the agrupated-data to their k's
;; List of lists, list of lists, list -> List of lists
(defun distance-k (agrupated-data list-of-k)
  (mapcar #'(lambda (d k)
		(mapcar #'(lambda (x) (distance-points k x)) d))
	  agrupated-data list-of-k))

;; List, list -> list
(defun ponderate (data distances)
  (apply #'append (mapcar #'(lambda (d dist) (make-list (round (expt (* 10 dist) 2)) :initial-element d))
	  data distances)))

;; Choses the k's for the k-means++
;; List of lists, list of lists, list -> list of lists
(defun chose-k++ (data list-of-k)
  (let*
      ((list-of-b (make-b data list-of-k))
       (agrupated-data (agrupate-data data list-of-k list-of-b))
       (distance (distance-k agrupated-data list-of-k)))
    (mapcar #'(lambda (d dist)
		(nth (random (1- (list-length (ponderate d dist))))
		     (ponderate d dist)))
	    agrupated-data distance)))

;; Calculates the squared error
;; List of lists, list of lists -> number
(defun squared-error (agrupated-data list-of-k)
  (let* ((list-of-errors
	  (mapcar #'(lambda (x) (expt x 2))
		  (apply #'append (distance-k agrupated-data list-of-k)))))
   (apply #'+ list-of-errors)))

;; Calculates the final values of K
;; List of lists, list of lists -> list of lists, list
(defun k-means (data list-of-k)
  (labels ((condition-test (k error)
	     (let* ((list-of-b (make-b data k)) (agrupated-data (agrupate-data data k list-of-b)))
	       (if (not (equal k (ajust-k data k list-of-b)))
		   (condition-test (ajust-k data k list-of-b) (append error (list (squared-error agrupated-data k))))
		   (values k list-of-b error)))))
    (condition-test list-of-k '())))

;; Ajusts the k's to the new values
;; List of lists, list of lists, list -> list of lists
;; (AJUST-K '((0 1) (1 2) (2 3) (3 4)) '((0 1) (1 2)) '(1 2 1 2))
;; -> ((1 2) (2 3))
(defun ajust-k (data list-of-k list-of-b)
  (let* ((agrupated-data (agrupate-data data list-of-k list-of-b)))
    (mapcar #'(lambda (d k)
		(if (not (null d)) (list
				    (/ (apply #'+ (mapcar #'first d)) (list-length d))
				    (/ (apply #'+ (mapcar #'second d)) (list-length d)))
		    k))
	    agrupated-data list-of-k)))

;; Agrupates the data using b
;; List of lists, list of lists, list -> list of lists
;; (agrupate-data '((0 1) (1 2) (2 3) (3 4)) '((0 1) (1 2)) '(1 2 1 2))
;; -> (((0 1) (2 3)) ((1 2) (3 4)))
(defun agrupate-data (data list-of-k list-of-b)
  (labels ((foo (num result)
	     (cond
	       ((eql num (1+ (list-length list-of-k))) result)
	       (t (foo (1+ num) (append result (list
						(labels ((foo2 (d b list)
							   (cond
							     ((null d) list)
							     (t (foo2 (rest d) (rest b) (if (eql (first b) num) (append list (list (first d))) list))))))
						  (foo2 data list-of-b '())))))))))
    (foo 1 '())))

;; Plots the data points with their respectives centers
;; List of lists, list of lists -> Graph
;; (plot data (CHOSE-K++ data LIST-OF-K++ (make-b data LIST-OF-K++)))
;; (plot data (make-random-k data 5))
(defun plot (data list-of-k)
  (multiple-value-bind (k b) (k-means data list-of-k)
    (let* ((agrupated-data (agrupate-data data k b)))
      (vgplot:plot
       (mapcar #'first (first agrupated-data)) (mapcar #'second (first agrupated-data)) "r+; k1"
       (mapcar #'first (second agrupated-data)) (mapcar #'second (second agrupated-data)) "b+; k2"
       (mapcar #'first (third agrupated-data)) (mapcar #'second (third agrupated-data)) "g+; k3"
       (mapcar #'first (fourth agrupated-data)) (mapcar #'second (fourth agrupated-data)) "c+; k4"
       (mapcar #'first (fifth agrupated-data)) (mapcar #'second (fifth agrupated-data)) "b+; k5"
       (mapcar #'first k) (mapcar #'second k) "k+; centroides"))))

;; Plots the error
;; List -> Graph
;; (multiple-value-bind (k b error) (k-means data (make-random-k data 5)) (declare (ignore k b)) (plot-error error))
;; (multiple-value-bind (k b error) (k-means data (chose-k++ data (make-random-k++ data 5))) (declare (ignore k b)) (plot-error error))
(defun plot-error (err)
  (vgplot:plot (loop for i from 0 upto (1- (list-length err)) collect i) err "r+; Squared error"))

;; Definition of the variables
(defvar data (file-to-list "./class/data"))
(defvar list-of-k (make-random-k data 5))
(defvar list-of-k++ (make-random-k++ data 5))
