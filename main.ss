#!/bin/chez-scheme --script

(random-seed 1)

(define pi 3.141592653589793)

(define randomwalk
  ; gives a list with the 'N' angles of the joints
  (lambda (N)
    (let rec ([angles (list)]
	      [n N])
      (if (= n 0)
	  angles
	  (rec (cons (random (* 2 pi)) angles) (- n 1))))))

(define distance
  ; calculates the distance between the first and last joint
  ; first joint is at origin
  ; because of rotational symmetry only the radius/distance from origin of the last joint is relevant
  (lambda (l angles)
    (let rec ([x 0]
	      [y 0]
	      [angles angles])
      (if (null? angles)
	  (sqrt (+ (expt x 2) (expt y 2)))
	  (rec (+ x (* (cos (car angles)) l)) (+ y (* (sin (car angles)) l)) (cdr angles))))))

(define probability-density
  ; returns a vector that represents a histogram
  ; each index represents a section on the number line (for each distance)
  ; the value at each index represents the probability-density for this distance 'L'
  (lambda (count-steps grid-steps maxL l N)
    (let ([counts (make-vector grid-steps 0)]
	  [eps (/ maxL grid-steps)])
      (let rec ([step count-steps])
	(if (= step 0)
	    (vector-map (lambda (x)
			  (/ (* count-steps eps)))
			counts)
	    (let ([ref (floor (/ (distance l (randomwalk N)) eps))])
	      (vector-set! counts
			   ref
			   (+ (vector-ref counts ref) 1))))))))

(let ([l 1])
  (display (distance l (randomwalk 7)))
  (newline)
  (display (probability-density 100 10 10 1 3))
  (newline)
  (display "moin")
  )
