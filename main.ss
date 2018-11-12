#!/bin/chez-scheme --script

(random-seed 1)

(define pi 3.141592653589793)

(define randomwalk
  ; gives a list with the 'N' angles of the joints
  (lambda (N)
    (let rec ([angles (list)] 					; create variable for list
	      [n N])
      (if (= n 0)
	  angles 						; return list
	  (rec (cons (random (* 2 pi)) angles) (- n 1)))))) 	; recursion step, insert random angle into 'angles'

(define distance
  ; calculates the distance between the first and last joint
  ; first joint is at origin
  ; because of rotational symmetry only the radius/distance from origin of the last joint is relevant
  (lambda (l angles)
    (let rec ([x 0] 										; initialize cartesian coordinates
	      [y 0]
	      [angles angles]) 									; pass list 'angles'
      (if (null? angles)
	  (sqrt (+ (expt x 2) (expt y 2))) 							; return distance
	  (rec (+ x (* (cos (car angles)) l)) (+ y (* (sin (car angles)) l)) (cdr angles)))))) 	; recursion step, call with new starting coordinates

(define probability-histogram
  ; returns a vector that represents a histogram
  ; each index represents a section on the number line (for each distance)
  ; the value at each index represents the probability-density for this distance 'L'
  (lambda (count-steps grid-steps l N)
    (let ([counts (make-vector grid-steps 0)] 							; create vector 'counts'
	  [grid-length (/ (* N l) grid-steps)])
      (let rec ([step count-steps])
	(if (= step 0)
	    (begin (vector-map (lambda (x) 							; normalize distribution
				 (/ (* count-steps grid-length)))
			       counts)
		   counts) 									; return vector
	    (begin (let ([ref (exact (floor (/ (distance l (randomwalk N)) grid-length)))])
		     (when (< ref (vector-length counts))
		       (vector-set! counts 							; add 1 to vector-entry
				    ref
				    (+ (vector-ref counts ref) 1))))
		   (rec (- step 1)))))))) 							; recursion step

(define force-calc
  (lambda (probability)
    (log)))

(let ([l 1]
      [N 5])                ; max 1000000 50 ?
  (display (probability-histogram 1000000 50 l N))
  (newline)
  (display "moin")
  )
