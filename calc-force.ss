#!/bin/chez-scheme --script

(load "lib/nmaths.ss")
(load "lib/plot.ss")
(import (nmaths))
(import (plot))

(random-seed 1)

(define pi 3.141592653589793)

(define randomwalk
  ; gives a list with the 'N' angles of the joints
  (lambda (N)
    (let ([anglemax (* 2 pi)])
      (let rec ([angles (list)] 				; create variable for list
		[n N])
	(if (= n 0)
	    angles 						; return list
	    (rec (cons (random anglemax) angles) (- n 1))))))) 	; recursion step, insert random angle into 'angles'

(define distance
  ; calculates the distance between the first and last joint
  ; first joint is at origin
  ; because of rotational symmetry only the radius/distance from origin of the last joint is relevant
  (lambda (l angles)
    (let rec ([x 0] 										; initialize cartesian coordinates
	      [y 0]
	      [angles angles]) 									; pass list 'angles'
      (if (null? angles)
	  (sqrt (+ (* x x) (* x x))) 								; return distance
	  (rec (+ x (* (cos (car angles)) l)) (+ y (* (sin (car angles)) l)) (cdr angles)))))) 	; recursion step, call with new starting coordinates

(define make-probability-histogram
  ; returns a vector that represents a histogram
  ; each index represents a section on the number line (for each distance)
  ; the value at each index represents the probability-density for this distance 'L'
  (lambda (count-steps grid-steps l N)
    (let ([counts (make-vector grid-steps 0)] 							; create vector 'counts'
	  [grid-length (/ (* N l) grid-steps)])
      (let rec ([step count-steps])
	(if (= step 0)
	    (vector-map (lambda (x) 								; normalize and return distribution
			  (/ x count-steps))
			counts)
	    (begin (let ([ref (exact (floor (/ (distance l (randomwalk N)) grid-length)))]) 	; calculate index
		     (when (< ref (vector-length counts))
		       (vector-set! counts 							; add 1 to vector-entry
				    ref
				    (+ (vector-ref counts ref) 1))))
		   (rec (- step 1)))))))) 							; recursion step

(define make-force-list
  (lambda (count-steps grid-steps l N tau)
    (let* ([hist (make-probability-histogram count-steps grid-steps l N)]
	   [ls (vector->list (vector-map (lambda (x)
					   (* tau (log (/ 1 x))))
					 hist))]
	   [grid-length (/ (* N l) grid-steps)])
      (let list-derivate ([ls ls])
	(if (null? (cdr ls))
	    '()
	    (cons (/ (- (cadr ls) (car ls)) grid-length) (list-derivate (cdr ls))))))))

(define make-force-table
  (lambda (count-steps grid-steps l N tau)
    (let ([grid-length (/ (* N l) grid-steps)])
      (let rec ([ls (make-force-list count-steps grid-steps l N tau)]
		[x (real->flonum (/ grid-length 2))])
	(if (null? ls)
	    '()
	    (cons (list x (car ls)) (rec (cdr ls) (+ x grid-length))))))))

(let ([l 1]
      [tau 1]
      [count-steps 1000000]
      [grid-steps 30])

  (list-file "table-3-L-force" (make-force-table count-steps grid-steps l 3 tau))
  (list-file "table-5-L-force" (make-force-table count-steps grid-steps l 5 tau))
  (list-file "table-8-L-force" (make-force-table count-steps grid-steps l 8 tau)))
