#!/bin/chez-scheme --script

(load "lib/nmaths.ss")
(load "lib/plot.ss")
(import (nmaths))
(import (plot))

(random-seed 3)

(define pi invpi)

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
  ; returns a list with forces calculated from 'make-probability-histogram'
  (lambda (count-steps grid-steps l N tau)
    (let* ([hist (make-probability-histogram count-steps grid-steps l N)]
	   [ls (vector->list (vector-map (lambda (x) 					; prepare list from vector, do some easy calculations
					   (* tau (log (/ 1 x))))
					 hist))]
	   [grid-length (/ (* N l) grid-steps)])
      (let list-derivate ([ls ls]) 							; recursive function that creates a new list with the difference quotient of the previous list
	(if (null? (cdr ls))
	    '()
	    (cons (/ (- (cadr ls) (car ls)) grid-length) (list-derivate (cdr ls)))))))) ; recursion step

(define make-force-table
  ; adds the related 'L' value to the force-list and then returns it in form of a table
  (lambda (count-steps grid-steps l N tau)
    (let ([grid-length (/ (* N l) grid-steps)])
      (let rec ([ls (make-force-list count-steps grid-steps l N tau)]
		[x (real->flonum (/ grid-length 2))])
	(if (null? ls)
	    '()
	    (cons (list x (car ls)) (rec (cdr ls) (+ x grid-length)))))))) ; recursion step

(let ([l 1]
      [tau 1]
      [count-steps (expt 10 6)]
      [grid-steps 25])

  (define to-file
    (lambda (N)
      (display "N = ")
      (display N)
      (newline)
      (list-file (format "table-~s-L-force" N) (make-force-table count-steps grid-steps l N tau))))

  (to-file 3)
  (to-file 5)
  (to-file 8)
  
  ; putting the data tables into files so they can be plotted
  )
