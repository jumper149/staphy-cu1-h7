(library (nmaths)
	 (export invpi invphi bisection golden-section derivate integrate)
	 (import (chezscheme))

(define invpi (acos -1))

(define invphi (/ (- (sqrt 5) 1) 2))
(define invphi2 (/ (- 3 (sqrt 5)) 2))

(define sign
  (lambda (x)
    (if (< x 0)
	-1
	1)))

(define bisection
  (lambda (f a b eps)
    (let* ([dx (- b a)]
	   [c (+ a (/ dx 2))])
      (if (< dx eps)
	  c
	  (if (= (sign (f c)) (sign (f a)))
	      (bisection f c b eps)
	      (bisection f a c eps))))))

(define golden-section
  (lambda (f a b eps)
    (let ([dx (- b a)])
      (if (< dx eps)
	  (+ a (/ dx 2))
	  (let ([c (+ a (* invphi2 dx))]
		[d (+ a (* invphi dx))])
	    (let ([fc (f c)]
		  [fd (f d)])
	      (if (< fc fd)
		  (golden-section f a d eps)
		  (golden-section f c b eps))))))))

(define derivate
  (lambda (f h)
    (lambda (x)
      (/ (- (f (+ x h)) (f x)) h))))

(define integrate
  (lambda (method . args)
    (case method
      [(riemann) (apply riemann-steps args)]
      [(trapezoidal) (apply trapezoidal-steps args)]
      [(simpson) (apply simpson-steps args)]
      [(monte-carlo) (apply monte-carlo args)])))

(define riemann-steps
  (lambda (f a b n)
    (letrec ([sum (lambda (f a n dx)
		 (if (= n 0)
		     0
		     (+ (* (f (+ a (/ dx 2))) dx) (sum f (+ a dx) (- n 1) dx))))]
	     [dx (/ (- b a) n)])
      (sum f a n dx))))

(define trapezoidal-steps
  (lambda (f a b n)
    (letrec ([sum (lambda (f a n dx)
		 (if (= n 0)
		     0
		     (+ (* 0.5 (+ (f a) (f (+ a dx))) dx) (sum f (+ a dx) (- n 1) dx))))]
	     [dx (/ (- b a) n)])
      (sum f a n dx))))

(define simpson-steps
  (letrec ([sum  (lambda (f b dx n)
		(cond
		  [(= n 0) (f b)]
		  [(odd? n) (+ (* 4 (f b)) (sum f (- b dx) dx (- n 1)))]
		  [(even? n) (+ (* 2 (f b)) (sum f (- b dx) dx (- n 1)))]))])
    (lambda (f a b n)
      (let ([dx (/ (- b a) n)])
	(* (/ dx 3) (+ (f b) (sum f b dx (- n 1))))))))

(define monte-carlo
  (lambda (f a b n)
    (let* ([vol (- b a)]
	  [presign (sign vol)])
      (if (< presign 0)
	  (begin (set! vol (- 0 vol))
		 (set! a b))
	  #t)
      (letrec ([sum (lambda (f n)
		     (if (= n 0)
			 0
			 (+ (f (+ (random vol) a)) (sum f (- n 1)))))])
	(* presign (/ vol n) (sum f n))))))

(define gradient
  (lambda (f x)
    (let ([fn (lambda (n)
		(lambda (var)
		  (let ([xn x])
		    (vector-set! xn n var)
		    (f xn))))])
      (let* ([n (vector-length x)]
	     [i n])
	(if (= i 0)
	    (make-vector n)
	    (+ 1))))))

)
