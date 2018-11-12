(library (plot)
	 (export list-display list-file plot plot-display plot-file hist hist-display hist-file)
	 (import (chezscheme))

(define list-table
  (lambda (x f n)
    (if (= n 0)
	'()
	(cons x (list-table (f x) f (- n 1))))))

(define list-linspace
  (lambda (a b n)
    (list-table a (lambda (x) (+ x (/ (- b a) n))) n)))

(define plot-eval
  (lambda (f x)
    (cons x (cons (f x) '()))))

(define plot-map
  (lambda (f ls)
    (map (lambda (x) (plot-eval f x)) ls)))

(define plot
  (lambda (f a b n)
    (plot-map f (list-linspace a b n))))

(define line-display
  (lambda (line)
    (if (null? line)
	(newline)
	(begin (display (car line))
	       (display " ")
	       (line-display (cdr line))))))

(define line-file
  (lambda (file line)
    (if (file-exists? file)
	(delete-file file)
	#f)
    (with-output-to-file file (lambda () (line-display line)))))

(define list-display
  (lambda (ls)
    (if (null? ls)
	#t
	(begin (line-display (car ls))
	       (list-display (cdr ls))))))

(define list-file
  (lambda (file ls)
    (if (file-exists? file)
	(delete-file file)
	#f)
    (with-output-to-file file (lambda () (list-display ls)))))

(define plot-display
  (lambda (f a b n)
    (list-display (plot f a b n))))

(define plot-file
  (lambda (file f a b n)
    (if (file-exists? file)
	(delete-file file)
	#f)
    (with-output-to-file file (lambda () (plot-display f a b n)))))

(define hist-eval
  (lambda (f)
    (cons (f) '())))

(define hist
  (lambda (f n)
    (if (= n 0)
	'()
	(cons (hist-eval f) (hist f (- n 1))))))

(define hist-display
  (lambda (f n)
    (list-display (hist f n))))

(define hist-file
  (lambda (file f n)
    (if (file-exists? file)
	(delete-file file)
	#f)
    (with-output-to-file file (lambda () (hist-display f n)))))

)
