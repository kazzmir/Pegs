#lang scheme

(require "../peg.ss")

(define p
  (peg
    (start start)
    (grammar
      (start (((apply fac 20)) $))
      ((fac n) (((predicate (> n 0)) (bind m (apply fac (- n 1))) (bind x (predicate (* n m))))
		x)
	       (("0") 1)))))

(p (lambda (i)
     (string-ref "0" i)))
