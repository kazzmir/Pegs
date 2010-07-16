#lang scheme

(require "../peg.ss")

(define p1
  (peg
    (start s)
    (grammar
      (s ((x1) $))
      (x1 (("x1") "p1")))))

(define p2
  (peg
    (start s)
    (grammar
      (s (("f" x1) $))
      (x1 (("x2") "p2")
	  (((foreign p1 x1)) $)))))

(p2 (lambda (i)
      (string-ref "fx1" i)))

(p2 (lambda (i)
      (string-ref "fx2" i)))
