#lang scheme

(require "peg.ss")

(define num 0)
(define (test expected actual)
  (set! num (add1 num))
  (when (not (equal? expected actual))
    (printf "Failure for test ~a. '~a' != '~a'\n" num expected actual)))

(define (test-fail expected actual)
  (set! num (add1 num))
  (when (equal? expected actual)
    (printf "Failure for test ~a. '~a' = '~a'\n" num expected actual)))

(define (test1)
  (define p
    (peg
      (start blah)
      (grammar
	(blah (((do "a")) 'a)
	      (("b") 'b)
	      ))))

  (test (parse p "a") 'a)
  (test (parse p "b") 'b)
  (test (parse p "ab") 'a)
  (test-fail (parse p "c") 'c)

  )

(define (test2)
  (define p
    (peg
      (start x)
      (grammar
	(x (((bind q "a") (bind s _)) q)))))

  (test (parse p "a") "a")
  (test-fail (parse p "a") 'b))

(define (test3)
  (define p
    (peg
      (start blah)
      (grammar
	(blah (((bind x something) (predicate (equal? x 'a))) x))
	(something (("a") 'a)
		   (("b") 'b)))))

  (test (parse p "a") 'a)
  (test-fail (parse p "b") 'b))

(define (test4)
  (define p
    (peg
      (start blah)
      (grammar
	(blah (((bind x (* "a"))) x)))))

  (test (parse p "aaa") '("a" "a" "a")))

(define (test5)
  (define p
    (peg
      (start blah)
      (grammar
	;; (blah (((apply foo #\a)) $))
        (blah ((z) $))
        (z (((apply foo #\a)) $))
	((foo n) (((bind c _) (predicate (and (char? c)
					      (char=? c n)))) 1)))))

  (test (parse p "a") 1))

(define (test6)
  (define p
    (peg
      (start blah)
      (grammar
	(blah (((bind x (* letter))) x))
	(nonsense (("this is nonsense") 'b))
	(dontcare (("dontcare") 'c))
	#; (letter (((apply char-range #\a #\z)) _))
	((char-range start end) (((bind c _)
				  (predicate (and (char? c)
						  (char>=? c start)
						  (char<=? c end))))
				 c))

	(letter ((nonsense dontcare) (void))
		(("a") 'a)
		(("b") 'a)
		(("c") 'a)
		(("d") 'a)
		(("e") 'a)
		(("f") 'a)
		(("g") 'a)
		(("h") 'a)
		(("i") 'a)
                (("j") 'a)
		(("k") 'a)
		(("l") 'a)
		(("m") 'a)
		(("n") 'a)
		(("o") 'a)
		(("p") 'a)
		(("q") 'a)
		(("r") 'a)
		(("s") 'a)
		(("t") 'a)
		(("u") 'a)
		(("v") 'a)
		(("w") 'a)
		(("x") 'a)
		(("y") 'a)
		(("z") 'a)))))

  (define str (list->string (build-list 100000
					(lambda (n)
					  (integer->char
					    (+ (char->integer #\a)
					       (random 26)))))))
  (define f (parse p str))
  (void))

#;
(define (test7)
  (define p
    (peg
      (start blah)
      (grammar
	(blah (((except "f")) $)))))

  (test (parse p "a") #\a))

(define (test8)
  (define p
    (peg
      (start a)
      (grammar
	(a (((apply b) (apply c d)) $))
	(b (("b") $))
	((c nt) ((nt "c") $))
	(d (("d") $)))))

  (test (parse p "bdc") "c"))


(test1)
(test2)
(test3)
(test4)
(test5)

(time (test6))
;; (test7)
(test8)

(printf "Done\n")
