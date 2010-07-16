#lang scheme

;; A simple infix calculator made out of a peg
;; Try some of these inputs
;;
;; 1
;; 1 + 1
;; 5 * 90 - 32 / 2
;; cos(max(9+4,8 - 2)) - pi
;; 
;; Hit enter on a blank line to end input

(require "../peg.ss")

(define (convert-to-decimal n)
  (if (= 0 n)
    0
    (/ n (expt 10 (ceiling (/ (log n) (log 10)))))))

(define calc
  (peg
    (start s)
    (grammar
      (s (((* line)) $))
      (line (((bind c expr) w "\n")
	     (begin
	       (printf "= ~a\n" c)
	       c)))
      (w (((* space-tab)) (void)))
      (space-tab ((" ") $)
		 (("	") $))
      (expr ((prec1) $))
      (num (((bind d digit) (bind rest (* digit)) (bind decimal (? "." (* digit))))
	    (+ (string->number (apply string-append d rest))
	       (convert-to-decimal
		 (string->number (apply string-append "0" decimal))))))
      (base (("(" w (bind e expr) w ")") e)
	    (((bind o op) w "(" w (bind e expr) w ")")
	      (o e))
	    (((bind o multi-op) w "(" w (bind e1 expr) (bind es (* w "," w expr)) w ")")
	     (apply o e1 es))
	    (("pi") pi)
	    ((num) $))
      (multi-op (("gcd") gcd)
		(("lcm") lcm)
		(("max") max)
		(("min") min))
      (op (("sinh") sinh) 
	  (("cosh") cosh)
	  (("random") random)
	  (("round") round)
	  (("floor") floor)
	  (("ceiling") ceiling)
	  (("truncate") truncate)
	  (("sin") sin)
	  (("cos") cos)
	  (("log") log))
      (digit (("0") $)
	     (("1") $)
	     (("2") $)
	     (("3") $)
	     (("4") $)
	     (("5") $)
	     (("6") $)
	     (("7") $)
	     (("8") $)
	     (("9") $))
      (prec-last ((base) $))
      (prec1 (((bind n prec2) w "+" w (bind e prec1))
	     (+ n e))
	    (((bind n prec2) w "-" w (bind e prec1))
	     (- n e))
	    ((prec2) $))
      (prec2 (((bind n prec3) w "*" w (bind e prec2))
	       (* n e))
	      (((bind n prec3) w "/" w (bind e prec2))
	       (/ n e))
	      ((prec3) $))
      (prec3 (((bind n prec4) w "^" w (bind e prec3))
	      (expt n e))
	     ((prec4) $))
      (prec4 (("-" (bind n expr)) (- n))
	     ((prec-last) $)))))

(printf "Type some arithmetic, then hit enter. Press enter on a blank line to end the calculator\n")
(printf "Try some of these examples:\n")
(printf "1\n")
(printf "1 + 1\n")
(printf "5 * 90 - 32 / 2\n")
(printf "cos(max(9+4,8 - 2)) - pi\n")
(printf "\n")

(calc (let ((input (make-hash)))
        (lambda (i)
          (let ((n (hash-ref input i
                             ;; now this is sort of a hack in that I know the peg
                             ;; will only ever increment i by 1 so I only have to
                             ;; read the next char, otherwise it will already be
                             ;; in the hash.
                             (lambda ()
                               (let ((next (read-char)))
                                 (hash-set! input i next)
                                 next)))))
            (if (eof-object? n)
              end-of-input
              n)))))
