#lang racket/base

(require (planet "main.ss" ("dherman" "memoize.plt" 3 1)))
(require scheme/stxparam
         (for-syntax racket/base))

;; * optimization idea
;; find the first possible character that all the productions of a nonterminal
;; could match and store those productions in a list with that character. so if
;; the current character has no mapping between characters -> productions then
;; the entire nonterminal can be skipped. Something like
;; (nt ((p1 ...))
;;     ((p2 ...))
;;     ((p3 ...)))
;; p1 = a
;; p2 = c, d, f
;; p3 = a, z
;; [a] = p1 / p3
;; [c] = p2
;; [d] = p2
;; [f] = p2
;; [z] = p2
;;
;; In this case p2 can be skipped completely if the current input is 'a'.
;; Things to handle:
;;   * productions that can match any letter, either through _ or arguments
;;   * only performing the lookup when there are more than N (1,2?) productions
;;
;; Done as of r1188. This optimization has almost a negligable affect on the
;; ruby parser.
;;
;; * binding idea
;; Bind each element of a production in a let and pass in the next element as well
;; as all the current bindings
;; (let* ((n1 (lambda (current input last) ...)))
;;       ((n2 (lambda (current input last) ...)))
;;   (let ((r1 (n1 ...)))
;;     (if r1
;;       (let ((some-var (Result-value r1)))
;;       (let ((r2 (n2 (result-... r1) ...)))
;;         (if r2 ...))
;;       #f))
;;
;; How is this better than
;; ((lambda (current input last)
;;   ...
;;   ((lambda (current input last)
;;     ...)
;;    current input last))
;;  current input last)
;;
;; * position/span idea
;; Pass the result's position and span to the action routine. This can be done
;; by adding position and span to the arguments where the action routine is created.
;; (lambda (last vars ...) ...)
;; =>
;; (lambda (last position span vars ...) ...)
;;
;; Also bind $position and $span as syntactic renamers just as $ is renamed to last.
;; 
;; Done in r1201.
;;
;; * dont memo the "last" field
;; Instead of passing last directory, bind it to an argument and use syntax-parameterize
;; to treat $ as that argument.

#|
(pattern action)

(primary ((<<x (bind n (save)) (bind h here-doc) (continue n h))))

pattern :=
nonterminal
"literal"
#\char
(bind foo pattern)
(* pattern)
(? pattern)
(predicate action pattern)
|#

#|
  0 1 2 3 4 5 6
a 
b
c
d
e
f
g
|#

#|
(* foo) = foo1
foo1 = (((bind f foo) (bind f* foo1)) (cons f f*)
	() (list))
|#

;; (((nonterminal result index) ...) ...)

#|
(peg
  (start foobar)
  (grammar
    (foobar ((hello world) 1)
            ((blah) 2))
    (hello ((go away) 3))))
|#

;; higher values of verbose will print out more information while the parse executes
(define-for-syntax verbose 0)

;; debug-statistics? = #t will keep track of statistics for each nonterminal.
;; (dump-statistics) will print the statistics in a table.
;;  invoke # - times this nonterminal was called - memo + computed
;;  total # - total time taken to get the result for the nonterminal. this includes
;;    time to look up the result in the memo table + time to compute the body of
;;    the nonterminal.
;;  computed # - times the body was called
;;  time # - time just for the body, does not include the memo lookup time
;;  transient? #t/#f - if #t, this nonterminal should probably be transient if its
;;    not already. A nonterminal should be transient when the computed time is less than
;;    half the total time which means that half the time was spent looking up the result
;;    in the memo table. A transient nonterminal is not stored in the memo table so
;;    all the time spent in the memo table is eliminated.
;;
;;    The computed*2 > total is just a hueristic. You are free to set any nonterminal
;;    to transient that you feel is worthwhile.
;;
;;    To make a nonterminal transient simply prepend 'transient' to the nonterminal definition.
;;    (foo (("a") $))
;;    =>
;;    (transient foo (("a") $))
;;
;; Statistics are global and are not reset.
(define-for-syntax debug-statistics? #f)

;; what the peg stores intermediate results in
(define-struct Result (input value start-index end-index))

(define-syntax (log stx)
  (syntax-case stx ()
    ((_ num x ...)
     (with-syntax ((verbose verbose))
       #'(when (>= verbose num)
           (apply printf (list x ...)))))))

#;
(define (log . v)
  (when verbose
    (apply printf v)))

(define nothing (lambda () #f))
(provide end-of-input)
(define end-of-input (lambda () #f))

(define (default-action last)
  (lambda (input column)
    (make-Result input last -1 column)))

#;
(define-syntax (define-action)
  #'(lambda (input column)
      (make-Result input $ -1 column)))

(define-for-syntax (do-literal string answer)
  (with-syntax ((string string)
                (answer answer))
    #`(let ((ls (quote #,(if (char? (syntax->datum #'string))
                           (list (syntax->datum #'string))
                           (string->list (syntax->datum #'string))))))
        (lambda (input column)
          (let loop ((current column)
                     (letters ls))
            (log 3 "Letters ~a\n" letters)
            (if (null? letters)
              (begin
                (log 3 "Continuing to the next part after matching '~a'\n" string)
                ((answer string) input current))
              ;; (list string current)
              (begin
                (log 4 "Matching '~a' to input '~a' at column ~a\n" (car letters)
                     (input current)
                     current)
                (let ((next (input current)))
                  (cond
                    ((eq? next end-of-input) #f)
                    ((char=? next (car letters))
                     (loop (add1 current) (cdr letters)))
                    (else #f))))))))))

;; matches eof and nothing else
(define-for-syntax (do-eof answer)
  (with-syntax ((answer answer))
    #'(lambda (input column)
        (if (eq? (input column) end-of-input)
          ((answer eof) input (add1 column))
          #f))))

;; matches nothing
(define-for-syntax (do-epsilon answer)
  (with-syntax ((answer answer))
    #'(lambda (input column)
        ((answer $) input column))))

;; matches any 1 character
(define-for-syntax (do-any answer)
  (with-syntax ((answer answer))
    #'(lambda (input column)
        (let ((this (input column)))
          ((answer this) input (add1 column))))))

;; matches some nonterminal
(define-for-syntax (do-nonterminal nt answer)
  (with-syntax ((nt nt)
                (answer answer))
    (define (compute)
      (if (not debug-statistics?)
        #'(((nt) $) input column)
        #'(let-values (((r x cpu y)
                        (time-apply ((nt) $) (list input column))))
            (add-stat-total 'nt cpu)
            (car r))))
    #`(lambda (input column)
        (let ((result #,(compute)))
          ; (log "Result of ~a was ~a\n" 'nt result)
          ; (log 2 "Result of ~a was ~a\n" 'nt result)
          (if (not result)
            #f
            ((answer (Result-value result)) (Result-input result) (Result-end-index result)))))))

;; returns the result of the first pattern in (pattern ...) to match
(define-for-syntax (do-or pattern answer)
  (syntax-case pattern ()
    ((sub)
     (with-syntax ((answer answer))
       #'(let ((t-proc (translate-choice (sub) default-action)))
           (lambda (input column)
             (let ((result ((t-proc $) input column)))
               (if result
                 ((answer (Result-value result)) (Result-input result) (Result-end-index result))
                 #f))))))
    ((sub1 sub ...)
     (with-syntax ((answer answer)
                   (rest (do-or #'(sub ...) answer)))
       #'(let ((t-proc (translate-choice (sub1) default-action)))
           (lambda (input column)
             (let ((result ((t-proc $) input column)))
               (if result
                 ((answer (Result-value result)) (Result-input result) (Result-end-index result))
                 (rest input column)))))))))

#|
;; or using a sub-peg
(define-for-syntax (do-or pattern answer)
  (syntax-case pattern ()
    ((sub ...)
     (with-syntax ((answer answer)
                   (first-nt (gensym)))
       #'(lambda (input column)
           (let ((sub-peg (peg
                            (start first-nt)
                            (grammar (first-nt ((sub) default-action) ...)))))
             (answer input column 
                     (sub-peg (lambda (c)
                                (input (+ column c)))))))))))
|#

;; apply some arguments to a nonterminal and invoke the nonterminal
(define-for-syntax (do-apply nt args answer)
  (with-syntax ((nt nt)
                ((fargs ...) args)
                (answer answer))
    #'(lambda (input column)
        ;; func cannot be moved above the lambda because fargs might refer to
        ;; variables that were bound in a previous pattern, or something like that..
        (let* ((func (nt fargs ...))
               (result ((func $) input column)))
          (if result
            ((answer (Result-value result)) (Result-input result) (Result-end-index result))
            #f)))))

(define-for-syntax (do-foreign fpeg nt answer)
  (with-syntax ((nt nt)
                (fpeg fpeg)
                (answer answer))
    #'(lambda (input column)
        (let* ((result (fpeg input #:nonterminal 'nt #:output #t #:column column)))
          (if result
            ((answer (Result-value result)) (Result-input result) (Result-end-index result))
            #f)))))

;; doesn't match pattern
;; consumes no input
(define-for-syntax (do-not pattern answer)
  (with-syntax ((pattern pattern)
                (answer answer))
    #'(let ((t-proc (translate-choice pattern default-action)))
        (lambda (input column)
          (let ((result ((t-proc $) input column)))
            (if result
              #f
              ((answer $) input column)))))))

;; saves the current state of the parser
(define-for-syntax (do-save answer)
  (with-syntax ((answer answer))
    #'(lambda (input column)
        (make-Result input
                     (make-Result input $ -1 column)
                     -1 column))))

(define-for-syntax (do-continue start end answer)
  (with-syntax ((start start)
                (end end)
                (answer answer))
    #'(lambda (input column)
        ((answer (Result-value start)) (Result-input end) (Result-end-index start)))))

(define-for-syntax (do-skip start end)
  (with-syntax ((start start)
                (end end))
    #'(lambda (input column)
        (make-Result input
                     (make-Result (lambda (i)
                                    (if (<= i (Result-end-index start))
                                      (input i)
                                      (input (+ i (- (Result-end-index end)
                                                     (Result-end-index start))))))
                                  $
                                  -1
                                  column)
                     -1
                     column))))

;; ensures that pattern will match
;; consumes no input
(define-for-syntax (do-ensure pattern answer)
  (with-syntax ((pattern pattern)
                (answer answer))
    #'(let ((t-proc (translate-choice pattern default-action)))
        (lambda (input column)
          (let ((result ((t-proc $) input column)))
            (if result
              ((answer $) input column)
              #f))))))

#|
(define-for-syntax (do-let sub-patterns sub-actions answer)
  (with-syntax (((sub-patterns ...) sub-patterns)
		((sub-actions ...) sub-actions)
		(answer answer))
    #'(lambda (input column)
	(let ((sub-peg (peg
			 (start first-nt)
			 (grammar (first-nt ((sub) default-action) ...)))))
	     (answer input column 
		     (sub-peg (lambda (c)
				(input (+ column c))))))
|#

#|
("foo" + <<x + "bad") + 2
blah
x

(bind s (slurp "\n"))

(bind here (jump heredoc slurp))

(rest ...
      (lambda (i)
        (if (< i (+ column (length s)))
          i
          (input (+ i (position here))))))
|#

;; binds a variable to the result of pattern
(define-for-syntax (do-bind var pattern answer)
  (with-syntax ((var var)
                (pattern pattern)
                (answer answer))
    #'(let ((t-proc (translate-choice pattern default-action)))
        (lambda (input column)
          (let ((result ((t-proc $) input column)))
            (if result
              (let ((var (Result-value result))
                    (next-column (Result-end-index result)))
                ((answer var) (Result-input result) next-column))
              #f))))))

;; performs arbitrary computation, if the computation is anything
;; besides #f the parse will continue. the result of the computation
;; is given to the next pattern as its result
(define-for-syntax (do-predicate predicate answer)
  (with-syntax ((predicate predicate)
                (answer answer))
    #'(lambda (input column)
        (let ((next predicate))
          (if next
            ((answer next) input column)
            #f)))))

;; repeat a pattern 0 or more times
;; returns a list
(define-for-syntax (do-repeat pattern answer)
  (with-syntax ((pattern pattern)
                (answer answer))
    #'(lambda (input column)
        (let ((proc (translate-choice pattern default-action)))
          (let loop ((column column)
                     (input input)
                     (all '()))
            (let ((result ((proc $) input column)))
              (if (not result)
                ((answer (reverse all)) input column)
                (loop (Result-end-index result) (Result-input result) (cons (Result-value result) all)))))))))

;; matches a pattern in which case it returns the result of the pattern
;; or doesn't match the pattern in which case '() is returned
(define-for-syntax (do-maybe pattern answer)
  (with-syntax ((pattern pattern)
                (answer answer))
    #'(lambda (input column)
        (let ((proc (translate-choice pattern default-action))
              ;; what should nothing be?
              (nothing '()))
          (let ((result ((proc $) input column)))
            (if (not result)
              ((answer nothing) input column)
              ((answer (Result-value result)) (Result-input result) (Result-end-index result) )))))))

#;
(define (parse-choice choice input column last)
  (choice input column last))

(define (create-parser symbol productions transient?)
  (define-syntax (stat-compute stx)
    (syntax-case stx ()
      ((_ func args ...)
       (if debug-statistics?
         #'(let-values (((r x cpu y)
                         (time-apply func (list args ...))))
             (add-stat-compute symbol cpu)
             (car r))
         #'(func args ...)))))
  (let ((do-work
          (lambda (input column)
            (let ((id (gensym)))
              (log 1 "[~a] Parse input with symbol ~a at column ~a char '~a'\n" id symbol column (input column))
              (let loop ((choices (let ((v (productions (input column))))
                                    (if (null? v)
                                      (productions #t)
                                      v))
                                  #;
                           (hash-ref productions (input column)
                                            (lambda () (hash-ref productions #t))))
                         (num 1))
                (log 2 "[~a] Current choice ~a ~a of ~a\n" id (if (null? choices) #f (car choices)) num symbol)
                (if (null? choices)
                  (begin
                    (log 2 "[~a] Nonterminal ~a failed to parse\n" id symbol)
                    #f)
                  (let* ((result (((car choices) #f)
                                  input column)))
                    (if result
                      (begin
                        (log 1 "[~a] Parsed with ~a = ~a at column ~a. Next char at ~a is '~a'\n" id symbol (Result-value result) column (Result-end-index result) (input (Result-end-index result)))
                        result)
                      (loop (cdr choices) (add1 num))))))))))
    (let ((func (if (not transient?)
                  (memo-lambda (input column)
                    (stat-compute do-work input column))
                  #;
                  (let ((m (memo-lambda (input column)
                             (stat-compute do-work input column))))
                    (lambda (input column)
                      (printf "Compute ~a\n" symbol)
                      (m input column)))
                  do-work)))
      (lambda (last) func))))

(define-syntax (translate-choice stx)
  (syntax-case stx ()
    ((_ () action) #'action)
    ((_ (choice choice* ...) action)
     (with-syntax ((rest #'(translate-choice (choice* ...) action)))
       (define (store expr)
         #`(lambda (last)
             (syntax-parameterize (($ (make-rename-transformer #'last)))
               #,expr)))
       (syntax-case #'choice (bind except + * ? predicate not
                                   ensure or apply foreign do save continue skip)
         (()
          (store (do-epsilon #'action)))
         (eof (equal? (syntax->datum #'eof) 'eof)
              (store (do-eof #'rest)))
         (nt (and (identifier? #'nt)
                  (not (equal? (syntax->datum #'nt) '_)))
             (store (do-nonterminal #'nt #'rest)))
         (nt (equal? (syntax->datum #'nt) '_)
             (store (do-any #'rest)))
         ((do . next)
          (with-syntax ((m (gensym))
                        ((patterns ...) #'next))
            #'(translate-choice ((bind m patterns ...) choice* ...) action)))
         #;
         ((let (sub-pattern sub-action) ...)
          (do-let (syntax->list #'(sub-pattern ...))
                  (syntax->list #'(sub-action ...))
                  #'rest))
         ((bind var . next)
          (store (do-bind #'var #'next #'rest)))
         ((* . pattern)
          (store (do-repeat #'pattern #'rest)))
         ((+ . pattern)
          (with-syntax (((patterns ...) #'pattern))
            #'(translate-choice ((bind first patterns ...)
                               (bind next (* patterns ...))
                               (predicate (cons first next))
                               choice* ...)
                              action)))
         ((foreign peg nt)
          (store (do-foreign #'peg #'nt #'rest)))
         ((save)
          (store (do-save #'rest)))
         ((continue start end)
          (store (do-continue #'start #'end #'rest)))
         ((skip start end)
          (store (do-skip #'start #'end)))
         ((or . pattern)
          (store (do-or #'pattern #'rest)))
         ((? . pattern)
          (store (do-maybe #'pattern #'rest)))
         ((predicate what)
          (store (do-predicate #'what #'rest)))
         ((apply nt . args)
          (store (do-apply #'nt #'args #'rest)))
         ((not . pattern)
          (store (do-not #'pattern #'rest)))
         ((ensure . pattern)
          (store (do-ensure #'pattern #'rest)))
         ((except . patterns)
          (with-syntax (((patterns ...) #'patterns))
            #'(translate-choice ((not patterns ...) _ choice* ...) action)))
         (lit
           (or (string? (syntax->datum #'lit))
               (char? (syntax->datum #'lit)))
           (store (do-literal #'lit #'rest))))))))

(provide $ $position $span)
(define-syntax-parameter $ 
                         (lambda (stx)
                           (raise-syntax-error #f "$ is bound to the last element in a peg production and can only be used in the action part" stx)))

(define-syntax-parameter $position 
                         (lambda (stx)
                           (raise-syntax-error #f "$position is bound to the position in a peg production and can only be used in the action part" stx)))

(define-syntax-parameter $span 
                         (lambda (stx)
                           (raise-syntax-error #f "$span is bound to the span in a peg production and can only be used in the action part" stx)))

(define statistics (make-hash))

(define-struct stat (times total-time c-times compute-time))

(provide dump-statistics)
(define (dump-statistics)
  (for-each (lambda (key)
              (let ((s (hash-ref statistics key)))
                (printf "~a\r\t\t\tinvoke\t~a\ttotal\t~a\tcomputed\t~a\ttime\t~a\t\tmake-transient? ~a\n"
                        key
                        (stat-times s)
                        (stat-total-time s)
                        (stat-c-times s)
                        (stat-compute-time s)
                        (< (* 2 (stat-compute-time s))
                           (stat-total-time s)))))
            (sort
              (hash-map statistics (lambda (k v) k))
              (lambda (a b)
                (let ((a-time (stat-total-time (hash-ref statistics a)))
                      (b-time (stat-total-time (hash-ref statistics b))))
                  (< a-time b-time))))))

(define (add-stat-total name time)
  (hash-set! statistics name
             (let ((old-stat (hash-ref statistics name (lambda ()
                                                         (hash-set! statistics name (make-stat 0 0 0 0))
                                                         (hash-ref statistics name)))))
               (make-stat (add1 (stat-times old-stat))
                          (+ time (stat-total-time old-stat))
                          (stat-c-times old-stat)
                          (stat-compute-time old-stat)))))

(define (add-stat-compute name compute)
  (hash-set! statistics name
             (let ((old-stat (hash-ref statistics name (lambda ()
                                                         (hash-set! statistics name (make-stat 0 0 0 0))
                                                         (hash-ref statistics name)))))
               (make-stat (stat-times old-stat)
                          (stat-total-time old-stat)
                          (add1 (stat-c-times old-stat))
                          (+ compute (stat-compute-time old-stat))))))

;; reduce a list of letters to a list of unique letters
(define-for-syntax (reduce letters)
  (cond
    #;
    ((memq 'any letters) (list 'any))
    (else (let loop ((all '())
                     (letters letters))
            (cond
              ((null? letters) all)
              ((memq (car letters) all)
               (loop all (cdr letters)))
              (else (loop (cons (car letters) all)
                          (cdr letters))))))))

(define-for-syntax (extract-vars elements)
    (let loop ((vars '())
               (elements elements))
      (cond
        ((null? elements) (reverse vars))
        (else (syntax-case (car elements) (bind)
                ((bind v stuff) (loop (cons #'v vars)
                                      (cdr elements)))
                (_ (loop vars (cdr elements))))))))

(define-for-syntax (create-actions all-choices)
  (map (lambda (choices)
         (map (lambda (choice)
                (define (create vars action)
                  (with-syntax ((func (gensym))
                                (last (gensym))
                                (action action)
                                ((vars ...) vars))
                    #'(func (lambda (last position span vars ...)
                              (syntax-parameterize (($ (make-rename-transformer #'last))
                                                    ($position (make-rename-transformer #'position))
                                                    ($span (make-rename-transformer #'span)))
                                action)))))
                (syntax-case choice ()
                  ((action) (create '() #'action))
                  ((elements action)
                   (create (extract-vars (syntax->list #'elements))
                           #'action))))
              (syntax->list choices)))
       all-choices))

(define-for-syntax (translate-choices all-choices all-actions)
  (map (lambda (choices action-names)
         (map (lambda (choice action-name)
                (syntax-case choice ()
                  ((action) 
                   (with-syntax ((action-name action-name))
                     #'(translate-choice ()
                                         (lambda (last)
                                           (lambda (input column)
                                             (make-Result
                                               input
                                               (action-name last column 0)
                                               -1
                                               column))))))
                  ((elements action)
                   (with-syntax (((bound-vars ...)
                                  (extract-vars (syntax->list
                                                  #'elements)))
                                 (action-name action-name))
                     #'(lambda (start-last)
                         (lambda (start-input start-column)
                           (let ((t (translate-choice elements
                                                      (lambda (last)
                                                        (lambda (input column)
                                                          (make-Result
                                                            input
                                                            (action-name last start-column (- column start-column) bound-vars ...)
                                                            -1
                                                            column))))))
                             ((t start-last) start-input start-column))))))))
              (syntax->list choices)
              (syntax->list action-names)))
       all-choices
       all-actions))


(define-for-syntax (create-nonterminals modifiers nonterminals
                                        translated-choices choices
                                        compute-letter)
  (map (lambda (mod nt choices raw-choices)
         (define (create name args)
           (with-syntax (((args ...) args)
                         (name name)
                         ((cs ...) choices)
                         (memo (syntax-case mod (none transient)
                                 (none #'#f)
                                 (transient #'#t)))
                         ((var-cs ...) (generate-temporaries choices)))
             #;
             (for-each (lambda (c)
                         (printf "For choice ~a set is ~a\n"
                                 (syntax->datum c)
                                 (reduce (compute-letter c #'(args ...)))))
                       (syntax->list raw-choices))

             (let ((letters (reduce (apply append '()
                                           (map (lambda (c)
                                                  ;; (printf "Computing letter for choice ~a\n" (syntax->datum c))
                                                  (compute-letter c #'(args ...)))
                                                (syntax->list raw-choices))))))
               (with-syntax ((((letter var ...) ...)
                              (map (lambda (letter)
                                     (let loop ((all '())
                                                (vars (syntax->list #'(var-cs ...)))
                                                (choices (syntax->list raw-choices)))
                                       (cond
                                         ((null? choices)
                                          (cons (if (eq? letter 'any) #t letter)
                                                (reverse all)))
                                         ((let ((chars (compute-letter (car choices)
                                                                       #'(args ...))))
                                            (or (memv letter chars)
                                                (memv 'any chars)))
                                          (loop (cons (car vars)
                                                      all)
                                                (cdr vars)
                                                (cdr choices)))
                                         (else (loop all
                                                     (cdr vars)
                                                     (cdr choices))))))
                                   letters)))
                 (with-syntax ((hasher 
                                 #'(lambda (c)
                                     (case c
                                       ((letter) (list var ...))
                                       ...
                                       (else '())))))
                   #'(memo-lambda (args ...)
                       (let ((var-cs cs) ...)
                         (create-parser 'name hasher memo))))))))
         (syntax-case nt ()
           ((name args ...)
            (create #'name (syntax->list #'(args ...))))
           (name
             (create #'name '()))))
       modifiers
       nonterminals
       translated-choices
       choices))

(provide peg)
(define-syntax (peg stx)
  (define (create-peg peg-stx)
    (syntax-case peg-stx (start grammar)
      ((peg (start start-nt) (grammar (modifier nonterminal choice ...) ...))
       (let ((first-input (make-hash)))
         (define (compute-element element args)
           (define (is-arg? id)
             ;; (printf "Id is ~a args are ~a\n" (syntax->datum id) (syntax->datum args))
             (memq (syntax->datum id)
                   (syntax->datum args)))
           ;; (printf "compute element ~a\n" (syntax->datum element))
           (syntax-case element (bind except + * ? predicate not
                                      ensure or apply foreign do save continue skip)
             (() '())
             (eof 
               (equal? (syntax->datum #'eof) 'eof)
               (if (is-arg? #'eof)
                 '(any)
                 (list #'end-of-input)))
             (nt (and (identifier? #'nt)
                      (not (equal? (syntax->datum #'nt) '_)))
                 (if (is-arg? #'nt)
                   '(any)
                   (compute-firsts #'nt)))
             (nt (equal? (syntax->datum #'nt) '_)
                 '(any))
             ((do . next)
              (compute-element (car (syntax->list #'next)) args))
             ((bind var . next)
              (compute-element (car (syntax->list #'next)) args))
             ((* . pattern) '(any))
             ((+ . pattern) (compute-element (car (syntax->list #'pattern)) args))
             ((foreign peg nt) '(any))
             ((save) '(any))
             ((continue start end) '(any))
             ((skip start end) '(any))
             ((or . pattern) (apply append '()
                                    (map (lambda (c)
                                           (compute-element c args))
                                         (syntax->list #'pattern))))
             ((? . pattern) '(any))
             ((predicate what) '(any))
             ((apply nt . args)
              (if (is-arg? #'nt)
                '(any)
                (compute-firsts #'nt)))
             ((not . pattern) '(any))
             ((ensure . pattern) '(any))
             ((except . patterns) '(any))
             (lit
               (or (string? (syntax->datum #'lit))
                   (char? (syntax->datum #'lit)))
               (cond
                 ((string? (syntax->datum #'lit))
                  (list (string-ref (syntax->datum #'lit) 0)))
                 ((char? (syntax->datum #'lit))
                  (list (syntax->datum #'lit)))))))
         (define (compute-letter production args)
           (syntax-case production ()
             ((action) '(any))
             ((elements actions)
              (compute-element (car (syntax->list #'elements))
                               args))))
         (define (compute-firsts nt)
           (hash-ref first-input (syntax->datum nt)
                     (lambda ()
                       (let ((result
                               (let loop ((nts (syntax->list
                                                 #'((nonterminal choice ...) ...))))
                                 (if (null? nts)
                                   (error 'compute-firsts "Could not find nonterminal ~a\n"
                                          (syntax->datum nt))
                                   (syntax-case (car nts) ()
                                     (((name args ...) productions ...)
                                      (eq? (syntax->datum #'name)
                                           (syntax->datum nt))
                                      (reduce (apply append '()
                                             (map (lambda (c)
                                                    (compute-letter c #'(args ...)))
                                                  (syntax->list #'(productions ...))))))
                                     ((name productions ...)
                                      (eq? (syntax->datum #'name)
                                           (syntax->datum nt))
                                      (reduce (apply append '()
                                             (map (lambda (c)
                                                    (compute-letter c #''()))
                                                  (syntax->list #'(productions ...))))))
                                     (_ (loop (cdr nts))))))))
                         (hash-set! first-input (syntax->datum nt) result)
                         result))))
       (with-syntax (((((action-name action-func) ...) ...)
                      (create-actions (syntax->list #'((choice ...) ...)))))
         (with-syntax ((((translated-choices ...) ...)
                        (translate-choices (syntax->list #'((choice ...) ...))
                                           (syntax->list #'((action-name ...) ...)))))
           (with-syntax (((nt-func ...)
                          (create-nonterminals (syntax->list #'(modifier ...))
                                               (syntax->list #'(nonterminal ...))
                                               (syntax->list #'((translated-choices ...) ...))
                                               (syntax->list #'((choice ...) ...))
                                               compute-letter))
                         ((nt-name ...)
                          (map (lambda (nt)
                                 (syntax-case nt ()
                                   ((name args ...) #'name)
                                   (name #'name)))
                               (syntax->list #'(nonterminal ...)))))
             #'(let ((action-name action-func) ... ...)
                 (letrec ((nt-name nt-func) ...)
                   (let ((names->functions (lambda (name)
                                             (case name
                                               ((nt-name) nt-name)
                                               ...
                                               (else (error 'names->functions "Cannot find ~a" name))
                                               ))))
                     (lambda (input #:nonterminal (nt 'start-nt) #:output (output #f) #:column (column 0))
                       (log 1 "Start parsing with nonterminal ~a at column ~a\n" nt column)
                       (let* ((result (((
                                         (names->functions nt)
                                         #;
                                         (hash-ref names->functions nt)
                                         ) #f) input column)))
                         (log 1 "Result of parsing is ~a\n" (if result (Result-value result) #f))
                         (if output
                           result
                           (if result
                             (Result-value result)
                             #f))))))))))))))

  (syntax-case stx (start grammar)
    ((peg (start start-nt) (grammar stuff ...))
     (with-syntax (((real-grammar ...)
                    (map (lambda (nt+productions)
                           (syntax-case nt+productions (transient none)
                             ((transient name prods ...) #'(transient name prods ...))
                             ((none name prods ...) #'(none name prods ...))
                             ((name prods ...) #'(none name prods ...))))
                         (syntax->list #'(stuff ...)))))
       (create-peg
         #'(peg (start start-nt)
                (grammar real-grammar ...))))))
  )

(define (parse-string parser string)
  (let* ((s (string->list string))
         (v (list->vector s))
         (max (length s)))
    (parser (lambda (i)
              (if (>= i max)
                end-of-input
                (vector-ref v i))))))

(define liner (make-hash))
(provide dump-liner)
(define (dump-liner)
  (for-each (lambda (key)
              (let ((s (hash-ref liner key)))
                (printf "column ~a ~a\n"
                        key
                        s)))
            (sort
              (hash-map liner (lambda (k v) k))
              (lambda (a b)
                (< (hash-ref liner a) (hash-ref liner b)))))
  (printf "Total ~a\n" (apply + 0 (hash-map liner (lambda (k v) v)))))

;; peek-char is slower than string-ref, it seems
(define (parse-port parser port)
  (define max-length (expt 2 14))
  (parameterize ((current-input-port port))
    (let* ((all (read-string max-length))
           (maximum (if (eof-object? all) 0 (string-length all))))
      (parser 
        (lambda (i)
          ;; comment/uncomment this line for per character statistics via dump-liner
          ;; (hash-set! liner i (add1 (hash-ref liner i (lambda () (hash-set! liner i 0) 0))))
          (if (>= i maximum)
            (let ((next (read-string max-length)))
              (if (eof-object? next)
                end-of-input
                (begin
                  (set! all (string-append all next))
                  (set! maximum (string-length all))
                  (string-ref all i))))
            (string-ref all i)))

        #;
        (lambda (i)
          (let* ((index (floor (/ i max-length)))
                 (str (hash-ref strings index
                                (lambda ()
                                  (log 5 "Reading next ~a characters\n" max-length)
                                  (let ((str (read-string max-length)))
                                    (log 5 "Read ~a\n" str)
                                    (hash-set! strings index str)
                                    str)))))
            (if (or (eof-object? str)
                    (>= (modulo i max-length) (string-length str)))
              end-of-input
              (string-ref str (modulo i max-length)))))))))

(define (parse-file parser file)
  (with-input-from-file file (lambda ()
                               (parse-port parser (current-input-port)))))

(provide parse)
(define (parse parser obj)
  (cond
    ((string? obj) (parse-string parser obj))
    ((path? obj) (parse-file parser obj))
    ((port? obj) (parse-port parser obj))
    (else (error 'parse "You gave me a ~a. Please pass a string or a path to the parse method.\n" obj))))

#;
(define (test1)
  (define p
    (peg
      (start blah)
      (grammar
        (blah ((foobar " " "1") 23)
              ((foobar (bind x " ") (bind y "2")) (string-append x y))
              ((foobar (bind x " ") "3") 40)
              )
        (foobar (("hello" "animals") 99)
                (("hello" " " "world") 9)))))

  (let ((s (string->list "hello world 2")))
    (p (lambda (i)
         (list-ref s i)))))

#;
(define (test2)
  (define p
    (peg
      (start blah)
      (grammar
        (blah (((bind x ones) (bind z (? "food")) (bind y twos)) (list x z y))
              )
        (ones (((bind x (* "1"))) x))
        (twos (((bind x (* "2"))) x))
        )))

  (let* ((s (string->list "111112222"))
         (max (length s)))
    (p (lambda (i)
         (if (>= i max)
           'you-cant-possibly-match-this
           (list-ref s i)))))
  )

#;
(test2)
