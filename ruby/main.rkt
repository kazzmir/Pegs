#lang racket/base

(require racket/class
	 racket/match
	 racket/list
     racket/contract
	 racket/set
     "debug.rkt"
     (for-syntax "debug.rkt")
	 (for-syntax racket/base))

(define logger (make-logger 'ruby (current-logger)))

#|
(let ((global Nil) ...)
  (let ((function blah) ...)
    (let ((locals foo) ...)
      body)
|#

;; I should probably export ruby-specific versions of these things
(provide #%app #%datum #%top #%module-begin
	 void)

(define RubyClass
  (class* object% ()
          (init-field instance-class)
          (init-field name)

	  (define/public (instance? thing)
            (is-a? thing instance-class))

          (define/public (new b . args)
            (apply make-object instance-class args))

          (define/public (to_s b)
            (send String new b name))

          (super-new)))

(define ObjectClass
  (class* object% ()
          (define/public (to_s b)
            (send String new ""))
          (super-new)))

(provide Object)
(define Object
  (new RubyClass 
       (instance-class ObjectClass)
       (name "Object")))

(provide String)
(define String
  (new RubyClass
       (name "String")
       (instance-class
         (class* ObjectClass ()
                 (init-field value)
                 (define/override (to_s b) this)
         (define (remove-newline value)
           (if (and (> (string-length value) 0)
                    (char=? (string-ref value (sub1 (string-length value))) #\newline))
             (substring value 0 (sub1 (string-length value)))
             value))
         (define/public (chomp block)
           (send String new #f (remove-newline value)))
         (define/public (&== block arg)
		   (equal? value (send (send (convert-to-object arg) to_s #f) &ruby->native)))
         (define/public (scan block arg)
           (regexp-match arg value))
		 (define/public (&!= block arg)
           (not (&== block arg)))
         (define/public (&ruby->native) value)
         (super-new)))))

(provide Proc)
(define Proc
  (new RubyClass
       (name "Proc")
       (instance-class
         (class* ObjectClass ()
                 (init-field closure)
                 (define/public (call block . vals)
                   (apply closure block vals))
                 (super-new)))))

(define Boolean
  (new RubyClass
       (name "Boolean")
       (instance-class
         (class* ObjectClass ()
                 (init-field value)

                 (define/public (&&& block bool)
                   (equal? value (if (boolean? bool)
                                   bool
                                   (get-field value bool))))

                 (super-new)))))

(provide Fixnum)
(define Fixnum
  (new RubyClass
       (name "Fixnum")
       (instance-class
         (class* ObjectClass ()
                 (init-field value)

                 (define (make v)
                   (send Fixnum new #f v))

                 (define/override (to_s b)
                   (send String new #f (format "~a" value)))

                 (define/public (&!= block n)
                   (not (&== block n)))

                 (define/public (&== block n)
                   (match n
                     [(? (lambda (x) (send Fixnum instance? x)))
                      (equal? value
                              (get-field value n))]
                     [(? number?) (equal? value n)]
                     [else #f]))

                 (define/public (&+ b n)
                   (let ([n-value (get-field value n)])
                     (make (+ value n-value))))

                 (define/public (&- b n)
                   (let ((n-value (get-field value n)))
                     (make (- value n-value))))

                 (define/public (@- b)
                   (make (- value))) 

                 (define/public (times b)
                   (for/last ((i (in-range value)))
                             (send/apply b call #f
                                         (list (make i)))))

                 (define/public (&< b n)
                   (let ((n-value (get-field value n)))
                     (< value n-value)))

                 (super-new)))))

(define Fixnum/c (make-flat-contract #:name 'Fixnum #:first-order
                                (lambda (x)
                                  (send Fixnum instance? x))))

(define/contract (fixnum->number number)
                 (-> (or/c Fixnum/c number?) number?)
  (cond
    [(number? number) number]
    [(send Fixnum instance? number) (get-field value number)]
    [else (error 'fixnum->number "not a fixnum or a number ~a" number)]
    ))

(define (racket->ruby value)
  (cond
    [(number? value) (send Fixnum new #f value)]
    [else (error 'racket->ruby "don't know how to convert ~a" value)]))

(provide Array)
(define Array
  (new RubyClass
       (name "Array")
       (instance-class
         (class* ObjectClass ()
                 (init-field value)

		 (define (make value)
		   (send Array new #f value))

		 (define/public (get-values) value)

		 (define/public (hash block)
		   (send Fixnum new #f (equal-hash-code value)))

         (define/public (compact! block)
           (set! value (filter (lambda (x) (not (nil? x))) value)))

         (define/public (find_all block)
           (make (filter (lambda (x) (send block call #f x)) value )))

         (define (fill-number block with-value start range)
           (set! value (append (take value start)
                               (for/list ([i (in-range 
                                               (fix-range (fixnum->number range))
                                               range)])
                                 (if block
                                   (block with-value)
                                   with-value)))))

         (define no-value (gensym))
         (define (fill-sequence block with-value range)
           (define starting #f)
           (define ending #f)
           (define filled (for/list ([index range])
                            (define fixed (fix-range index))
                            (when (or (not starting)
                                      (< fixed starting))
                              (set! starting fixed))
                            (when (or (not ending)
                                      (> fixed ending))
                              (set! ending fixed))
                            (cond
                              [(and block (not (eq? with-value no-value)))
                               (send block call #f with-value)]
                              [(and block (eq? with-value no-value))
                               (send block call #f (racket->ruby fixed))]
                              [else with-value])))
           (set! value (append (take value (or starting 0))
                               filled
                               (drop value (min (or ending (length value))
                                                (length value))))))

         (define/public (fill block [with-value no-value] [start 0] [range (length value)])
           (debug "at fill args are block ~a with-value ~a start ~a range ~a\n" block with-value start range)
           ;; (define real-start (fix-range (fixnum->number start)))
           (cond
             [(sequence? with-value) (fill-sequence block no-value with-value)]
             [(sequence? start) (fill-sequence block with-value start)]
             [else (fill-number block with-value
                                (fix-range (fixnum->number start))
                                (fix-range (fixnum->number range)))])
           
           this)

         (define/public (pop block)
           (define return (list-ref value (sub1 (length value))))
           (set! value (take value (sub1 (length value))))
           return)

         ;; negative numbers wrap around starting from the end
         (define (fix-range position)
           (match position
             [(? negative? x)
              (+ x (length value))]
             [x x]))

         ;; x = [0,1,2,3,4,5]
         ;; x[1,3] = 10
         ;; x == [0, 10, 4, 5]
		 (define/public (splice position range expression)
           (define real-position (fix-range (fixnum->number position)))
           (define real-range (min (- (length value)
                                      real-position)
                                   (fixnum->number range)))
           (set! value (append (take value real-position)
                               (list expression)
                               (drop value (+ real-position real-range))))
           this

           #|
           (printf "splice ~a to ~a with ~a\n" low high expression)
           (set! value (append (drop value (fixnum->number low))
                               (list expression)
                               (take value (fixnum->number high))))
           |#
           )

		 (define/public (get-item arg)
           (list-ref value (fix-range (fixnum->number arg))))

		 (define/public (get-item-range low high)
		   (make (take (drop value low)
                       (- high low))))

		 (define/public (&& block arg)
           (make (set-map (set-intersect (apply set value) (apply set (get-field value arg))) values)))

		 (define/public (&or block arg)
           (make (set-map (set-union (apply set value)
                                     (apply set (get-field value arg)))
                          values)))

		 (define/public (&- block arg)
		   (make (set-map (set-subtract (apply set value)
					     (apply set (get-field value arg)))
				  values)))

         (define/override (to_s b)
           (let ((vs (map (lambda (v)
                            (if (number? v)
                              (send String new #f (number->string v))
                              (send v to_s b)))
                          value)))
             (send String new #f (apply string-append
                                        (map (lambda (s)
                                               (send (send s to_s #f) &ruby->native))
                                             vs)))))

		 (define/public (&* block arg)
		   (match arg
			  [(? (lambda (x) (send String instance? x)))
			   (send String new #f
                     (apply string-append
                            (map (lambda (x) (send (send (convert-to-object x) to_s #f) &ruby->native))
                                 (add-between value arg))))]
			  [else
			    (define new-values
			      (apply append
				     (for/list ([i (in-range 0 (fixnum->number (convert-to-object arg)))])
					       value)))
			    (send Array new #f new-values)]))

		 (define/public (&+ block arg)
                   (define new-values
		     (append value (send arg get-values)))
		   (send Array new #f new-values))

		 (define/public (&!= block arg)
		   (not (equal? value (send arg get-values))))

         (define/public (each func)
           (for/last ([i value])
                     (send func call #f i)))

         (define/public (inject func start)
           (foldl (lambda (new accum)
                    (send func call #f accum new))
                  start
                  value))

         (super-new)))))

(provide puts)
(define (puts block value)
  (printf "~a\n" (let ((v* value))
                   (if (number? v*)
                     v*
                     (send (send v* to_s block) &ruby->native)))))

(provide (rename-out (ruby-print print)))
(define (ruby-print block . vals)
  (printf "~a" (apply string-append ""
                      (map (lambda (v)
                             (let ((v* v))
                               (if (number? v*)
                                 (number->string v*)
                                 (send (send v* to_s block) &ruby->native))))
                           vals))))

(provide (rename-out (ruby-lambda lambda)))
(define (ruby-lambda block)
  block)

(define-struct nil ())
(provide make-nil)

(define-for-syntax (make-variable-id id)
  (let ((var (string->symbol (string-append "var-"
                                            (symbol->string (syntax->datum id))))))
    (datum->syntax id var id id)))

(define-syntax define-syntax*
  (syntax-rules ()
    [(_ (name args ...) body ...)
     (begin
       (define-syntax name (lambda (args ...) body ...))
       (provide name))]
    [(_ name expr)
     (begin
       (define-syntax name expr)
       (provide name))]
    ))

(define-syntax* &Program
  (syntax-rules ()
    ((_ compstmt)
     (time compstmt))))

(define-for-syntax (do-constant stx)
  (syntax-case stx (&Constant)
    ((&Constant id) #'id)))

(define-for-syntax (do-id stx)
  (syntax-case stx (&Identifier)
    [(&Identifier id) #'id
     #;
     (let ((var (string->symbol (string-append "ruby:"
                                               (symbol->string (syntax->datum #'id))))))
       (datum->syntax #'id var #'id #'id))]
    [else (raise-syntax-error 'do-id "not an identifier" stx)]
    ))

(define-syntax* &Constant
  (lambda (stx)
    (syntax-case stx ()
      ((_ id)
       (do-id #'(&Identifier id))))))

(define-for-syntax (variable var)
  (syntax-case var (&Variable)
    [(&Variable id)
     (begin
       #;
       (printf "Do variable for ~a\n" #'id)
       (make-variable-id (do-id #'id)))]
    [else (raise-syntax-error 'variable "not a variable" var)]))

(define-for-syntax (do-block block)
  (syntax-case block (&Block)
    ((_ var* body)
     (with-syntax (((var ...) (map variable (syntax->list #'var*))))
       #'(send Proc new #f (lambda (b var ...) body))))))

(define-for-syntax (bound? id)
  (case (identifier-binding id)
    ((lexical) #t)
    (else #f)))

(define-syntax* convert-to-number
  (syntax-rules ()
    [(_ x) (if (number? x) x
             (get-field value x))]))

(define-syntax* convert-to-object
  (syntax-rules ()
    [(_ x) (cond
             [(number? x) (send Fixnum new #f x)]
             [else x])]))

(define-syntax* &Method-call
  (lambda (stx)
    (syntax-case stx ()
      ((_ object op args block)
       (with-syntax ((fop (do-id #'op))
                     (fblock (if (syntax-e #'block)
                               (do-block #'block)
                               #'block)))
         (let ([math-op (memq (syntax->datum #'fop) '(&- &+ &< &> &<= &>=))])
           (syntax-case #'args (&Call-args)
             ((&Call-args (cargs ...) crest cblock)
              (with-syntax ([(cargs-eval ...) (generate-temporaries #'(cargs ...))])
                (if math-op
                  #'(let ([object-eval object]
                          [cargs-eval cargs]
                          ...)
                      (cond
                        [(number? object-eval)
                         (+ object-eval (convert-to-number cargs-eval) ...)]
                        #;
                        [(and (number? object-eval)
                              (number? cargs-eval)
                              ...)
                         (fop object-eval cargs-eval ...)]
                        [else (send/apply object-eval fop fblock
                                          (append (list (convert-to-object cargs-eval)
                                                        ...)
                                                  '()))]))
                  #'(let ([object-eval object])
                      (send/apply (cond
                                    [(number? object-eval)
                                     (send Fixnum new #f object-eval)]
                                    [(boolean? object-eval)
                                     (send Boolean new #f object-eval)]
                                    [else object-eval])
                                  fop fblock
                                  (append (list cargs ...) '())))))))))))))

(define-syntax* &Class
  (lambda (stx)
    (syntax-case stx ()
      [(_ class-name- super body publics)
       (with-syntax ([class-name (do-constant #'class-name-)])
         (with-syntax ([string-name (symbol->string (syntax->datum #'class-name))]
                       [(public-names ...) (map do-id (syntax->list #'publics))])
           #'(define class-name
               (new RubyClass
                    (name string-name)
                    (instance-class
                      (class* ObjectClass ()
                              (define/override (to_s b)
                                (send String new #f (format "<#~a>" string-name)))
                              (define/public (&ruby->native) (to_s #f))
                              (public public-names ...)
                              body
                              (super-new)))))))])))

(define-syntax* &Compstmt
  (lambda (stx)
    (syntax-case stx ()
      ((_) #'(make-nil))
      [(_ body)
       (syntax-case #'body (&Assignment)
         ((&Assignment lhs expr)
          (with-syntax ((var (variable #'lhs)))
            (if (bound? #'var)
              #'(begin (set! var expr)
                       var)
              #'(let ((var expr))
                  var))))
         (_
           #'(begin body)))]
      [(_ body0 body1 bodys ...)
       (syntax-case #'body0 (&Assignment &Array-lookup)
         [(&Assignment (&Array-lookup object start end)
		       expression)
	  (with-syntax ([var (variable #'object)])
            (if (bound? #'var)
	      #'(begin
		  (send var splice start end expression)
		  (&Compstmt body1 bodys ...))
	      (raise-syntax-error "~a is unbound" #'var)))]
         [(&Assignment lhs expr)
          (with-syntax ((var (variable #'lhs)))
            (if (bound? #'var)
              #'(begin (set! var expr)
                       (&Compstmt body1 bodys ...))
              #'(let ((var expr))
                  (&Compstmt body1 bodys ...))))]
         (_
           #'(begin body0 (&Compstmt body1 bodys ...))))])))

(define-syntax* &If
  (lambda (stx)
    (syntax-case stx ()
      ((_ condition body elseifs else*)
       (with-syntax (((rest ...)
                      (map (lambda (elseif-stx)
                             (syntax-case elseif-stx (&Elseif)
                               ((_ condition body)
                                #'(condition body))))
                           (syntax->list #'elseifs))))
         #'(cond
             (condition body)
             rest ...
             (else else*)))))))

(define-syntax* &Array
  (lambda (stx)
    (syntax-case stx ()
      ((_ arg ...)
       #'(send Array new #f (list arg ...))))))

(define-for-syntax (do-arg stx)
  (syntax-case stx (&Normal-argument)
    ((&Normal-argument id)
     (make-variable-id (do-id #'id)))))

(provide (rename-out (- &-)
                     #;
                     (+ &+)
                     (< &<)
                     (not racket:not)
                     (regexp racket:regexp))
         quote)

(provide defined?)
(define (defined? block something)
  #t)

;; all operations should be method calls
#;
(define-syntax* &Operation
  (lambda (stx)
    (syntax-case stx ()
      ((_ op left right)
       #'(let ((a left)
               (b right))
           (if (and (number? a)
                    (number? b))
             (op a b)
             (send a op #f b)))))))

(define-syntax* &Variable
  (lambda (stx)
    (syntax-case stx ()
      ((_ id)
       (with-syntax ((var-id (make-variable-id (do-id #'id))))
         #;
         (printf "Binding ~a ~a is ~a\n" #'var-id (syntax->datum #'var-id)
                 (identifier-binding #'var-id))
         (case (identifier-binding #'var-id)
           ((lexical) #'var-id)
           (else (with-syntax ((f (do-id #'id)))
                   #'(f #f)))))))))

(define-for-syntax (do-args stx)
  ;; (printf "Wacka ~a\n" (syntax->datum stx))
  (syntax-case stx (args rest1 block)
    ((f (args fargs ...) (rest1 frest) (block fblock))
     #;
     (begin
       (printf "a ~a b ~a c ~a\n"
               (syntax->datum #'(fargs ...))
               (syntax->datum #'frest)
               (syntax->datum #'fblock))
       #t)
     (with-syntax (((zargs ...) (map do-arg (syntax->list #'(fargs ...)))))
       #'(fblock zargs ...)))))

(define-syntax* &Number
  (syntax-rules ()
    [(_ v) v]
    #;
    ((_ v) (send Fixnum new #f v))))

(define-syntax* &String-literal
  (lambda (stx)
    (define (append-all stuff)
      (syntax-case stuff ()
        [(a b rest ...)
         (if (and (string? (syntax-e #'a))
                  (string? (syntax-e #'b)))
           (with-syntax ([both (string-append (syntax-e #'a) (syntax-e #'b))])
             (append-all #'(both rest ...)))
           (with-syntax ([more (append-all #'(b rest ...))])
             #;
             (debug "a is ~a -- `~a' string? ~a\n" #'a (syntax-e #'a) (string? (syntax-e #'a)))
             (if (string? (syntax-e #'a))
               #'(string-append a more)
               #'(if (string? a)
                   (string-append a more)
                   (string-append (send (send a to_s #f) &ruby->native) more))
               #;
               #'(string-append (send a &ruby->native) more)))
           )]
        [(a rest ...) (with-syntax ([more (append-all #'(rest ...))])
                        (if (string? (syntax-e #'a))
                          #'(string-append a more)
                          #'(if (string? a)
                              (string-append a more)
                              (string-append (send (send a to_s #f) &ruby->native) more))))]
        [() #'""]))
    (syntax-case stx ()
      [(_ v ...)
       (with-syntax ([result (append-all #'(v ...))])
         #'(send String new #f result))])))

(define-syntax* &Function-call
  (lambda (stx)
    (syntax-case stx (&Function-call op args block)
      ((&Function-call (op fop*)
                      (args fargs)
                      (block fblock*))
       (with-syntax ((fop (do-id #'fop*))
                     (fblock (if (syntax-e #'fblock*)
                               (do-block #'fblock*)
                               #'fblock*)))
         (syntax-case #'fargs (&Call-args)
           ((&Call-args (cargs ...) ccrest bclock)
            #'(fop fblock cargs ...)
            #;
            #'(apply fop fblock (append (list cargs ...) '())))))))))

(define-syntax* &Body
  (lambda (stx)
    (syntax-case stx (&Body)
      ((&Body body) #'body))))

(define-syntax* &Definition-statement
  (lambda (stx)
    (syntax-case stx (name args body)
      ((_ (name dname)
          (args dargs)
          (body dbody))
       (with-syntax (((margs ...) (do-args #'dargs))
                     (d-id (do-id #'dname)))
         ;; (printf "Binding ~a args ~a\n" #'(margs ...) (syntax->datum #'(margs ...)))
         #'(define (d-id margs ...) dbody))))))

(define-syntax* (&For stx)
  (syntax-case stx ()
    [(_ vars range body)
     (with-syntax ([(var ...) (map variable (syntax->list #'vars))])
       #'(for ([var ... range])
           body))]))

(define-syntax* (&Range stx)
  (syntax-case stx ()
    [(_ low high)
     #'(let ([high-number (fixnum->number high)]
             [low-number (fixnum->number low)])
         (if (< high-number low-number)
           (in-range low-number high-number -1)
           (in-range low-number high-number)))]))

(define-syntax* (&Range-inclusive stx)
  (syntax-case stx ()
    [(_ low high)
     #'(let ([high-number (fixnum->number high)]
             [low-number (fixnum->number low)])
         (if (< high-number low-number)
           (in-range low-number (- 1 high-number) -1)
           (in-range low-number (+ 1 high-number))))]))

(define-syntax* &String-computation
  (syntax-rules ()
   [(_ expression)
    (send (send (convert-to-object expression) to_s #f) &ruby->native)]))
  
(define-syntax* (&Array-lookup stx)
  (syntax-case stx (&Range &Range-inclusive)
    [(_ object start end)
     (with-syntax ([var (variable #'object)])
       #'(send var get-item-range (fixnum->number start) (fixnum->number end)))]
    [(_ object (&Range low high))
     (with-syntax ([var (variable #'object)])
       #'(send var get-item-range (fixnum->number low) (fixnum->number high)))]
    [(_ object (&Range-inclusive low high))
     (with-syntax ([var (variable #'object)])
       #'(send var get-item-range (fixnum->number low) (+ 1 (fixnum->number high))))]
    [(_ object item)
     (with-syntax ([var (variable #'object)])
       #'(send var get-item item))]))
