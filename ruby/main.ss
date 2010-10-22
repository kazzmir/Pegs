#lang racket/base

(require racket/class
	 racket/match
	 racket/list
	 racket/set
	 (for-syntax racket/base))

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
		 (define/public (&!= block arg)
		   (equal? value (send arg &ruby->native)))
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

                 (define/public (&+ n b)
                   (let ((n-value (get-field value n)))
                     (make (+ value n-value))))

                 (define/public (&- n b)
                   (let ((n-value (get-field value n)))
                     (make (- value n-value))))

                 (define/public (@- b)
                   (make (- value))) 

                 (define/public (times b)
                   (for/last ((i (in-range value)))
                             (send/apply b call #f
                                         (list (make i)))))

                 (define/public (&< n b)
                   (let ((n-value (get-field value n)))
                     (< value n-value)))

                 (super-new)))))

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

		 (define/public (splice low high expression)
                   (set! value (append (drop value low)
				       (list expression)
				       (take value high))))

		 (define/public (get-item arg)
                   (list-ref value arg))

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
                     (send String new #f (apply string-append ""
                                                         (map (lambda (s)
                                                                (send s &ruby->native))
                                                              vs)))))

		 (define/public (&* block arg)
		   (match arg
			  [(? (lambda (x) (send String instance? x)))
			   (send String new #f
				 (map (lambda (x) format "~a" x)
				      (add-between value (send arg &ruby->native))))]
			  [else
			    (define new-values
			      (apply append
				     (for/list ([i (in-range 0 arg)])
					       value)))
			    (send Array new #f new-values)]))

		 (define/public (&+ block arg)
                   (define new-values
		     (append value (send arg get-values)))
		   (send Array new #f new-values))

		 (define/public (&!= block arg)
		   (not (equal? value (send arg get-values))))

                 (define/public (each func)
                   (for/last ((i value))
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
              (with-syntax ([(cargs-eval ...) (generate-temporaries #'(cargs ...))]
			    )
                (if math-op
		  #'(let ([object-eval object]
			  [cargs-eval cargs]
			  ...)
		      (cond
			[(and (number? object-eval)
			      (number? cargs-eval)
			      ...)
			   (fop object-eval cargs-eval ...)]
			[else (send/apply object-eval fop fblock
				    (append (list cargs-eval ...) '()))]))
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

(provide (rename-out (- &-) (+ &+) (< &<)
		     (+ racket:+)
		     (not racket:not)
		     (regexp racket:regexp)
		     )
	 quote
	 )

(provide defined?)
(define (defined? something)
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
    ((_ v) v)))

(define-syntax* &String-literal
  (lambda (stx)
    (syntax-case stx ()
      ((_ v ...) #'(send String new #f 
                        (apply string-append "" (list v ...)))))))



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
     #'(in-range low high)]))

(define-syntax* &String-computation
  (syntax-rules ()
   [(_ expression)
    (send (send expression to_s #f) &ruby->native)]))
  
(define-syntax* (&Array-lookup stx)
  (syntax-case stx (&Range)
    [(_ object start end)
     (with-syntax ([var (variable #'object)])
       #'(send var get-item-range start end))]
    [(_ object (&Range low high))
     (with-syntax ([var (variable #'object)])
       #'(send var get-item-range low high))]
    [(_ object item)
     (with-syntax ([var (variable #'object)])
       #'(send var get-item item))]))
