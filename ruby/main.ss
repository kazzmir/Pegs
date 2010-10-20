#lang racket/base

(require racket/class)

#|
(let ((global Nil) ...)
  (let ((function blah) ...)
    (let ((locals foo) ...)
      body)
|#

(require (for-syntax scheme/base))

(provide #%app #%datum #%top #%module-begin)

(define RubyClass
  (class* object% ()
          (init-field instance-class)
          (init-field name)

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

(define-for-syntax (make-variable-id id)
  (let ((var (string->symbol (string-append "var-"
                                            (symbol->string (syntax->datum id))))))
    (datum->syntax id var id id)))

(define-syntax define-syntax*
  (syntax-rules ()
    ((_ name expr)
     (begin
       (define-syntax name expr)
       (provide name)))))

(define-syntax* &Program
  (syntax-rules ()
    ((_ compstmt)
     (time compstmt))))

(define-for-syntax (do-constant stx)
  (syntax-case stx (&Constant)
    ((&Constant id) #'id)))

(define-for-syntax (do-id stx)
  (syntax-case stx (&Identifier)
    ((&Identifier id) #'id
     #;
     (let ((var (string->symbol (string-append "ruby:"
                                               (symbol->string (syntax->datum #'id))))))
       (datum->syntax #'id var #'id #'id)))))

(define-syntax* &Constant
  (lambda (stx)
    (syntax-case stx ()
      ((_ id)
       (do-id #'(&Identifier id))))))

(define-for-syntax (variable var)
  (syntax-case var (&Variable)
    ((&Variable id)
     (make-variable-id (do-id #'id)))))

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
              (with-syntax ([(cargs-eval ...) (generate-temporaries #'(cargs ...))])
                (if math-op
                #'(let ([object-eval object]
                        [cargs-eval cargs]
                        ...)
                    (if (and (number? object-eval)
                             (number? cargs-eval)
                             ...)
                      (fop object-eval cargs-eval ...)
                      (send/apply object-eval fop fblock
                                  (append (list cargs-eval ...) '()))))
                #'(send/apply object fop fblock
                              (append (list cargs ...) '()))))))))))))

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
      ((_ body)
       (syntax-case #'body (&Assignment)
         ((&Assignment lhs expr)
          (with-syntax ((var (variable #'lhs)))
            (if (bound? #'var)
              #'(begin (set! var expr)
                       var)
              #'(let ((var expr))
                  var))))
         (_
           #'(begin body))))
      ((_ body0 body1 bodys ...)
       (syntax-case #'body0 (&Assignment)
         ((&Assignment lhs expr)
          (with-syntax ((var (variable #'lhs)))
            (if (bound? #'var)
              #'(begin (set! var expr)
                       (&Compstmt body1 bodys ...))
              #'(let ((var expr))
                  (&Compstmt body1 bodys ...)))))
         (_
           #'(begin body0 (&Compstmt body1 bodys ...))))))))

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

(provide (rename-out (- &-) (+ &+) (< &<)))

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
  (printf "Wacka ~a\n" (syntax->datum stx))
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

(provide &Definition-statement)
(define-syntax &Definition-statement
  (lambda (stx)
    (syntax-case stx (name args body)
      ((_ (name dname)
          (args dargs)
          (body dbody))
       (with-syntax (((margs ...) (do-args #'dargs))
                     (d-id (do-id #'dname)))
         ;; (printf "Binding ~a args ~a\n" #'(margs ...) (syntax->datum #'(margs ...)))
         #'(define (d-id margs ...) dbody))))))
