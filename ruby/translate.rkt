#lang racket/base

;; Good reference for ruby syntax and semantics
;; http://web.njit.edu/all_topics/Prog_Lang_Docs/html/ruby/syntax.html

(require racket/match
         (for-syntax racket/base))

(require "ast.rkt"
         "ruby.rkt")

(define no-ctxt-stx (read-syntax #f (open-input-string "Ruby")))

;; creates a syntax object given an s-expression and some line information
(define (make/loc syntax source position span)
  (let ((s (list source #f #f position (max 0 (sub1 span)))))
    (datum->syntax #f syntax s no-ctxt-stx)))

(define enable-debug (make-parameter #f))
(define-syntax (debug stx)
  (syntax-case stx ()
    [(_ stuff ...)
     #'(when (enable-debug)
         (printf stuff ...))]))

(define verbose 0)

(provide ruby-ast->syntax)
(define (ruby-ast->syntax source ruby-ast starting-position)
  (define (make/loc* value loc span)
    (make/loc value source (+ loc starting-position) span))
  (define (find-definitions compstmt)
    (match compstmt
           ((struct Body (loc pos (struct Compstmt (loc2 pos2 stmts))
                              else* rescue ensure))
            (let inner-loop
              ([names '()]
               [stmts stmts])
              (if (null? stmts)
                names
                (match (car stmts)
                       ((struct Definition-statement (loc pos name args body))
                        (inner-loop (cons (loop name) names) (cdr stmts)))
                       (else (inner-loop names (cdr stmts)))))))))
  (define (build-regexp contents)
    ;; FIXME
    #f
    )
  (define (loop ast)
    (match ast
      ((struct Program (loc pos compstmt))
       (debug "Program ~a\n" compstmt)
       (make/loc* `(&Program ,(loop compstmt))
                  loc pos))
      ((struct Identifier (loc pos name))
       (debug "Identifier ~a\n" name)
       (make/loc* `(&Identifier ,(make/loc* (string->symbol name) loc pos))
                  loc pos))

      ;; is this right?
      ((struct Constant (loc pos name))
       (make/loc* `(&Constant ,(make/loc* (string->symbol name) loc pos))
                  loc pos))

      [(struct Normal-argument (loc pos id))
       (debug "Normal-argument ~a\n" id)
       (make/loc* `(&Normal-argument ,(loop id)) loc pos)]

      [(struct Operation+ (loc pos)) (make/loc* '(&Identifier &+) loc pos)]
      [(struct Operation- (loc pos)) (make/loc* '(&Identifier &-) loc pos)]
      [(struct Operation< (loc pos)) (make/loc* '(&Identifier &<) loc pos)]
      [(struct Operation* (loc pos)) (make/loc* `(&Identifier &*) loc pos)]
      [(struct Operation!= (loc pos)) (make/loc* `(&Identifier &!=) loc pos)]
      [(struct Operation/ (loc pos)) (make/loc* '(&Identifier &/) loc pos)]
      [(struct Operation& (loc pos)) (make/loc* '(&Identifier &&) loc pos)]
      [(struct Operation-or (loc pos)) (make/loc* '(&Identifier &or) loc pos)]
      [(struct Operation== (loc pos)) (make/loc* '(&Identifier &==) loc pos)]
      [(struct Operation&& (loc pos)) (make/loc* '(&Identifier &&&) loc pos)]
      [(struct Operation<=> (loc pos)) (make/loc* '(&Identifier &<=>) loc pos)]
      [(struct Operation<< (loc pos)) (make/loc* '(&Identifier &<<) loc pos)]

      [(struct Symbol (location span id))
       (make/loc* ''id location span)]

      [(struct Regexp (loc pos contents options))
       (make/loc* `(racket:regexp ,(build-regexp contents)) loc pos)]

      [(struct Nil (loc pos))
       (make/loc* '(make-nil) loc pos)]

      [(struct Not (loc pos expression))
       (make/loc* `(racket:not ,(loop expression)) loc pos)]

      [(struct Array-lookup (loc pos array args))
       (make/loc* `(&Array-lookup ,(loop array) ,@(map loop args))
                  loc pos)]

      [(struct String-computation (loc pos expr))
       (make/loc* `(&String-computation ,(loop expr))
                  loc pos)]

      [(struct For-statement (loc pos vars expr body))
       (let ((vs (match vars
                   ((struct Empty-var (loc pos)) '())
                   ((struct Lhs (loc pos var)) (list (loop vars)))
                   ((struct Mlhs (loc pos vars rest))
                    (map loop vars)))))
         (make/loc* `(&For ,vs
                           ,(loop expr)
                           ,(loop body))
                    loc pos))]

      [(struct Range-inclusive (loc pos start end))
       (make/loc* `(&Range-inclusive ,(loop start) ,(loop end))
                  loc pos)]

      [(struct Range-exclusive (loc pos start end))
       (make/loc* `(&Range ,(loop start) ,(loop end))
                  loc pos)]

      ((struct Array (loc pos args))
       (debug "Array ~a\n" args)
       (make/loc* `(&Array ,@(map loop args)) loc pos))

      ((struct Variable (loc pos var))
       (make/loc* `(&Variable ,(loop var)) loc pos))

      ((struct Block (loc pos vars body))
       (debug "Block ~a ~a\n" vars body)
       (let ((vs (match vars
                   ((struct Empty-var (loc pos)) '())
                   ((struct Lhs (loc pos var)) (list (loop vars)))
                   ((struct Mlhs (loc pos vars rest))
                    (map loop vars)))))

         (make/loc* `(&Block (,@vs) ,(loop body)) loc pos)))

      ((struct String-escaped (loc pos value))
       (case value
         ((#\n) (make/loc* "\n" loc pos))))

      ((struct String-char (loc pos value))
       (make/loc* value loc pos))

      ((struct String-literal (loc pos str))
       (make/loc* `(&String-literal ,@(map loop str))
                  loc pos))

      ;; fix
      ((struct Body (loc pos body else rescue ensure))
       (debug "Body ~a ~a ~a ~a\n" body else rescue ensure)
       (make/loc* `(&Body ,(loop body)) loc pos))

      ((struct Else (loc pos body))
       (loop body))

      ((struct If-statement (loc pos condition body elseifs else))
       (debug "If ~a ~a ~a ~a\n" condition body elseifs else)
       (make/loc* `(&If ,(loop condition) ,(loop body)
                        (,@(map loop elseifs))
                        ,(if else (loop else) '(void)))
                  loc pos))

      ((struct Elseif (loc pos cond body))
       (debug "Elseif ~a ~a\n" cond body)
       (make/loc* `(&Elseif ,(loop cond) ,(loop body)) loc pos))

      ((struct Lhs (loc pos var))
       (loop var))

      ((struct Assignment (loc pos lhs expr))
       (make/loc* `(&Assignment ,(loop lhs) ,(loop expr))
                  loc pos))

      ((struct Function-arglist (loc pos args rest block))
       (debug "Function-arglist ~a ~a ~a\n" args rest block)
       (make/loc* `(&Function-arglist (args ,@(map (lambda (a)
                                                     (loop a)) args))
                                      (rest1 ,(if rest (loop rest)
                                                (make/loc* (gensym) loc pos)))
                                      (block ,(if block (loop block) 
                                                (make/loc* (gensym) loc pos))))
                  loc pos)
       #;
       (with-syntax (((args ...) (map ruby->s-expr args))
                     (rest (if rest (ruby->s-expr rest) (gensym)))
                     (block (if block (ruby->s-expr block) (gensym))))
         (make/loc #'(#:block block args ... . rest)
                   source loc pos)))

      [(struct Multiple-assignment (location span left-hand right-hand))
       ;; FIXME
       (make/loc* `(void) location span)]

      ((struct Call-args (loc pos args rest block))
       (debug "Call-args ~a ~a ~a\n" args rest block)
       (make/loc* `(&Call-args (,@(map (lambda (a)
                                         (loop a)) args))
                               ,(if rest (loop rest) (make/loc* '() loc pos))
                               ,(if block (loop block) (make/loc* #f loc pos)))
                  loc pos)
       #;
       (with-syntax (((args ...) (map ruby->s-expr args))
                     (rest (if rest (ruby->s-expr rest) #''()))
                     (block (if block (ruby->s-expr block) #f)))
         (make/loc #'(append (list args ...)
                             rest)
                   source loc pos)))

      ((struct Method-call (loc pos object op args block))
       (debug "Method-call ~a ~a ~a ~a\n" object op args block)
       (make/loc* `(&Method-call ,(loop object)
                                 ,(loop op)
                                 ,(loop args)
                                 ,(if block
                                    (loop block)
                                    (make/loc* #f loc pos)))
                  loc pos))

      ;; fix
      ((struct Function-call (loc pos op args block))
       (debug "Function-call ~a ~a ~a\n" op args block)
       (make/loc* `(&Function-call (op ,(loop op))
                                   (args ,(loop args))
                                   (block ,(if block (loop block)
                                             (make/loc* #f loc pos))))
                  loc pos)
       #;
       (make/loc (with-syntax ((op (ruby->s-expr op)))
                   (match args
                     ((struct Call-args (loc pos args rest block))
                      (make/loc
                        (with-syntax (((margs ...) (map ruby->s-expr args))
                                      (rest (if rest (ruby->s-expr rest) #''()))
                                      (block (if block (ruby->s-expr block) #'#f)))
                          #'(apply op #:block block (append (list margs ...) rest)))
                        loc pos))))
                 source loc pos))

      ((struct Class-statement (loc pos name super body))
       (debug "Class ~a ~a ~a\n" name super body)
       (make/loc* `(&Class ,(loop name)
                           ,(if super (loop super) (make/loc* #f loc pos))
                           ,(loop body)
                           ,(find-definitions body))
                  loc pos))

      ((struct Compstmt (loc pos stmts))
       (debug "Compstmt ~a\n" stmts)
       (make/loc* `(&Compstmt ,@(map (lambda (s)
                                       (loop s)) stmts))
                  loc pos))

      [(struct True (loc pos))
       (make/loc* '#t loc pos)]

      ((struct Definition-statement (loc pos name args body))
       (debug "Definition-statement ~a ~a ~a\n" name args body)
       (make/loc* `(&Definition-statement (name ,(loop name))
                                          (args ,(loop args))
                                          (body ,(loop body)))
                  loc pos)
       #;
       (with-syntax ((name (ruby->s-expr name))
                     (args (ruby->s-expr args))
                     (body (ruby->s-expr body)))
         (make/loc #'(define name (lambda args
                                    body))
                   loc pos)))

      ((struct Number (loc pos value))
       (debug "Number ~a\n" value)
       (make/loc* `(&Number ,(make/loc* value loc pos)) loc pos))

      ((struct Operation (loc pos op arg1 arg2))
       (debug "Operation ~a ~a ~a\n" op arg1 arg2)
       (loop (make-Method-call loc pos arg1 op (make-Call-args loc pos (list arg2) #f #f) #F))
       #;
       (make/loc* `(&Method-call ,(loop arg1)
                                 ,(loop op)
                                 ,(loop arg2) #f)
                  loc pos)
       #;
       (make/loc* `(&Operation ,(loop op)
                               ,(loop arg1)
                               ,(loop arg2))
                  loc pos))
      [else (error 'translate "don't know how to translate ~a originally as `~a' at position ~a to ~a" ast (pretty-print ast)
                   (Location-position ast)
                   (+ (Location-position ast)
                      (Location-span ast)))]
      ))
  (loop ruby-ast))

#;
(define-syntax (schemify stx)
  (define code "
def foo(x)
	x + 1
end
foo 2
    ")
  (let ((ast (parse code)))
    (ruby->scheme ast)))

#;
(schemify)
