#lang racket/base

;; Good reference for ruby syntax and semantics
;; http://web.njit.edu/all_topics/Prog_Lang_Docs/html/ruby/syntax.html

(require racket/match)

(require "ast.ss")

(define no-ctxt-stx (read-syntax #f (open-input-string "Ruby")))

(define (make/loc syntax source position span)
  (let ((s (list source #f #f position (max 0 (sub1 span)))))
    (datum->syntax #f syntax s no-ctxt-stx)))

(define verbose 0)

(provide ruby->s-expr)
(define (ruby->s-expr source ruby-ast starting-position)
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
  (define (loop ast)
    (match ast
	 ((struct Program (loc pos compstmt))
          (printf "Program ~a\n" compstmt)
          (make/loc* `(&Program ,(loop compstmt))
                     loc pos))
	 ((struct Identifier (loc pos name))
          (printf "Identifier ~a\n" name)
          (make/loc* `(&Identifier ,(make/loc* (string->symbol name) loc pos))
                    loc pos))

         ;; is this right?
         ((struct Constant (loc pos name))
          (make/loc* `(&Constant ,(make/loc* (string->symbol name) loc pos))
                    loc pos))

	 ((struct Normal-argument (loc pos id))
          (printf "Normal-argument ~a\n" id)
          (make/loc* `(&Normal-argument ,(loop id)) loc pos))

         ((struct Operation+ (loc pos)) (make/loc* '&+ loc pos))
	 ((struct Operation- (loc pos)) (make/loc* '&- loc pos))
	 ((struct Operation< (loc pos)) (make/loc* '&< loc pos))
	 ((struct Operation* (loc pos)) (make/loc* '&* loc pos))
	 ((struct Operation/ (loc pos)) (make/loc* '&/ loc pos))

         ((struct Array (loc pos args))
          (printf "Array ~a\n" args)
          (make/loc* `(&Array ,@(map loop args)) loc pos))

         ((struct Variable (loc pos var))
          (make/loc* `(&Variable ,(loop var)) loc pos))

         ((struct Block (loc pos vars body))
          (printf "Block ~a ~a\n" vars body)
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
          (printf "Body ~a ~a ~a ~a\n" body else rescue ensure)
          (make/loc* `(&Body ,(loop body)) loc pos))

         ((struct Else (loc pos body))
          (loop body))

         ((struct If-statement (loc pos condition body elseifs else))
          (printf "If ~a ~a ~a ~a\n" condition body elseifs else)
          (make/loc* `(&If ,(loop condition) ,(loop body)
                          (,@(map loop elseifs))
                          ,(if else (loop else) '(void)))
                     loc pos))

         ((struct Elseif (loc pos cond body))
          (printf "Elseif ~a ~a\n" cond body)
          (make/loc* `(&Elseif ,(loop cond) ,(loop body)) loc pos))

         ((struct Lhs (loc pos var))
          (loop var))

         ((struct Assignment (loc pos lhs expr))
          (make/loc* `(&Assignment ,(loop lhs) ,(loop expr))
                     loc pos))

	 ((struct Function-arglist (loc pos args rest block))
          (printf "Function-arglist ~a ~a ~a\n" args rest block)
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

	 ((struct Call-args (loc pos args rest block))
          (printf "Call-args ~a ~a ~a\n" args rest block)
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
          (printf "Method-call ~a ~a ~a ~a\n" object op args block)
          (make/loc* `(&Method-call ,(loop object)
                                    ,(loop op)
                                    ,(loop args)
                                    ,(if block (loop block)
                                       (make/loc* #f loc pos)))
                     loc pos))

	 ;; fix
	 ((struct Function-call (loc pos op args block))
          (printf "Function-call ~a ~a ~a\n" op args block)
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
          (printf "Class ~a ~a ~a\n" name super body)
          (make/loc* `(&Class ,(loop name)
                              ,(if super (loop super) (make/loc* #f loc pos))
                              ,(loop body)
                              ,(find-definitions body))
                     loc pos))

	 ((struct Compstmt (loc pos stmts))
          (printf "Compstmt ~a\n" stmts)
          (make/loc* `(&Compstmt ,@(map (lambda (s)
                                        (loop s)) stmts))
                    loc pos))

	 ((struct Definition-statement (loc pos name args body))
          (printf "Definition-statement ~a ~a ~a\n" name args body)
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
          (printf "Number ~a\n" value)
          (make/loc* `(&Number ,(make/loc* value loc pos)) loc pos))

	 ((struct Operation (loc pos op arg1 arg2))
          (printf "Operation ~a ~a ~a\n" op arg1 arg2)
          (make/loc* `(&Operation ,(loop op)
                                 ,(loop arg1)
                                 ,(loop arg2))
                    loc pos))))
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
