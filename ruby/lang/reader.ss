(module reader syntax/module-reader ruby

#:read ruby-read
#:read-syntax ruby-read-syntax
#:whole-body-readers? #t

(require "../ruby.rkt"
         "../translate.rkt"
         (only-in racket/pretty pretty-format))

(define (ruby-read port)
  (syntax->datum (ruby-read-syntax #f port)))

(define (ruby-read-syntax source-name port)
  (let-values (((line column position)
                (port-next-location port)))
    (let ((ast (parse port)))
      (when (not ast)
        (raise-syntax-error 'read "syntax error"))
      (let ((expr (ruby-ast->syntax source-name ast position)))
        (printf "Created ~a\n" (pretty-format (syntax->datum expr)))
        (list expr)))))
)
