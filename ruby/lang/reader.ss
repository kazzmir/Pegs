(module reader syntax/module-reader ruby

#:read ruby-read
#:read-syntax ruby-read-syntax
#:whole-body-readers? #t

(require "../ruby.ss")
(require "../translate.ss")

(define (ruby-read port)
  (syntax->datum (ruby-read-syntax #f port)))

(define (ruby-read-syntax source-name port)
  (let-values (((line column position)
                (port-next-location port)))
    (let ((ast (parse port)))
      (let ((expr (ruby->s-expr source-name ast position)))
        (printf "Created ~a\n" (syntax->datum expr))
        (list expr)))))
)
