#lang scheme

(require "ruby.rkt")

(define (doit file)
  (if (parse (string->path file))
    (printf "Ok\n")
    (printf "Syntax error\n")))

(time (doit (command-line #:args (file) file)))
