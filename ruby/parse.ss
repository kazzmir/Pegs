#lang scheme

(require "ruby.ss")

(define (doit file)
  (if (parse (string->path file))
    (printf "Ok\n")
    (printf "Syntax error\n")))

(time (doit (command-line #:args (file) file)))
