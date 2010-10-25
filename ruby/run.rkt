#lang racket/base

(require (only-in "../peg.rkt" dump-statistics dump-liner))
(require "ruby.rkt")
(require racket/cmdline)

(define (doit file)
  (let ((a (parse (string->path file))))
    (if a
      (pretty-print a)
      (format "Could not parse ~a" file))))

(printf "~a\n"
	(doit (command-line #:args (file) file)))

(dump-statistics)
(dump-liner)
