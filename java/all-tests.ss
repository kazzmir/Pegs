#!/bin/sh
#|
exec mzscheme -qu "$0" ${1+"$@"}
|#
#lang scheme

(require (only-in "peg.ss" dump-statistics))
(require "java.ss")
(require srfi/13)

(define failed-dirs '("test/fail"))

#;
(define passed-dirs '("test/pass/openjdk/*"))

(define passed-dirs '("test/pass"
                      "test/pass/gcj/*"
                      "test/pass/openjdk/*"))

#;
(define passed-dirs '("test/pass"
                      "test/pass/gcj/*"))

(define (doit file passed?)
  ;; (printf "File ~a\n" file)
  (if (passed? (parse file))
    (begin
      ;; (printf "~a ok\n" file)
      1)
    (begin
      (printf "~a failed!\n" file)
      0)))

;;  (printf "~a\n" (pretty-print (parse (string->path file)))))

(define (java-files dir)
  (with-handlers ((exn:fail? (lambda (e) '())))
    (let ((all (directory-list dir)))
      (map (lambda (name)
	     (build-path dir name))
	   (filter (lambda (name) (string-suffix? ".java" (path->string name)))
		   all)))))

(define (all-in-dir dir passed?)
  (let ([files (java-files dir)])
    (when (not (null? files))
      (printf "Checking directory ~a\n" dir))
    (values (apply + 0 (map (lambda (f)
			      (doit f passed?))
			    files))
	    (length files))))

(define (wildcard? dir)
  (char=? #\* (string-ref dir (sub1 (string-length dir)))))

(define (find-dirs dir)
  (let ([dir (string->path (substring dir 0 (sub1 (string-length dir))))])
    (define (dirs root)
      (let ([all (directory-list root)])
        (apply append
               (list (path->string root))
               (map (lambda (name)
                      (dirs name))
                    (filter (lambda (name)
                              ;; (printf "Checking if ~a is a directory\n" (path->string name))
                              (directory-exists? name))
                            (map (lambda (n)
                                   (build-path root n))
                                 all))))))
    (dirs dir)))

(define (check dirs passed?)
  (let loop ((passed 0)
	     (total 0)
	     (dirs dirs))
    (if (null? dirs)
      (begin
	(printf "~a passed out of ~a\n" passed total)
	(values passed total))
      (if (wildcard? (car dirs))
        (loop passed total (append (cdr dirs) (find-dirs (car dirs))))
        (let-values (((score number) (all-in-dir (car dirs) passed?)))
          (loop (+ passed score) (+ total number) (cdr dirs)))))))

(time
    (define-values (passed t1)
                   (begin
                     (printf "Checking passing tests\n")
                     (check passed-dirs (lambda (n) n))))
    (define-values (failed t2)
                   (begin
                     (printf "Checking failing tests\n")
                     (check failed-dirs (lambda (n) (not n)))))
    (printf "\nPassing ~a%\n" (* 100 (+ 0.0 (/ passed (+ t1 t2))))))

(dump-statistics)
