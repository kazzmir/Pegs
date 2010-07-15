#lang racket/base

;; Things to fix in the parser
;; * arithmetic precedence - r1359
;; * handle qwords differently (%w,%q,%x,...)
;; * handle different string types, "", '', ``
;; * Read #{..} properly in qword and heredoc
;; * command calls with no arguments are probably local variables. - already handled
;; same for primaries with no primary-right

;; Known divergencies
;;   * cannot use %=..= as a qword
;;   * cannot use do as the "end of line token" for a while/for/until statement
;;   instead only : or a newline can be used. this is because a primary will
;;   eat the do ... end that was meant for the for loop.
;;   for i in n do
;;    ...
;;   end
;;
;; Can parse 5.023kB/s as of r1210 on a 1.8ghz Pentium4
;; Can parse 5.047kB/s as of r1213 on a 1.8ghz Pentium4
;; Can parse 5.551kB/s as of r1269 on a 1.8ghz Pentium4
;; Can parse 4.114kB/s as of r1388 on a 1.8ghz Pentium4
;; Can parse 4.469kB/s as of r1466 on a 1.8ghz Pentium4 with mzscheme 4.1.2.5

(require "peg.ss")
(require "ast.ss")
(require racket/match)

(provide pretty-print
	 (rename-out (ruby-parse parse)))

(define scheme-string string)

;; '(a b c) "," -> "a,b,c"
(define (join vars between)
  (if (null? vars)
    ""
    (let loop ((vars (cdr vars))
	       (all (car vars)))
      (cond
	((null? vars) all)
	((null? (cdr vars))
	 (string-append all between (car vars)))
	(else 
	  (loop (cdr vars)
		(string-append all between (car vars))))))))

(define (pretty-print ast)
  (match ast
         ((list) (error 'pretty-print "empty list"))
         ((struct Program (loc pos compstmt)) (pretty-print compstmt))
         ((struct Compstmt (loc pos stmts))
          (join (map pretty-print stmts) "\n"))
         ((struct Object (loc pos)) "Object")
         ((struct Assignment (loc pos lhs expr)) (format "~a = ~a"
                                                 (pretty-print lhs)
                                                 (pretty-print expr)))
         ((struct Operation-assignment (loc pos op left right))
          (format "~a ~a ~a" (pretty-print left) (pretty-print op) (pretty-print right)))
         ((struct String-literal (loc pos str)) (format "\"~a\"" (join (map pretty-print str) "")))
         ((struct String-char (loc pos c)) c)
         ((struct String-escaped (loc pos c)) (format "\\~a" c))
         ((struct String-octal (loc pos c)) (format "\\~a" c))
         #;
         ((struct Dotted-call (obj function args block))
          (format "~a.~a(~a)~a" (pretty-print obj) (pretty-print function)
                  (join (map pretty-print args) ", ")
                  (if block
                    (pretty-print block)
                    "")))
         ((struct Call-args (loc pos args rest block))
          (join (append (map pretty-print args)
                        (if rest (list (format "*~a" (pretty-print rest))) '())
                        (if block (list (format "&~a" (pretty-print block))) '()))
                ", "))

         ((struct Method-call (loc pos object op args block))
          (format "~a.~a(~a)~a"
                  (pretty-print object)
                  (pretty-print op)
                  (pretty-print args)
                  (if block
                    (pretty-print block)
                  "")))
         ((struct Function-call (loc pos op args block))
          (format "~a(~a)~a"
                  (pretty-print op)
                  (pretty-print args)
                  ;;(join (map pretty-print args) ", ")
                  (if block
                    (pretty-print block)
                    "")))
         ((struct BEGIN (loc pos body))
          (format "BEGIN {\n~a\n}\n" (pretty-print body)))
         ((struct END (loc pos body))
          (format "END {\n~a\n}\n" (pretty-print body)))
         ((struct Alias (loc pos from to))
          (format "alias ~a ~a" (pretty-print from) (pretty-print to)))
         ((struct Undef (loc pos undefs))
          (format "undef ~a" (join (map pretty-print undefs) ", ")))
         ((struct Symbol (loc pos id))
          (format ":~a" (pretty-print id)))
         ((struct Symbol-dsym (loc pos id))
          (format ":\"~a\"" id))
         ((struct Instance-variable (loc pos id))
          (format "@~a" (pretty-print id)))
         ((struct Global-variable (loc pos id))
          (format "$~a" (pretty-print id)))
         ((struct Class-variable (loc pos id))
          (format "@@~a" (pretty-print id)))

         ((struct Multiple-assignment (loc pos mlhs mrhs))
          (format "~a = ~a" (pretty-print mlhs) (pretty-print mrhs)))

         ((struct Regexp (loc pos values options))
          (format "/~a/~a" (join (map pretty-print values) "")
                  (join (map pretty-print options) "")))

         ((struct Regexp-char (loc pos obj)) obj)
         ((struct Regexp-escaped (loc pos obj))
          (format "\\~a" obj))

         ((struct Assoc (loc pos key value))
          (format "~a => ~a" (pretty-print key) (pretty-print value)))
         ((struct Assoc-list (loc pos assocs))
          (format "{ ~a }" (join (map pretty-print assocs) ", ")))

         ((struct Regexp-option-ignore-case (loc pos)) "i")
         ((struct Regexp-option-extended (loc pos)) "x")
         ((struct Regexp-option-multiline (loc pos)) "m")
         ((struct Regexp-option-once (loc pos)) "o")
         ((struct Regexp-option-none (loc pos)) "n")
         ((struct Regexp-option-euc (loc pos)) "e")
         ((struct Regexp-option-sjis (loc pos)) "s")
         ((struct Regexp-option-utf8 (loc pos)) "u")

         ((struct Scoped-lookup (loc pos obj var))
          (format "~a::~a" (pretty-print obj) (pretty-print var)))

         ((struct Array (loc pos args))
          (format "[~a]" (join (map pretty-print args) ", ")))

         ((struct Array-lookup (loc pos obj args))
          (format "~a[~a]" (pretty-print obj)
                  (join (map pretty-print args) ", ")))

         ((struct Heredoc (loc pos value))
          (format "[heredoc ~a]" value))

         ((struct Range-inclusive (loc pos v1 v2))
          (format "(~a..~a)" (pretty-print v1) (pretty-print v2)))

         ((struct Range-exclusive (loc pos v1 v2))
          (format "(~a...~a)" (pretty-print v1) (pretty-print v2)))

         ((struct Super (loc pos)) "super")

         ((struct += (loc pos)) "+=")
         ((struct -= (loc pos)) "-=")
         ((struct *= (loc pos)) "*=")
         ((struct /= (loc pos)) "/=")
         ((struct %= (loc pos)) "%=")
         ((struct **= (loc pos)) "**=")
         ((struct &= (loc pos)) "&=")
         ((struct or= (loc pos)) "|=")
         ((struct ^= (loc pos)) "^=")
         ((struct <<= (loc pos)) "<<=")
         ((struct >>= (loc pos)) ">>=")
         ((struct &&= (loc pos)) "&&=")
         ((struct doubleor= (loc pos)) "||=")

         ((struct Block-call (loc pos expr block))
          (format "~a ~a" (pretty-print expr)
                  (pretty-print block)))

         ((struct Do-Block (loc pos vars body))
          (format "do\n|~a|\n~a\nend" (pretty-print vars) (pretty-print body)))

         ((struct Break (loc pos args))
          (format "break ~a" (pretty-print args)))
         ((struct Next (loc pos args))
          (format "next ~a" (pretty-print args)))
         ((struct Redo (loc pos args))
          (format "redo ~a" (join (map pretty-print args) ", ")))
         ((struct Retry (loc pos args))
          (format "retry ~a" (join (map pretty-print args) ", ")))

         ((struct Bitwise-negation (loc pos value))
          (format "~~~a" (pretty-print value)))

         ((struct Not (loc pos e))
          (format "not ~a" (pretty-print e)))
         ((struct And (loc pos e1 e2))
          (format "~a and ~a" (pretty-print e1) (pretty-print e2)))
         ((struct Or (loc pos e1 e2))
          (format "~a or ~a" (pretty-print e1) (pretty-print e2)))
         ((struct Nil (loc pos))
          (format "nil"))
         ((struct __FILE__ (loc pos))
          (format "__FILE__"))
         ((struct True (loc pos))
          (format "true"))
         ((struct False (loc pos))
          (format "false"))
         ((struct Self (loc pos))
          (format "self"))
         ((struct Variable (loc pos var))
          (format "~a" (pretty-print var)))
         ((struct Yield (loc pos args))
          (format "yield ~a" (join (map pretty-print args) ", ")))
         ((struct Return (loc pos args))
          (if args
            (format "return ~a" (pretty-print args))
            "return"))
         ((struct Dotted-access (loc pos obj op))
          (format "~a.~a" (pretty-print obj) (pretty-print op)))
         ((struct Backref (loc pos obj))
          (format "$~a" obj))
         ((struct Nthref (loc pos num))
          (format "$~a" num))

         ((struct Normal-argument (loc pos id))
          (pretty-print id))
         ((struct Default-argument (loc pos id arg))
          (format "~a = (~a)" (pretty-print id)
                  (pretty-print arg)))

         ((struct Operation-oror (loc pos)) "||")
         ((struct Operation-or (loc pos)) "|")
         ((struct Operation^ (loc pos)) "^")
         ((struct Operation&& (loc pos)) "&&")
         ((struct Operation& (loc pos)) "&")
         ((struct Operation<=> (loc pos)) "<=>")
         ((struct Operation== (loc pos)) "==")
         ((struct Operation!= (loc pos)) "!=")
         ((struct Operation=== (loc pos)) "===")
         ((struct Operation=~ (loc pos)) "=~")
         ((struct Operation!~ (loc pos)) "!~")
         ((struct Operation> (loc pos)) ">")
         ((struct Operation>= (loc pos)) ">=")
         ((struct Operation< (loc pos)) "<")
         ((struct Operation<= (loc pos)) "<=")
         ((struct Operation+ (loc pos)) "+")
         ((struct Operation- (loc pos)) "-")
         ((struct Operation* (loc pos)) "*")
         ((struct Operation/ (loc pos)) "/")
         ((struct Operation% (loc pos)) "%")
         ((struct Operation** (loc pos)) "**")
         ((struct Operation<< (loc pos)) "<<")
         ((struct Operation>> (loc pos)) ">>")
         ((struct Operation~ (loc pos)) "~")
         ((struct Operation+@ (loc pos)) "+@")
         ((struct Operation-@ (loc pos)) "-@")
         ((struct Operation-aref (loc pos)) "[]")
         ((struct Operation-arefset (loc pos)) "[]=")
         ((struct Operation-backtick (loc pos)) "`")

         ((struct Operation (loc pos op arg1 arg2))
          (format "(~a ~a ~a)" (pretty-print arg1)
                  (pretty-print op)
                  (pretty-print arg2)))
         ((struct QWords (loc pos strings left right))
          (format "%w~a~a~a" left (join strings " ") right))
         ((struct String-computation (loc pos expr))
          (format "#{~a}" (pretty-print expr)))
         ((struct Unless-statement (loc pos condition body else))
          (format "unless ~a then\n ~a ~a end"
                  (pretty-print condition)
                  (pretty-print body)
                  (if else
                    (pretty-print else)
                    "")))
         ((struct Class-expr (loc pos expr body))
          (format "class <<~a\n~a\nend" (pretty-print expr) (pretty-print body)))
         ((struct Class-statement (loc pos name super body))
          (format "class ~a ~a\n~a\nend"
                  (pretty-print name)
                  (if super
                    (format "< ~a" (pretty-print super))
                    "")
                  (pretty-print body)))
         ((struct For-statement (loc pos vars expr body))
          (format "for ~a in ~a\n~a\nend"
                  (pretty-print vars)
                  (pretty-print expr)
                  (pretty-print body)))
         ((struct While-statement (loc pos condition body))
          (format "while ~a do\n~a\nend"
                  (pretty-print condition)
                  (pretty-print body)))
         ((struct Case-statement (loc pos condition whens else))
          (format "case ~a\n~a ~a end"
                  (if condition
                    (pretty-print condition)
                    "")
                  (join (map pretty-print whens) "\n")
                  (if else
                    (pretty-print else)
                    "")))
         ((struct Ternary (loc pos expr then else))
          (format "~a ? ~a : ~a"
                  (pretty-print expr)
                  (pretty-print then)
                  (pretty-print else)))
         ((struct Args (loc pos args rest))
          (string-append
            (join (map pretty-print args) ", ")
            (if rest
              (format "*~a" (pretty-print rest))
              "")))
         ((struct Empty-var (loc pos)) "")
         ((struct Empty (loc pos)) "")
         ((struct Block (loc pos args body)) (format "{ ~a~a }"
                                             (match args
                                                    ((struct Empty-var (loc pos)) "")
                                                    (else (format "|~a| " (pretty-print args))))
                                             (pretty-print body)))
         ((struct Module-statement (loc pos name body))
          (format "module ~a\n~a\nend"
                  (pretty-print name)
                  (pretty-print body)))
         ((struct When-clause (loc pos args body))
          (format "when ~a then ~a"
                  (pretty-print args)
                  (pretty-print body)))
         ((struct If-statement (loc pos condition body elseifs else))
          (format "if ~a then\n~a ~a ~a\nend\n"
                  (pretty-print condition)
                  (pretty-print body)
                  (join (map pretty-print elseifs) "\n")
                  (if else 
                    (pretty-print else)
                    "")))
         ((struct Elseif (loc pos cond body))
          (format "elsif ~a then\n~a" (pretty-print cond) (pretty-print body)))
         ((struct Body (loc pos body else rescue ensure))
          (format "~a ~a ~a ~a" (pretty-print body)
                  (if else (format "else\n~a" (pretty-print else)) "")
                  (string-append (if (null? rescue) "" "\n")
                                 (apply string-append "\n" (map pretty-print rescue)))
                  (if ensure (format "ensure\n~a" (pretty-print ensure)) "")))
         ((struct Begin-statement (loc pos body))
          (format "begin\n~a\nend\n" (pretty-print body)))
         ((struct Rescue-clause (loc pos types args body))
          (format "rescue ~a~a\n~a"
                  (pretty-print types)
                  (match args
                         ((struct Empty (loc pos)) "")
                         (else (format " => ~a" (pretty-print args))))
                  (pretty-print body)))
         ((struct Else (loc pos body)) (format "else\n~a" (pretty-print body)))
         ((struct Until-statement (loc pos condition body))
          (format "until ~a do\n~a\nend"
                  (pretty-print condition)
                  (pretty-print body)))
         ((struct Definition-statement (loc pos name args body))
          (format "def ~a (~a)\n~a\nend"
                  (pretty-print name)
                  (pretty-print args)
                  (pretty-print body)))
         ((struct Definition-singleton-statement (loc pos name args body))
          (format "def ~a (~a)\n~a\nend"
                  (pretty-print name)
                  (pretty-print args)
                  (pretty-print body)))
         ((struct Singleton (loc pos stuff name))
          (format "~a.~a" (pretty-print stuff) (pretty-print name)))
         ((struct Function-arglist (loc pos args rest block))
          (join
            (append
              (map pretty-print args)
              (if rest
                (list (format "*~a" (pretty-print rest)))
                (list))
              (if block
                (list (format "&~a" (pretty-print block)))
                (list)))
            ", "))

         ;; is this used anywhere?
         ((struct Call-stmt (loc pos call vars body))
          (format "~a do ~a ~a end" (pretty-print call)
                  (if (null? vars)
                    ""
                    (format "|~a|\n" (pretty-print vars)))
                  (pretty-print body)))

         ((struct Identifier (loc pos name)) name)
         ((struct Identifier-none (loc pos)) "")
         ((struct Constant (loc pos name)) name)
         ((struct Number (loc pos value)) (format "~a" value))
         ((struct Lhs (loc pos var)) (format "~a" (pretty-print var)))
         ((struct Mlhs (loc pos vars rest))
          (format "~a"
                  (join
                    (let ((vs (map pretty-print vars)))
                      (if rest
                        (append vs
                                (list
                                  (format "*~a" (pretty-print rest))))
                        vs))
                    ", ")))
         ((struct Mrhs (loc pos vars rest))
          (format "~a~a"
                  (join (map pretty-print vars) ", ")
                  (if rest (format "*~a" (pretty-print rest)) "")))
         ((struct Inline-unless (loc pos expr stmt)) (format "~a unless ~a\n"
                                                     (pretty-print stmt)
                                                     (pretty-print expr)))
         ((struct Inline-until (loc pos expr stmt)) (format "~a until ~a\n"
                                                    (pretty-print stmt)
                                                    (pretty-print expr)))
         ((struct Inline-rescue (loc pos expr stmt)) (format "~a rescue ~a\n"
                                                     (pretty-print stmt)
                                                     (pretty-print expr)))
         ((struct Inline-while (loc pos expr stmt)) (format "~a while ~a\n"
                                                    (pretty-print stmt)
                                                    (pretty-print expr)))
         ((struct Inline-if (loc pos expr stmt)) (format "~a if ~a\n"
                                                 (pretty-print stmt)
                                                 (pretty-print expr)))))


(define (qword-match-ends left right)
  (cond
    ((or (not (char? left)) (not (char? right))) #f)
    ((char=? left #\( ) (char=? right #\)))
    ((char=? left #\[) (char=? right #\]))
    ((char=? left #\{) (char=? right #\}))
    ((char=? left #\<) (char=? right #\>))
    (else #t)))

(define (qwords-matcher left)
  (cond
    ((not (char? left)) #f)
    ((char=? left #\( ) #\))
    ((char=? left #\[) #\])
    ((char=? left #\{) #\})
    ((char=? left #\<) #\>)
    (else left)))

(define (convert-to-decimal n)
  (if (= 0 n)
    0
    (/ n (expt 10 (ceiling (/ (log n) (log 10)))))))

#;
(define-syntax keyword
  (syntax-rules ()
    ((_ nt str)
     (nt ((str (not identifier-char)) (void))))))

(define (ruby-zero loc span) (make-Number loc span 0))

(define (make-parser)
  (peg
    (start program)
    (grammar
      ;; none w time    1498    times   77310
      ;; transient w time    4435    times   77310
      (w (((* sw)) (void)))
      #;
      (w (((* " ")) (void))
	 (((* "	")) (void))
	 ((comment) (void)))
      (comment (("#" (* (except (or eof newline))) (or eof newline))
		(void))
	       (("=begin" (* (except (or eof newline)))
		 newline (* (or comment (except (or eof (do "=end" newline)))))
		 "=end" newline)
		(void)))
      (transient newline ((#\newline) $)
	       ((#\return #\newline) $))
      ;; none sw time    1917    times   50193
      ;; transient sw time    493     times   50193
      (transient sw ((#\space) (void))
	  ((#\tab) (void))
	  (("\\" newline) (void)))
      ;; none swn time    1485    times   9273
      ;; transient swn time    703     times   9273
      (transient swn ((sw) $)
	   ((comment) (void))
	   ((newline) (void)))
      (swn-no-comment
	((sw) $)
	((newline) (void)))
      (wn (((* swn)) (void)))
      (wn-no-comment (((* swn-no-comment)) (void)))
      (program ((wn (bind c compstmt) wn eof) (make-Program $position $span c)))
      (compstmt (((bind stmts stmts) w opt-terms)
		 (make-Compstmt $position $span stmts)))
      (stmts (((bind first stmt) (bind next (? (+ w term) w stmts)))
	      (cons first next))
	     ((";" (? w stmts)) $)
	     ((list)))
      (stmt (((bind left stmt-left) (bind right (* stmt-right)))
	     (foldl (lambda (proc obj) (proc obj)) left right)))
      ;; Each statement uses a new parser because after a statement
      ;; is parsed we don't really need the memo table for previous
      ;; parses. This has little effect on performance and keeps
      ;; memory usage constant. Without this, the parser would exhaust
      ;; its memory after reading about 3000 lines.
      (stmt-left1 (((foreign (make-parser) stmt-left)) $))
      (stmt-left ((literal-undef w (bind l undef-list)) (make-Undef $position $span l))
		 ((literal-alias w (bind a fitem) w (bind b fitem))
		  (make-Alias $position $span a b))
		 ((literal-alias w (bind a global-var) w (bind b global-var))
		  (make-Alias $position $span a b))
		 ((literal-alias w (bind a global-var) w (bind b backref))
		  (make-Alias $position $span a b))
		 ((literal-alias w (bind a global-var) w (bind b nth-ref))
		  (make-Alias $position $span a b))
		 (((bind lhs lhs) w "=" wn (bind command command-call))
		  (make-Assignment $position $span lhs command))
		 (((bind mlhs mlhs) w "=" wn (bind command command-call))
		  (make-Multiple-assignment $position $span mlhs command))
		 (((bind var var-lhs) w (bind op op-assign) wn (bind value command))
		  (make-Operation-assignment $position $span op var value))
                 ((literal-BEGIN wn "{" wn (bind c compstmt) wn "}")
                  (make-BEGIN $position $span c))
                 ((literal-END wn "{" wn (bind c compstmt) wn "}")
                  (make-END $position $span c))
		 ((primary "[" wn aref-args wn "]" op-assign command) $)
		 ((primary "." identifier op-assign command) $)
		 ((primary "." constant op-assign command) $)
		 ((primary "::" identifier op-assign command) $)
		 ((backref op-assign command) $)
		 (((bind lhs lhs) w "=" wn (bind mrhs mrhs))
		  (make-Assignment $position $span lhs mrhs))
		 ;; mrhs must come before arg because arg is a single case
		 ;; of mrhs
		 (((bind mlhs mlhs) w "=" wn (bind mrhs mrhs))
		  (make-Multiple-assignment $position $span mlhs mrhs))
		 (((bind mlhs mlhs) w "=" wn (bind arg arg))
		  (make-Multiple-assignment $position $span mlhs arg))
		 ((expr) $))
      (fitem ((fsym) $))
      (fsym ((fname) $)
	    ((symbol) $))
      (command-call ((block-command) $)
		    ((command) $)
		    ((return w (not inlines) (bind args call-args))
		     (make-Return $position $span args))
		    ((break w (not inlines) (bind args call-args))
		     (make-Break $position $span args))
		    ((next w (not inlines) (bind args call-args))
		     (make-Next $position $span args))
		    )
      (return ((literal-return) $))
      (break ((literal-break) $))
      (next ((literal-next) $))
      (block-command ((block-call "." operation w call-args)
                      (error 'block-command "a"))
		     ((block-call "::" operation w call-args) 
                      (error 'block-command "b"))
		     ((block-call) $))
      (var-lhs ((variable) $))
      (backref ((nth-ref) $)
	       ((back-ref) $))
      (aref-args 
		 (((bind a assocs) trailer) a)
		 ;; must come before plain args
		 ((
                   (not command (not w (or "," arg-right)))
                   (bind a args) w "," w "*" (bind rest arg) (? wn))
		  a)
		 ((
                   (not command (not w (or "," arg-right)))
                   (bind a args) trailer) a)
		 (((bind c command) (? wn)) (list c))
		 (("*" (bind a arg) (? wn)) (list a))
		 ((list)))
      (trailer ((newline) (void))
	       ((",") (void))
	       ((void)))
      (assocs (((bind a assoc) (bind rest (* wn "," wn assoc))) (cons a rest)))
      (assoc (((bind key arg) w "=>" wn (bind value arg)) (make-Assoc $position $span key value)))
      (nth-ref (("$" (bind c x0-9)) (make-Nthref $position $span c)))
      (back-ref (("$&") (make-Backref $position $span "&"))
		(("$`") (make-Backref $position $span "`"))
		(("$'") (make-Backref $position $span "'"))
		(("$+") (make-Backref $position $span "+")))
      (undef-list (((bind a fitem) (bind b (? w "," w undef-list)))
		   (cons a b)))
      (stmt-right ((inlines) $))
      (inline-until ((w literal-until w (bind expr expr) w (ensure (or inlines term "}" ")")))
		     (lambda (obj)
		       (make-Inline-until $position $span expr obj))))
      (inline-rescue ((w literal-rescue w (bind body stmt) w (ensure (or inlines term "}" ")")))
		      (lambda (obj)
			(make-Inline-rescue $position $span body obj))))
      (inline-unless ((w literal-unless w (bind expr expr) w (ensure (or inlines term "}" ")")))
		      (lambda (obj)
			(make-Inline-unless $position $span expr obj))))
      (inline-if ((w literal-if w (bind expr expr) w (ensure (or inlines term "}" ")")))
		   (lambda (obj)
		     (make-Inline-if $position $span expr obj))))
      (inline-while ((w literal-while w (bind expr expr) w (ensure (or inlines term "}" ")")))
		     (lambda (obj)
		       (make-Inline-while $position $span expr obj))))
      (inlines ((inline-if) $)
	       ((inline-while) $)
	       ((inline-unless) $)
	       ((inline-rescue) $)
	       ((inline-until) $))
      (inlines-or-void ((inlines) $)
		       ((void)))
      (opt-block-var (((bind vars (? "|" w (bind block-var (? block-var)) w "|" (predicate block-var))))
		      (if (null? vars)
			(make-Empty-var $position $span)
			vars)))
      (bodystmt (((bind c compstmt) (bind rescue (* wn opt-rescue))
				    (bind else (? wn opt-else))
				    (bind ensure (? wn opt-ensure)))
		 (make-Body $position $span c
			    (if (null? else) #f else)
			    rescue
			    (if (null? ensure) #f ensure))))
      (opt-rescue ((literal-rescue w (bind types exc-list) w (bind args exc-var)
		    w then wn (bind body compstmt))
		   (make-Rescue-clause $position $span types args body)))
      (opt-else ((literal-else wn compstmt) $))
      (opt-ensure ((literal-ensure wn compstmt) $))

      ;; mrhs must come before arg
      (exc-list ((mrhs) $)
		((arg) $)
		((make-Empty $position $span)))
      (exc-var (("=>" w lhs) $)
	       ((make-Empty $position $span)))
      (block-var ((mlhs) $)
		 ((lhs) $))

      (mlhs ((mlhs-basic) $)
	    (("(" w (bind m mlhs-entry) w ")") m))

      (mlhs-entry ((mlhs-basic) $)
		  (("(" w (bind m mlhs-entry) w ")") m))

      (mlhs-basic (((bind m mlhs-head) "*" (bind n (? mlhs-node)))
		   (make-Mlhs $position $span m
			      (if (null? n)
				#f n)))
		  (((bind m mlhs-head) (bind i (? mlhs-item)))
		   (make-Mlhs $position $span (append m
				      (if (null? i) i (list i)))
			      #f))
		  (("*" (bind m (? mlhs-node)))
		   (make-Mlhs $position $span '()
			      (if (null? m)
				#f m))))

      (mlhs-item ((mlhs-node) $)
		 (("(" w (bind m mlhs-entry) w ")") m))

      (mlhs-head (((+ (bind m mlhs-item) w "," w (predicate m)))
		  $))

      #|
      (((bind p primary) "[" wn (bind args aref-args) wn "]")
		  (make-Array-lookup p args))
		 (((bind p primary) "." (bind i identifier))
		  (make-Dotted-access $position $span p i))
		 (((bind p primary) "::" (bind i identifier))
		  (make-Dotted-access $position $span p i))
		 (((bind p primary) "." (bind c constant))
		  (make-Dotted-access $position $span p c))
		 (((bind p primary) "::" (bind c constant))
		  (make-Dotted-access $position $span p c))
	 |#

      (mlhs-node ((primary-lhs-special) $)
		 (("::" (bind c constant))
		  (make-Dotted-access $position $span (make-Object $position $span) c))
		 ((backref) $)
		 ((variable) $))

      #;
      (mlhs ((mlhs-basic) $)
	    (("(" w mlhs-entry w ")") $))
      #;
      (mlhs-basic ((mlhs-head) $))

      #;
      (transient mlhs (((bind item1 mlhs-item) w "," w (bind rest (? (bind first mlhs-item) (bind rest2 (* w "," w mlhs-item)) (predicate (cons first rest2)))))
	     (make-Mlhs (cons item1 rest))))

      #;
      (mlhs-item ((lhs) $)
		 (("(" w (bind m mlhs) w ")") m))

      ;; mrhs can be followed by => due to rescue clauses,
      ;; so use args-assocs here.
      (mrhs (((bind arg1 arg) w "," w
	      (bind args args-assocs)
	      (bind rest (? w "," w "*" arg)))
	     (make-Mrhs $position $span (cons arg1 args)
			(if (null? rest)
			  #f rest)))
	    (("*" (bind rest arg))
	     (make-Mrhs $position $span '() rest)))
      #;
      (argdecl (("(" w (bind args arglist) w ")") args)
	       (((bind args arglist) w term) args))
      #;
      (arglist (((bind first-arg identifier) (bind args (* w "," w identifier))
	         (bind rest (? w "," w "*" (bind id (? identifier)) (predicate (if (null? id) "" id))))
		 (bind block (? w "," w "&" identifier)))
		(make-Arglist (cons first-arg args)
			      (if (null? rest)
				#f rest)
			      (if (null? block)
				#f block)))
	       (((bind rest "*" (bind id (? identifier)) (predicate (if (null? id) "" id)))
		 (bind block (? w "," w "&" identifier)))
		(make-Arglist '()
			      (if (null? rest)
				#f rest)
			      (if (null? block)
				#f block)))
	       (((bind block (? "&" identifier)))
		(make-Arglist '()
			      #f
			      (if (null? block)
				#f block)))
	       )
      ;; nt args
      (args (((bind first arg-not-assoc)
	      (bind rest (* w "," wn (bind a arg-not-assoc))))
	     (cons first rest)))
      (args-assocs (((bind first arg)
	      (bind rest (* w "," wn (bind a arg))))
	     (cons first rest)))
      (arg-not-assoc (((bind a arg) (not w "=>")) a))
      ;; . cannot follow an arg
      ;; what about :: ?
      (arg (((bind left arg-left)
	     ;; if a regexp appears, make sure its not followed by an
	     ;; arg in which case it should been division. otherwise
	     ;; if regexp appears and no arg follows then it should
	     ;; have been a command with the regexp as an argument.
	     (not w regexp w (not arg))
	     (bind right (* w arg-right)) (not "."))
	    (foldl (lambda (proc obj) (proc obj)) left right)))

      ((left-associative next stuff)
       (((bind a next) (bind bs (* w stuff)))
              (foldl (lambda (new accum)
                       (new accum))
                     a bs)))

      (arg-left ((prec0) $))
      (prec0 (((bind d literal-defined) w (bind a prec0))
              (make-Function-call $position $span
                                  (make-Dotted-access $position $span (make-Object $position $span) d)
                                  (make-Call-args $position $span (list a) #f #f)
                                  #f))
             ((prec0.1) $))


      (prec0.1 (((bind lhs lhs) w "=" wn (bind arg prec0.1))
                  (make-Assignment $position $span lhs arg))
               (((bind var var-lhs) w (bind op op-assign) w (bind value prec0.1))
                (make-Operation-assignment $position $span op var value))
               (((bind p primary) "." (bind i identifier) w (bind op op-assign) wn (bind value prec0.1))
                (make-Operation-assignment $position $span op (make-Dotted-access $position $span p i) value))
               ((prec1) $))

      (prec1 (((apply left-associative prec2 prec1-stuff)) $))

      (prec1-stuff (("||" wn (bind b prec2))
                    (lambda (obj)
                      (make-Operation $position $span
                                      (make-Operation-oror $position $span)
                                      obj b))))

      (prec2 (((apply left-associative prec3 prec2-stuff)) $))
      (prec2-stuff (("&&" wn (bind b prec3))
                    (lambda (obj)
                      (make-Operation $position $span
                                      (make-Operation&& $position $span)
                                      obj b))))

      (prec3 (((apply left-associative prec4 prec3-stuff)) $))

      ;; <=> == === != =~ !~
      (prec3-stuff
        (("<=>" wn (bind b prec4))
         (lambda (obj)
           (make-Operation $position $span
                           (make-Operation<=> $position $span)
                           obj b)))
        (("===" wn (bind b prec4))
         (lambda (obj)
           (make-Operation $position $span
                           (make-Operation=== $position $span)
                           obj b)))
        (("==" wn (bind b prec4))
         (lambda (obj)
           (make-Operation $position $span
                           (make-Operation== $position $span)
                           obj b)))
        (("!=" wn (bind b prec4))
         (lambda (obj)
           (make-Operation $position $span
                           (make-Operation!= $position $span)
                           obj b)))
        (("=~" wn (bind b prec4))
         (lambda (obj)
           (make-Operation $position $span
                           (make-Operation=~ $position $span)
                           obj b)))
        (("!~" wn (bind b prec4))
         (lambda (obj)
           (make-Operation $position $span
                           (make-Operation!~ $position $span)
                           obj b))))

      ;; <= < > >=
      (prec4 (((apply left-associative prec5 prec4-stuff)) $))
      (prec4-stuff
        (("<=" wn (bind b prec5))
         (lambda (obj)
           (make-Method-call $position $span obj
                             (make-Identifier $position $span "&<=")
                             (make-Call-args $position $span (list $) #f #f)
                             #f)))
        (("<" (not "<") wn prec5)
         (lambda (obj)
           (make-Method-call $position $span obj
                             (make-Identifier $position $span "&<")
                             (make-Call-args $position $span (list $) #f #f)
                             #f)))
        ((">=" wn prec5)
         (lambda (obj)
           (make-Method-call $position $span obj
                             (make-Identifier $position $span "&>=")
                             (make-Call-args $position $span (list $) #f #f)
                             #f)))
        ((">" (not ">") wn prec5)
         (lambda (obj)
           (make-Method-call $position $span obj
                             (make-Identifier $position $span "&>")
                             (make-Call-args $position $span (list $) #f #f)
                             #f))))

      ;; ^ |
      (prec5 (((apply left-associative prec6 prec5-stuff)) $))
      (prec5-stuff
        (("^" wn (bind b prec6))
         (lambda (obj)
           (make-Operation $position $span
                           (make-Operation^ $position $span)
                           obj b)))
        (("|" wn (bind b prec6))
         (lambda (obj)
           (make-Operation $position $span
                           (make-Operation-or $position $span)
                           obj b))))

      ;; &
      (prec6 (((apply left-associative prec7 prec6-stuff)) $))
      (prec6-stuff (("&" wn (bind b prec7))
                    (lambda (obj)
                      (make-Operation $position $span
                                      (make-Operation& $position $span)
                                      obj b))))

      ;; >> <<
      (prec7 (((bind a prec8) (bind bs (* prec7-stuff)))
              (foldl (lambda (new accum)
                       (new accum))
                     a bs)))
      (prec7-stuff
        ((w ">>" wn (bind b prec8))
         (lambda (obj)
           (make-Operation $position $span
                           (make-Operation>> $position $span)
                           obj b)))
        (("<<" wn (bind b prec8))
         (lambda (obj)
           (make-Operation $position $span
                           (make-Operation<< $position $span)
                           obj b)))
        ((w "<<" (not (or (do "-" any-identifier)
                        any-identifier))
          wn (bind b prec8))
         (lambda (obj)
           (make-Operation $position $span
                           (make-Operation<< $position $span)
                           obj b))))

      ;; + -
      

      (prec8 (((apply left-associative prec9 prec8-stuff)) $))

      (prec8-stuff (("+" wn prec9)
                    (lambda (obj)
                      (make-Method-call $position $span obj
                                 (make-Identifier $position $span "&+")
                                 (make-Call-args $position $span (list $) #f #f)
                                 #f)))
                   (("-" wn prec9)
                    (lambda (obj)
                      (make-Method-call $position $span obj
                                        (make-Identifier $position $span "&-")
                                        (make-Call-args $position $span (list $) #f #f)
                                        #f))))

      ;; * / %
      (prec9 (((apply left-associative prec10 prec9-stuff)) $))
      (prec9-stuff
        (("*" wn (bind b prec10))
         (lambda (obj)
           (make-Operation $position $span
                           (make-Operation* $position $span)
                           obj b)))
        (("/" wn (bind b prec10))
         (lambda (obj)
           (make-Operation $position $span
                           (make-Operation/ $position $span)
                           obj b)))
        (("%" (not (or "W" "w" "Q" "q" "x" "r" "s"))
          wn (bind b prec10))
         (lambda (obj)
           (make-Operation $position $span
                           (make-Operation% $position $span)
                           obj b))))

      (prec10 (("!" wn (bind b arg))
               (make-Not $position $span b))
              (("~" wn (bind b prec10))
               (make-Bitwise-negation $position $span b))
              (("+" wn (bind b prec10))
               (make-Method-call $position $span b
                                 (make-Identifier $position $span "@+")
                                 (make-Call-args $position $span '() #f #f)
                                 #f))
              (("-" wn (bind b prec10))
               (make-Method-call $position $span b
                                 (make-Identifier $position $span "@-")
                                 (make-Call-args $position $span '() #f #f)
                                 #f))
              ((prec11) $))

      ;; right associative
      (prec11 (((bind a prec12) w "**" wn (bind b prec11))
               (make-Operation $position $span
                           (make-Operation** $position $span)
                           a b))
              ((prec12) $))

      (prec12 (((bind p prec13) "[" wn (bind ref aref-args) wn "]" w (bind op op-assign) wn (bind value arg))
               (make-Operation-assignment $position $span op (make-Array-lookup $position $span p ref) value))
              ((prec13) $))

      (prec13 ((prec-last) $))

               #|
	  (("**") (make-Operation** $position $span))
	  (("~") (make-Operation~ $position $span))
	  (("+@") (make-Operation+@ $position $span))
	  (("-@") (make-Operation-@ $position $span))
	  (("[]=") (make-Operation-arefset $position $span))
	  (("[]") (make-Operation-aref $position $span))
	  (("`") (make-Operation-backtick $position $span))
	  )
|#
      
      #;
      (prec-last ((primary) $))

      (prec-last (((bind lhs lhs) w "=" wn (bind arg arg))
                  (make-Assignment $position $span lhs arg))
                 (((bind var var-lhs) w (bind op op-assign) w (bind value arg))
                  (make-Operation-assignment $position $span op var value))
                 (((bind p primary) "." (bind i identifier) w (bind op op-assign) wn (bind value arg))
                  (make-Operation-assignment $position $span op (make-Dotted-access $position $span p i) value))
                 (((bind d literal-defined) w (bind a primary))
                  (make-Function-call $position $span
                                  (make-Dotted-access $position $span (make-Object $position $span) d)
                                  (make-Call-args $position $span (list a) #f #f)
                                  #f))
                 ((primary) $))

        #;
      (prec-last (((bind lhs lhs) w "=" wn (bind arg arg))
                  (make-Assignment $position $span lhs arg))
                 ((lhs w "=" w literal-rescue arg)
                  (error 'arg-left "lhs rescue"))
                 (((bind var var-lhs) w (bind op op-assign) w (bind value arg))
                  (make-Operation-assignment $position $span op var value))
                 (((bind p primary) "[" wn (bind ref aref-args) wn "]" w (bind op op-assign) wn (bind value arg))
                  (make-Operation-assignment $position $span op (make-Array-lookup $position $span p ref) value))
                 (((bind p primary) "." (bind i identifier) w (bind op op-assign) wn (bind value arg))
                  (make-Operation-assignment $position $span op (make-Dotted-access $position $span p i) value))
                 ((primary "." constant w op-assign wn arg)
                  (error 'arg-left "c"))
                 ((primary "::" identifier w op-assign wn arg)
                  (error 'arg-left "d"))
                 ((primary "::" constant w op-assign wn arg)
                  (error 'arg-left "e"))
                 (("::" constant w op-assign wn arg)
                  (error 'arg-left "f"))
                 ((backref w op-assign wn arg)
                  (error 'arg-left "g"))
                 (("-" (bind n numeric) "**" (bind pow arg))
                  (make-Function-call $position $span
                                      (make-Dotted-access
                                        $position $span
                                        (make-Function-call
                                          $position $span
                                          (make-Dotted-access $position $span (ruby-zero $position $span) (make-Operation- $position $span))
                                          (make-Call-args $position $span (list n) #f #f)
                                          #f)
                                        (make-Operation** $position $span))
                                      (make-Call-args $position $span (list pow) #f #f)
                                      #f))
                 (("-" float "**" arg)
                  (error 'arg-left "i"))
                 ;; (("+" unsigned-numeric (predicate #f)) (error "can't get here"))
                 (("+" (not unsigned-numeric) (bind a arg))
                  (make-Function-call $position $span (make-Operation+ $position $span) a #f))
                 ;; (("-" unsigned-numeric (predicate #f)) (error "can't get here"))
                 (("-" (not unsigned-numeric) (bind a arg))
                  (make-Function-call $position $span (make-Operation- $position $span) a #f))
                 ;; (("-" (not integer) arg) $)
                 ;; (("!" w (bind a arg)) (make-Not $position $span a))
                 #;(("~" w arg)
                  (make-Bitwise-negation $position $span $))
                 ;; apparently defined? isn't a function of Object. Its some
                 ;; mysterious global thing, so come up with the proper thing
                 ;; instead of (make-Dotted-access ...)
                 (((bind d literal-defined) w (bind a arg))
                  (make-Function-call $position $span
                                      (make-Dotted-access $position $span (make-Object $position $span) d)
                                      (make-Call-args $position $span (list a) #f #f)
                                      #f))
                 ((primary) $)
                 )

      (arg-right ((".." wn (bind to arg)) (lambda (obj) (make-Range-inclusive $position $span obj to)))
		 (("..." wn (bind to arg)) (lambda (obj) (make-Range-exclusive $position $span obj to)))
		 (("?" w (bind a1 arg) w ":" w (bind a2 arg))
		  (lambda (arg)
		    (make-Ternary $position $span arg a1 a2)))
		 )
      (transient symbol ((":" (bind s sym)) (make-Symbol $position $span s)))
      (dsym ((":" "\"" (bind s (apply string-contents "\"")) "\"")
	     (make-Symbol-dsym $position $span (apply string-append "" (map pretty-print s))))
            ((":" "'" (bind s (apply string-contents "'")) "'")
	     (make-Symbol-dsym $position $span (apply string-append "" (map pretty-print s)))))
      (sym ((any-fname) $)
	   ((instance-var) $)
	   ((global-var) $)
	   ((class-var) $))
      (op-assign (("+=") (make-+= $position $span))
		 (("-=") (make--= $position $span))
		 (("*=") (make-*= $position $span))
		 (("/=") (make-/= $position $span))
		 (("%=") (make-%= $position $span))
		 (("**=") (make-**= $position $span))
		 (("&&=") (make-&&= $position $span))
		 (("&=") (make-&= $position $span))
		 (("||=") (make-doubleor= $position $span))
		 (("|=") (make-or= $position $span))
		 (("^=") (make-^= $position $span))
		 (("<<=") (make-<<= $position $span))
		 ((">>=") (make->>= $position $span)))
      #;
      (call ((function) $)
	    ((command) $)
	    )
      (fname ((constant) $)
	     ((fid) $)
	     (((bind i identifier) "=")
	      (make-Identifier $position $span (string-append (Identifier-name i) "=")))
	     ((identifier) $)
	     ((reserved) $)
	     ((op) $))

      (any-fname ((fname) $)
		 ((any-identifier) $))

      ;; used in primary-right to allow certain operations
      (equals-op (("===") (make-Operation=== $position $span))
		 (("==") (make-Operation== $position $span))
		 (("=~") (make-Operation=~ $position $span)))

      (op (("||") (make-Operation-oror $position $span)) 
	  (("|") (make-Operation-or $position $span))
	  (("^") (make-Operation^ $position $span))
	  (("&&") (make-Operation&& $position $span))
	  (("&") (make-Operation& $position $span))
	  (("<=>") (make-Operation<=> $position $span))
	  (("===") (make-Operation=== $position $span))
	  (("==") (make-Operation== $position $span))
	  (("=~") (make-Operation=~ $position $span))
	  (("!~") (make-Operation!~ $position $span))
	  (("!=") (make-Operation!= $position $span))
	  (("<<" (not (or (do "-" any-identifier)
			  any-identifier)))
	   (make-Operation<< $position $span))
	  ((">>") (make-Operation>> $position $span))
	  ((">=") (make-Operation>= $position $span))
	  ((">") (make-Operation> $position $span))
	  (("<=") (make-Operation<= $position $span))
	  ;; even though << is above <, we still need to ensure that
	  ;; another < doesn't come next so that heredoc can parse
	  (("<" (not "<")) (make-Operation< $position $span))
	  (("**") (make-Operation** $position $span))
	  (("*") (make-Operation* $position $span))
	  (("/") (make-Operation/ $position $span))
	  (("%" (not (or "W" "w" "Q" "q" "x" "r" "s")))
	   (make-Operation% $position $span))
	  (("~") (make-Operation~ $position $span))
	  (("+@") (make-Operation+@ $position $span))
	  (("-@") (make-Operation-@ $position $span))
	  (("+") (make-Operation+ $position $span))
	  (("-") (make-Operation- $position $span))
	  (("[]=") (make-Operation-arefset $position $span))
	  (("[]") (make-Operation-aref $position $span))
	  (("`") (make-Operation-backtick $position $span))
	  )
      #;
      (function (((bind p primary-left) (bind x1 function-right) (bind xs (* function-right)))
		 (foldl (lambda (proc obj) (proc obj)) (x1 p) xs))
		(((bind left function-other) (bind right (* function-right)))
		 (foldl (lambda (proc obj) (proc obj)) left right)))
      #;
      (function-right (((bind block (? w brace-block))
			"." (bind op operation2) (bind args (? paren-args))
			(ensure "[") (not w op-assign))
		       (lambda (obj) (make-Function-call
                                       $position $span
				       (make-Dotted-access $position $span obj op)
				       (if (null? args)
					 (make-Call-args '() #f #f)
					 args)
				       (if (null? block)
					 #f block))))
		      (((bind block (? w brace-block))
			"." (bind op operation2) (bind args (? paren-args))
			w (ensure inlines))
		       (lambda (obj) (make-Function-call $position $span
                                                         (make-Dotted-access $position $span obj op)
							 (if (null? args)
							   (make-Call-args $position $span '() #f #f)
							   args)
							 (if (null? block)
							   #f block))))
		      (((bind block (? w brace-block))
			"." (bind op operation2) (bind args (? paren-args))
			(not w call-args) (not w op-assign))
		       (lambda (obj) (make-Function-call $position $span
                                                         (make-Dotted-access $position $span obj op)
							 (if (null? args)
							   (make-Call-args $position $span '() #f #f)
							   args)
							 (if (null? block)
							   #f block))))
		      (((bind block (? w brace-block))
			"::" (bind op operation3) (bind args (? paren-args)))
		       (lambda (obj)
			 (make-Function-call $position $span
                                             (make-Dotted-access $position $span obj op)
					     (if (null? args)
					       (make-Call-args $position $span '() #f #f)
					       args)
					     (if (null? block) #f block)))))
      #;
      (function-other (((bind op operation) (bind args paren-args))
		       (make-Function-call $position $span op args #f))
		      ((literal-super (not w call-args) (bind a (? w paren-args)))
		       (make-Function-call $position $span (make-Super $position $span)
					   (if (null? a)
					     (make-Call-args $position $span '() #f #f)
					     a)
					     #f)))
      (paren-args (("(" wn ")") (make-Call-args $position $span '() #f #f))
		  (("(" wn (bind args call-args) wn ")")
		   args)
		  (("(" wn (bind call block-call) wn ")")
		   call)
		  (("(" wn (bind args args) w "," w (bind call block-call) wn ")")
		   args))
      (block-call (((bind left block-call-left) (bind right (* block-call-right)))
		   (foldl (lambda (proc obj) (proc obj)) left right)))
      (block-call-left (((bind c command) w (bind d do-block))
			(make-Block-call $position $span c d)))
      (transient block-call-right (("." operation (not w "(") (? paren-args)) (lambda (obj) obj))
			(("::" operation (not w "(") (? paren-args)) (lambda (obj) obj)))
      (do-block ((literal-do wn (bind vars opt-block-var) wn (bind body compstmt) wn literal-end)
		 (make-Do-Block $position $span vars body)))
      [command 
	;; disallow . to follow a command, because in that case it should have
	;; been a primary. also disallow :: ?
	[((bind op operation) w (bind block cmd-brace-block) (not w "."))
	 (make-Function-call $position $span op (make-Call-args $position $span '() #f #f) block)]
        ;; allowing : after an operation is a hack to allow
        ;;  someop:blah
        ;; but disallows
        ;;  someop+blah
        ;; because the latter would be addition of `someop' and `blah'
        [((bind op operation) (or sw (ensure (or ":" "'" "\"" "`"))) w (not (or inlines "("))
                              (not (ensure (or "+" "-")) arg (not w ","))
                              (bind args call-args) (bind block (? w cmd-brace-block)))
	 (make-Function-call $position $span op args (if (null? block) #f block))]
        [((bind p primary) "." (bind op operation2-reserved) w
                           (bind args call-args)
                           (bind block (? w cmd-brace-block)))
	 (make-Function-call $position $span
                             (make-Dotted-access $position $span p op)
                             args
                             (if (null? block) #f block))]
	[((bind p primary) "::" (bind op operation2) w (bind args call-args) (bind block (? w cmd-brace-block)))
	 (make-Function-call $position $span
                             (make-Dotted-access $position $span p op)
                             args
                             (if (null? block) #f block))]
	;; fix these
	[(literal-super w (not inlines)
			(bind a call-args))
	 (make-Function-call
           $position $span
	   (make-Super $position $span)
	   a
	   #f)]
	[(literal-yield w (not inlines)
			call-args)
	 (make-Yield $position $span (list $))]
	]

      (cmd-brace-block (("{" wn (bind vars opt-block-var) wn (bind body compstmt) wn "}")
			(make-Block $position $span vars body)))
      (brace-block (("{" wn (bind vars opt-block-var) wn (bind body compstmt) wn "}")
		   (make-Block $position $span vars body))
		   ;; do block cannot be followed by a semicolon
		  ((literal-do wn (bind vars opt-block-var) wn (bind body compstmt) wn literal-end)
		   (make-Block $position $span vars body)))
      (call-args 
	;; assocs must come first because otherwise args will eat
	;; up the first value in v => e
	(((bind a2 assocs)
	  (bind a3 (? w "," wn "*" arg))
	  (bind a4 (? w "," wn block-arg)))
	 (make-Call-args $position $span
                         a2
			 (if (null? a3)
			   #f a3)
			 (if (null? a4)
			   #f a4)))
	((
          ;; prevent args from matching when a command should have matched
          ;; , and arg-right can follow an arg so check for them
          ;; * TODO after changing precedence op might not be the only thing
          ;; that can follow an arg. recheck this at some point
          (not command (not w (or "," op arg-right)))
          (bind a1 args)
	  (bind a2 (? w "," wn assocs))
	  (bind a3 (? w "," wn "*" arg))
	  (bind a4 (? w "," wn block-arg)))
	 (make-Call-args $position $span
                         (if (null? a2)
			   a1
			   (append a1 a2))
			 (if (null? a3)
			   #f a3)
			 (if (null? a4)
			   #f a4)))

	(((bind a3 (do "*" arg))
	  (bind a4 (? w "," w block-arg)))
	 (make-Call-args $position $span '() a3 (if (null? a4)
				  #f a4)))
	(((bind a4 block-arg))
	 (make-Call-args $position $span '() #f a4))
	;; command has to come last so as not to interfere
	;; with args
	((command) (make-Call-args $position $span (list $) #f #f))
	)
      #;
      (call-args (((bind a assocs)) (make-Call-args a #f #f))
		 (((bind a args) w "," w "*" (bind rest arg))
		  (make-Call-args a rest #f))
		 (((bind a args) (bind block (? w "," w block-arg)))
		  (make-Call-args a #f
				  (if (null? block)
				    #f block)))
		 (((bind c command)) (make-Call-args (list c) #f #f))
		 (("*" (bind a arg) w "," w (bind b block-arg))
		  (make-Call-args '() a b))
		 (("*" (bind a arg))
		  (make-Call-args '() a #f))
		 (((bind b block-arg)) (make-Call-args '() #f b))
		 )

      ;; only used to prevent call-args from matching.
      (call-args2 (((bind c command)) (make-Call-args $position $span (list c) #f #f))
		 (((bind a args)) (make-Call-args $position $span a #f #f))
		 (("*" (bind a arg))
		  (make-Call-args $position $span '() a #f))
		 (((bind b block-arg)) (make-Call-args $position $span '() #f b))
		 )

      (block-arg (("&" w arg) $))
      (operation ((fid) $)
		 ((identifier) $)
		 ((constant) $))
      (operation2-reserved ((operation2) $)
			   ((reserved) $))
      (operation2 ((fid) $)
		  ((identifier) $)
		  ((constant) $)
		  ((op) $))
      (operation3 ((fid) $)
		  ((identifier) $)
		  ((op) $))
      (fid (((bind id any-identifier) (bind end fid-ending))
	    (make-Identifier (Location-position id)
                             (+ $span (Location-span id))
                             (string-append (Identifier-name id) end))))
      (transient fid-ending
		 (("!") $)
		 (("?") $))
      (primary (((bind left primary-left-real) (bind right (* w primary-right)))
		(foldl (lambda (proc obj) (proc obj)) left right)))
      
      (primary-lhs-special
	(((bind left primary-left-real) (bind right (* (bind f w primary-right)
						       (ensure primary-lhs-right)
						       (predicate f)))
					(bind right-lhs primary-lhs-right))
	 (right-lhs (foldl (lambda (proc obj) (proc obj)) left right))))

      (primary-lhs-right ((w "[" wn (bind args aref-args) wn "]")
			  (lambda (obj)
			    (make-Array-lookup $position $span obj args)))
			 (("." (bind i identifier/reserved))
			  (lambda (obj)
			    (make-Dotted-access $position $span obj i)))
			 (("::" (bind i identifier/reserved))
			  (lambda (obj)
			    (make-Dotted-access $position $span obj i)))
			 (("." (bind c constant))
			  (lambda (obj)
			    (make-Dotted-access $position $span obj c)))
			 (("::" (bind c constant))
			  (lambda (obj)
			    (make-Dotted-access $position $span obj c))))

      (primary-right (("[" wn (bind args aref-args) wn "]" (not w op-assign) (not w "=" (not after-equals)))
		      (lambda (obj) (make-Array-lookup $position $span obj args)))
		     (("." (bind op operation2-reserved) (bind args (? paren-args))
		       (ensure "[") (not w op-assign)
		       (bind block (? w brace-block)))
		      (lambda (obj) (make-Method-call
                                      $position $span
                                      obj op
				      (if (null? args)
					(make-Call-args $position $span '() #f #f)
					args)
				      (if (null? block)
					#f block))))
                     ;; inlines and then brace-block??
		      (("." (bind op operation2-reserved) (bind args (? paren-args))
			w (ensure inlines)
			(bind block (? w brace-block)))
		       (lambda (obj) (make-Method-call
                                       $position $span
                                       obj op
				       (if (null? args)
					 (make-Call-args $position $span '() #f #f)
					 args)
				       (if (null? block)
					 #f block))))
		      #;
		      (("." (bind op operation2) (bind args paren-args)
			(bind block (? w brace-block))
			(or (ensure w "==")
			    (not w (or "=" op-assign))))
		       (lambda (obj) (make-Function-call
                                       $position $span
				       (make-Dotted-access $position $span obj op)
				       (if (null? args)
					 (make-Call-args $position $span '() #f #f)
					 args)
				       (if (null? block)
					 #f block))))
		      #;
		      (("." (bind op operation2)
			(not w call-args)
			(bind block (? w brace-block))
			(or (ensure w "==")
			    (not w (or "=" op-assign))))
		       (lambda (obj) (make-Function-call
                                       $position $span
				       (make-Dotted-access $position $span obj op)
				       (if (null? args)
					 (make-Call-args $position $span '() #f #f)
					 args)
				       (if (null? block)
					 #f block))))
                      (("::" (bind c constant)
                        (not w (or paren-args brace-block)))
                       ;; this is the only place Scoped-lookup should be used
                       (lambda (obj) (make-Scoped-lookup $position $span obj c)))
		      (((or "." "::") wn (bind oper operation2-reserved) (bind args (? paren-args))
			(bind block (? w brace-block))
			
			;; [ prevents call-args from matching array
			(or (ensure w "[")
			    ;; arg-right ensures that the primary can be
			    ;; followed by division
			    (ensure w arg-right)
			    (ensure w "::" (or constant identifier))
			    (ensure w (not regexp) op)
			    (not w call-args))
			(or (ensure w (or equals-op "=>"))
			    (not w (or "=" op-assign))))
		       (lambda (obj) (make-Method-call
                                       $position $span
                                       obj oper
				       (if (null? args)
					 (make-Call-args $position $span '() #f #f)
					 args)
				       (if (null? block)
					 #f block))))
                      #;
		      (("::" (bind op3 operation3) (bind args (? paren-args))
			(bind block (? w brace-block))
			
			(or (ensure w "[")
			    ;; arg-right ensures that the primary can be
			    ;; followed by division
			    (ensure w arg-right)
			    (ensure w "::" (or constant identifier))
			    (ensure w (not regexp) op)
			    (not w call-args))
			(or (ensure w (or equals-op "=>"))
			    (not w (or "=" op-assign)))
			)
		       (lambda (obj)
			 (make-Method-call
                           $position $span
                           obj op3
			   (if (null? args)
			     (make-Call-args $position $span '() #f #f)
			     args)
			   (if (null? block) #f block))))
                      )

      #;
      (primary (((bind f function)  (bind right (* primary-left-rest)))
		(foldl (lambda (proc obj) (proc obj)) f right)
		#;
		(let ((f (if (null? block)
			   function
			   (make-Function-call $position $span
                                               (Function-call-op function)
					       (Function-call-args function)
					       block))))
		  (foldl (lambda (proc obj) (proc obj)) f right)))
	       ((primary-left) $)
	       )
      (primary-left (((bind left primary-left-real) (bind right (* primary-left-rest)))
		     (foldl (lambda (proc obj) (proc obj)) left right)))
      (primary-left-rest (("::" (bind c constant))
			  (lambda (obj) (make-Scoped-lookup $position $span obj c)))
			 (("[" wn (bind args aref-args) wn "]" (not w op-assign) (not w "=" (not after-equals)))
			  (lambda (obj) (make-Array-lookup $position $span obj args))))
      (after-equals (("==") $)
		    (("=") $)
                    ((">") $)
		    (("~") $))
      (primary-left-real (((bind op operation) w (bind args paren-args) (bind block (? w brace-block)))
			  (make-Function-call $position $span op args
					      (if (null? block) #f block)))
			 (((bind op operation) w (bind block brace-block))
			  (make-Function-call $position $span op (make-Call-args $position $span '() #f #f) block))
			 (("(" wn ")") (make-Nil $position $span))
			 (("(" wn (bind e expr) wn ")") e)
			 (("(" wn (bind c compstmt) wn ")") c)
			 (("::" constant) $)
			 ((literal) $)
			 ((variable) $)
			 ((regexp) $)
			 ((words) $)
			 ((here-doc) $)
			 ((ruby-string) $)
			 ((backref) $)
			 ((fid) (make-Function-call $position $span $
						    (make-Call-args $position $span '() #f #f)
						    #f))
			 ((wierd-?-thing) $)
			 (("[" wn (bind args aref-args) wn "]") (make-Array $position $span args))
			 (("{" wn (bind a assoc-list) wn "}") (make-Assoc-list $position $span a))
			 ((return-primary) $)
			 ((literal-yield (bind args (? w "(" (bind args (? call-args)) ")" (predicate args))) (not w (not inlines) call-args))
			  (if (null? args)
			    (make-Yield $position $span '())
			    (make-Yield $position $span (list args))))
			 ((literal-defined "(" arg ")") $)
			 ((if-statement) $)
			 ((unless-statement) $)
			 ((while-statement) $)
			 ((until-statement) $)
			 ((case-statement) $)
			 ((for-statement) $)
			 ((begin-statement) $)
			 ((class-statement) $)
			 ((module-statement) $)
			 ((definition-statement) $)
			 ((definition-singleton-statement) $)
			 ((literal-break) (make-Break $position $span (make-Call-args $position $span '() #f #f)))
			 ((literal-next) (make-Next $position $span (make-Call-args $position $span '() #f #f)))
			 ((literal-redo) (make-Redo $position $span '()))
			 ((literal-retry) (make-Retry $position $span '()))

			 ;; hack to make inlines work
			 ((literal-super (ensure w inlines))
			  (make-Function-call $position $span (make-Super $position $span)
					      (make-Call-args $position $span '() #f #f)
					      #f))
			 
			 ;; super can have a brace-block here because it used to
			 ;; be in method_call and so brace-block was distributed
			 ;; to all elements of a method_call
			 ((literal-super (not w call-args) (bind a (? w paren-args))
					 (bind block (? w brace-block)))
			  (make-Function-call $position $span (make-Super $position $span)
					      (if (null? a)
						(make-Call-args $position $span '() #f #f)
						a)
					      (if (null? block) #f block)))
			 )

      ;; hack to make mlhs/lhs not interfere with arg
      #;
      (primary-left-real2 (((bind op operation) (bind args paren-args) (bind block (? w brace-block)))
			  (make-Function-call $position $span op args
					      (if (null? block) #f block)))
			 (("(" wn ")") (make-Nil $position $span))
			 (("(" wn (bind e expr) wn ")") e)
			 (("(" wn (bind c compstmt) wn ")") c)
			 (("::" constant) $)
			 ((literal) $)
			 ((variable) $)
			 ((regexp) $)
			 ((words) $)
			 ((here-doc) $)
			 ((ruby-string) $)
			 ((backref) $)
			 ((fid) (make-Function-call $position $span $
						    (make-Call-args $position $span '() #f #f)
						    #f))
			 ((wierd-?-thing) $)
			 (("[" wn (bind args aref-args) wn "]") (make-Array $position $span args))
			 (("{" wn (bind a assoc-list) wn "}") (make-Assoc-list $position $span a))
			 ((return-primary) $)
			 ((literal-yield (bind args (? w "(" (bind args (? call-args)) ")" (predicate args))))
			  (make-Yield $position $span args))
			 ((literal-defined "(" arg ")") $)
			 ((if-statement) $)
			 ((unless-statement) $)
			 ((while-statement) $)
			 ((until-statement) $)
			 ((case-statement) $)
			 ((for-statement) $)
			 ((begin-statement) $)
			 ((class-statement) $)
			 ((module-statement) $)
			 ((definition-statement) $)
			 ((definition-singleton-statement) $)
			 ((literal-break) (make-Break $position $span '()))
			 ((literal-next) (make-Next $position $span '()))
			 ((literal-redo) (make-Redo $position $span '()))
			 ((literal-retry) (make-Retry $position $span '()))
			 
			 ;; super can have a brace-block here because it used to
			 ;; be in method_call and so brace-block was distributed
			 ;; to all elements of a method_call
			 ((literal-super (not w call-args) (bind a (? w paren-args))
					 (bind block (? w brace-block)))
			  (make-Function-call $position $span (make-Super $position $span)
					      (if (null? a)
						(make-Call-args $position $span '() #f #f)
						a)
					      (if (null? block) #f block)))
			 )

      (ignore ((void)))

      (here-doc (("<<" (bind i heredoc-identifier) (bind s1 (save))
		  read-until-newline
		  (bind begin (save))
		  newline
		  (bind here (apply here-doc-real i ignore))
		  (bind end (save))
		  (bind ss (skip begin end))
		  (continue s1 ss))
		 (make-Heredoc $position $span here))
		(("<<-" (bind i heredoc-identifier) (bind s1 (save))
		  read-until-newline
		  (bind begin (save))
		  newline
		  (bind here (apply here-doc-real i w))
		  (bind end (save))
		  (bind ss (skip begin end))
		  (continue s1 ss))
		 (make-Heredoc $position $span here)))

      (heredoc-identifier ((any-identifier) $)
			  (("'" (bind i any-identifier) "'") i)
			  (("\"" (bind i any-identifier) "\"") i)
			  (("`" (bind i any-identifier) "`") i))

      (read-until-newline (((bind c (* (except (or newline eof)))))
			   (apply string-append "" (map string c))))

      ((here-doc-real ender front)
       (((bind all (* (apply here-doc-real-line ender front)))
	 front any-identifier)
	(apply string-append "" (map (lambda (n)
				       (string-append n "\n"))
				     all))))
      ((here-doc-real-line ender front)
       (((not
	   front
	   (bind i any-identifier)
	   (predicate (if (null? i)
			#f
			(equal? (Identifier-name ender)
				(Identifier-name i)))))
	 (bind line read-until-newline)
	 newline)
	line))

      ;; inline syntax for doing ord(c)
      (transient wierd-?-thing (("?" (bind c (or printable-ascii
				       escape-char)))
		      (make-Number $position $span (char->integer c))))

      (escape-char (("\\0") (integer->char 0))
		   (("\\a") (integer->char 7))
		   (("\\b") (integer->char 8))
		   (("\\t") (integer->char 9))
		   (("\\n") (integer->char 10))
		   (("\\v") (integer->char 11))
		   (("\\f") (integer->char 12))
		   (("\\r") (integer->char 13))
		   (("\\e") (integer->char 27))
		   (("\\M-" escape-char)
		    (integer->char
		      (bitwise-ior
			(char->integer $)
			#x80)))
		   (("\\M-" printable-ascii)
		    (integer->char
		      (bitwise-ior
			(bitwise-and (char->integer $) #xff)
			#x80)))
		   (("\\C-?") (integer->char 127))
		   (("\\C-" (or printable-ascii
				escape-char))
		    (integer->char
		      (bitwise-and (char->integer $) #x9f))))

      (printable-ascii ((#\!) $) ((#\") $) ((#\#) $) ((#\$) $) ((#\%) $)
		       ((#\&) $) ((#\') $) ((#\() $) ((#\)) $) ((#\*) $)
		       ((#\+) $) ((#\,) $) ((#\-) $) ((#\.) $) ((#\/) $)
		       ((#\0) $) ((#\1) $) ((#\2) $) ((#\3) $) ((#\4) $)
		       ((#\5) $) ((#\6) $) ((#\7) $) ((#\8) $) ((#\9) $)
		       ((#\:) $) ((#\;) $) ((#\<) $) ((#\=) $) ((#\>) $)
		       ((#\?) $) ((#\@) $) ((#\A) $) ((#\B) $) ((#\C) $)
		       ((#\D) $) ((#\E) $) ((#\F) $) ((#\G) $) ((#\H) $)
		       ((#\I) $) ((#\J) $) ((#\K) $) ((#\L) $) ((#\M) $)
		       ((#\N) $) ((#\O) $) ((#\P) $) ((#\Q) $) ((#\R) $)
		       ((#\S) $) ((#\T) $) ((#\U) $) ((#\V) $) ((#\W) $)
		       ((#\X) $) ((#\Y) $) ((#\Z) $) ((#\[) $) ((#\]) $)
		       ((#\^) $) ((#\_) $) ((#\`) $) ((#\a) $) ((#\b) $)
		       ((#\c) $) ((#\d) $) ((#\e) $) ((#\f) $) ((#\g) $)
		       ((#\h) $) ((#\i) $) ((#\j) $) ((#\k) $) ((#\l) $)
		       ((#\m) $) ((#\n) $) ((#\o) $) ((#\p) $) ((#\q) $)
		       ((#\r) $) ((#\s) $) ((#\t) $) ((#\u) $) ((#\v) $)
		       ((#\w) $) ((#\x) $) ((#\y) $) ((#\z) $) ((#\{) $)
		       ((#\|) $) ((#\}) $) ((#\~) $))
      
      (return-primary ((return w (ensure inlines))
		       (make-Return $position $span #f))
		      ((return w (not call-args))
		       (make-Return $position $span #f)))

      ;; fix this, only 'w' can be qwords
      ;; q/Q = string
      ;; W = words
      ;; x = xstring
      ;; r = regexp
      ;; s = symbol
      (words (((apply words* words-w))
	      (make-QWords $position $span (car $) (cadr $) (caddr $)))
	     ;; TODO: this is wrong
	     (((apply words* words-W))
	      (make-QWords $position $span (car $) (cadr $) (caddr $)))
	     ;; TODO: this is wrong
	     (((apply words* words-q))
	      (make-QWords $position $span (car $) (cadr $) (caddr $)))
	     ;; TODO: this is wrong
	     (((apply words* words-Q))
	      (make-QWords $position $span (car $) (cadr $) (caddr $)))
	     ;; TODO: this is wrong
	     (((apply words* words-x))
	      (make-QWords $position $span (car $) (cadr $) (caddr $)))
	     ;; TODO: this is wrong
	     (((apply words* words-r))
	      (make-QWords $position $span (car $) (cadr $) (caddr $)))
	     ;; TODO: this is wrong
	     (((apply words* words-s))
	      (make-QWords $position $span (car $) (cadr $) (caddr $)))
	     ;; %(...) is legal
	     ;; which also means %=...= is legal, but this is hard to
	     ;; recognize when = is used.
	     ;; x = %=foo=
	     ;; y = 2
	     (((apply words* ignore))
	      (make-QWords $position $span (car $) (cadr $) (caddr $))))

      (words-w (("w") $))
      (words-W (("W") $))
      (words-q (("q") $))
      (words-Q (("Q") $))
      (words-x (("x") $))
      (words-s (("s") $))
      (words-r (("r") $))

      ((words* letter) (("%" letter (bind left-side qword-valid-left) wn-no-comment
			(bind s (? (apply qstrings (qwords-matcher left-side))))
			wn-no-comment (bind right-side _)
			(predicate (qword-match-ends left-side right-side)))
			(list s left-side right-side)))

      ;; %= is illegal on the left side of a qword.. ruby allows this!
      ;; Try to support this someday..
      (qword-valid-left (((except (or sw a-zA-Z0-9 "="))) $))

      ((qstrings ender)
       (((bind first (apply qstring ender)) (bind rest (* swn-no-comment (apply qstring ender))))
	(cons first rest)))

      ((qstring ender) (((bind cs (* (apply qstring-char ender))))
			(apply string-append "" cs)))

      ((qstring-char ender)
       (("(" (bind s (apply qstring ender)) ")")
	(format "(~a)" s))
       ;; I doubt this is right
       (("{" w (bind s compstmt) w "}")
	(format "{~a}" (pretty-print s)))
       ;; same for this
       (("#{" w (bind c compstmt) w "}")
	(format "#{~a}" (pretty-print c)))
       (((bind c _) (predicate (and (char? c)
				    (not (char=? ender c))
				    (not (char-blank? c)))))
	(scheme-string c)))

      (regexp (("/" (bind c xregexp-contents) "/" (not "=" (not (or "=" "~"))) (bind options (* regexp-options))
                (not w prec9))
	       (make-Regexp $position $span c options)))
      (regexp-options (("i") (make-Regexp-option-ignore-case $position $span))
		      (("x") (make-Regexp-option-extended $position $span))
		      (("m") (make-Regexp-option-multiline $position $span))
		      (("o") (make-Regexp-option-once $position $span))
		      (("n") (make-Regexp-option-none $position $span))
		      (("N") (make-Regexp-option-none $position $span))
		      (("e") (make-Regexp-option-euc $position $span))
		      (("E") (make-Regexp-option-euc $position $span))
		      (("s") (make-Regexp-option-sjis $position $span))
		      (("S") (make-Regexp-option-sjis $position $span))
		      (("u") (make-Regexp-option-utf8 $position $span))
		      (("U") (make-Regexp-option-utf8 $position $span)))

      (xregexp-contents (((bind c (* regexp-content))) c))
      (regexp-content (("#{" (bind c compstmt) "}")
		       (make-String-computation $position $span c))
		      (("#$" string-dvar) $)
		      (("#@" string-dvar) $)
		      ((normal-regexp-content) $))
      (normal-regexp-content (("\\" (bind c _))
			      (make-Regexp-escaped $position $span c))
			     (((bind c (except slash-or-newline)))
			      (make-Regexp-char $position $span (scheme-string c))))

      (slash-or-newline (("/") $)
			((newline) $)
			((eof) $))

      (string* (("\"" (bind c (apply string-contents "\"")) "\"")
		(make-String-literal $position $span c))
	       (("'" (bind c (apply string-contents "'")) "'")
		(make-String-literal $position $span c))
	       (("`" (bind c (apply string-contents "`")) "`")
		(make-String-literal $position $span c)))
      ((string-contents end) (((bind c (* (apply string-content end)))) c))

      #;
      (string**-contents (((bind c (* (except "'"))))
			  (map make-String-char (map scheme-string c))))

      ((string-content end)
       (("#{" (bind c compstmt) "}")
	(make-String-computation $position $span c))
       (("#$" string-dvar) $)
       (("#@" string-dvar) $)
       (((apply normal-string-content end)) $))
      ((normal-string-content end)
       ((escape) $)
       (((bind c _) (predicate (not (string=? (string c) end))))
	(make-String-char $position $span (scheme-string c))))

      (transient escape (("\\" (bind o1 octal-digit) (bind o2 octal-digit) (bind o3 octal-digit))
	       (make-String-octal $position $span (string-append (string o1) (string o2) (string o3))))
	      (("\\" (bind c _))
	       (make-String-escaped $position $span c)))

      (octal-digit (((apply char-range #\0 #\7)) $))

      (string-dvar ((global-var) $)
		   ((instance-var) $)
		   ((class-var) $)
		   ((backref) $))

      (assoc-list (((bind a assocs) trailer) a)
		  (((bind a args) trailer) a)
		  ((list)))

      ;; it would be nice to make these macros..
      (transient literal-alias (("alias" (not identifier-char)) (make-Identifier $position $span "alias")))
      (transient literal-and (("and" (not identifier-char)) (make-Identifier $position $span "and")))
      (transient literal-BEGIN (("BEGIN" (not identifier-char)) (make-Identifier $position $span "BEGIN")))
      (transient literal-begin (("begin" (not identifier-char)) (make-Identifier $position $span "begin")))
      (transient literal-break (("break" (not identifier-char)) (make-Identifier $position $span "break")))
      (transient literal-case (("case" (not identifier-char)) (make-Identifier $position $span "case")))
      (transient literal-class (("class" (not identifier-char)) (make-Identifier $position $span "class")))
      (transient literal-def (("def" (not identifier-char)) (make-Identifier $position $span "def")))
      ;; defined? can be followed by any character
      (transient literal-defined (("defined?") (make-Identifier $position $span "defined?")))
      (transient literal-do (("do" (not identifier-char)) (make-Identifier $position $span "do")))
      (transient literal-else (("else" (not identifier-char)) (make-Identifier $position $span "else")))
      (transient literal-elsif (("elsif" (not identifier-char)) (make-Identifier $position $span "elsif")))
      (transient literal-END (("END" (not identifier-char)) (make-Identifier $position $span "END")))
      (transient literal-end (("end" (not identifier-char)) (make-Identifier $position $span "end")))
      (transient literal-ensure (("ensure" (not identifier-char)) (make-Identifier $position $span "ensure")))
      (transient literal-false (("false" (not identifier-char)) (make-Identifier $position $span "false")))
      (transient literal-for (("for" (not identifier-char)) (make-Identifier $position $span "for")))
      (transient literal-if (("if" (not identifier-char)) (make-Identifier $position $span "if")))
      (transient literal-in (("in" (not identifier-char)) (make-Identifier $position $span "in")))
      (transient literal-module (("module" (not identifier-char)) (make-Identifier $position $span "module")))
      (transient literal-not (("not" (not identifier-char)) (make-Identifier $position $span "not")))
      (transient literal-next (("next" (not identifier-char)) (make-Identifier $position $span "next")))
      (transient literal-redo (("redo" (not identifier-char)) (make-Identifier $position $span "redo")))
      (transient literal-or (("or" (not identifier-char)) (make-Identifier $position $span "or")))
      (transient literal-rescue (("rescue" (not identifier-char)) (make-Identifier $position $span "rescue")))
      (transient literal-retry (("retry" (not identifier-char)) (make-Identifier $position $span "retry")))
      (transient literal-return (("return" (not identifier-char)) (make-Identifier $position $span "return")))
      (transient literal-super (("super" (not identifier-char)) (make-Identifier $position $span "super")))
      (transient literal-nil (("nil" (not identifier-char)) (make-Identifier $position $span "nil")))
      (transient literal-__FILE__ (("__FILE__" (not identifier-char)) (make-Identifier $position $span "__FILE__")))
      (transient literal-self (("self" (not identifier-char)) (make-Identifier $position $span "self")))
      (transient literal-undef (("undef" (not identifier-char)) (make-Identifier $position $span "undef")))
      (transient literal-then (("then" (not identifier-char)) (make-Identifier $position $span "then")))
      (transient literal-true (("true" (not identifier-char)) (make-Identifier $position $span "true")))
      (transient literal-unless (("unless" (not identifier-char)) (make-Identifier $position $span "unless")))
      (transient literal-until (("until" (not identifier-char)) (make-Identifier $position $span "until")))
      (transient literal-while (("while" (not identifier-char)) (make-Identifier $position $span "while")))
      (transient literal-when (("when" (not identifier-char)) (make-Identifier $position $span "when")))
      (transient literal-yield (("yield" (not identifier-char)) (make-Identifier $position $span "yield")))

      (reserved 
	((literal-BEGIN) $)
	((literal-END) $)
	((literal-alias) $)
	((literal-and) $)
	((literal-begin) $)
	((literal-break) $)
	((literal-case) $)
	((literal-class) $)
	((literal-def) $)
	((literal-defined) $)
	((literal-do) $)
	((literal-else) $)
	((literal-elsif) $)
	((literal-end) $)
	((literal-ensure) $)
	((literal-false) $)
	((literal-for) $)
	((literal-if) $)
	((literal-in) $)
	((literal-module) $)
	((literal-next) $)
	((literal-not) $)
	((literal-or) $)
	((literal-redo) $)
	((literal-rescue) $)
	((literal-retry) $)
	((literal-return) $)
	((literal-self) $)
	((literal-super) $)
	((literal-then) $)
	((literal-true) $)
	((literal-undef) $)
	((literal-unless) $)
	((literal-until) $)
	((literal-when) $)
	((literal-while) $)
	((literal-yield) $))

      (identifier-char ((a-zA-Z0-9_) $))

      (definition-statement ((literal-def w (bind name fname) w (bind args function-arglist) wn (bind body bodystmt) wn literal-end)
			     (make-Definition-statement
                               $position $span
			       name args body)))

      (singleton ((variable) $))

      (definition-singleton-statement
	((literal-def w (bind singleton singleton) (or "." "::") (bind name fname) w (bind args function-arglist) wn (bind body bodystmt) wn literal-end)
	 (make-Definition-singleton-statement
           $position $span
	   (make-Singleton $position $span singleton name)
	   args
	   body)))

      (module-statement ((literal-module w (bind name cpath) wn (bind body compstmt) wn literal-end)
			 (make-Module-statement $position $span name body)))

      (class-statement ((literal-class w "<<" w (bind expr expr) w term wn (bind body bodystmt) wn literal-end)
			(make-Class-expr $position $span expr body))
		       ((literal-class w (bind name cpath) (bind super (? w "<" w expr))
			 w term wn (bind body bodystmt) wn literal-end)
			(make-Class-statement
                          $position $span
			  name (if (null? super) #f super) body)))

      (function-arglist (("(" w (bind args f-args) wn ")") args)
			(((bind args f-args) w term) args))

      #;
      (f-args (((bind regular-args (? (bind a f-arg) (? w "," w) (predicate a)))
		(bind default-args (? (bind a f-optarg) (? w "," w) (predicate a)))
		(bind rest-args (? (bind a f-rest-arg) (? w "," w) (predicate a)))
		(bind block (? f-block-arg)))

	       (make-Function-arglist
		 (append regular-args default-args)
		 (if (null? rest-args) #f rest-args)
		 (if (null? block) #f block))))

      (f-args (((bind default-args f-optarg)
		(bind rest-args (? w "," w f-rest-arg))
		(bind block (? w "," w f-block-arg)))
	       (make-Function-arglist
                 $position $span
		 default-args
		 (if (null? rest-args) #f rest-args)
		 (if (null? block) #f block)))

	      (((bind regular-args f-arg)
		(bind default-args (? w "," w f-optarg))
		(bind rest-args (? w "," w f-rest-arg))
		(bind block (? w "," w f-block-arg)))
	       (make-Function-arglist
                 $position $span
		 (append regular-args default-args)
		 (if (null? rest-args) #f rest-args)
		 (if (null? block) #f block)))

	      (((bind rest-args f-rest-arg)
		(bind block (? w "," w f-block-arg)))
	       (make-Function-arglist
                 $position $span
		 '()
		 (if (null? rest-args) #f rest-args)
		 (if (null? block) #f block)))

	      (((bind block f-block-arg))
	       (make-Function-arglist
                 $position $span
		 '()
		 #f
		 (if (null? block) #f block)))

	      ((make-Function-arglist $position $span '() #f #f)))

      #;
      (call-args 
	;; assocs must come first because otherwise args will eat
	;; up the first value in v => e
	(((bind a2 assocs)
	  (bind a3 (? w "," w "*" arg))
	  (bind a4 (? w "," w block-arg)))
	 (make-Call-args a2
			 (if (null? a3)
			   #f a3)
			 (if (null? a4)
			   #f a4)))
	(((bind a1 args)
	  (bind a2 (? w "," w assocs))
	  (bind a3 (? w "," w "*" arg))
	  (bind a4 (? w "," w block-arg)))
	 (make-Call-args (if (null? a2)
			   a1
			   (append a1 a2))
			 (if (null? a3)
			   #f a3)
			 (if (null? a4)
			   #f a4)))

	(((bind a3 (do "*" arg))
	  (bind a4 (? w "," w block-arg)))
	 (make-Call-args '() a3 (if (null? a4)
				  #f a4)))
	(((bind a4 block-arg))
	 (make-Call-args '() #f a4))
	;; command has to come last so as not to interfere
	;; with args
	((command) (make-Call-args (list $) #f #f))
	)

      #;
      (f-args (((bind args f-arg) w "," w (bind default f-optarg) w "," w f-rest-arg (bind block (? opt-f-block-arg)))
	       (make-Function-arglist (append args default) 
				      rest
				      (if (null? block) #f block)))
	      ((f-arg w "," w f-optarg (? w opt-f-block-arg)) $)
	      ((f-arg w "," w f-rest-arg (? w opt-f-block-arg)) $)
	      (((bind args f-arg) (? opt-f-block-arg))
	       (make-Function-arglist args #f #f))
	      ((f-optarg (? opt-f-block-arg)) $)
	      ((f-rest-arg (? opt-f-block-arg)) $)
	      ((f-block-arg) $))

      (f-optarg (((bind first f-opt) (bind rest (? w "," w f-optarg)))
		 (cons first rest)))
      (f-opt (((bind id identifier) w "=" w (bind arg arg))
	      (make-Default-argument $position $span id arg)))

      (f-rest-arg (("*" identifier) $)
		  (("*") (make-Identifier-none $position $span)))
      (opt-f-block-arg ((f-block-arg) $))
      (f-block-arg (("&" identifier) $))

      (f-arg (((bind first f-norm-arg) (bind rest (* w "," w f-norm-arg)))
	      (cons first rest)))
      (f-norm-arg (((bind x constant) (not w "="))
		   (make-Normal-argument $position $span x))
		  (((bind x instance-var) (not w "="))
		   (make-Normal-argument $position $span x))
		  (((bind x global-var) (not w "="))
		   (make-Normal-argument $position $span x))
		  (((bind x class-var) (not w "="))
		   (make-Normal-argument $position $span x))
		  (((bind x identifier) (not w "="))
		   (make-Normal-argument $position $span x)))

      (cpath (("::" cname) $)
	     (((bind p primary)
	       (predicate (Scoped-lookup? p)))
	      p)
	     ((cname) $)
	     #;((primary "::" cname) $)
	     )

      (cname ((constant) $)
	     (((bind id identifier))
	      (error (format "class/module name '~a' must be a constant!" (pretty-print id)))))

      (begin-statement ((literal-begin wn (bind b bodystmt) wn literal-end)
			(make-Begin-statement $position $span b)))

      (rescue-clause ((wn literal-rescue w exc-list w exc-var w ruby-do wn (bind body compstmt))
		      (make-Rescue-clause
			(make-Args $position $span '() #f)
			body)))

      (for-statement ((literal-for w (bind vars block-var) w literal-in w (bind expr expr) w ruby-do wn (bind body compstmt) w literal-end)
		      (make-For-statement $position $span vars expr body)))

      (case-statement ((literal-case w (bind condition (? expr)) (bind whens (+ when-clause)) (bind else (? wn literal-else wn compstmt)) wn literal-end)
		       (make-Case-statement
                         $position $span
			 (if (null? condition)
			   #f
			   condition)
			 whens
			 (if (null? else)
			   #f
			   (make-Else $position $span else)))))
      (when-clause ((wn literal-when w (bind args when-args) w then wn (bind body compstmt))
		    (make-When-clause
                      $position $span
		      args
		      body)))

      (while-statement ((literal-while w (bind expr expr) w ruby-do  wn (bind body compstmt) wn literal-end)
			(make-While-statement
                          $position $span
			  expr
			  body)))

      (until-statement ((literal-until w (bind expr expr) w ruby-do wn (bind body compstmt) wn literal-end)
			(make-Until-statement
                          $position $span
			  expr
			  body)))

      ;; none if-statement time    46494   times   2191
      ;; transient if-statement time    48987   times   2191
      (if-statement ((literal-if w (bind cond expr) w then wn (bind body compstmt) wn (bind elses (* elseif)) wn (bind else (? literal-else wn compstmt)) wn literal-end) 
			  (make-If-statement
                            $position $span
			    cond
			    body
			    elses
			    (if (null? else)
			      #f
			      (make-Else $position $span else)))))

      (elseif ((literal-elsif w (bind cond expr) w then w (bind body compstmt) wn)
	       (make-Elseif $position $span cond body)))

      (unless-statement ((literal-unless w (bind expr expr) w then wn (bind body compstmt) wn (bind else (? literal-else wn compstmt)) wn literal-end)
			 (make-Unless-statement
                           $position $span
			   expr
			   body
			   (if (null? else)
			     #f
			     (make-Else $position $span else)))))
      
      (when-args (((bind args args) (bind rest (? w "," w "*" arg)))
		  (make-Args $position $span args (if (null? rest) #f rest)))
		 (("*" (bind arg arg))
		  (make-Args $position $span '() arg)))
      (then ((term) $)
	    ((":") $)
	    (("then") $)
	    ((term then) $))
      (literal ((numeric) $)
	       ((dsym) $)
	       ((symbol) $))
      (ruby-string (((bind first string*) (bind rest (* w ruby-string)))
	       first))
      #;
      (string (("\"" (bind str (* (except "\""))) "\"")
	       (make-String-literal (apply string-append "" (map scheme-string str))))
	      (("'" (bind str (* (except "'"))) "'")
	       (make-String-literal (apply string-append "" (map scheme-string str))))
	      (("`" (bind str (* (except "`"))) "`")
	       (make-String-literal (apply string-append "" (map scheme-string str)))))
      (unsigned-numeric ((unsigned-float) $)
			((unsigned-integer) $))
      (numeric ((float) $)
	       ((integer) $)
	       ((binary) $)
               ((octal) $)
	       ((hex) $))
      (transient hex (("0x" (bind first hex-digit) (* (? "_") hex-digit))
	    (make-Number $position $span (string->number (apply string-append (string first) (map string $)) 16))))
      (hex-digit (((apply char-range #\a #\z)) $)
		 (((apply char-range #\A #\Z)) $)
		 (((apply char-range #\0 #\9)) $))

      (transient octal (("0o" (bind first octal-digit) (* (? "_") octal-digit))
                        (make-Number $position $span (string->number (apply string-append (string first)
						   (map string $))
					    8))))

      (transient binary (("0b" (bind first binary-digit) (* (? "_") binary-digit))
	       (make-Number $position $span (string->number (apply string-append (string first)
						   (map string $))
					    2))))
      (binary-digit (((apply char-range #\0 #\1)) $))

      (e^ (((or "e" "E") unsigned-integer) $)
	  (((or "e" "E") "-" unsigned-integer)
	   (make-Number $position $span (- (Number-value $))))
	  (((or "e" "E") "+" unsigned-integer) $))
      (integer (((bind sign (? (or "-" "+")))
		 (bind num unsigned-integer)
		 (bind e (? e^)))
		(make-Number
                  $position $span
		  (let ((raw (if (null? sign)
			       (Number-value num)
			       (if (string=? sign "-")
				 (- (Number-value num))
				 (Number-value num)))))
		    (if (null? e)
		      raw
		      (* raw (expt 10 (Number-value e))))))))
      (unsigned-integer (((not "0x")
                          (not "0o")
			  (not "0b")
                          (? "0d")
			  (bind first x0-9)
			  (bind rest (* integer-num)))
			 (let ((num (string->number (apply string-append first rest))))
			   (make-Number $position $span num))))
      (integer-num ((x0-9) $)
		   (("_" x0-9) $))
      (float (((bind left integer) "." (bind right unsigned-integer)
				   (bind e (? e^)))
	      (let ((raw (+ (Number-value left) (convert-to-decimal (Number-value right)))))
		(if (null? e)
		  (make-Number $position $span raw)
		  (make-Number $position $span (* raw (expt 10 (Number-value e))))))))
      (unsigned-float
	(((bind left unsigned-integer) "." (bind right unsigned-integer))
	 (make-Number $position $span (+ (Number-value left) (convert-to-decimal (Number-value right))))))

      #|
(((bind p primary) w "[" wn (bind args aref-args) wn "]")
	    (make-Array-lookup p args))
	   (((bind p primary) "." (bind i identifier))
	    (make-Dotted-access p i))
	   (((bind p primary) "::" (bind i identifier))
	    (make-Dotted-access p i))
	   (((bind p primary) "." (bind c constant))
	    (make-Dotted-access p c))
	   (((bind p primary) "::" (bind c constant))
	    (make-Dotted-access p c))
	   |#


      (lhs ((primary-lhs-special) $)
	   (("::" (bind c constant))
	    (make-Dotted-access $position $span (make-Object $position $span) c))
	   ((backref) $)
	   (((bind var variable))
	    (make-Lhs $position $span var)))

      (variable ((literal-nil) (make-Nil $position $span))
		((literal-self) (make-Self $position $span))
		((literal-true) (make-True $position $span))
		((literal-__FILE__) (make-__FILE__ $position $span))
		((literal-false) (make-False $position $span))
		((instance-var) (make-Variable $position $span $))
		((global-var) (make-Variable $position $span $))
		((class-var) (make-Variable $position $span $))
		((constant) $)
		((identifier) (make-Variable $position $span $)))
      (instance-var (("@" (bind id any-identifier)) (make-Instance-variable $position $span id)))
      (global-var
	(("$_" (not identifier-char)) (make-Global-variable $position $span (make-Identifier $position $span "_")))
        (("$-" a-zA-Z) (make-Global-variable $position $span (make-Identifier $position $span $)))
	(("$~") (make-Global-variable $position $span (make-Identifier $position $span "~")))
	(("$*") (make-Global-variable $position $span (make-Identifier $position $span "*")))
	(("$$") (make-Global-variable $position $span (make-Identifier $position $span "$")))
	(("$?") (make-Global-variable $position $span (make-Identifier $position $span "?")))
	(("$!") (make-Global-variable $position $span (make-Identifier $position $span "!")))
	(("$@") (make-Global-variable $position $span (make-Identifier $position $span "@")))
	(("$/") (make-Global-variable $position $span (make-Identifier $position $span "/")))
	(("$\\") (make-Global-variable $position $span (make-Identifier $position $span "\\")))
	(("$;") (make-Global-variable $position $span (make-Identifier $position $span ";")))
	(("$,") (make-Global-variable $position $span (make-Identifier $position $span ",")))
	(("$.") (make-Global-variable $position $span (make-Identifier $position $span ".")))
	(("$=") (make-Global-variable $position $span (make-Identifier $position $span "=")))
	(("$:") (make-Global-variable $position $span (make-Identifier $position $span ":")))
	(("$<") (make-Global-variable $position $span (make-Identifier $position $span "<")))
	(("$>") (make-Global-variable $position $span (make-Identifier $position $span ">")))
	(("$\"") (make-Global-variable $position $span (make-Identifier $position $span "\"")))
	(("$" (bind id any-identifier)) (make-Global-variable $position $span id)))
      (class-var (("@@" (bind id any-identifier)) (make-Class-variable $position $span id)))

      [identifier/reserved [(reserved) $]
                           [(identifier) $]]

      (identifier (((not reserved)
		    (bind first a-z_) (bind rest (* a-zA-Z0-9_))
		    ;; fid endings
		    (not (or "?" "!"))
                    (bind result (predicate (let ((str (apply string-append first rest)))
					      str
					      #;
				 (if (reserved-word? str)
				   #f str)))))
		   (make-Identifier $position $span result)))
      ;; this identifier can be a reserved word
      (any-identifier (((bind first a-zA-Z_) (bind rest (* a-zA-Z0-9_))
		        (bind result (predicate (apply string-append first rest))))
		       (make-Identifier $position $span result)))
      (constant (((bind first A-Z) (bind rest (* a-zA-Z0-9_))
                  (bind result (predicate (apply string-append first rest))))
		   (make-Constant $position $span result)))
      (ruby-do ((term) $)
	       ((":") $)
	       (("do") (void)))
      (opt-terms (((* term)) $)
		 ((void)))
      (term ((";") (void))
	    ((newline) (void))
	    ((comment) (void)))
      (expr (((bind left expr-left) (bind right (* w expr-right)))
	     (foldl (lambda (proc obj) (proc obj)) left right)))
      (expr-right ((literal-and wn (bind e expr))
		   (lambda (obj)
		     (make-And $position $span obj e)))
		  ((literal-or wn (bind e expr))
		   (lambda (obj)
		     (make-Or $position $span obj e))))
      (expr-left
	;; hack to make sure that command-call doesn't overtake division
	;; but if a comma is next, then it must be a command
	;; (((ensure arg (not w (or call-args ","))) arg) $)
        ;; avoid a command followed by something that can follow division
        ;; I am not sure this is right now..
	(((bind c command-call) (ensure (or inlines (not w prec9)))) c)
	(("!" w (bind c command-call)) (make-Not $position $span c))
	((literal-not w (bind e expr)) (make-Not $position $span e))
	((arg) $)
	)
      ;; none a-z time    1120    times   51092
      ;; transient a-z time    3002    times   51092
      (a-z_ ((a-z) $)
	    (("_") $))
      (a-zA-Z_ ((a-z) $)
	       ((A-Z) $)
	       (("_") $))
      (a-zA-Z ((a-z) $)
	      ((A-Z) $))
      (a-zA-Z0-9_ ((a-zA-Z_) $)
		  ((x0-9) $))
      (a-zA-Z0-9 ((a-zA-Z) $)
		 ((x0-9) $))
      (a-z (((bind n (apply char-range #\a #\z)))
	    (scheme-string n)))
      ((char-range start end) (((bind c _)
				(predicate (and (char? c)
						(char>=? c start)
						(char<=? c end))))
			       c))
      #;
      (a-z (("a") $)
	   (("b") $)
	   (("c") $)
	   (("d") $)
	   (("e") $)
	   (("f") $)
	   (("g") $)
	   (("h") $)
	   (("i") $)
	   (("j") $)
	   (("k") $)
	   (("l") $)
	   (("m") $)
	   (("n") $)
	   (("o") $)
	   (("p") $)
	   (("q") $)
	   (("r") $)
	   (("s") $)
	   (("t") $)
	   (("u") $)
	   (("v") $)
	   (("w") $)
	   (("x") $)
	   (("y") $)
	   (("z") $))

      (A-Z (((bind n (apply char-range #\A #\Z)))
	    (scheme-string n)))
      #;
      (A-Z (("A") $)
	   (("B") $)
	   (("C") $)
	   (("D") $)
	   (("E") $)
	   (("F") $)
	   (("G") $)
	   (("H") $)
	   (("I") $)
	   (("J") $)
	   (("K") $)
	   (("L") $)
	   (("M") $)
	   (("N") $)
	   (("O") $)
	   (("P") $)
	   (("Q") $)
	   (("R") $)
	   (("S") $)
	   (("T") $)
	   (("U") $)
	   (("V") $)
	   (("W") $)
	   (("X") $)
	   (("Y") $)
	   (("Z") $))

      (x0-9 (((bind n (apply char-range #\0 #\9)))
	    (scheme-string n)))

      #;
      (x0-9 (("0") $)
	    (("1") $)
	    (("2") $)
	    (("3") $)
	    (("4") $)
	    (("5") $)
	    (("6") $)
	    (("7") $)
	    (("8") $)
	    (("9") $))

      (x1-9 (((bind n (apply char-range #\1 #\9)))
	    (scheme-string n)))

      #;
      (x1-9 (("1") $)
	    (("2") $)
	    (("3") $)
	    (("4") $)
	    (("5") $)
	    (("6") $)
	    (("7") $)
	    (("8") $)
	    (("9") $)))))

(define (ruby-parse obj)
  (parse (make-parser) obj))
