#lang scheme
(require parser-tools/yacc)
(require parser-tools/lex)
(require (prefix-in re: parser-tools/lex-sre))
;;(require algol60/cfg-parser)
;; (require errortrace/errortrace)

;; BNF came from this site
;; http://www.ruby-doc.org/docs/ruby-doc-bundle/Manual/man-1.4/yacc.html

(provide (all-defined-out))

(define-empty-tokens ruby-empty-tokens
  (;; operators
   + - / * ** & % ^ \. .. ... = <=> < > <= >= != === ==
   =>
   =~ !~ ! ~ << >> \[ \] \| && \|\| defined? \, ? :
   eof newline \( \) \{ symbol-begin
   super yield not return then else elseif nil self
   end-of-line alias break in class true false ensure
   undef do end if unless while until rescue
   or and def
   lowest
   string-begin
   string-end
   dvar-begin
   dvar-end
   ))

(define-tokens ruby-tokens
  (number varname global string string2 here-doc regexp
   symbol op-assign constant
   global-variable class-variable instance-variable
   ))

(define-lex-abbrevs
  (letter (re:or (re:/ #\a #\z) (re:/ #\A #\Z) #\_))
  (upper (re:/ #\A #\Z))
  (alpha (re:or letter (re:/ #\0 #\9)))
  (decimal (re:+ (re:/ #\0 #\9)))
  (upper-identifier (re:+ upper))
  (identifier (re:: letter (re:* alpha))))

(define (ruby-any next token)
  (lambda x
    (values next token)))

(define (ruby-lexer-string string level)
  (define-lex-abbrevs
    (global-variable (re:: "#" "$" identifier))
    (class-variable (re:: "#" "@" "@" identifier))
    (instance-variable (re:: "#" "@" identifier))
    )
  ;; (printf "Make lexer with string ~a\n" string)
  (lexer
    ((re:: "#" "{") (values (ruby-any (ruby-lexer level) 'dvar-begin)
			    (token-string string)))
    (global-variable
     (values (ruby-any (ruby-lexer-string "" level)
		       (token-global-variable (substring lexeme 2)))
	     (token-string string)))
    (class-variable
     (values (ruby-any (ruby-lexer-string "" level)
		       (token-class-variable (substring lexeme 3)))
	     (token-string string)))
    (instance-variable
     (values (ruby-any (ruby-lexer-string "" level)
		       (token-instance-variable (substring lexeme 2)))
	     (token-string string)))
    ((re:: "\"") 
     (values (ruby-any (ruby-lexer (sub1 level)) 'end-of-string)
	     (token-string string)))
    ((re:: any-char)
     ((ruby-lexer-string (string-append string lexeme) level) input-port))))

(define (ruby-lexer in-string-level)
  (lexer
    ((re:or 
       "+" "-" "/" "*" "**" "&" "%" "^" ".." "..." "=" "<=>" "<" ">"
       "<=" ">=" "!=" "===" "==" "=>"
       "=~" "!~" "!" "~" "<<" ">>" "|" "&&" "||" "defined?"
        "eof" ":" "(" ")" "[" "]" "."
       "or" "and" "def" "ensure"
       "?" ":" "else" "elseif"
       "alias" "super" "yield" "not" "return"
       "then" "nil" "self" "do" "end" "class"
       "false" "true" "until" "undef" "break" "in" "unless"
       "rescue" "if" "while" "nil")
     (values (ruby-lexer in-string-level)
	     (string->symbol lexeme)))
    ((re:or ";") (values (ruby-lexer in-string-level) 'end-of-line))
    ((re:or "+=" "-=" "**=" "/=" "%=" "*="
	    "=~" "&=" "|=" "^=" "<<=" ">>=" "&&=" "||=")
     (values (ruby-lexer in-string-level)
	     (token-op-assign (string->symbol lexeme))))
    (upper-identifier (values (ruby-lexer in-string-level) (token-constant lexeme)))
    ((re:: identifier) (values (ruby-lexer in-string-level) (token-varname lexeme)))
    ((re:: "$" identifier) (values (ruby-lexer in-string-level) (token-global (substring lexeme 1))))
    ;; ((re:: "\"" (re:~ "\"") "\"") (token-string lexeme))
    ((re:: "\"") (values (ruby-lexer-string "" (add1 in-string-level)) 'string-begin))
    ((re:: "}") (if (> in-string-level 0)
		  (values (ruby-lexer-string "" in-string-level) 'dvar-end)
		  (string->symbol lexeme)))
    ((re:: ":" identifier) (values (ruby-lexer in-string-level) (token-symbol lexeme)))
    ((re:: decimal) (values (ruby-lexer in-string-level) (token-number lexeme)))
    ;; ((re:: ":") (token-symbol lexeme))
    ((eof) (values (ruby-lexer in-string-level) 'eof))
    (#\newline (values (ruby-lexer in-string-level) 'newline))
    ((re:or #\space #\tab) ((ruby-lexer in-string-level) input-port))))

(define ruby-parser
  (parser
    (start Program)
    (end eof)
    (error (lambda (token-ok token-name token-value) 
	     (printf "parser error at '~a' ~a\n" token-name token-value)))
    (tokens ruby-tokens ruby-empty-tokens)
    (debug "debug")
    (yacc-output "yacc.crap")
    (precs (nonassoc lowest)
	   (nonassoc \{)
	   (nonassoc if unless while until)
	   (left or and)
	   (right not)
	   (nonassoc defined?)
	   (right = op-assign)
	   (left rescue)
	   (right ? :)
	   (nonassoc .. ...)
	   (left &&)
	   (left \|\|)
	   (nonassoc == <=> === != =~ !~)
	   (left > >= < <=)
	   (left \| ^)
	   (left &)
	   (left << >>)
	   (left + -)
	   (left * / %)
	   ;; unary minus
	   ;; (right -)
	   (right **)
	   ;; unary plus
	   (right ! ~)
	   )

    #|
%nonassoc tLOWEST
%nonassoc tLBRACE_ARG
%nonassoc  kIF_MOD kUNLESS_MOD kWHILE_MOD kUNTIL_MOD
%left  kOR kAND
%right kNOT
%nonassoc kDEFINED
%right '=' tOP_ASGN
%left kRESCUE_MOD
%right '?' ':'
%nonassoc tDOT2 tDOT3
%left  tOROP
%left  tANDOP
%nonassoc  tCMP tEQ tEQQ tNEQ tMATCH tNMATCH
%left  '>' tGEQ '<' tLEQ
%left  '|' '^'
%left  '&'
%left  tLSHFT tRSHFT
%left  '+' '-'
%left  '*' '/' '%'
%right tUMINUS_NUM tUMINUS
%right tPOW
%right '!' '~' tUPLUS

%token tLAST_TOKEN
|#

    (grammar
      ;; (program ((number) $1))
      (Program ((Compstmt) $1))
      (Compstmt ((Stmts Opt-terms) (reverse $1)))
      (Opt-terms (() null)
		 ((Terminals) null))
      (maybe-expression-list (() (list))
			     ((Expression-list) $1))
      (Expression-list ((end-of-line Expression) (list $2))
		       ((Expression-list Expression) (cons $2 $1)))
      (end-of-line? (() null)
		    ((end-of-line) null))
      (Stmts (() null)
	     ((Stmt) (list $1))
	     ((Stmts Terminals Stmt) (cons $3 $1)))
      (Stmt ;;((alias fitem :((varname) $1))
	    ((Stmt if Expression-value) (list 'if $1 $3))
	    ((Stmt unless Expression-value) (list 'unless $1 $3))
	    ((Stmt while Expression-value) (list 'while $1 $3))
	    ((Stmt until Expression-value) (list 'until $1 $3))
	    ((Stmt rescue Stmt) (list 'rescue $1 $3))
	    ((Lhs = Command-call) (list '= $1 $3))
	    ((Mlhs = Command-call) (list 'mlhs= $1 $3))
	    ((Primary-value \[ Aref-args \] op-assign Command-call) $1)
	    ((Primary-value \. varname op-assign Command-call) $1)
	    ((Lhs = Mrhs) (list '= $1 $3))
	    ((Expression) $1))
      (Mlhs ((Mlhs-basic) $1)
	    ((\( Mlhs-entry \)) $2))
      (Mlhs-basic ((Mlhs-head) $1)
		  ((Mlhs-head Mlhs-item) $1)
		  ((Mlhs-head * Mlhs-node) $1)
		  ((Mlhs-head *) $1)
		  ((* Mlhs-node) $2)
		  ((*) null))
      (Mlhs-head ((Mlhs-item \,) $1)
		 ((Mlhs-head Mlhs-item \,) $1))
      (Mlhs-item ((Mlhs-node) $1)
		 ((\( Mlhs-entry \)) $2))
      (Mlhs-entry ((Mlhs-basic) $1)
		  ((\( Mlhs-entry \)) $2))
      (Mlhs-node ((Variable) $1)
		 ((Primary-value \[ Aref-args \]) $1))
      (Variable ((varname) $1)
		((constant) $1)
		((true) #t)
		((false) #f))
      (Primary-value ((Primary) $1))
      (Aref-args (() null)
		 ((Command Opt-newline) $1)
		 ((Args Trailer) $1))
      (Trailer ((newline) null)
	       ((\,) null)
	       (() null))
      (Lhs ((Variable) $1))
      (Mrhs ((Args \, Arg-value) $1)
	    ((Args \, * Arg-value) $1)
	    ((* Arg-value) $2))
      (Args ((Arg-value) $1)
	    ((Args \, Arg-value) (cons $3 $1)))
      (Arg-value ((Arg) $1))
      (Arg ((Lhs = Arg) $1)
	   ((Lhs = Arg rescue Arg) $1)
	   ((Arg + Arg) $1)
	   ((Arg - Arg) $1)
	   ((Arg * Arg) $1)
	   ((Arg / Arg) $1)
	   ((Arg % Arg) $1)
	   ((Arg ** Arg) $1)
	   ((Arg \| Arg) $1)
	   ((Arg ^ Arg) $1)
	   ((Arg & Arg) $1)
	   ((Arg <=> Arg) $1)
	   ((Arg .. Arg) $1)
	   ((Arg ... Arg) $1)
	   ((Arg < Arg) $1)
	   ((Arg > Arg) $1)
	   ((Arg <= Arg) $1)
	   ((Arg >= Arg) $1)
	   ((Arg == Arg) $1)
	   ((Arg === Arg) $1)
	   ((Arg != Arg) $1)
	   ((Arg =~ Arg) $1)
	   ((Arg !~ Arg) $1)
	   ((! Arg) $2)
	   ((~ Arg) $2)
	   ((Arg << Arg) $1)
	   ((Arg >> Arg) $1)
	   ((Arg && Arg) $1)
	   ((Arg \|\| Arg) $1)
	   ((defined? Opt-newline Arg) $3)
	   ((Arg ? Arg : Arg) $1)
	   ((Primary) $1)
	   )
      ;; fix rescue
      (Bodystmt ((Compstmt #;Optional-rescue
			   Optional-else Optional-ensure) $1))
      (Optional-rescue ((rescue Exception-list Exception-variable Then) $2)
		       (() null))
      (Exception-list ((Arg-value) $1)
		     ((Mrhs) $1)
		     (() null))
      (Exception-variable ((=> Lhs) $2)
			  (() null))
      (Optional-ensure ((ensure Compstmt) $2)
		       (() null))
      (Primary ((Literal) $1)
	       ((Many-strings) $1)
	       ((if Expression-value Then Compstmt If-tail end) $2)
	       ((return) null)
	       ((def varname Function-arglist Bodystmt end) $2)
	       #;
	       ((Xstring) $1)
	       )
      (Function-arglist ((\( Function-args Opt-newline \)) $2)
			((Function-args Terminal) $1))
      (Function-args ((Function-arg Function-optional-block-arg) $1)
		     (() null))
      (Function-arg ((Function-normal-arg) $1)
		    ((Function-arg \, Function-normal-arg) $1))
      (Function-normal-arg ((varname) $1))
      (Function-optional-block-arg ((\, Function-block-arg) $2)
				   (() null))
      (Function-block-arg ((Block-arg-mark varname) $1))
      (Block-arg-mark ((&) null))
      (If-tail ((Optional-else) $1)
	       ((elseif Expression-value then Compstmt If-tail) $2))
      (Optional-else (() null)
		     ((else Compstmt) $2))
      (Then ((then) null))
      (Xstring ((number) $1))
      (Many-strings ((Strings) $1))
      (Strings ((String1) $1)
	       ((Strings String1) $1))
      (Literal ((number) $1)
	       ((symbol) $1)
	       ((Dsym) $1))
      (String1 ((string-begin String-contents string-end) $2))
      ;; (symbol ((: varname) $2))
      (Xstring-contents (() null)
			((Xstring-contents String-content) $2))
      (String-contents (() null)
		       ((String-contents String-content) $1))
      (String-content ((string) $1)
		       ((String-var) $1)
		       ((dvar-begin Compstmt dvar-end) $2))
      (String-var ((global-variable) $1)
		  ((class-variable) $1)
		  ((instance-variable) $1))
      (Dsym ((symbol-begin Xstring-contents) $2))
      (Opt-newline (() null)
		   ((newline) null))
      (Expression-value ((Expression) $1))
      (Terminals ((Terminal) null)
		 ((Terminals Terminal) null))
      (Terminal ((end-of-line) null)
		((newline) null))
      (Expressions ((end-of-line Expression) $2))
      (Expression ((Command-call) $1)
		  ((Expression and Expression) $1)
		  ((Expression or Expression) $1)
		  ((not Expression) $2)
		  ((! Command-call) $2)
		  ((Arg) $1))
      (Command-call ((Command) $1))
      (Command ((Operation Command-args) $1)
	       ((super Command-args) $2)
	       ((yield Command-args) $2))
      (Command-args ((Open-args) $1))
      (Open-args ((Call-args) $1)
		 ((\( \)) null)
		 ((\( Call-args2 \)) $2))
      (Call-args ((Command) $1)
		 ((Args Optional-block-arg) $1)
		 ((Args \, * Arg-value Optional-block-arg) $1)
		 ((Assocs Optional-block-arg) $1)
		 ((Assocs \, * Arg-value Optional-block-arg) $1)
		 ((Args \, Assocs Optional-block-arg) $1)
		 ((Args \, Assocs \, * Arg Optional-block-arg) $1)
		 ((* Arg-value Optional-block-arg) $2)
		 ((Block-arg) $1))
      (Call-args2 ((Arg-value \, Args Optional-block-arg) $1)
		  ((Arg-value \, Block-arg) $1)
		  ((Arg-value \, * Arg-value Optional-block-arg) $1)
		  ((Arg-value \, Args \, * Arg-value Optional-block-arg) $1)
		  ((Block-arg) $1))
      (Assocs ((Assoc) $1)
	      ((Assocs \, Assoc) $1))
      (Assoc ((Arg-value => Arg-value) $1))
      (Optional-block-arg ((\, Block-arg) $2)
			  (() null))
      (Block-arg ((& Arg-value) $2))
      (Operation ((varname) $1)
		 ((constant) $1))
      )
    ))

(define (parse input)
  (ruby-parser (let ((lexer (ruby-lexer 0)))
		 (lambda () (let-values (((next-lexer lexeme)
					  (lexer input)))
			      (set! lexer next-lexer)
			      lexeme)))))

(provide lex)
(define (lex string)
  (let ((lexer (let ((port (open-input-string string)))
		 (let ((lexer (ruby-lexer 0)))
		   (lambda ()
		     (let-values (((next-lexer lexeme)
					  (lexer port)))
			      (set! lexer next-lexer)
			      lexeme))))))
    (let loop ((arg (lexer))
	       (tokens '()))
      (if (not (eq? 'eof arg))
	(loop (lexer) (cons arg tokens))
	(reverse tokens)))))
