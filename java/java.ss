#lang scheme

;; Rules come from this Java 6 BNF
;; http://www.cmis.brighton.ac.uk/staff/rnb/bosware/javaSyntax/rulesLinked.html

(require "../peg.ss")

(provide pretty-print
         (rename-out (java-parse parse)))

(define (pretty-print ast)
  (printf "~a\n" ast))

(define (make-parser)
  (peg
    (start compilation-unit)
    (grammar
      [compilation-unit [(wn (? package-clause) wn (* import wn) (* wn type-declaration) wn eof)
                         'compilation-unit]]

      [package-clause [(package wn qualified-identifier wn ";") 'package]]
      [qualified-identifier [(identifier (* "." identifier)) $]]
      [identifier [((not reserved) a-zA-Z$_ (* a-zA-Z0-9$_)) $]]

      [a-zA-Z_ [(a-z) $]
               [(A-Z) $]
               [("_") $]]
      [a-zA-Z$_ [(a-zA-Z_) $]
                [("$") $]]
      [a-zA-Z0-9$_ [(a-zA-Z0-9_) $]
                   [("$") $]]
      [a-zA-Z0-9_ [(a-z) $]
                  [(A-Z) $]
                  [(x0-9) $]
                  [("_") $]]
      [a-z [((apply char-range #\a #\z)) $]]
      [A-Z [((apply char-range #\A #\Z)) $]]
      [x0-9 [((apply char-range #\0 #\9)) $]]
      [x1-9 [((apply char-range #\1 #\9)) $]]
      [(char-range start end) [((bind c _)
				(predicate (and (char? c)
						(char>=? c start)
						(char<=? c end))))
			       c]]

      [type-declaration [(class-declaration) $]
                        [(interface-declaration) $]
                        [(";") $]]
      [class-declaration [(normal-class-declaration) $]
                         [(enum-declaration) $]]
                         
      [enum-declaration [((* wn class-modifier) wn enum wn identifier wn (? interfaces) enum-body) $]]
      [interfaces [(implements wn interface-type-list) $]]
      [class-modifier [(annotation) $]
                      [(public) $]
                      [(protected) $]
                      [(private) $]
                      [(abstract) $]
                      [(static) $]
                      [(final) $]
                      [(strictfp) $]]

      [enum-body [("{" wn (? enum-constants) wn (? ",") wn (? enum-body-declarations) wn "}") $]]
      [enum-body-declarations [(";" (* wn class-body-declaration)) $]]
      [enum-constants [(enum-constant (* wn "," wn enum-constant)) $]]
      [enum-constant [((* wn annotation) wn identifier (? wn arguments) (? wn class-body)) $]]
      [arguments [("(" wn argument-list wn ")") $]]

      [normal-class-declaration
        [((* wn class-modifier) wn class wn identifier (? wn type-parameters) (? wn super-class) (? wn extends-interfaces) wn class-body) 'class]]
      [type-parameters [("<" wn type-parameter-list wn ">") $]]
      [type-parameter-list [(type-parameter (* wn "," type-parameter)) $]]
      [type-parameter [(type-variable (? wn type-bound)) $]]
      [type-bound [(extends wn class-or-interface-type (? wn additional-bound-list)) $]]
      [additional-bound-list [("&" wn interface-type (? wn additional-bound-list)) $]]
      [super-class [(extends wn class-type) $]]
      [class-body [("{" (* wn class-body-declaration) wn "}") $]]
      [class-body-declaration [(method-declaration) $]
                              [(constructor-declaration) $]
                              [(field-declaration) $]
                              [(class-declaration) $]
                              [(interface-declaration) $]
                              [(instance-initializer) $]
                              [(static-initializer) $]
                              [(";") $]]
      [field-declaration [((* field-modifier wn) wn type wn variable-declarator (* wn "," wn variable-declarator) wn ";") $]]
      [field-modifier [(annotation) $]
                      [(public) $]
                      [(protected) $]
                      [(private) $]
                      [(static) $]
                      [(final) $]
                      [(transient*) $]
                      [(volatile) $]]

      [instance-initializer [(block) $]]
      [static-initializer [(static wn block) $]]

      [method-declaration [(method-header wn method-body) $]]
      [method-modifier [(annotation) $]
                       [(public) $]
                       [(private) $]
                       [(protected) $]
                       [(abstract) $]
                       [(static) $]
                       [(final) $]
                       [(synchronized) $]
                       [(native) $]
                       [(strictfp) $]]

      [method-header [((* method-modifier wn) wn (* type-parameters wn) result-type wn method-declarator wn (? throws-clause)) $]]
      [result-type [(type) $]
                   [(void) $]]

      [type [(reference-type) $]
            [(primitive-type) $]]

      [primitive-type [(int) $]
                      [(byte) $]
                      [(short) $]
                      [(long) $]
                      [(char) $]
                      [(float) $]
                      [(double) $]
                      [(boolean) $]]

      [reference-type [(class-or-interface-type (* wn "[" wn "]")) $]
                      [(primitive-type (* wn "[" wn "]")) $]]

      [reference-type-list [(reference-type (* wn "," wn reference-type)) $]]

      [class-or-interface-type [(identifier wn (? type-arguments) (* wn "." wn identifier (? wn type-arguments))) $]]

      [method-declarator [(identifier wn "(" wn (? formal-parameter-list) wn ")" wn (? dims)) $]]
      [formal-parameter-list [((* formal-parameter wn "," wn) last-formal-parameter) $]]
      [formal-parameter [((* variable-modifier wn) wn type wn variable-declarator-id) $]]
      [last-formal-parameter [((* variable-modifier wn) wn type (? "...") wn variable-declarator-id) $]]
      [method-body [(block) $]
                   [(";") $]]
      [block [("{" wn (* block-statement wn) wn "}") $]]
      [block-statement [(local-variable-declaration wn ";") $]
                       [(class-declaration) $]
                       [(statement) $]]

      [statement [(statement-without-trailing-substatement) $]
                 [(labeled-statement) $]
                 [(if-then-else-statement) $]
                 [(if-then-statement) $]
                 [(while-statement) $]
                 [(for-statement) $]]

      [labeled-statement [(identifier wn ":" wn statement) $]]

      [if-then-statement [(if wn "(" wn expression wn ")" wn statement) $]]
      [if-then-else-statement [(if wn "(" wn expression wn ")" wn statement wn else wn statement) $]]

      [while-statement [(while wn "(" wn expression wn ")" wn statement) $]]

      [for-statement [(basic-for-statement) $]
                     [(enhanced-for-statement) $]]

      [basic-for-statement [(for wn "(" wn (? for-init) wn ";" wn (? expression) wn ";" wn (? for-update) wn ")" wn statement) $]]
      [for-init [(statement-expression-list) $]
                [(local-variable-declaration) $]]

      [for-update [(statement-expression-list) $]]

      [statement-expression-list [(statement-expression (* wn "," wn statement-expression)) $]]

      [enhanced-for-statement [(for wn "(" wn (* variable-modifier wn) wn type wn identifier wn ":" wn expression wn ")" wn statement) $]]

      [statement-without-trailing-substatement
        [(block) $]
        [(empty-statement) $]
        [(statement-expression wn ";") $]
        [(assert-statement) $]
        [(switch-statement) $]
        [(do-statement) $]
        [(break-statement) $]
        [(continue-statement) $]
        [(return-statement) $]
        [(synchronized-statement) $]
        [(throw-statement) $]
        [(try-statement) $]]

      [empty-statement [(";") $]]

      [assert-statement [(assert wn expression ":" wn expression) $]
                        [(assert wn expression) $]]

      [switch-statement [(switch wn "(" wn expression wn ")" wn switch-block) $]]
      [switch-block [("{" wn (? switch-block-statement-groups) wn (? switch-labels) wn "}") $]]
      [switch-block-statement-groups
        [((+ wn switch-block-statement-group)) $]]
      [switch-block-statement-group
        [(switch-labels (+ wn block-statement)) $]]
      [switch-labels [((+ wn switch-label)) $]]
      [switch-label [(case wn constant-expression wn ":") $]
                    [(case wn enum-constant-name wn ":") $]
                    [(default wn ":") $]]
      [constant-expression [(expression) $]]
      [enum-constant-name [(identifier) $]]

      [do-statement [(do wn statement wn while wn "(" wn expression wn ")" wn ";") $]]

      [break-statement [(break wn (? identifier) wn ";") $]]
      [continue-statement [(continue wn (? identifier) wn ";") $]]

      [return-statement [(return (? wn expression) wn ";") $]]

      [expression [(assignment-expression) $]]

      [synchronized-statement [(synchronized wn "(" wn expression wn ")" wn block) $]]

      [throw-statement [(throw wn expression wn ";") $]]

      [try-statement [(try wn block wn (? catches) wn finally wn block) $]
                     [(try wn block wn catches) $]]
      
      [catches [(catch-clause (? wn catches)) $]]
      [catch-clause [(catch wn "(" wn formal-parameter wn ")" wn block) $]]

      [NI [("XXnot implementedXX") $]]

      [statement-expression [(assignment) $]
                            [(method-invocation) $]
                            [(pre-increment-expression) $]
                            [(pre-decrement-expression) $]
                            [(post-increment-expression) $]
                            [(post-decrement-expression) $]
                            [(class-instance-creation-expression) $]]


      [pre-increment-expression [("++" wn unary-expression) $]]
      [pre-decrement-expression [("--" wn unary-expression) $]]
      
      ;; not sure about this
      ;; [method-name [(identifier (? "." ambiguous-name (not wn "("))) $]]
      ;; [method-name [(identifier (? "." ambiguous-name)) $]]
      [method-name [(identifier) $]]
      ;; [primary-expression-name [(NI) $]]
      ;; [primary-expression-name [(identifier (? "." primary-expression-name (not wn "("))) $]]
      [primary-expression-name [(identifier (not wn "(")) $]]
      [ambiguous-name [(identifier (? "." ambiguous-name)) $]]
      [simple-type-name [(identifier) $]]

      ;; primary, class-instance-creation-expression, array-creation-expression,
      ;; field-access, and array-access are all related. indirect left recursion
      ;; is sorted out through fusion + direct left recursion removal
      ;; M = M' | P MR
      ;; C = C' | P CR
      ;; F = F' | P FR
      ;; A = A' | P' AR
      ;; P = P' | PR
      ;; P' = M | C | F | A | L
      ;;
      ;; MCFA = (M' | ((MCFA | L) | PR) MR) |
      ;;        (C' | ((MCFA | L) | PR) CR) |
      ;;        (F' | ((MCFA | L) | PR) FR) |
      ;;        (A' | (MCFA | L) AR)
      ;;
      ;; MCFA = M' | MCFA MR | L MR | PR MR |
      ;;        C' | MCFA CR | L CR | PR CR |
      ;;        F' | MCFA FR | L FR | PR FR
      ;;        A' | MCFA AR | L AR
      ;;
      ;; MCFA = M' MCFA* | C' MCFA* | F' MCFA* | A' MCFA |
      ;;        L MR MCFA* | PR MR MCFA* |
      ;;        L CR MCFA* | PR CR MCFA* |
      ;;        L FR MCFA* | PR FR MCFA* |
      ;;        L AR MCFA*
      ;;
      ;; MCFA* = MR MCFA* | CR MCFA* | FR MCFA* | AR MCFA*
      ;;
      ;; M = M' | P MR
      ;; C = C' | P CR
      ;; F = F' | P FR
      ;; A = A' | P' AR
      ;; P' = MCFA | L

      ;; to keep the implementation close to the algebra I am using the
      ;; following variables
      ;; P-left = primary-no-new-array
      ;; P-right = array-creation-expression
      ;; M-left = non-recursive stuff in method-invocation
      ;; M-right = stuff after primary in method-invocation
      ;; C-left = non-recursive stuff in class-instance-creation-expression
      ;; C-right = stuff after primary in class-instance-creation-expression
      ;; F-left = non-recursive stuff in field-access
      ;; F-right = stuff after primary in field-access
      ;; A-left = non-recursive stuff in array-access
      ;; A-right = stuff after P-left in array-access
      
      [(primary ender)
       [((apply primary-no-new-array ender)) $]
       [(array-creation-expression) $]]

      [class-instance-creation-expression
        [((apply primary C-right)) 'class-instance-creation-expression]
        [(C-left (not (or M-right C-right F-right A-right))) $]]
      [method-invocation
        [((apply primary M-right)) 'method-invocation]
        [(M-left (not (or M-right C-right F-right A-right))) $]]
      [field-access
        [((apply primary F-right)) 'field-access]
        [(F-left (not (or M-right C-right F-right A-right))) 'field-access]]
      [array-access
        #;
        [(P-right wn A-right) 'array-access]
        [((apply Amcfa A-right)) 'array-access]
        [(A-left (not (or M-right C-right F-right A-right))) $]]

      ;; [primary-dont-care [((except (or "." "["))) $]]
      [primary-dont-care [(()) 'dont-care]]

      [P-right [(array-creation-expression) $]] 

      [C-left
        [(new (? wn type-arguments) wn class-or-interface-type wn "(" wn (? argument-list) wn ")" (? wn class-body)) $]]
      [C-right [("." new wn (? type-arguments) wn identifier wn (? type-arguments) wn "(" wn (? argument-list) wn ")") $]]


      [M-left [(method-name wn braced-argument-list) $]
              [(super "." method-invocation-rest) $]
              [(type-name "." super "." method-invocation-rest) $]
              [(type-name "." non-wild-type-arguments identifier wn braced-argument-list) $]]

      [M-right [("." method-invocation-rest) $]]

      [F-left [(super "." identifier) $]
              [(type-name "." super "." identifier) $]]
      [F-right [("." identifier) $]]
      [A-left [(expression-name wn "[" wn expression wn "]") $]]
      [A-right [("[" wn expression wn "]") $]]

      [(primary-no-new-array ender)
       [((apply mcfa ender)) $]
       #;
       [(primary-l (not wn (or M-right C-right F-right A-right))) $]]

      ;; [(class-instance-creation-expression) $]
      ;; [(field-access) $]
      ;; [(method-invocation) $]
      ;; [(array-access) $]]

      ;; MCFA = M' MCFA* | C' MCFA* | F' MCFA* | A' MCFA |
      ;;        L MR MCFA* | PR MR MCFA* |
      ;;        L CR MCFA* | PR CR MCFA* |
      ;;        L FR MCFA* | PR FR MCFA* |
      ;;        L AR MCFA*
      ;;
      ;; MCFA* = MR MCFA* | CR MCFA* | FR MCFA* | AR MCFA*


      [(mcfa ender) [(M-left wn (apply mcfa* ender)) (list 'm-left $)]
                    [(C-left wn (apply mcfa* ender)) (list 'c-left $)]
                    [(F-left wn (apply mcfa* ender)) (list 'f-left $)]
                    [(A-left wn (apply mcfa* ender)) (list 'a-left $)]
                    [(primary-l wn (apply mcfa* ender)) $]
                    ;; [(primary-l M-right mcfa*) $]
                    ;; [(primary-l C-right mcfa*) $]
                    ;; [(primary-l F-right mcfa*) $]
                    ;; [(primary-l A-right mcfa*) $]
                    [(P-right wn (apply mcfa* ender)) $]
                    ;; [(P-right M-right mcfa*) $]
                    ;; [(P-right C-right mcfa*) $]
                    ;; [(P-right F-right mcfa*) $]
                    ]

      [(Amcfa ender) [(M-left wn (apply mcfa* ender)) (list 'm-left $)]
                    [(C-left wn (apply mcfa* ender)) (list 'c-left $)]
                    [(F-left wn (apply mcfa* ender)) (list 'f-left $)]
                    [(A-left wn (apply mcfa* ender)) (list 'a-left $)]
                    [(primary-l wn (apply mcfa* ender)) $]]

      [(mcfa* ender) [((apply ender) (not wn (or M-right C-right F-right A-right))) $]
                     [(M-right wn (apply mcfa* ender)) (list 'm-right $)]
                     [(C-right wn (apply mcfa* ender)) (list 'c-right $)]
                     [(F-right wn (apply mcfa* ender)) (list 'f-right $)]
                     [(A-right wn (apply mcfa* ender)) (list 'a-right $)]
                     ;; [((not (apply ender)) ()) 'mcfa*]
                     ]

      [primary-l 
        [(literal) $]
        [(type "." class) $]
        [(void "." class) $]
        [(this) $]
        [(type-name "." this) $]
        [(primary-expression-name) $]
        [("(" wn expression wn ")") $]]
      
      [type-arguments [("<" wn actual-type-argument-list wn ">") $]]
      [actual-type-argument-list [(actual-type-argument (* wn "," wn actual-type-argument)) $]]
      [actual-type-argument [(reference-type) $]
                            [(wild-card) $]]
      [wild-card [("?" (? wn wild-card-bounds)) $]]
      [wild-card-bounds [(extends wn reference-type) $]
                        [(super wn reference-type) $]]

      [argument-list [(expression (* wn "," wn expression)) $]]

      [literal [(integer-literal) $]
               [(float-literal) $]
               [(boolean-literal) $]
               [(character-literal) $]
               [(string-literal) $]
               [(null-literal) $]]

      [float-literal [(decimal-float-literal) $]
                     [(hexadecimal-float-literal) $]]

      [decimal-float-literal [(digits "." (? digits) (? exponent) (? float-suffix)) $]
                             [("." digits (? exponent) (? float-suffix)) $]
                             [(digits exponent (? float-suffix)) $]
                             [(digits (? exponent) float-suffix) $]]

      [exponent [(exponent-indicator signed-integer) $]]
      [exponent-indicator [((or "e" "E")) $]]
      [signed-integer [((? sign) digits) $]]
      [sign [("+") $]
            [("-") $]]

      [float-suffix [("f") $]
                    [("F") $]
                    [("d") $]
                    [("D") $]]

      [hexadecimal-float-literal [(NI) $]]

      [boolean-literal [("true") $]
                       [("false") $]]

      [character-literal [("'" single-character "'") $]
                         [("'" escape-character "'") $]]
      [single-character [((except (or "'" "\\"))) $]]
      [escape-character [("\\u" hex-digit hex-digit hex-digit hex-digit) $]
                        [("\\" (or "b" "t" "n" "f" "r" "\"" "'" "\\" octal-escape)) $]]
      [octal-escape [(NI) $]]

      [string-literal [("\"" (* string-contents) "\"") $]]
      [string-contents [("\\" _) $]
                       [((except "\"")) $]]

      [null-literal [(null) $]]

      [integer-literal [(decimal-integer-literal) $]
                       [(hex-integer-literal) $]
                       [(octal-integer-literal) $]]

      [digits [(digit (* digit)) $]]
      [digit [(x0-9) $]]
      [decimal-integer-literal [(decimal-numeral (not (or "." "x" "f" "F" "e" "E")) (? integer-type-suffix)) $]]
      [decimal-numeral [("0" (not digit)) $]
                       [(x1-9 (* x0-9)) $]]
      [integer-type-suffix [((or "l" "L")) $]]
      [hex-integer-literal [(hex-numeral (? integer-type-suffix)) $]]
      [octal-integer-literal [(octal-numeral (? integer-type-suffix)) $]]

      [hex-numeral [("0" (or "x" "X") hex-digits) $]]
      [hex-digits [((+ hex-digit)) $]]
      [hex-digit [((apply char-range #\a #\f)) $]
                 [((apply char-range #\A #\F)) $]
                 [(x0-9) $]]

      [octal-numeral [("0" octal-digits) $]]
      [octal-digits [((+ octal-digit)) $]]
      [octal-digit [((apply char-range #\0 #\7)) $]]

      [array-creation-expression [(new wn array-creation-expression-rest) $]]
      [array-creation-expression-rest
        [(primitive-type wn dim-specifier) $]
        [(class-or-interface-type wn dim-specifier) $]]
      [dim-specifier [(dim-exprs wn (? dims)) $]
                     [(dims wn array-initializer) $]]
      [dim-exprs [((+ dim-expr wn)) $]]
      [dim-expr [("[" wn expression wn "]") $]]
      [array-initializer [("{" wn (? variable-initializer (* wn "," wn variable-initializer))
                           wn (? ",") wn "}") $]]

      [type-name [(identifier (? "." package-or-type-name)) $]]

      [braced-argument-list [("(" wn (? argument-list) wn ")") $]]
      [method-invocation-rest [((? non-wild-type-arguments) wn identifier wn braced-argument-list) $]]

      [non-wild-type-arguments [("<" wn reference-type-list wn ">") $]]

      [assignment [(left-hand wn assignment-operator wn assignment-expression) $]]

      ;; this order is probably right. at least expression-name should
      ;; probably be last.
      [left-hand [(array-access) $]
                 [(field-access) $]
                 [(expression-name) $]]

      [assignment-operator [("=") $] [("*=") $] [("/=") $]
                           [("%=") $] [("+=") $] [("-=") $]
                           [("<<=") $] [(">>=") $] [(">>>=") $]
                           [("&=") $] [("^=") $] [("|=") $]]
      [assignment-expression [(assignment) $]
                             [(conditional-expression) $]]
      [conditional-expression [(conditional-or-expression wn "?" wn
                                expression wn ":" wn conditional-expression)
                               $]
                              [(conditional-or-expression) $]]
      [conditional-or-expression [(conditional-and-expression wn "||" wn conditional-or-expression)
                                  $]
                                 [(conditional-and-expression) $]]
      [conditional-and-expression [(inclusive-or-expression wn "&&" wn conditional-and-expression)
                                   $]
                                  [(inclusive-or-expression) $]]
      [inclusive-or-expression [(exclusive-or-expression wn "|" wn inclusive-or-expression)
                                $]
                               [(exclusive-or-expression) $]]
      [exclusive-or-expression [(and-expression wn "^" wn exclusive-or-expression)
                                $]
                               [(and-expression) $]]
      [and-expression [(equality-expression wn "&" wn and-expression) $]
                      [(equality-expression) $]]
      [equality-expression [(relational-expression (* wn equality-expression-rest)) $]]
      [equality-expression-rest [("==" wn equality-expression) $]
                                [("!=" wn equality-expression) $]]
      [relational-expression [(shift-expression wn (* wn relational-expression-rest)) $]]
      [relational-expression-rest [("<" wn relational-expression) $]
                                  [(">" wn relational-expression) $]
                                  [("<=" wn relational-expression) $]
                                  [(">=" wn relational-expression) $]
                                  [("instanceof" wn reference-type) $]]
      [shift-expression [(additive-expression (* wn shift-expression-rest)) $]]
      [shift-expression-rest [("<<" wn shift-expression) $]
                             [(">>>" wn shift-expression) $]
                             [(">>" wn shift-expression) $]]
      [additive-expression [(multiplicative-expression (* wn additive-expression-rest)) $]]
      [additive-expression-rest [("+" (not "+") wn additive-expression) $]
                                [("-" (not "-") wn additive-expression) $]]
      [multiplicative-expression [(unary-expression (* wn multiplicative-expression-rest)) $]]
      [multiplicative-expression-rest [("*" wn multiplicative-expression) $]
                                      [("/" wn multiplicative-expression) $]
                                      [("%" wn multiplicative-expression) $]]

      [unary-expression-not-plus-minus
        [(cast-expression) $]
        [((apply postfix-expression primary-dont-care)) $]
        [("~" wn unary-expression) $]
        [("!" wn unary-expression) $]]

      [(postfix-expression ender)
       [((apply primary primary-dont-care) (apply postfix-rest ender)) $]
       [(expression-name (apply postfix-rest ender)) $]
       #;
       [(post-increment-expression) $]
       #;
       [(post-decrement-expression) $]]

      [(postfix-rest ender)
       [((apply ender) (not wn post++/post--)) $]
       [(wn post++ (apply postfix-rest ender)) $]
       [(wn post-- (apply postfix-rest ender)) $]]

      [postfix-dont-care [(()) 'dont-care]]

      [post-increment-expression [((apply postfix-expression post++)) $]]
      [post-decrement-expression [((apply postfix-expression post--)) $]]

      [post++/post-- [(post++) $]
                     [(post--) $]]

      [post++ [("++") $]]
      [post-- [("--") $]]

      [cast-expression [("(" wn primitive-type (? dims) wn ")" wn unary-expression) $]
                       [("(" wn reference-type wn ")" wn unary-expression-not-plus-minus) $]]

      [unary-expression [(pre-increment-expression) $]
                        [(pre-decrement-expression) $]
                        [("+" (not "+") unary-expression) $]
                        [("-" (not "-") unary-expression) $]
                        [(unary-expression-not-plus-minus) $]]

      [dims [("[" wn "]" (? wn dims)) $]]


      [expression-name [(ambiguous-name) $]]

      [local-variable-declaration [((* variable-modifier wn) wn type wn variable-declarator (* wn "," wn variable-declarator)) $]]
      [variable-modifier [(final) $]
                         [(annotation) $]]
      [variable-declarator [(variable-declarator-id wn "=" wn variable-initializer) $]
                           [(variable-declarator-id) $]]

      [variable-declarator-id [(identifier wn (? dims)) $]]
      [variable-initializer [(expression) $]
                            [(array-initializer) $]]

      [constructor-declaration [((* constructor-modifier wn) wn 
                                 constructor-declarator wn
                                 (? throws-clause) wn
                                 constructor-body) $]]

      [identifier-char [(a-zA-Z0-9_) $]
                       [("$") $]]
      [public [("public" (not identifier-char)) $]]
      [protected [("protected" (not identifier-char)) $]]
      [private [("private" (not identifier-char)) $]]
      [interface [("interface" (not identifier-char)) $]]
      [class [("class" (not identifier-char)) $]]
      [static [("static" (not identifier-char)) $]]
      [if [("if" (not identifier-char)) $]]
      [while [("while" (not identifier-char)) $]]
      [for [("for" (not identifier-char)) $]]
      [else [("else" (not identifier-char)) $]]
      [abstract [("abstract" (not identifier-char)) $]]
      [final [("final" (not identifier-char)) $]]
      [native [("native" (not identifier-char)) $]]
      [new [("new" (not identifier-char)) $]]
      [synchronized [("synchronized" (not identifier-char)) $]]
      [transient* [("transient" (not identifier-char)) $]]
      [implements [("implements" (not identifier-char)) $]]
      [volatile [("volatile" (not identifier-char)) $]]
      [strictfp [("strictfp" (not identifier-char)) $]]
      [throws [("throws" (not identifier-char)) $]]
      [throw [("throw" (not identifier-char)) $]]
      [try [("try" (not identifier-char)) $]]
      [default [("default" (not identifier-char)) $]]
      [super [("super" (not identifier-char)) $]]
      [enum [("enum" (not identifier-char)) $]]
      [do [("do" (not identifier-char)) $]]
      [return [("return" (not identifier-char)) $]]
      [continue [("continue" (not identifier-char)) $]]
      [assert [("assert" (not identifier-char)) $]]
      [case [("case" (not identifier-char)) $]]
      [switch [("switch" (not identifier-char)) $]]
      [this [("this" (not identifier-char)) $]]
      [null [("null" (not identifier-char)) $]]
      [break [("break" (not identifier-char)) $]]
      [package [("package" (not identifier-char)) $]]
      [extends [("extends" (not identifier-char)) $]]
      [catch [("catch" (not identifier-char)) $]]
      [finally [("finally" (not identifier-char)) $]]
      [void [("void" (not identifier-char)) $]]
      [int [("int" (not identifier-char)) $]]
      [float [("float" (not identifier-char)) $]]
      [double [("double" (not identifier-char)) $]]
      [boolean [("boolean" (not identifier-char)) $]]
      [char [("char" (not identifier-char)) $]]
      [byte [("byte" (not identifier-char)) $]]
      [short [("short" (not identifier-char)) $]]
      [long [("long" (not identifier-char)) $]]

      [reserved [(public) $]
                [(protected) $]
                [(private) $]
                [(interface) $]
                [(static) $]
                [(abstract) $]
                [(finally) $]
                [(final) $]
                [(native) $]
                [(new) $]
                [(break) $]
                [(if) $]
                [(do) $]
                [(while) $]
                [(for) $]
                [(package) $]
                [(this) $]
                [(null) $]
                [(else) $]
                [(super) $]
                [(extends) $]
                [(enum) $]
                [(implements) $]
                [(synchronized) $]
                [(case) $]
                [(default) $]
                [(transient*) $]
                [(throws) $]
                [(throw) $]
                [(assert) $]
                [(try) $]
                [(switch) $]
                [(volatile) $]
                [(strictfp) $]
                [(class) $]
                [(void) $]
                [(int) $]
                [(float) $]
                [(double) $]
                [(boolean) $]
                [(char) $]
                [(byte) $]
                [(short) $]
                [(long) $]]

      [throws-clause [(throws wn exception-type (* wn "," wn exception-type)) $]]
      [exception-type [(class-type) $]
                      [(type-variable) $]]

      [class-type [(class-or-interface-type) $]]
      [type-variable [(identifier) $]]

      [constructor-modifier [(public) $]
                            [(protected) $]
                            [(private) $]]
      [constructor-declarator [((? type-parameters) wn simple-type-name wn "(" wn (? formal-parameter-list) wn ")") $]]
      [constructor-body [("{" wn (? explicit-constructor-invocation) wn (* block-statement wn) wn "}") $]]

      [explicit-constructor-invocation
        [((? non-wild-type-arguments) wn this wn "(" wn (? argument-list) wn ")") $]
        [((? non-wild-type-arguments) wn super wn "(" wn (? argument-list) wn ")") $]
        [((apply primary primary-dont-care) wn "." wn (? non-wild-type-arguments) wn super wn "(" wn (? argument-list) wn ")") $]]

      [interface-declaration [(normal-interface-declaration) $]
                             [(annotation-type-declaration) $]]

      [annotation [("@" wn type-name (? wn annotation-rest)) $]]
      [annotation-rest [(normal-annotation-rest) $]
                       [(single-element-annotation-rest) $]]
      [normal-annotation-rest [("(" wn (? element-value-pairs) wn ")") $]]
      [element-value-pairs [(element-value-pair (* wn "," element-value-pair)) $]]
      [element-value-pair [(identifier wn "=" wn element-value) $]]
      [element-value [(conditional-expression) $]
                     [(annotation) $]
                     [(element-value-array-initializer) $]]
      [element-value-array-initializer [("{" wn (? element-values) wn (? ",") wn "}") $]]
      [element-values [(element-value (* wn "," element-value)) $]]

      [single-element-annotation-rest [("(" wn element-value wn ")") $]]

      [annotation-type-declaration [((? interface-modifiers) wn "@" wn interface wn identifier wn annotation-type-body) $]]
      [annotation-type-body [("{" wn (? annotation-type-element-declarations) wn "}") $]]
      [annotation-type-element-declarations [((+ wn annotation-type-element-declaration)) $]]
      [annotation-type-element-declaration
        [((* wn abstract-method-modifier) wn type wn identifier wn "(" wn ")" (? wn default-value) wn ";") $]
        [(constant-declaration) $]
        [(class-declaration) $]
        [(interface-declaration) $]
        [(enum-declaration) $]
        [(annotation-type-declaration) $]
        [(";") $]]

      [default-value [(default wn element-value) $]]

      [normal-interface-declaration 
        [((? interface-modifiers) wn interface wn identifier (? wn type-parameters) (? wn extends wn interface-type-list) wn interface-body) 'interface]]

      [extends-interfaces [(implements wn interface-type-list) $]]
      [interface-type-list [(interface-type (* wn "," wn interface-type)) $]]
      [interface-type [(class-or-interface-type) $]]

      [interface-body [("{" wn (* interface-member-declaration wn) wn "}") $]]
      [interface-member-declaration [(constant-declaration) $]
                                    [(abstract-method-declaration) $]
                                    [(class-declaration) $]
                                    [(interface-declaration) $]
                                    [(";") $]]

      [constant-declaration [((* constant-modifier wn) type wn variable-declarator (* wn "," wn variable-declarator) wn ";") $]]
      [constant-modifier [(annotation) $]
                         [(public) $]
                         [(static) $]
                         [(final) $]]
      [abstract-method-declaration [((* abstract-method-modifier wn) (? wn type-parameters)
                                     result-type wn method-declarator wn (? throws-clause)
                                     wn ";")
                                    $]]

      [interface-modifiers [((+ wn interface-modifier)) $]]
      [interface-modifier [(annotation) $]
                          [(public) $]
                          [(protected) $]
                          [(private) $]
                          [(abstract) $]
                          [(static) $]
                          [(strictfp) $]]

      [abstract-method-modifier [(annotation) $]
                                [(public) $]
                                [(abstract) $]]

      #;
      [modifiers [((* wn modifier)) $]]
      #;
      [modifier [(public) $]
                [(protected) $]
                [(private) $]
                [(static) $]
                [(abstract) $]
                [(final) $]
                [(native) $]
                [(synchronized) $]
                [(transient*) $]
                [(volatile) $]
                [(strictfp) $]]

      [import [("import" wn import-rest) 'import]]
      [import-rest [(package-or-type-name wn type-import-declaration-rest) $]
                   [(static wn type-name wn static-import-declaration-rest) $]]
      [package-or-type-name [(identifier (* "." identifier)) $]]
      [type-import-declaration-rest [(";") $]
                                    [(".*" wn ";") $]]
      [static-import-declaration-rest [(";") $]
                                      [(".*" wn ";") $]]

      [sw [((or #\space #\tab)) (void)]]
      [newline [((or #\newline #\return)) (void)]]
      [comment [("/*" (* (except (or "*/" eof))) "*/") $]
               [("//" (* (except (or newline eof))) newline) $]]
      [swn [((or sw newline comment)) (void)]]
      [w [((* sw)) (void)]]
      [wn [((* swn)) $]])))

(define (java-parse obj)
  (parse (make-parser) obj))
