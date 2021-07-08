#lang racket
(provide (all-defined-out))
(provide simple-math-lexer)
(provide simple-math-parser)
(require (lib "eopl.ss" "eopl"))
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define-datatype program program?
  (a-program (stmts statements?)))

(define-datatype statements statements?
  (a-statement (stmt statement?))
  (multiple-statements (stmts statements?) (stmt statement?)))

(define-datatype statement statement?
  (a-compound-statement (compound-stmt compound-stmt?))
  (a-single-statement (simple-stmt simple-stmt?)))

(define-datatype simple-stmt simple-stmt?
  (an-assignment-stmt (assignment1 assignment?))
  (a-return-stmt (return-stmt1 return-stmt?))
  (a-global-stmt (global-stmt1 global-stmt?))
  (pass-stmt)
  (break-stmt)
  (continue-stmt))

(define-datatype compound-stmt compound-stmt?
  (a-function-def (function-def1 function-def?))
  (a-compound-if-stmt (if-stmt1 if-stmt?))
  (a-compound-for-stmt (for-stmt1 for-stmt?)))

(define-datatype assignment assignment?
  (an-assignment (id string?) (exp expression?)))

(define-datatype return-stmt return-stmt?
  (a-return)
  (a-return-exp (exp expression?)))

(define-datatype global-stmt global-stmt?
  (a-global (id string?)))

(define-datatype function-def function-def?
  (function-def-with-params
   (id string?) (params1 params?) (stmts statements?))
  (function-def-no-params (id string?) (stmts statements?))
  )

(define-datatype params params?
  (single-param (param1 param-with-default?))
  (multiple-params (params1 params?) (param1 param-with-default?)))

(define-datatype param-with-default param-with-default?
  (a-param (id string?) (exp expression?)))

(define-datatype if-stmt if-stmt?
  (an-if-stmt
    (exp expression?)
    (stmts statements?)
    (else-block1 else-block?)))

(define-datatype else-block else-block?
  (an-else-block
   (stmts statements?)))

(define-datatype for-stmt for-stmt?
  (a-for-stmt (id string?) (exp expression?) (stmts statements?)))

(define-datatype expression expression?
  (an-expression (disjunction1 disjunction?)))

(define-datatype disjunction disjunction?
  (a-conjunction (conjunction1 conjunction?))
  (multiple-conjunctions (disjunction1 disjunction?) (cojunction1 conjunction?)))


(define-datatype conjunction conjunction?
  (an-inversion (inversion1 inversion?))
  (multiple-inversions (conjunction1 conjunction?)(inversion1 inversion?)))

(define-datatype inversion inversion?
  (a-comparison (comparison1 comparison?))
  (not-inversion (inversion1 inversion?)))

(define-datatype comparison comparison?
  (a-sum (sum1 sum?))
  (a-sum-compare (sum1 sum?) (compare-op-sum-pairs1 compare-op-sum-pairs?)))

(define-datatype compare-op-sum-pairs compare-op-sum-pairs?
  (a-compare-op-sum-pair (compare-op-sum-pair1 compare-op-sum-pair?))
  (multiple-compare-op-sum-pair
   (compare-op-sum-pairs1 compare-op-sum-pairs?)
   (compare-op-sum-pair1 compare-op-sum-pair?)))

(define-datatype compare-op-sum-pair compare-op-sum-pair?
  (an-eq-sum (eq-sum1 eq-sum?))
  (a-lt-sum (lt-sum1 lt-sum?))
  (a-gt-sum (gt-sum1 gt-sum?)))

(define-datatype eq-sum eq-sum?
  (new-eq-sum (sum1 sum?)))

(define-datatype lt-sum lt-sum?
  (new-lt-sum (sum1 sum?)))

(define-datatype gt-sum gt-sum?
  (new-gt-sum (sum1 sum?)))

(define-datatype sum sum?
  (plus-sum (sum1 sum?) (term1 term?))
  (minus-sum (sum1 sum?) (term1 term?))
  (a-term (term1 term?)))

(define-datatype term term?
  (mul-term (term1 term?) (factor1 factor?))
  (div-term (term1 term?) (factor1 factor?))
  (a-factor (factor1 factor?)))

(define-datatype factor factor?
  (plus-factor (factor1 factor?))
  (minus-factor (factor1 factor?))
  (a-power-factor (power1 power?)))

(define-datatype power power?
  (a-power (atom1 atom?) (factor1 factor?))
  (a-primary (primary1 primary?)))

(define-datatype primary primary?
  (an-atom (atom1 atom?))
  (an-array-ref (primary1 primary?) (exp expression?))
  (a-no-param-function-call (primary1 primary?))
  (with-param-function-call (primary1 primary?) (agrs1 arguments?)))

(define-datatype arguments arguments?
  (single-expression (exp expression?))
  (multiple-expressions (args1 arguments?) (exp expression?)))

(define-datatype atom atom?
  (an-id (id string?))
  (true-value)
  (false-value)
  (none-value)
  (a-number (number number?))
  (a-list (list1 plist?)))

(define-datatype plist plist?
  (empty-list)
  (non-empty-list (exps expressions?)))

(define-datatype expressions expressions?
  (only-expression (exp expression?))
  (multiple-expression (exps expressions?) (exp expression?)))




;lexer
(define-tokens a (NUMBER ID))
(define-empty-tokens b (EOF SEMI-COLON PLUS MINUS MUL DIVIDE COLON COMMA OPEN-PARENTHESIS CLOSE-PARENTHESIS
                            OPEN-BRACKET CLOSE-BRACKET ASSIGN LESS GREATER POWER EQUALITY TRUE FALSE
                            NONE NOT AND OR IN FOR ELSE IF DEF GLOBAL RETURN CONTINUE BREAK PASS))


(define simple-math-lexer
           (lexer
            (";" (token-SEMI-COLON))
            ("+" (token-PLUS))
            ("-" (token-MINUS))
            ("*" (token-MUL))
            ("/" (token-DIVIDE))
            (":" (token-COLON))
            ("," (token-COMMA))
            ("(" (token-OPEN-PARENTHESIS))
            (")" (token-CLOSE-PARENTHESIS))
            ("[" (token-OPEN-BRACKET))
            ("]" (token-CLOSE-BRACKET))
            ("=" (token-ASSIGN))
            ("<" (token-LESS))
            (">" (token-GREATER))
            ("**" (token-POWER))
            ("==" (token-EQUALITY))
            ("True" (token-TRUE))
            ("False" (token-FALSE))
            ("None" (token-NONE))
            ("not" (token-NOT))
            ("and" (token-AND))
            ("or" (token-OR))
            ("in" (token-IN))
            ("for" (token-FOR))
            ("else" (token-ELSE))
            ("if" (token-IF))
            ("def" (token-DEF))
            ("global" (token-GLOBAL))
            ("return" (token-RETURN))
            ("continue" (token-CONTINUE))
            ("break" (token-BREAK))
            ("pass" (token-PASS))
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUMBER (string->number lexeme)))
            ((:: (:+ (:or (char-range #\A #\Z) (char-range #\a #\z))) (:* (:or (char-range #\A #\Z) (char-range #\a #\z) (char-range #\0 #\9))))
             (token-ID lexeme)) 
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))
            )
  )

(define simple-math-parser
           (parser
            (start Program)
            (end EOF)
            (error void)
            (tokens a b)
            (grammar
             (Program
              ((Statements) (a-program $1))
              )
             (Statements
              ((Statement SEMI-COLON) (a-statement $1))
              ((Statements Statement SEMI-COLON) (multiple-statements $1 $2))
              )
             (Statement
              ((Compound-stmt) (a-compound-statement $1))
              ((Simple-stmt) (a-single-statement $1))
              )
             (Simple-stmt
              ((Assignment) (an-assignment-stmt $1))
              ((Return-stmt) (a-return-stmt $1))
              ((Global-stmt) (a-global-stmt $1))
              ((PASS) (pass-stmt))
              ((BREAK) (break-stmt))
              ((CONTINUE) (continue-stmt))
              )
             (Compound-stmt
              ((Function-def) (a-function-def $1))
              ((If-stmt) (a-compound-if-stmt $1))
              ((For-stmt) (a-compound-for-stmt $1))
              )
             (Assignment
              ((ID ASSIGN Expression) (an-assignment $1 $3))
              )
             (Return-stmt
              ((RETURN) (a-return))
              ((RETURN Expression) (a-return-exp $2))
              )
             (Global-stmt
              ((GLOBAL ID) (a-global $2))
              )
             (Function-def
              ((DEF ID OPEN-PARENTHESIS Params CLOSE-PARENTHESIS COLON Statements) (function-def-with-params $2 $4 $7))
              ((DEF ID OPEN-PARENTHESIS CLOSE-PARENTHESIS COLON Statements) (function-def-no-params $2 $6))  
              )
             (Params
              ((Param-with-default) (single-param $1))
              ((Params COMMA Param-with-default) (multiple-params $1 $3))
              )
             (Param-with-default
              ((ID ASSIGN Expression) (a-param $1 $3))
              )
             (If-stmt
              ((IF Expression COLON Statements Else-block)
               (an-if-stmt $2 $4 $5))
              )
             (Else-block
              ((ELSE COLON Statements) (an-else-block $3)) 
              )
             (For-stmt
              ((FOR ID IN Expression COLON Statements) (a-for-stmt $2 $4 $6))
              )
             (Expression
              ((Disjunction) (an-expression $1))
              )
             (Disjunction
              ((Conjunction) (a-conjunction $1))
              ((Disjunction OR Conjunction) (multiple-conjunctions $1 $3))
              )
             (Conjunction
              ((Inversion) (an-inversion $1))
              ((Conjunction AND Inversion) (multiple-inversions $1 $3))
              )
             (Inversion
              ((NOT Inversion) (not-inversion $2))
              ((Comparison) (a-comparison $1)) 
              )
             (Comparison
              ((Sum Compare-op-Sum-pairs) (a-sum-compare $1 $2))
              ((Sum) (a-sum $1))
              )
             (Compare-op-Sum-pairs
              ((Compare-op-Sum-pair) (a-compare-op-sum-pair $1))
              ((Compare-op-Sum-pairs Compare-op-Sum-pair) (multiple-compare-op-sum-pair $1 $2))
              )
             (Compare-op-Sum-pair
              ((Eq-Sum) (an-eq-sum $1))
              ((Lt-Sum) (a-lt-sum $1))
              ((Gt-Sum) (a-gt-sum $1))
              )
             (Eq-Sum
              ((EQUALITY Sum) (new-eq-sum $2))
              )
             (Lt-Sum
              ((LESS Sum) (new-lt-sum $2))
              )
             (Gt-Sum
              ((GREATER Sum) (new-gt-sum $2))
              )
             (Sum
              ((Sum PLUS Term) (plus-sum $1 $3))
              ((Sum MINUS Term) (minus-sum $1 $3))
              ((Term) (a-term $1))
              )
             (Term
              ((Term MUL Factor) (mul-term $1 $3))
              ((Term DIVIDE Factor) (div-term $1 $3))
              ((Factor) (a-factor $1))
              )
             (Factor
              ((PLUS Factor) (plus-factor $2))
              ((MINUS Factor) (minus-factor $2))
              ((Power) (a-power-factor $1))
              )
             (Power
              ((Atom POWER Factor) (a-power $1 $3))
              ((Primary) (a-primary $1))
              )
             (Primary
              ((Atom) (an-atom $1))
              ((Primary OPEN-BRACKET Expression CLOSE-BRACKET) (an-array-ref $1 $3))
              ((Primary OPEN-PARENTHESIS CLOSE-PARENTHESIS) (a-no-param-function-call $1))
              ((Primary OPEN-PARENTHESIS Arguments CLOSE-PARENTHESIS) (with-param-function-call $1 $3))
              )
             (Arguments
              ((Expression) (single-expression $1))
              ((Arguments COMMA Expression) (multiple-expressions $1 $3))
              )
             (Atom
              ((ID) (an-id $1))
              ((TRUE) (true-value))
              ((FALSE) (false-value))
              ((NONE) (none-value))
              ((NUMBER) (a-number $1))
              ((List) (a-list $1))
              )
             (List
              ((OPEN-BRACKET Expressions CLOSE-BRACKET) (non-empty-list $2))
              ((OPEN-BRACKET CLOSE-BRACKET) (empty-list))
              )
             (Expressions
              ((Expressions COMMA Expression) (multiple-expression $1 $3))
              ((Expression) (only-expression $1))
              )
            )))

;test
;(define lex-this (lambda (lexer input) (lambda () (lexer input))))
;(define my-lexer (lex-this simple-math-lexer (open-input-string "a=1+2;")))
;(let ((parser-res (simple-math-parser my-lexer))) parser-res)

