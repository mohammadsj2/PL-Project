#lang racket
(require (file "parser.rkt"))
(require (file "valueofexp.rkt"))
(require (lib "eopl.ss" "eopl"))

(define (run-program p env)
  (cases program p
      (a-program (stmts) (run-stmts stmts env))))

(define (run-stmts stmts env);khoroojish env hast
  (cases statements stmts
    (a-statement (stmt) (run-stmt stmt env))
    (multiple-statements (stmts2 stmt) (let ([env (run-stmts stmts2 env)])
                                         (run-stmt stmt env)))))

(define (run-stmt stmt env)
  (cases statement stmt
    (a-compound-statement (compound-stmt) (run-compound-stmt compound-stmt env))
    (a-single-statement (simple-stmt) (run-simple-stmt simple-stmt env))))

(define (run-compound-stmt c env)
  0
  )

(define (run-simple-stmt stmt env)
  (cases simple-stmt stmt
    (an-assignment-stmt (a) (run-assignment-stmt a env))
    (a-return-stmt (r) 0)
    (a-global-stmt (g) 0)
    (pass-stmt () 0)
    (break-stmt () 0)
    (continue-stmt () 0)))

(define (run-assignment-stmt a env)
  (cases assignment a
    (an-assignment (id exp) (let ([ref (apply-env env id)]
                                  [val (value-of-exp exp env)])
                              (if (equal? ref 'not-found)
                                  (extend-env id (newref val) env)
                                  (begin
                                    (setref ref val)
                                    env))))))

;Test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "a=1-2*3;b=a**2;c=a<b or a>b;a=56;d=not c;a=[a,b,c,d];h=a[2];")))
(let ((parser-res (simple-math-parser my-lexer)))
  parser-res
  (run-program parser-res (empty-env)))
