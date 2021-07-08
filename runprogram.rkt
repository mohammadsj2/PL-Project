#lang racket
(require (file "parser.rkt"))
(require (file "valueofexp.rkt"))
(require (lib "eopl.ss" "eopl"))

(define (run-program p env)
  (cases program p
      (a-program (stmts) (run-statements stmts p env))))

(define (run-stmts stmts env);khoroojish env hast
  (cases statements stmts
    (a-statement (stmt) (run-stmt stmt env))
    (multiple-statements (stmts2 stmt) (let ([env (run-stmts stmts2 env)])
                                         (run-stmt stmt env)))))

(define (run-stmt stmt env)
  (cases statement stmt
    (a-compound-statement (compound-stmt) (run-compound-stmt compound-stmt env))
    (a-single-statement (simple-stmt) (run-simple-stmt simple-stmt env))))

(define (run-simple-stmt stmt env)
  (cases simple-stmt stmt
    (an-assignment-stmt (a) 0)
    (a-return-stmt (r) 0)
    (a-global-stmt (g) 0)
    (pass-stmt () 0)
    (break-stmt () 0)
    (continue-stmt () 0)))