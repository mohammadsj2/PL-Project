#lang racket
(require (file "parser.rkt"))
(require (file "valueofexp.rkt"))
(require (lib "eopl.ss" "eopl"))

(define (run-program p env)
  (cases program p
      (a-program (stmts) (run-stmts stmts env))))

(define (is-stop? out)
  (or (cadr out) (caddr out) (cadddr out)))

(define (out->env out) (car out))

(define (run-stmts stmts env) ;output: (list new-env break-flag continue-flag return-flag return-val) 
  (cases statements stmts
    (a-statement (stmt) (run-stmt stmt env))
    (multiple-statements (stmts2 stmt) (let ([out (run-stmts stmts2 env)])
                                         (if (is-stop? out)
                                             out
                                             (run-stmt stmt (out->env out)))))))

(define (run-stmt stmt env)
  (cases statement stmt
    (a-compound-statement (compound-stmt) (run-compound-stmt compound-stmt env))
    (a-single-statement (simple-stmt) (run-simple-stmt simple-stmt env))))

(define (run-compound-stmt c env)
  (cases compound-stmt c
    (a-function-def (f) 0) ;TODO function phase
    (a-compound-if-stmt (if-st) (run-if if-st env))
    (a-compound-for-stmt (for-st) (run-for for-st env))))

(define (set-var var val env)
  (let ([ref (apply-env env var)])
    (if (equal? ref 'not-found)
        (extend-env var (newref val) env)
        (begin
          (setref ref val)
          env))))

(define (run-for for-st env)
  (cases for-stmt for-st
    (a-for-stmt (id exp stmts)
                (let ([l (expval->list (value-of-exp exp env))])
                  (run-for-helper id l stmts env)))))

(define (run-for-helper id l stmts env)
  (if (null? l) (list env #f #f #f (non-val))
      (letrec ([env2 (set-var id (car l) env)]
               [lp (cdr l)]
               [out (run-stmts stmts env2)]
               [env3 (car out)]
               [break-flag (cadr out)]
               [return-flag (cadddr out)]
               [return-val (car (cddddr out))])
        (cond
          [return-flag (list env3 #f #f #t return-val)]
          [break-flag out]
          [(null? lp) (list env3 #f #f #f (non-val))]
          [else (run-for-helper id lp stmts env3)]))))
;TODO for ro intori zadam ke moteghayyere tooye for biroonesham hast! in ghalate? kollan bayad scope bandi raayat beshe?
;TODO too tabe oke scope esh jodae chon too ghesmate value-of-exp e seda zadanesh vali inja na
            
(define (run-if if-st env)
  (cases if-stmt if-st
    (an-if-stmt (exp stmts else-blk)
                (let ([cond-bool (expval->bool (value-of-exp exp env))])
                  (if cond-bool
                      (run-stmts stmts env)
                      (cases else-block else-blk
                        (an-else-block (else-stmts) (run-stmts else-stmts env))))))))

(define (run-simple-stmt stmt env)
  (cases simple-stmt stmt
    (an-assignment-stmt (a) (run-assignment-stmt a env))
    (a-return-stmt (r) (run-return r env))
    (a-global-stmt (g) 0) ;TODO function phase
    (pass-stmt () (list env #f #f #f (non-val)))
    (break-stmt () (list env #t #f #f (non-val)))
    (continue-stmt () (list env #f #t #f (non-val)))))


(define (run-return r env)
  (cases return-stmt r
    (a-return () (list env #f #f #t (non-val)))
    (a-return-exp (exp) (list env #f #f #t (value-of-exp exp)))))
  
(define (run-assignment-stmt a env)
  (cases assignment a
    (an-assignment (id exp) (let ([val (value-of-exp exp env)])
                              (list (set-var id val env) #f #f #f (non-val))))))

;Test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "a=1-2*3;b=a**2;c=False * True;a=56;d=not c;a=[a,b,c,d]+[55,66];h=a+a; if False: dd = 4444; else: dd=5555;;for dd in h:continue;dddd=50;;")))
(let ((parser-res (simple-math-parser my-lexer)))
  parser-res
  (run-program parser-res (empty-env)))
