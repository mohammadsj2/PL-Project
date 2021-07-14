#lang racket
(provide (all-defined-out))

(require (file "parser.rkt"))
(require (lib "eopl.ss" "eopl"))
(require (file "file_reader.rkt"))

; Value of Expressions

(define (empty-store) '())

(define the-store (empty-store))

(define (get-store) the-store)

(define (initialize-store!) (set! the-store (empty-store)))

(define (refrence? v) (integer? v))

(define (newref v) (let ([len (length the-store)])
                     (set! the-store (append the-store (list v)))
                     (debug (list 'newref len v))
                     len))

(define (deref r) (list-ref the-store r))

(define (debug l)
  ;(displayln (cons 'debug: l)))
  #f)

(define (setref r v)
  (begin
    (debug (list "setref" r v))
    (set! the-store
          (for/list ([x the-store][i (range (length the-store))])
            (if (equal? i r) v x)))))

(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (list-val (list list?))
  (proc-val (proc proc?))
  (non-val)
  )

(define (expval->bool e)
  (cases expval e
    (bool-val (b) b)
    (else 'error-in-expval->bool)))

(define (expval->num e)
  (cases expval e
    (num-val (n) n)
    (else 'error-in-expval->num)))

(define (expval->list e)
  (cases expval e
    (list-val (l) l)
    (else 'error-in-expval->list)))

(define (expval->proc e)
  (cases expval e
    (proc-val (p) p)
    (else 'error-in-expval->proc)))

(define-datatype proc proc?
  (procedure
   (params1 (list-of string?))
   (body statements?)
   (env environment?)))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var string?)
   (val refrence?)
   (env environment?))
  (extend-env-proc
   (proc-name string?)
   (params1 (list-of string?))
   (body statements?)
   (saved-env environment?)))

(define (apply-env env search-var)
  (cases environment env
    (empty-env () 'not-found)
    (extend-env (var val env2) (if (equal? var search-var) val (apply-env env2 search-var)))
    (extend-env-proc (proc-name params1 body saved-env)
                     (if (equal? proc-name search-var)
                         (procedure
                          params1
                          body
                          saved-env)
                         (apply-env saved-env search-var)))))

(define (value-of-exp exp env)
  (cases expression exp
    (an-expression (disjunction1) (value-of-disj disjunction1 env))))

(define (value-of-disj disj env)
  (cases disjunction disj
    (a-conjunction (conjunction1) (value-of-conj conjunction1 env))
    (multiple-conjunctions (disj conj) (let ([val1 (expval->bool (value-of-disj disj env))]
                                             [val2 (expval->bool (value-of-conj conj env))])
                                         (bool-val (or val1 val2))
                                         ))))

(define (value-of-conj conj env)
  (cases conjunction conj
    (an-inversion (inv) (value-of-inv inv env))
    (multiple-inversions (conj inv) (let ([val1 (expval->bool (value-of-conj conj env))]   
                                          [val2 (expval->bool (value-of-inv inv env))])
                                      (bool-val (and val1 val2))
                                      ))))

(define (value-of-inv inv env)
  (cases inversion inv
    (a-comparison (comp) (value-of-comp comp env))
    (not-inversion (inv) (let ([val (expval->bool (value-of-inv inv env))])
                           (bool-val (not val))))))

(define (value-of-comp comp env)
  (cases comparison comp
    (a-sum (sm) (value-of-sum sm env))
    (a-sum-compare (sm comp-op-sm-ps)
                   (letrec ([val1 (expval->num (value-of-sum sm env))]
                            [l (get-comp-op-sm-p-list comp-op-sm-ps env)])
                     (value-of-comp-op-sm-ps (cons val1 l) env))
                   )))

(define (get-comp-op-sm-p-list c env)
  (cases compare-op-sum-pairs c
    (a-compare-op-sum-pair (c) (list c))
    (multiple-compare-op-sum-pair (c1s c2) (append (get-comp-op-sm-p-list c1s env) (list c2)))))

(define (value-of-sum-of-comp-op comp env)
  (cases compare-op-sum-pair comp
    (an-eq-sum (eq) (cases eq-sum eq
                      (new-eq-sum (s) (value-of-sum s env))))
    (a-lt-sum (lt) (cases lt-sum lt
                     (new-lt-sum (s) (value-of-sum s env))))
    (a-gt-sum (gt) (cases gt-sum gt
                     (new-gt-sum (s) (value-of-sum s env))))))

(define (value-of-comp-op-sm-ps comp-op-list env)
  (if (equal? (length comp-op-list) 1) (bool-val #t)
      (let ([val1 (car comp-op-list)]
            [val2 (expval->num (value-of-sum-of-comp-op (cadr comp-op-list) env))]
            [comp (cadr comp-op-list)]
            [l2 (cddr comp-op-list)])
        (let ([lp (cons val2 l2)])
          (cases compare-op-sum-pair comp
            (an-eq-sum (eq) (if (equal? val1 val2)
                                (value-of-comp-op-sm-ps lp env)
                                (bool-val #f)))
            (a-lt-sum (lt) (if (< val1 val2)
                               (value-of-comp-op-sm-ps lp env)
                               (bool-val #f)))
            (a-gt-sum (gt) (if (> val1 val2)
                               (value-of-comp-op-sm-ps lp env)
                               (bool-val #f))))))))
        
  
(define (value-of-sum sm env)
  (cases sum sm
    (plus-sum (s t) (let ([eval1 (value-of-sum s env)]
                          [eval2 (value-of-term t env)])
                      (cases expval eval1
                        (num-val (val1) (num-val (+ val1 (expval->num eval2))))
                        (bool-val (val1) (bool-val (or val1 (expval->bool eval2))))
                        (list-val (val1) (list-val (append val1 (expval->list eval2))))
                        (else 'error))))
    (minus-sum (s t) (let ([val1 (expval->num (value-of-sum s env))]
                           [val2 (expval->num (value-of-term t env))])
                       (num-val (- val1 val2))))
    (a-term (t) (value-of-term t env))))

(define (value-of-term t env)
  (cases term t
    (mul-term (t f) (let ([eval1 (value-of-term t env)])
                      ;[eval2 (value-of-factor f env)])
                      (cases expval eval1
                        (num-val (val1) (if (equal? val1 0)
                                            (num-val 0)
                                            (num-val (* val1 (expval->num (value-of-factor f env))))))
                        (bool-val (val1) (if (not val1)
                                             (bool-val #f)
                                             (bool-val (and val1 (expval->bool (value-of-factor f env))))))
                        (else 'error))))
    (div-term (t f) (let ([val1 (expval->num (value-of-term t env))]
                          [val2 (expval->num (value-of-factor f env))])
                      (num-val (/ val1 val2))))
    (a-factor (f) (value-of-factor f env))))

(define (value-of-factor f env)
  (cases factor f
    (plus-factor (f) (value-of-factor f env))
    (minus-factor (f) (let ([val (expval->num (value-of-factor f env))])
                        (num-val (- val))))
    (a-power-factor (p) (value-of-power p env))))

(define (value-of-power p env)
  (cases power p
    (a-power (a f) (let ([val1 (expval->num (value-of-atom a env))]
                         [val2 (expval->num (value-of-factor f env))])
                     (num-val (expt val1 val2))))
    (a-primary (p) (value-of-primary p env))))

(define (value-of-primary p env)
  (cases primary p
    (an-atom (a) (value-of-atom a env))
    (an-array-ref (p2 exp) (let ([l (expval->list (value-of-primary p2 env))]
                                 [ref (expval->num (value-of-exp exp env))])
                             (list-ref l ref)))
    (a-no-param-function-call (primary1) (let ([proc (expval->proc (value-of-primary primary1 env))])
                                           (apply-procedure proc `())))
    (with-param-function-call (primary1 args1)(if (is-print primary1)
                                                  (begin (print-values (map (lambda (exp) (value-of-exp exp env)) (arguments->list args1)) ) (display "\n") (num-val -13))
                                                  (let ([proc (expval->proc (value-of-primary primary1 env))])
                                                    (apply-procedure proc (map (lambda (exp) (value-of-exp exp env)) (arguments->list args1))))
                                                  )
      )
                                               
    )
  )

(define (print-value eval)
  (cases expval eval
  (num-val (num) (display num))
  (bool-val (bool) (display bool))
  (list-val (list) (begin (display "[") (print-values list) (display "]")))
  (proc-val (proc) (display proc))
  (non-val (display "None"))
  )
  )

(define (print-values expvals )
  (cond
    ((null? expvals) (num-val -13))
    (else
     (begin
       (print-value (car expvals))
       (if (null? (cdr expvals)) (display "") (display " "))
       (print-values (cdr expvals)))
     )
    )
  )

(define (is-print p)
  (cases primary p
     (an-atom (atom1)
              (cases atom atom1
                (an-id (id)
                       (if (equal? id "print") #t #f))
                (else
                 #f
                 )))
     (else #f)))

(define (arguments->list args1)
  (cases arguments args1
    (single-expression (exp) (list exp))
    (multiple-expressions (args2 exp) (append (arguments->list args2) (list exp)))))


(define (apply-procedure proc1 arg-vals)
  (cases proc proc1
    (procedure (params1 body saved-env)
               (result-return-val
                (run-stmts body (extend-env-with-arguments params1 arg-vals saved-env))))))

(define (extend-env-with-arguments arg-names arg-vals env)
  (let extend-env-with-argument ([name-vals (map list (take arg-names (length arg-vals)) arg-vals)]
                                 [saved-env env])
    (cond
      ((null? name-vals) saved-env)
      (else (let ([name (caar name-vals)]
                  [val (cadar name-vals)])
              (extend-env name (newref val) (extend-env-with-argument (cdr name-vals) saved-env)))))))

(define (value-of-atom a env)
  (cases atom a
    (an-id (id) (deref (apply-env env id)))
    (true-value () (bool-val #t))
    (false-value () (bool-val #f))
    (none-value () (non-val))
    (a-number (n) (num-val n))
    (a-list (pl) (value-of-plist pl env))))

(define (import-list-to-store l)
  (for/list ([x l]) (newref x)))

(define (value-of-plist pl env)
  (cases plist pl
    (empty-list () (list-val '()))
    (non-empty-list (exps) (list-val (value-of-exps exps env)))));(import-list-to-store (value-of-exps exps env))))))

(define (value-of-exps exps env) ;khoroojish ye list mamoolie ke javabe exp ha tooshe
  (cases expressions exps
    (only-expression (exp) (list (value-of-exp exp env)))
    (multiple-expression (exps exp) (let ([l (value-of-exps exps env)]
                                          [val (value-of-exp exp env)])
                                      (append l (list val))))))
    
    


; Running of statements


(struct result (new-env break-flag continue-flag return-flag return-val) #:transparent)

(define (run-program p env)
  (cases program p
    (a-program (stmts) (run-stmts stmts env))))

(define (is-stop? out)
  (or (result-break-flag out) (result-continue-flag out) (result-return-flag out)))

(define (run-stmts stmts env)
  (cases statements stmts
    (a-statement (stmt) (run-stmt stmt env))
    (multiple-statements (stmts2 stmt) (let ([out (run-stmts stmts2 env)])
                                         (if (is-stop? out)
                                             out
                                             (run-stmt stmt (result-new-env out)))))))

(define (run-stmt stmt env)
  (cases statement stmt
    (a-compound-statement (compound-stmt) (run-compound-stmt compound-stmt env))
    (a-single-statement (simple-stmt) (run-simple-stmt simple-stmt env))))

(define (run-compound-stmt c env)
  (cases compound-stmt c
    (a-function-def (f) (def-func f env))
    (a-compound-if-stmt (if-st) (run-if if-st env))
    (a-compound-for-stmt (for-st) (run-for for-st env))))

(define (def-func func-def-st env)
  (cases function-def func-def-st
    (function-def-with-params (id params1 stmts)
                              (create-procedure id (params->list params1) stmts env))
    (function-def-no-params (id stmts)
                            (create-procedure id `() stmts env))))

(define (params->list func-params)
  (cases params func-params
    (single-param (param1) (list param1))
    (multiple-params (params1 param1) (append (params->list params1) (list param1)))))

(define (create-procedure id params1 stmts env)
  (result (extend-env-with-procedure-new id params1 stmts env) #f #f #f (non-val)))

(define (extend-env-with-procedure id params1 stmts env)
  (extend-env-proc
   id
   (map
    (lambda (p)
      (cases param-with-default p
        (a-param (id exp) id)))
    params1)
   stmts
   (let extend-env-with-param-with-default ([params2 params1]
                                            [saved-env env])
     (cond
       ((null? params2) saved-env)
       (else (cases param-with-default (car params2)
               (a-param (id exp)
                        (let ([val (value-of-exp exp env)])
                          (extend-env id (newref val) (extend-env-with-param-with-default (cdr params2) saved-env))))))))))

(define (extend-env-with-procedure-new id params1 stmts env)
  (let ([func-var-ref (newref (non-val))])
        
    (let ([new-env (extend-env id func-var-ref env)])
      (let ([proc (procedure
                   (map
                    (lambda (p)
                      (cases param-with-default p
                        (a-param (id exp) id)))
                    params1)
                   stmts
                   (let extend-env-with-param-with-default ([params2 params1]
                                                            [saved-env new-env])
                     (cond
                       ((null? params2) saved-env)
                       (else (cases param-with-default (car params2)
                               (a-param (id exp)
                                        (let ([val (value-of-exp exp env)])
                                          (extend-env id (newref val) (extend-env-with-param-with-default (cdr params2) saved-env)))))))))])
        (begin
          (setref func-var-ref (proc-val proc))
          new-env)))))

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
  (if (null? l) (result env #f #f #f (non-val))
      (letrec ([env2 (set-var id (car l) env)]
               [lp (cdr l)]
               [out (run-stmts stmts env2)]
               [env3 (result-new-env out)])
        (cond
          [(result-return-flag out) (result env3 #f #f #t (result-return-val out))]
          [(result-break-flag out) out]
          [(null? lp) (result env3 #f #f #f (non-val))]
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
    (a-global-stmt (g) (result env #f #f #f (non-val))) ;TODO function phase
    (pass-stmt () (result env #f #f #f (non-val)))
    (break-stmt () (result env #t #f #f (non-val)))
    (continue-stmt () (result env #f #t #f (non-val)))
    (print-stmt
     (args)
     (begin
       (print-values (map (lambda (exp) (value-of-exp exp env)) (arguments->list args)))
       (display "\n")
       (result env #f #f #f (non-val))
       )
     )
    )
  )


(define (run-return r env)
  (cases return-stmt r
    (a-return () (result env #f #f #t (non-val)))
    (a-return-exp (exp) (result env #f #f #t (value-of-exp exp env)))))
  
(define (run-assignment-stmt a env)
  (cases assignment a
    (an-assignment (id exp) (let ([val (value-of-exp exp env)])
                              (result (set-var id val env) #f #f #f (non-val))))))

;Test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
;If you want to run code directly
(define my-lexer (lex-this simple-math-lexer (open-input-string "def f():print(10);; a=10; print(f()); print(a, a, [a, a]); print([True]);")))

;(define my-lexer (lex-this simple-math-lexer (open-input-string (read-instructions-from-file "program.txt"))))
;(let ((parser-res (simple-math-parser my-lexer)))
;  parser-res
;  (run-program parser-res (empty-env)))
