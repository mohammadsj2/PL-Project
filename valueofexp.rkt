#lang racket
(provide (all-defined-out))
(require (file "parser.rkt"))
(require (file "runprogram.rkt"))
(require (lib "eopl.ss" "eopl"))


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
  (displayln (cons 'debug: l)))

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
                             (deref (list-ref l ref))))
    (a-no-param-function-call (primary1) (let ([proc (expval->proc (value-of-primary primary1 env))])
                                           (apply-procedure proc `())))
    (with-param-function-call (primary1 args1)(let ([proc (expval->proc (value-of-primary primary1 env))])
                                                (apply-procedure proc (map (arguments->list args1) value-of-exp))))))


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
                  [val (cdar name-vals)])
              (extend-env name val (extend-env-with-argument (cdr name-vals) saved-env)))))))

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
    
    
