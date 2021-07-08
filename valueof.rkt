#lang racket
(require (file "parser.rkt"))
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "a=1+2;")))
(let ((parser-res (simple-math-parser my-lexer))) parser-res)

(define (empty-store) '())

(define the-store (empty-store))

(define (get-store) the-store)

(define (initialize-store!) (set! the-store (empty-store)))

(define (refrence? v) (integer? v))

(define (newref v) (let ([len (length the-store)])
                     (set! the-store (append the-store (list v)))
                     len))

(define (deref r) (list-ref the-store r))

(define (setref r v)
  (set! the-store
        (for/list ([x the-store][i (range (length the-store))])
          (if (equal? i r) v x))))

(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool bool?))
  (list-val (list list?))
  (non-val)
)

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var string?)
   (val expval?)
   (env environment?))
)

(define (apply-env env search-var)
  (cases environment env
    (empty-env () 'not-found)
    (extend-env (var, val, env2) (if (equal? var search-var) val (apply-env env2 search-var)))))

(define (value-of-exp exp env)
  (cases expression exp
    (an-expression (disjunction1) (value-of-disj disjunction1 env))))

(define (value-of-disj disj env)
  (cases disjunction disj
    (a-conjunction (conjunction1) (value-of-conj conjunction1 env))
    (multiple-conjunctions (disj, conj) (let ([val1 (expval->bool (value-of-disj disj env))]     ;TODO inja age env taghir kone masalan too ye tabei hame chi avaz mishe
                                              [val2 (expval->bool (value-of-conj conj env))]
                                              (bool-val (or val1 val2))
                                            )))))

(define (value-of-conj conj env)
  (cases conjunction conj
    (an-inversion (inv) (value-of-inv inv env))
    (multiple-inversions (conj,inv) (let ([val1 (expval->bool (value-of-conj conj env))]     ;TODO inja age env taghir kone masalan too ye tabei hame chi avaz mishe
                                          [val2 (expval->bool (value-of-inv inv env))]
                                          (bool-val (and val1 val2))
                                       )))))

(define (value-of-inv inv env)
  (cases inversion inv
    (a-comparison (comp) (value-of-comp comp env))
    (not-inversion (inv) (let ([val (expval->bool (value-of-inv inv env))])
                           (bool-val (not val))))))

(define (value-of-comp comp env)
  (cases comparison comp
    (a-sum (sm) (value-of-sum sm env))
    (a-sum-compare (sm, comp-op-sm-ps) 0))) ;;TODO ino nemifahmam aslan bayad chi khorooji bede

(define (value-of-sum sm env)
  (cases sum sm
    (plus-sum (s, t) (let ([val1 (expval->num (value-of-sum s env))]
                           [val2 (expval->num (value-of-term t env))])
                       (num-val (+ val1 val2))))
    (minus-sum (s, t) (let ([val1 (expval->num (value-of-sum s env))]
                           [val2 (expval->num (value-of-term t env))])
                       (num-val (- val1 val2))))
    (a-term (t) (value-of-term t env))))

(define (value-of-term t env)
  (cases term t
    (mul-term (t, f) (let ([val1 (expval->num (value-of-term t env))]
                           [val2 (expval->num (value-of-factor f env))])
                       (num-val (* val1 val2))))
    (div-term (t, f) (let ([val1 (expval->num (value-of-term t env))]
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
    (a-power (a, f) (let ([val1 (expval->num (value-of-atom a env))]
                          [val2 (expval->num (value-of-factor f env))])
                      (num-val (expt val1 val2))))
    (a-primary (p) (value-of-primary p env))))

(define (value-of-primary p env)
  (cases primary p
    (an-atom (a) (value-of-atom a env))
    (an-array-ref (p2 exp) (let ([l (expval->list (value-of-primary p2 env))]
                                 [ref (expval->num (value-of-exp exp env))])
                             (deref (list-ref l ref))))) 
    ;TODO baghie chizash baraye tabe
)


(define (value-of-atom a env)
  (cases atom a
    (an-id (id) (deref (apply-env env id))));TODO in kollan fk konam tabe biad kharab mishe bekhatere inke env global taghir mikone too tabe
    (true-value () (bool-val #t))
    (false-value () (bool-val #f))
    (none-value () (non-val))
    (a-number (n) (num-val n))
    (a-list (pl) (value-of-plist pl env))))

(define (import-list-to-store l)
  (for/list ([x in l]) (newref x)))

(define (value-of-plist pl env)
  (cases plist pl
    (empty-list () (expval->list '()))
    (non-empty-list (exps) (expval->list (import-list-to-store (value-of-exps exps env))))))

(define (value-of-exps exps env) ;khoroojish ye list mamoolie ke javabe exps ha tooshe
  (cases expressions exps
    (only-expression (exp) (list (value-of-exp exp env)))
    (multiple-expression (exps,exp) (let ([l (value-of-exps exps env)]
                                          [val (value-of-exp exp env)])
                                      (append l (list val))))))
    
    
    