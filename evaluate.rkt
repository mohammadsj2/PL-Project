#lang racket

(require (file "file_reader.rkt"))
(require (file "lazy-eval.rkt"))
(require (lib "eopl.ss" "eopl"))
(require (file "parser.rkt"))


(define (evaluate addr)
  (begin
    (displayln (string-append "Evaluating: " addr))
    (define lex-this (lambda (lexer input) (lambda () (lexer input))))
    (define my-lexer (lex-this simple-math-lexer (open-input-string (read-instructions-from-file addr))))
    (let ((parser-res (simple-math-parser my-lexer)))
      parser-res
      (run-program parser-res (empty-env)))
    )
  )

;(evaluate "Tests/program1.txt")
;(evaluate "Tests/program2.txt")
(define ev (evaluate "Tests/global.txt"))
