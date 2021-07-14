#lang racket

(require (file "file_reader.rkt"))
(require (file "translator.rkt"))
(require (lib "eopl.ss" "eopl"))
(require (file "parser.rkt"))


(define (evaluate addr)
  (begin
    (define lex-this (lambda (lexer input) (lambda () (lexer input))))
    (define my-lexer (lex-this simple-math-lexer (open-input-string (read-instructions-from-file addr))))
    (let ((parser-res (simple-math-parser my-lexer)))
      parser-res
      (run-program parser-res (empty-env)))
    )
  )

(evaluate "program.txt")
