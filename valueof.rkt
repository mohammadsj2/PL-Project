#lang racket
(require (file "parser.rkt"))
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "a=1+2;")))
(let ((parser-res (simple-math-parser my-lexer))) parser-res)
