#lang racket
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (append-strings lines)
  (cond
    ((null? lines) "")
    (else
     (string-append (car lines) (append-strings (cdr lines)))
     )
    )
  )

(define (read-instructions-from-file input_addr)
    (append-strings (file->lines input_addr))
  )


(read-instructions-from-file "my_file.txt")