#lang racket
(provide read-instructions-from-file)

(define (read-instructions-from-file input_addr)
    (apply string-append (file->lines input_addr))
  )

;(read-instructions-from-file "program.txt")