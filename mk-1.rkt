#lang racket
(require racket/class)

(define book-class%
  (class object%
    (field (pages 5))
    (define/public (letters)
      (* pages 500))
    (super-new)))

(send (new book-class%) letters)
