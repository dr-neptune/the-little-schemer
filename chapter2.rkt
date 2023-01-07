#lang racket
(require racket)

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (lat? ls)
  (andmap atom? ls))

(define (lat? ls)
  (match ls
    ['() #t]
    [(list (? atom?) ...) (lat? (cdr ls))]
    [_ #f]))

(define (lat? ls)
  (cond [(empty? ls) #t]
        [(atom? (car ls)) (lat? (cdr ls))]
        [else #f]))

(lat? '(jack sprat could eat no chicken fat))
(lat? '((jack) sprat could eat no chicken fat))
(lat? '())

;; book version
(define (lat? l)
  (cond [(null? l) #t]
        [(atom? (car l)) (lat? (cdr l))]
        [else #f]))

(lat? '(bacon and eggs))

;; lat? recurses over a list and checks if each element is an atom

(define (lat? ls)
  (match ls
    ['() #t]
    [(cons (? atom?) tail) (lat? tail)]
    [_ #f]))

(define (member? a lat)
  (match lat
    ['() #f]
    [(list (? (λ (v) (equal? a v))) tail) #t]
    [_ (member? a (rest lat))]))

(define (member? a lat)
  (cond [(null? lat) #f]
        [else (or (eq? (car lat) a)
                  (member? a (cdr lat)))]))

(member? 'tea '(coffee tea macha))
(member? 'tea '(coffee black-tea macha))
(member? 'tea '())
